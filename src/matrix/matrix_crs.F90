module matrix_crs
!$  use :: omp_lib
    use, intrinsic :: iso_fortran_env
    use :: stdlib_sorting, only:sort
    use :: module_core, only:allocate_array, deallocate_array
    use :: module_domain, only:type_domain
    use :: matrix_base, only:abst_matrix
    use :: matrix_coo, only:type_coo
    implicit none
    private

    public :: type_crs
    public :: type_crs_gemv
    public :: type_crs_add

    type, extends(abst_matrix) :: type_crs
        integer(int32) :: nnz ! number of non-zero elements
        integer(int32) :: num_row ! number of rows
        integer(int32) :: num_ptr ! size of ptr (num_row+1 entries)
        integer(int32), allocatable :: ptr(:) ! pointers to row starts (1-based)
        integer(int32), allocatable :: ind(:) ! column indices of non-zeros
        real(real64), allocatable :: val(:) ! non-zero values
    contains
        procedure, public, pass(self) :: initialize => initialize_type_crs !&
        procedure, public, pass(self) :: find       => find_crs !&
        procedure, public, pass(self) :: set        => set_crs !&
        procedure, public, pass(self) :: set_all    => set_all_crs !&
        procedure, public, pass(self) :: add        => add_crs !&
        procedure, public, pass(self) :: destroy    => destroy_crs !&
    end type type_crs

contains

    subroutine initialize_type_crs(self, domain)
        implicit none
        class(type_crs), intent(inout) :: self
        type(type_domain), intent(inout) :: domain

        ! --- ローカル変数 ---
        type(type_coo) :: coo
        integer(int32) :: i, r, pos
        integer(int32), allocatable :: rcm_row(:), rcm_col(:)
        integer(int32), allocatable :: write_pos(:)

        ! =================================================================
        ! Step 1: coo%initializeを呼び出し、COO行列を作成する
        ! =================================================================
        call coo%initialize(domain)

        self%nnz = coo%nnz
        self%num_row = domain%get_num_nodes()
        self%num_ptr = self%num_row + 1

        if (self%nnz == 0) then
            call allocate_array(self%ptr, self%num_ptr)
            self%ptr = 1
            return
        end if

        ! =================================================================
        ! Step 2: COOの行・列インデックスをRCM番号に変換
        ! =================================================================
        call allocate_array(rcm_row, self%nnz)
        call allocate_array(rcm_col, self%nnz)

        call domain%reordering%to_reordered(coo%row, rcm_row)
        call domain%reordering%to_reordered(coo%col, rcm_col)

        ! =================================================================
        ! Step 3: RCM適用後のデータから直接CRS行列を構築
        ! =================================================================
        call allocate_array(self%ptr, self%num_ptr)
        call allocate_array(self%ind, self%nnz)
        call allocate_array(self%val, self%nnz)
        self%ptr = 0

        ! -- Pass 1: 各行の非ゼロ要素数をカウント
        do i = 1, self%nnz
            self%ptr(rcm_row(i) + 1) = self%ptr(rcm_row(i) + 1) + 1
        end do

        ! -- Pass 2: 累積和を計算して、各行の開始ポインタを決定
        self%ptr(1) = 1
        do i = 2, self%num_ptr
            self%ptr(i) = self%ptr(i) + self%ptr(i - 1)
        end do

        ! -- Pass 3: indとvalを正しい位置に配置
        call allocate_array(write_pos, self%num_row)
        write_pos(:) = self%ptr(1:self%num_row)

        do i = 1, self%nnz
            r = rcm_row(i)
            pos = write_pos(r)

            self%ind(pos) = rcm_col(i)
            self%val(pos) = 0.0d0

            write_pos(r) = pos + 1
        end do

        call deallocate_array(write_pos)
        call deallocate_array(rcm_row)
        call deallocate_array(rcm_col)

        call coo%destroy()

    end subroutine initialize_type_crs

    pure function find_crs(self, row, col) result(index)
        implicit none
        class(type_crs), intent(in) :: self
        integer(int32), intent(in) :: row
        integer(int32), intent(in) :: col
        integer(int32) :: index

        integer(int32) :: i
        integer(int32) :: search_start, search_end

        index = 0 ! 見つからなかった場合のデフォルト値

        ! 検索範囲を設定
        search_start = self%ptr(row)
        search_end = self%ptr(row + 1) - 1

        ! 範囲が存在しない場合は終了
        if (search_start > search_end) return

        ! 線形探索 (最初から最後まで順番に探す)
        do i = search_start, search_end
            if (self%ind(i) == col) then
                index = i
                return ! 見つかったら即座に終了
            end if
        end do

    end function find_crs

    subroutine set_crs(self, row, col, value)
        implicit none
        class(type_crs), intent(inout) :: self
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value

        integer(int32) :: index

        index = self%find(row, col)
        self%val(index) = value

    end subroutine set_crs

    subroutine set_all_crs(self, value)
        implicit none
        class(type_crs), intent(inout) :: self
        real(real64), intent(in) :: value

        integer(int32) :: i

        do i = 1, self%nnz
            self%val(i) = value
        end do

    end subroutine set_all_crs

    subroutine add_crs(self, row, col, value)
        implicit none
        class(type_crs), intent(inout) :: self
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value

        integer(int32) :: index

        index = self%find(row, col)
        self%val(index) = self%val(index) + value

    end subroutine add_crs

    subroutine destroy_crs(self)
        implicit none
        class(type_crs), intent(inout) :: self

        call deallocate_array(self%ptr)
        call deallocate_array(self%ind)
        call deallocate_array(self%val)

        self%nnz = 0
        self%num_row = 0
        self%num_ptr = 0
    end subroutine destroy_crs

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Matrix calculation
    !-------------------------------------------------------------------------------------------------------------------------------
    subroutine type_crs_gemv(alpha, A, x, beta, y)
        ! y := alpha*A*x + beta*y
        implicit none
        real(real64), intent(in) :: alpha
        type(type_crs), intent(in) :: A
        real(real64), intent(in) :: x(:)
        real(real64), intent(in) :: beta
        real(real64), intent(inout) :: y(:)

        integer(int32) :: i, j, is, ie
        real(real64) :: sum

        !$omp parallel do private(i, j, is, ie, sum)
        do i = 1, A%num_row
            sum = 0.0d0
            is = A%ptr(i)
            ie = A%ptr(i + 1) - 1
            do j = is, ie
                sum = sum + A%val(j) * x(A%ind(j))
            end do
            y(i) = alpha * sum + beta * y(i)
        end do
        !$omp end parallel do

    end subroutine type_crs_gemv

    subroutine type_crs_add(alpha, A, B, C)
        ! C := alpha*A + B
        !
        ! [ATTENTION] Assumes A, B, and C have the exact same sparsity pattern.
        !
        implicit none
        real(real64), intent(in) :: alpha
        type(type_crs), intent(in) :: A
        type(type_crs), intent(in) :: B
        type(type_crs), intent(inout) :: C

        integer(int32) :: i

        !$omp parallel do
        do i = 1, A%nnz
            C%val(i) = alpha * A%val(i) + B%val(i)
        end do
        !$omp end parallel do

    end subroutine type_crs_add

end module matrix_crs
