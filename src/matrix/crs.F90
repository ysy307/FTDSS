module Matrix_CRS
    use, intrinsic :: iso_fortran_env
    use :: stdlib_sorting, only:sort
    use :: module_core, only:allocate_array, deallocate_array
    use :: module_domain, only:type_domain
    use :: matrix_coo, only:type_coo
    implicit none
    private

    public :: type_crs
    public :: operator(*)
    public :: operator(+)
    ! public :: Transpose_CRS

    type :: type_crs
        integer(int32) :: nnz ! number of non-zero elements
        integer(int32) :: num_row ! number of rows
        integer(int32) :: num_ptr ! size of ptr (num_row+1 entries)
        integer(int32), allocatable :: ptr(:) ! pointers to row starts (1-based)
        integer(int32), allocatable :: ind(:) ! column indices of non-zeros
        real(real64), allocatable :: val(:) ! non-zero values
    contains
        procedure, public, pass(self) :: initialize => initialize_type_crs
        procedure, public, pass(self) :: find => type_crs_find
        procedure, public, pass(self) :: Copy => type_crs_copy
    end type type_crs

    interface operator(*)
        module procedure type_crs_matrix_vector_product
        module procedure Multiplication_Scalar_Matrix_CRS
        module procedure Multiplication_Matrix_Scalar_CRS
    end interface
    interface operator(+)
        module procedure Matrix_Addition_CRS
    end interface

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

        call coo%destory()

    end subroutine initialize_type_crs

    function type_crs_matrix_vector_product(A, x) result(y)
        implicit none
        type(type_crs), intent(in) :: A
        real(real64), intent(in) :: x(A%num_row)
        real(real64) :: y(A%num_row)
        integer(int32) :: i, j, is, ie
        real(real64) :: sum

        y(:) = 0.0d0
        do i = 1, A%num_row
            sum = 0.0d0
            is = A%ptr(i)
            ie = A%ptr(i + 1) - 1
            do j = is, ie
                sum = sum + A%val(j) * x(A%ind(j))
            end do
            y(i) = sum
        end do
    end function type_crs_matrix_vector_product

    function Matrix_Addition_CRS(A, B) result(C)
        implicit none
        type(type_crs), intent(in) :: A, B
        type(type_crs) :: C
        integer(int32) :: k

        ! Assume same sparsity structure
        C = A%Copy()
        do k = 1, A%nnz
            C%val(k) = A%val(k) + B%val(k)
        end do
    end function Matrix_Addition_CRS

    function Multiplication_Scalar_Matrix_CRS(A, b) result(C)
        implicit none
        type(type_crs), intent(in) :: A
        real(real64), intent(in) :: b
        type(type_crs) :: C
        integer(int32) :: k

        ! Assume same sparsity structure
        C = A%Copy()
        do k = 1, A%nnz
            C%val(k) = A%val(k) * b
        end do
    end function Multiplication_Scalar_Matrix_CRS

    function Multiplication_Matrix_Scalar_CRS(a, B) result(C)
        implicit none
        real(real64), intent(in) :: a
        type(type_crs), intent(in) :: B
        type(type_crs) :: C
        integer(int32) :: k

        ! Assume same sparsity structure
        C = B%Copy()
        do k = 1, B%nnz
            C%val(k) = B%val(k) * a
        end do
    end function Multiplication_Matrix_Scalar_CRS

    subroutine type_crs_find(self, column, indes, location)
        implicit none
        class(type_crs), intent(in) :: self
        integer(int32), intent(in) :: column, indes
        integer(int32), intent(out) :: location

        integer(int32) :: low, high, mid

        location = 0 ! 見つからなかった場合のデフォルト値

        ! 検索範囲を設定
        low = self%ptr(column)
        high = self%ptr(column + 1) - 1

        ! 範囲が存在しない場合は終了
        if (low > high) return

        ! 二分探索
        do while (low <= high)
            mid = low + (high - low) / 2 ! オーバーフローを防ぐための計算

            if (self%ind(mid) < indes) then
                low = mid + 1
            else if (self%ind(mid) > indes) then
                high = mid - 1
            else
                location = mid
                return
            end if
        end do

    end subroutine type_crs_find

    function type_crs_copy(self) result(B)
        implicit none
        class(type_crs) :: self
        type(type_crs) :: B

        B%num_row = self%num_row
        B%num_ptr = self%num_ptr
        B%nnz = self%nnz
        call allocate_array(B%ptr, self%num_ptr)
        call allocate_array(B%ind, self%nnz)
        call allocate_array(B%val, self%nnz)
        B%ptr(:) = self%ptr(:)
        B%ind(:) = self%ind(:)
        B%val(:) = self%val(:)
    end function type_crs_copy

end module Matrix_CRS
