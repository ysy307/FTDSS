module matrix_coo
    use, intrinsic :: iso_fortran_env
    use :: module_core, only:allocate_array, deallocate_array, unique
    use :: module_domain, only:type_domain
    use :: matrix_base, only:abst_matrix
    implicit none
    private

    public :: type_coo
    public :: type_coo_gemv
    public :: type_coo_add

    type, extends(abst_matrix) :: type_coo
        integer(int32) :: nnz = 0 ! number of non-zero elements
        integer(int32), allocatable :: row(:)
        integer(int32), allocatable :: col(:)
        real(real64), allocatable :: val(:) ! non-zero values
    contains
        procedure, public, pass(self) :: initialize => initialize_type_coo
        procedure, public, pass(self) :: find => find_coo
        procedure, public, pass(self) :: destroy => destroy_coo
    end type

contains

    subroutine initialize_type_coo(self, domain)
        implicit none
        class(type_coo), intent(inout) :: self
        type(type_domain), intent(inout) :: domain

        ! --- ローカル変数宣言 ---
        integer(int32) :: num_nodes, num_elements, num_sides, computation_dimension
        integer(int32) :: local_nodes
        integer(int32) :: num_total_cell_nodes
        integer(int32) :: i, j, k
        integer(int32) :: counter
        integer(int32), allocatable :: row_indices(:), col_indices(:)
        ! --- Variables for sorting and finding uniques ---
        integer(int64), allocatable :: packed_indices(:), unique_packed_indices(:)
        integer(int32) :: unique_idx
        integer(int64) :: last_key

        ! --- 初期設定 ---
        num_nodes = domain%get_num_nodes()
        num_sides = domain%get_num_sides()
        num_elements = domain%get_num_elements()

        self%nnz = 0

        ! --- 計算対象となる最大次元より低い次元のセルノード数を計算 ---
        ! これが最大のセルノード数となる
        num_total_cell_nodes = 0
        computation_dimension = domain%get_computation_dimension()
        if (computation_dimension == 3) then
            ! (3D implementation would go here)
        end if
        if (computation_dimension == 2) then
            do i = 1, num_elements
                num_total_cell_nodes = num_total_cell_nodes + domain%elements(i)%e%get_num_nodes()**2
            end do
            do i = 1, num_sides
                num_total_cell_nodes = num_total_cell_nodes + domain%sides(i)%s%get_num_nodes()**2
            end do
        end if

        if (num_total_cell_nodes <= 0) then
            print *, "Error: No valid nodes found in the domain."
            return
        else
            call allocate_array(row_indices, num_total_cell_nodes)
            call allocate_array(col_indices, num_total_cell_nodes)
        end if

        ! 重複ありですべての配置場所を割り当てる
        counter = 0
        if (computation_dimension == 3) then
            ! (3D implementation would go here)
        end if
        if (computation_dimension == 2) then
            do i = 1, num_elements
                local_nodes = domain%elements(i)%e%get_num_nodes()
                do j = 1, local_nodes
                    do k = 1, local_nodes
                        counter = counter + 1
                        row_indices(counter) = domain%elements(i)%e%connectivity(j)
                        col_indices(counter) = domain%elements(i)%e%connectivity(k)
                    end do
                end do
            end do
            do i = 1, num_sides
                local_nodes = domain%sides(i)%s%get_num_nodes()
                do j = 1, local_nodes
                    do k = 1, local_nodes
                        counter = counter + 1
                        row_indices(counter) = domain%sides(i)%s%connectivity(j)
                        col_indices(counter) = domain%sides(i)%s%connectivity(k)
                    end do
                end do
            end do
        end if

        ! --- 重複を削除し、最終的なCOO構造を構築 ---
        if (counter > 0) then
            ! Pack (row, col) into a single 64-bit integer for sorting
            call allocate_array(packed_indices, int(counter, kind=int64))
            do i = 1, counter
                packed_indices(i) = ishft(int(row_indices(i), int64), 32) + int(col_indices(i), int64)
            end do

            ! Sort the packed indices
            call unique(packed_indices, unique_packed_indices)

            ! --- COO配列を確保し、アンパックして格納 ---
            self%nnz = int(size(unique_packed_indices), kind=int32)
            call allocate_array(self%row, self%nnz)
            call allocate_array(self%col, self%nnz)
            call allocate_array(self%val, self%nnz)
            self%val(:) = 0.0d0

            do i = 1, self%nnz
                self%row(i) = int(ishft(unique_packed_indices(i), -32), kind=int32)
                self%col(i) = int(iand(unique_packed_indices(i), int(z'FFFFFFFF', int64)), kind=int32)
            end do

            ! --- 一時配列の解放 ---
            call deallocate_array(packed_indices)
            call deallocate_array(unique_packed_indices)
        end if

        ! Deallocate temporary arrays
        call deallocate_array(row_indices)
        call deallocate_array(col_indices)

    end subroutine initialize_type_coo

    subroutine find_coo(self, row, col, index)
        implicit none
        class(type_coo), intent(in) :: self
        integer(int32), intent(in) :: row, col
        integer(int32), intent(inout) :: index

        integer(int32) :: i

        if (self%nnz == 0) then
            index = -1
            return
        end if

        ! --- 二分探索で行と列の組み合わせを探す ---
        index = -1
        do i = 1, self%nnz
            if (self%row(i) == row .and. self%col(i) == col) then
                index = i
                return
            end if
        end do
    end subroutine find_coo

    subroutine destroy_coo(self)
        implicit none
        class(type_coo), intent(inout) :: self

        ! --- COO構造の解放 ---
        call deallocate_array(self%row)
        call deallocate_array(self%col)
        call deallocate_array(self%val)

        self%nnz = 0
    end subroutine destroy_coo

    subroutine type_coo_gemv(alpha, A, x, beta, y)
        ! y := alpha*A*x + beta*y
        use omp_lib
        implicit none
        real(real64), intent(in) :: alpha
        type(type_coo), intent(in) :: A
        real(real64), intent(in) :: x(:)
        real(real64), intent(in) :: beta
        real(real64), intent(inout) :: y(:)

        integer(int32) :: i

        ! y := beta*y
        !$omp parallel do default(shared) private(i)
        do i = 1, size(y)
            y(i) = beta * y(i)
        end do
        !$omp end parallel do

        ! y += alpha*A*x
        !$omp parallel do default(shared) private(i)
        do i = 1, A%nnz
            !$omp atomic
            y(A%row(i)) = y(A%row(i)) + alpha * A%val(i) * x(A%col(i))
        end do
        !$omp end parallel do

    end subroutine type_coo_gemv

    subroutine type_coo_add(alpha, A, B, C)
        ! C := alpha*A + B
        implicit none
        real(real64), intent(in) :: alpha
        type(type_coo), intent(in) :: A
        type(type_coo), intent(in) :: B
        type(type_coo), intent(inout) :: C

        integer(int32) :: i
        real(real64), allocatable :: tmp(:)

        call allocate_array(tmp, A%nnz)

        do i = 1, A%nnz
            tmp(i) = alpha * A%val(i) + B%val(i)
        end do

        do i = 1, A%nnz
            C%val(i) = tmp(i)
        end do

        call deallocate_array(tmp)
    end subroutine type_coo_add

end module matrix_coo
