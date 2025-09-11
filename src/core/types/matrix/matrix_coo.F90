submodule(core_types_matrix) core_types_matrix_coo

    implicit none

contains

    module subroutine initialize_type_coo(self, num_nodes, row, col)
        implicit none
        class(type_coo), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in), optional :: row(:)
        integer(int32), intent(in), optional :: col(:)

        if (.not. present(row) .or. .not. present(col)) then
            print *, "Error: row and col must be provided for COO initialization."
            stop
        end if

        self%num_row = num_nodes
        self%num_col = num_nodes
        self%nnz = size(row)

        if (self%nnz > 0) then
            call allocate_array(self%row, self%nnz)
            self%row = row(:)
            call allocate_array(self%col, self%nnz)
            self%col = col(:)
            call allocate_array(self%val, self%nnz)
            self%val = 0.0d0
        end if
    end subroutine initialize_type_coo

    module pure function get_nnz_coo(self) result(nnz)
        implicit none
        class(type_coo), intent(in) :: self
        integer(int32) :: nnz

        nnz = self%nnz

    end function get_nnz_coo

    module pure function get_num_row_coo(self) result(num_row)
        implicit none
        class(type_coo), intent(in) :: self
        integer(int32) :: num_row

        num_row = self%num_row

    end function get_num_row_coo

    module pure function get_num_col_coo(self) result(num_col)
        implicit none
        class(type_coo), intent(in) :: self
        integer(int32) :: num_col

        num_col = self%num_col

    end function get_num_col_coo

    module function get_row_coo(self) result(row)
        implicit none
        class(type_coo), intent(in), target :: self
        integer(int32), dimension(:), pointer :: row

        row => self%row

    end function get_row_coo

    module function get_col_coo(self) result(col)
        implicit none
        class(type_coo), intent(in), target :: self
        integer(int32), dimension(:), pointer :: col

        col => self%col

    end function get_col_coo

    module function get_val_coo(self) result(val)
        implicit none
        class(type_coo), intent(in), target :: self
        real(real64), dimension(:), pointer :: val

        val => self%val

    end function get_val_coo

    module pure function find_coo(self, row, col) result(index)
        implicit none
        class(type_coo), intent(in) :: self
        integer(int32), intent(in) :: row, col
        integer(int32) :: index, i

        index = 0
        do i = 1, self%nnz
            if (self%row(i) == row .and. self%col(i) == col) then
                index = i
                return
            end if
        end do
    end function find_coo

    module subroutine set_coo(self, row, col, value)
        implicit none
        class(type_coo), intent(inout) :: self
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value
        integer(int32) :: index

        index = self%find(row, col)
#ifdef USE_DEBUG
        if (index > 0) then
#endif
            self%val(index) = value
#ifdef USE_DEBUG
        else
            print *, "Warning(set_coo): Element not in sparsity pattern.", row, col
        end if
#endif
    end subroutine set_coo

    module subroutine set_row_coo(self, row, value)
        implicit none
        class(type_coo), intent(inout) :: self
        integer(int32), intent(in) :: row
        real(real64), intent(in) :: value

        integer(int32) :: i
        do i = 1, self%nnz
            if (self%row(i) == row) then
                self%val(i) = value
            end if
        end do

    end subroutine set_row_coo

    module subroutine set_all_coo(self, value)
        implicit none
        class(type_coo), intent(inout) :: self
        real(real64), intent(in) :: value

        self%val = value
    end subroutine set_all_coo

    module subroutine zero_coo(self)
        implicit none
        class(type_coo), intent(inout) :: self

        self%val = 0.0d0
    end subroutine zero_coo

    module subroutine add_coo(self, row, col, value)
        implicit none
        class(type_coo), intent(inout) :: self
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value
        integer(int32) :: index

        index = self%find(row, col)
#ifdef USE_DEBUG
        if (index > 0) then
#endif
            self%val(index) = self%val(index) + value
#ifdef USE_DEBUG
        else
            print *, "Warning(add_coo): Element not in sparsity pattern.", row, col
        end if
#endif
    end subroutine add_coo

    module subroutine add_matrix_coo(self, alpha, B, C)
        implicit none
        class(type_coo), intent(in) :: self ! This is matrix A
        real(real64), intent(in) :: alpha
        class(abst_matrix), intent(in) :: B
        class(abst_matrix), intent(inout) :: C
        ! NOTE: これはA, B, Cが全く同じ非ゼロパターンを持つという
        !       非常に強い仮定の下での簡易的な実装です。
        !       実用的な疎行列の加算はより複雑なアルゴリズムを要します。
        select type (B_coo => B)
        type is (type_coo)
            select type (C_coo => C)
            type is (type_coo)
                if (self%nnz /= B_coo%nnz .or. self%nnz /= C_coo%nnz) then
                    print *, "ERROR(add_matrix_coo): In this simplified version, NNZ must be identical."
                    stop
                end if
                ! この単純な操作は、row/colのパターンが完全に一致する場合にのみ有効
                C_coo%val = alpha * self%val + B_coo%val
            end select
        end select
    end subroutine add_matrix_coo

    module subroutine gemv_coo(self, alpha, x, beta, y)
        ! y := alpha*A*x + beta*y
        implicit none
        class(type_coo), intent(in) :: self
        real(real64), intent(in) :: alpha
        real(real64), intent(in) :: x(:)
        real(real64), intent(in) :: beta
        real(real64), intent(inout) :: y(:)

        integer(int32) :: i

        !$omp parallel do default(shared) private(i)
        do i = 1, self%nnz
            !$omp atomic
            y(self%row(i)) = alpha * self%val(i) * x(self%col(i)) + beta * y(self%row(i))
        end do
        !$omp end parallel do

    end subroutine gemv_coo

    module subroutine display_coo(self)
        implicit none
        class(type_coo), intent(in) :: self
        integer(int32) :: i

        print *, "COO Matrix (max_dims=", self%num_row, "x", self%num_col, ", nnz=", self%nnz, ")"
        do i = 1, self%nnz
            write (*, '(2(i8, ", "), es16.8)') self%row(i), self%col(i), self%val(i)
        end do
    end subroutine display_coo

    module subroutine destroy_coo(self)
        implicit none
        class(type_coo), intent(inout) :: self
        call deallocate_array(self%row)
        call deallocate_array(self%col)
        call deallocate_array(self%val)
        self%nnz = 0
    end subroutine destroy_coo

end submodule core_types_matrix_coo
