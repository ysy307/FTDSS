submodule(core_types_matrix) core_types_matrix_crs
    implicit none

contains

    module subroutine initialize_type_crs(self, num_nodes, row, col)
        implicit none
        class(type_crs), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in), optional :: row(:) ! CRSではptr配列に相当
        integer(int32), intent(in), optional :: col(:) ! CRSではind配列に相当

        if (.not. present(row) .or. .not. present(col)) then
            print *, "Error: row(ptr) and col(ind) must be provided for CRS initialization."
            stop
        end if

        self%num_row = num_nodes
        self%num_ptr = num_nodes + 1
        self%nnz = size(col)

        call allocate_array(self%ptr, self%num_ptr + 1)
        self%ptr = row(:)

        call allocate_array(self%ind, self%nnz)
        self%ind = col(:)

        call allocate_array(self%val, self%nnz)
        self%val = 0.0d0
    end subroutine initialize_type_crs

    module pure function get_nnz_crs(self) result(nnz)
        implicit none
        class(type_crs), intent(in) :: self
        integer(int32) :: nnz

        nnz = self%nnz

    end function get_nnz_crs

    module pure function get_num_ptr_crs(self) result(num_ptr)
        implicit none
        class(type_crs), intent(in) :: self
        integer(int32) :: num_ptr

        num_ptr = self%num_ptr

    end function get_num_ptr_crs

    module pure function get_num_row_crs(self) result(num_row)
        implicit none
        class(type_crs), intent(in) :: self
        integer(int32) :: num_row

        num_row = self%num_row

    end function get_num_row_crs

    module function get_ptr_crs(self) result(ptr)
        implicit none
        class(type_crs), intent(in), target :: self
        integer(int32), dimension(:), pointer :: ptr

        ptr = self%ptr

    end function get_ptr_crs

    module function get_ind_crs(self) result(ind)
        implicit none
        class(type_crs), intent(in), target :: self
        integer(int32), dimension(:), pointer :: ind

        ind = self%ind

    end function get_ind_crs

    module function get_val_crs(self) result(val)
        implicit none
        class(type_crs), intent(in), target :: self
        real(real64), dimension(:), pointer :: val

        val = self%val

    end function get_val_crs

    module pure function find_crs(self, row, col) result(index)
        implicit none
        class(type_crs), intent(in) :: self
        integer(int32), intent(in) :: row, col
        integer(int32) :: index
        integer(int32) :: lo, hi, mid

        index = 0
        if (row > self%num_row .or. row < 1) return

        lo = self%ptr(row)
        hi = self%ptr(row + 1) - 1

        if (lo > hi) return

        ! 二分探索で列インデックスを探す
        do while (lo <= hi)
            mid = lo + (hi - lo) / 2
            if (self%ind(mid) == col) then
                index = mid
                return
            else if (self%ind(mid) < col) then
                lo = mid + 1
            else
                hi = mid - 1
            end if
        end do
    end function find_crs

    module subroutine set_crs(self, row, col, value)
        implicit none
        class(type_crs), intent(inout) :: self
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
            print *, "Warning(set_crs): Element not in sparsity pattern.", row, col
        end if
#endif
    end subroutine set_crs

    module subroutine set_all_crs(self, value)
        implicit none
        class(type_crs), intent(inout) :: self
        real(real64), intent(in) :: value

        self%val = value
    end subroutine set_all_crs

    module subroutine set_row_crs(self, row, value)
        implicit none
        class(type_crs), intent(inout) :: self
        integer(int32), intent(in) :: row
        real(real64), intent(in) :: value

        integer(int32) :: is, ie

        is = self%ptr(row)
        ie = self%ptr(row + 1) - 1
        self%val(is:ie) = value
    end subroutine set_row_crs

    module subroutine zero_crs(self)
        implicit none
        class(type_crs), intent(inout) :: self

        self%val = 0.0d0
    end subroutine zero_crs

    module subroutine add_crs(self, row, col, value)
        implicit none
        class(type_crs), intent(inout) :: self
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
            print *, "Warning(add_crs): Element not in sparsity pattern.", row, col
        end if
#endif
    end subroutine add_crs

    module subroutine add_matrix_crs(self, alpha, B, C)
        implicit none
        class(type_crs), intent(in) :: self ! This is matrix A
        real(real64), intent(in) :: alpha
        class(abst_matrix), intent(in) :: B
        class(abst_matrix), intent(inout) :: C
        ! NOTE: これはA, B, Cが全く同じ非ゼロパターンを持つという
        !       非常に強い仮定の下での簡易的な実装です。

        select type (B_crs => B)
        type is (type_crs)
            select type (C_crs => C)
            type is (type_crs)
                if (self%nnz /= B_crs%nnz .or. self%nnz /= C_crs%nnz) then
                    print *, "ERROR(add_matrix_crs): In this simplified version, NNZ must be identical."
                    stop
                end if
                C_crs%val = alpha * self%val + B_crs%val
            end select
        end select
    end subroutine add_matrix_crs

    module subroutine gemv_crs(self, alpha, x, beta, y)
        ! y := alpha*A*x + beta*y
        implicit none
        class(type_crs), intent(in) :: self
        real(real64), intent(in) :: alpha
        real(real64), intent(in) :: x(:)
        real(real64), intent(in) :: beta
        real(real64), intent(inout) :: y(:)

        integer(int32) :: i, j, is, ie
        real(real64) :: sum

        !$omp parallel do private(i, j, is, ie, sum)
        do i = 1, self%num_row
            sum = 0.0d0
            is = self%ptr(i)
            ie = self%ptr(i + 1) - 1
            do j = is, ie
                sum = sum + self%val(j) * x(self%ind(j))
            end do
            y(i) = alpha * sum + beta * y(i)
        end do
        !$omp end parallel do

    end subroutine gemv_crs

    module subroutine display_crs(self)
        implicit none
        class(type_crs), intent(in) :: self
        integer(int32) :: i, r
        integer(int32) :: row_start, row_end

        print *, "CRS Matrix (dims=", self%num_row, "x?", ", nnz=", self%nnz, ")"
        do r = 1, self%num_row
            row_start = self%ptr(r)
            row_end = self%ptr(r + 1) - 1
            do i = row_start, row_end
                write (*, '(2(i8, ", "), es16.8)') r, self%ind(i), self%val(i)
            end do
        end do
    end subroutine display_crs

    module subroutine destroy_crs(self)
        implicit none
        class(type_crs), intent(inout) :: self

        call deallocate_array(self%ptr)
        call deallocate_array(self%ind)
        call deallocate_array(self%val)
        self%nnz = 0
        self%num_row = 0
    end subroutine destroy_crs

end submodule core_types_matrix_crs
