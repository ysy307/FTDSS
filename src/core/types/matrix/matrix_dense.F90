submodule(core_types_matrix) core_types_matrix_dense
    implicit none

contains

    !----------------------------------------------------------
    ! 初期化
    !----------------------------------------------------------
    module subroutine initialize_dense(self, num_nodes, row, col)
        implicit none
        class(type_dense), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in), optional :: row(:)
        integer(int32), intent(in), optional :: col(:)

        self%num_row = num_nodes
        self%num_col = num_nodes
        call allocate_array(self%val, self%num_row, self%num_col)
        self%val = 0.0d0
    end subroutine initialize_dense

    !----------------------------------------------------------
    ! メモリ解放
    !----------------------------------------------------------
    module subroutine destroy_dense(self)
        implicit none
        class(type_dense), intent(inout) :: self

        call deallocate_array(self%val)
        self%num_row = 0
        self%num_col = 0
    end subroutine destroy_dense

    module pure function get_num_row_dense(self) result(num_row)
        implicit none
        class(type_dense), intent(in) :: self
        integer(int32) :: num_row

        num_row = self%num_row

    end function get_num_row_dense

    module pure function get_num_col_dense(self) result(num_col)
        implicit none
        class(type_dense), intent(in) :: self
        integer(int32) :: num_col

        num_col = self%num_col

    end function get_num_col_dense

    !----------------------------------------------------------
    ! 要素を設定
    !----------------------------------------------------------
    module subroutine set_value_dense(self, row, col, value)
        implicit none
        class(type_dense), intent(inout) :: self
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value

        self%val(row, col) = value
    end subroutine set_value_dense

    !----------------------------------------------------------
    ! 全要素を一括設定
    !----------------------------------------------------------
    module subroutine set_all_dense(self, value)
        class(type_dense), intent(inout) :: self
        real(real64), intent(in) :: value

        self%val = value
    end subroutine set_all_dense

    !----------------------------------------------------------
    ! 全要素をゼロにする
    !----------------------------------------------------------
    module subroutine zero_dense(self)
        implicit none
        class(type_dense), intent(inout) :: self

        self%val = 0.0d0
    end subroutine zero_dense

    !----------------------------------------------------------
    ! 要素に値を加算
    !----------------------------------------------------------
    module subroutine add_value_dense(self, row, col, value)
        implicit none
        class(type_dense), intent(inout) :: self
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value

        self%val(row, col) = self%val(row, col) + value
    end subroutine add_value_dense

    !----------------------------------------------------------
    ! 行列加算: C = alpha*A + B
    ! Aがselfとなり、Cが更新される
    !----------------------------------------------------------
    module subroutine add_matrix_dense(self, alpha, B, C)
        implicit none
        class(type_dense), intent(in) :: self ! This is matrix A
        real(real64), intent(in) :: alpha
        class(abst_matrix), intent(in) :: B
        class(abst_matrix), intent(inout) :: C

        select type (B_dense => B)
        type is (type_dense)
            select type (C_dense => C)
            type is (type_dense)
#ifdef USE_DEBUG
                ! 次元チェック
                if (any([self%num_row, self%num_col] /= [B_dense%num_row, B_dense%num_col]) .or. &
                    any([self%num_row, self%num_col] /= [C_dense%num_row, C_dense%num_col])) then
                    print *, "ERROR(add_matrix_dense): Matrix dimensions do not match."
                    stop
                end if
#endif
                C_dense%val = alpha * self%val + B_dense%val
            end select
        end select
    end subroutine add_matrix_dense

    subroutine gemv_dense(self, alpha, x, beta, y)
        implicit none
        class(type_dense), intent(in) :: self
        real(real64), intent(in) :: alpha
        real(real64), intent(in) :: x(:)
        real(real64), intent(in) :: beta
        real(real64), intent(inout) :: y(:)

#ifdef _MKL
        interface
            subroutine dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
                use, intrinsic :: iso_fortran_env
                implicit none
                character(len=1), intent(in) :: trans
                integer, intent(in) :: m, n, lda, incx, incy
                real(real64), intent(in) :: alpha, beta
                real(real64), intent(in) :: a(lda, *), x(*), y(*)
            end subroutine dgemv
        end interface

        call dgemv('N', self%num_row, self%num_col, alpha, self%val, self%num_row, x, 1, beta, y, 1)
#else
        integer(int32) :: i

        !$omp parallel do private(i)
        do i = 1, self%num_row
            y(i) = alpha * dot_product(self%val(i, :), x) + beta * y(i)
        end do
        !$omp end parallel do
#endif

    end subroutine gemv_dense

    !----------------------------------------------------------
    ! 行列を表示
    !----------------------------------------------------------
    module subroutine display_dense(self)
        implicit none
        class(type_dense), intent(in) :: self
        integer :: i
        if (.not. allocated(self%val)) then
            print *, "Matrix is not allocated."
            return
        end if
        print '("Matrix (", i0, "x", i0, "):")', self%num_row, self%num_col
        do i = 1, self%num_row
            write (*, '(10(es12.4e2))') self%val(i, :)
        end do
    end subroutine display_dense

end submodule core_types_matrix_dense
