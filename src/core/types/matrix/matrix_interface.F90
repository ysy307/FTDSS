module core_types_matrix
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    public :: abst_matrix

    type, abstract :: abst_matrix
        ! Abstract base type for matrices.
    contains
        procedure(abst_initialize), pass(self), deferred :: initialize !&
        procedure(abst_find),       pass(self), deferred :: find !&
        procedure(abst_set),        pass(self), deferred :: set !&
        procedure(abst_set_all),    pass(self), deferred :: set_all !&
        procedure(abst_zero),       pass(self), deferred :: zero !&
        procedure(abst_add),        pass(self), deferred :: add !&
        procedure(abst_display),    pass(self), deferred :: display !&
        procedure(abst_destroy),    pass(self), deferred :: destroy !&
    end type abst_matrix

    abstract interface
        subroutine abst_initialize(self, num_nodes, row, col)
            import :: abst_matrix, int32
            implicit none
            class(abst_matrix), intent(inout) :: self
            integer(int32), intent(in) :: num_nodes
            integer(int32), intent(in), optional :: row(:)
            integer(int32), intent(in), optional :: col(:)

        end subroutine abst_initialize

        pure function abst_find(self, row, col) result(index)
            import :: abst_matrix, int32
            implicit none
            class(abst_matrix), intent(in) :: self
            integer(int32), intent(in) :: row
            integer(int32), intent(in) :: col
            integer(int32) :: index

        end function abst_find

        subroutine abst_set(self, row, col, value)
            import :: abst_matrix, int32, real64
            implicit none
            class(abst_matrix), intent(inout) :: self
            integer(int32), intent(in) :: row, col
            real(real64), intent(in) :: value

        end subroutine abst_set

        subroutine abst_set_all(self, value)
            import :: abst_matrix, real64
            implicit none
            class(abst_matrix), intent(inout) :: self
            real(real64), intent(in) :: value

        end subroutine abst_set_all

        subroutine abst_zero(self)
            import :: abst_matrix
            implicit none
            class(abst_matrix), intent(inout) :: self

        end subroutine abst_zero

        subroutine abst_add(self, row, col, value)
            import :: abst_matrix, int32, real64
            implicit none
            class(abst_matrix), intent(inout) :: self
            integer(int32), intent(in) :: row, col
            real(real64), intent(in) :: value

        end subroutine abst_add

        subroutine abst_display(self)
            import :: abst_matrix
            implicit none
            class(abst_matrix), intent(in) :: self

        end subroutine abst_display

        subroutine abst_destroy(self)
            import :: abst_matrix
            implicit none
            class(abst_matrix), intent(inout) :: self

        end subroutine abst_destroy
    end interface

end module core_types_matrix
