module matrix_base
    use, intrinsic :: iso_fortran_env
    use :: module_domain, only:type_domain
    implicit none
    private

    public :: abst_matrix

    type, abstract :: abst_matrix
        ! Abstract base type for matrices.
    contains
        procedure(abst_initialize), pass(self), deferred :: initialize
        procedure(abst_find), pass(self), deferred :: find
        ! procedure(abst_copy), pass(self), deferred :: copy
        procedure(abst_destroy), pass(self), deferred :: destroy
    end type abst_matrix

    abstract interface
        subroutine abst_initialize(self, domain)
            import :: abst_matrix, type_domain
            implicit none
            class(abst_matrix), intent(inout) :: self
            type(type_domain), intent(inout) :: domain

        end subroutine abst_initialize

        subroutine abst_find(self, row, col, index)
            import :: abst_matrix, int32
            implicit none
            class(abst_matrix), intent(in) :: self
            integer(int32), intent(in) :: row, col
            integer(int32), intent(inout) :: index
        end subroutine abst_find

        ! function abst_copy(self) result(new_matrix)
        !     import :: abst_matrix
        !     implicit none
        !     class(abst_matrix), intent(in) :: self
        !     class(abst_matrix), allocatable :: new_matrix
        ! end function abst_copy

        subroutine abst_destroy(self)
            import :: abst_matrix
            implicit none
            class(abst_matrix), intent(inout) :: self
        end subroutine abst_destroy
    end interface

end module matrix_base
