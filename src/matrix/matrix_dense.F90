module matrix_dense
    use, intrinsic :: iso_fortran_env
    use :: module_core, only:allocate_array, deallocate_array, unique
    use :: module_domain, only:type_domain
    use :: matrix_base, only:abst_matrix
    implicit none
    private

    public :: type_dense

    type, extends(abst_matrix) :: type_dense
        real(real64), allocatable :: val(:, :)
    contains
        procedure, public, pass(self) :: initialize => initialize_type_dense
        procedure, public, pass(self) :: find => find_dense
        procedure, public, pass(self) :: copy => copy_dense
        procedure, public, pass(self) :: destory => destory_dense
    end type
contains
    subroutine initialize_type_dense(self, domain)
        implicit none
        class(type_dense), intent(inout) :: self
        type(type_domain), intent(inout) :: domain
        integer(int32) :: num_nodes

        num_nodes = domain%get_num_nodes()
        call allocate_array(self%val, num_nodes, num_nodes)
        self%val(:, :) = 0.0d0

    end subroutine initialize_type_dense

    subroutine find_dense(self, row, col, index)
        implicit none
        class(type_dense), intent(in) :: self
        integer(int32), intent(in) :: row, col
        integer(int32), intent(inout) :: index

        return
    end subroutine find_dense

    function copy_dense(self) result(B)
        implicit none
        class(type_dense), intent(in) :: self
        class(abst_matrix), allocatable :: B

        select type (matrix => B)
        type is (type_dense)
            call allocate_array(matrix%val, size(self%val, 1), size(self%val, 2))
            matrix%val(:, :) = self%val(:, :)
        end select
    end function copy_dense

    subroutine destory_dense(self)
        implicit none
        class(type_dense), intent(inout) :: self

        if (allocated(self%val)) deallocate (self%val)
    end subroutine destory_dense
end module matrix_dense
