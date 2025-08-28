module field_jacobian_matrix
    use, intrinsic :: iso_fortran_env
    use :: module_core
    implicit none
    private

    public :: type_jacobian_matrix

    type :: type_jacobian_matrix
        integer(int32) :: size = 0
        type(type_crs) :: data
    contains
        procedure :: initialize => initialize_jacobian_matrix
        procedure :: set_local => set_local_jacobian_matrix
        procedure :: zero => zero_jacobian_matrix
        ! procedure :: print => print_jacobian
        procedure :: destroy => destroy_jacobian_matrix
    end type type_jacobian_matrix

contains

    subroutine initialize_jacobian_matrix(self, num_dof, row, col)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: num_dof
        integer(int32), intent(in) :: row(:)
        integer(int32), intent(in) :: col(:)

        self%size = num_dof
        call self%data%initialize(num_dof, row, col)

    end subroutine initialize_jacobian_matrix

    subroutine set_local_jacobian_matrix(self, connectivity, local_data)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: connectivity(:)
        type(type_dense), intent(in) :: local_data

        integer(int32) :: i, j
        integer(int32) :: num_nodes

        num_nodes = size(connectivity)

        do i = 1, num_nodes
            do j = 1, num_nodes
                call self%data%add(connectivity(i), connectivity(j), local_data%val(i, j))
            end do
        end do

    end subroutine set_local_jacobian_matrix

    subroutine zero_jacobian_matrix(self)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self

        call self%data%zero()
    end subroutine zero_jacobian_matrix

    subroutine destroy_jacobian_matrix(self)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self

        call self%data%destroy()
        self%size = 0
    end subroutine destroy_jacobian_matrix

end module field_jacobian_matrix
