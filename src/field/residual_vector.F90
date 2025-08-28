module field_residual_vector
    use, intrinsic :: iso_fortran_env
    use :: module_core
    implicit none
    private

    public :: type_residual_vector

    type :: type_residual_vector
        private
        integer(int32) :: size = 0
        real(real64), allocatable :: data(:)
    contains
        procedure, pass(self), public :: initialize => initialize_residual_vector
        procedure, pass(self), public :: set_local => set_local_residual_vector
        procedure, pass(self), public :: zero => zero_residual_vector
        procedure, pass(self), public :: destroy => destroy_residual_vector
    end type type_residual_vector

contains

    subroutine initialize_residual_vector(self, num_dof)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: num_dof

        self%size = num_dof
        call allocate_array(self%data, num_dof)
        self%data(:) = 0.0d0
    end subroutine initialize_residual_vector

    subroutine set_local_residual_vector(self, connectivity, local_data)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: connectivity(:)
        real(real64) :: local_data(:)

        integer(int32) :: i

        do i = 1, size(connectivity)
            self%data(connectivity(i)) = local_data(i)
        end do

    end subroutine set_local_residual_vector

    subroutine zero_residual_vector(self)
        implicit none
        class(type_residual_vector), intent(inout) :: self

        self%data(:) = 0.0d0
    end subroutine zero_residual_vector

    subroutine destroy_residual_vector(self)
        implicit none
        class(type_residual_vector), intent(inout) :: self

        if (allocated(self%data)) then
            call deallocate_array(self%data)
        end if
        self%size = 0
    end subroutine destroy_residual_vector

end module field_residual_vector
