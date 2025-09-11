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
        ! --- initialize/destroy ---
        procedure, pass(self), public :: initialize => initialize_residual_vector
        procedure, pass(self), public :: destroy => destroy_residual_vector

        ! --- getter ---
        procedure, pass(self), public :: get_size => get_size_residual_vector
        procedure, pass(self), public :: get_data => get_data_residual_vector

        ! --- setter ---
        procedure, pass(self), private :: set_value => set_value_residual_vector
        procedure, pass(self), private :: set_local => set_local_residual_vector
        generic, public :: set => set_value, set_local

        ! --- operation ---
        procedure, pass(self), private :: add_value => add_value_residual_vector
        procedure, pass(self), private :: add_local => add_local_residual_vector
        generic, public :: add => add_value, add_local
        procedure, pass(self), public :: zero => zero_residual_vector
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

    pure function get_size_residual_vector(self) result(size)
        implicit none
        class(type_residual_vector), intent(in) :: self
        integer(int32) :: size

        size = self%size

    end function get_size_residual_vector

    function get_data_residual_vector(self) result(data)
        implicit none
        class(type_residual_vector), intent(in), target :: self
        real(real64), dimension(:), pointer :: data

        data => self%data

    end function get_data_residual_vector

    subroutine set_value_residual_vector(self, row, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row
        real(real64), intent(in) :: value

        self%data(row) = value
    end subroutine set_value_residual_vector

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

    subroutine add_value_residual_vector(self, row, col, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value

        self%data(row) = self%data(row) + value
    end subroutine add_value_residual_vector

    subroutine add_local_residual_vector(self, connectivity, local_data)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: connectivity(:)
        real(real64), intent(in) :: local_data(:)

        integer(int32) :: i

        do i = 1, size(connectivity)
            self%data(connectivity(i)) = self%data(connectivity(i)) + local_data(i)
        end do

    end subroutine add_local_residual_vector

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
