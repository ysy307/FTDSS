module field_residual_vector
    use, intrinsic :: iso_fortran_env
    use :: module_core
    implicit none
    private

    public :: type_residual_vector

    type :: type_residual_vector
        private
        integer(int32) :: size = 0
        integer(int32) :: num_nodes = 0
        integer(int32) :: num_dofs = 0
        real(real64), allocatable :: data(:)
    contains
        ! --- initialize/destroy ---
        procedure, pass(self), public :: initialize => initialize_residual_vector
        procedure, pass(self), public :: destroy => destroy_residual_vector

        ! --- getter ---
        procedure, pass(self), public :: get_size => get_size_residual_vector
        procedure, pass(self), public :: get_num_nodes => get_num_nodes_residual_vector
        procedure, pass(self), public :: get_num_dofs => get_num_dofs_residual_vector
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

    subroutine initialize_residual_vector(self, num_nodes, num_dofs)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in) :: num_dofs

        self%num_dofs = num_dofs
        self%num_nodes = num_nodes
        self%size = num_nodes * num_dofs

        call allocate_array(self%data, self%size)
        self%data(:) = 0.0d0

    end subroutine initialize_residual_vector

    pure function get_size_residual_vector(self) result(size)
        implicit none
        class(type_residual_vector), intent(in) :: self
        integer(int32) :: size

        size = self%size

    end function get_size_residual_vector

    pure function get_num_nodes_residual_vector(self) result(num_nodes)
        implicit none
        class(type_residual_vector), intent(in) :: self
        integer(int32) :: num_nodes

        num_nodes = self%num_nodes

    end function get_num_nodes_residual_vector

    pure function get_num_dofs_residual_vector(self) result(num_dofs)
        implicit none
        class(type_residual_vector), intent(in) :: self
        integer(int32) :: num_dofs

        num_dofs = self%num_dofs

    end function get_num_dofs_residual_vector

    function get_data_residual_vector(self) result(data)
        implicit none
        class(type_residual_vector), intent(in), target :: self
        real(real64), dimension(:), pointer :: data

        data => self%data

    end function get_data_residual_vector

    subroutine set_value_residual_vector(self, row_dof, row, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: row
        real(real64), intent(in) :: value

        integer(int32) :: actual_row

        actual_row = (row_dof - 1) * self%num_nodes + row

        self%data(actual_row) = value
    end subroutine set_value_residual_vector

    subroutine set_local_residual_vector(self, row_dof, connectivity, local_data)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: connectivity(:)
        real(real64) :: local_data(:)

        integer(int32) :: i
        integer(int32) :: actual_row, index

        actual_row = (row_dof - 1) * self%num_nodes

        do i = 1, size(connectivity)
            index = actual_row + connectivity(i)
            self%data(index) = local_data(i)
        end do

    end subroutine set_local_residual_vector

    subroutine add_value_residual_vector(self, row_dof, row, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof, row
        real(real64), intent(in) :: value

        integer(int32) :: actual_row
        actual_row = (row_dof - 1) * self%num_nodes + row
        self%data(actual_row) = self%data(actual_row) + value
    end subroutine add_value_residual_vector

    subroutine add_local_residual_vector(self, row_dof, connectivity, local_data)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: connectivity(:)
        real(real64), intent(in) :: local_data(:)

        integer(int32) :: i
        integer(int32) :: actual_row, index

        actual_row = (row_dof - 1) * self%num_nodes

        do i = 1, size(connectivity)
            index = actual_row + connectivity(i)
            self%data(index) = self%data(index) + local_data(i)
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

        call deallocate_array(self%data)
        self%num_dofs = 0
        self%num_nodes = 0
        self%size = 0
    end subroutine destroy_residual_vector

end module field_residual_vector
