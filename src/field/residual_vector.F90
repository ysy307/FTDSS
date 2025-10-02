module field_residual_vector
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_domain, only:type_domain
    use :: module_linalg
    implicit none
    private

    public :: type_residual_vector

    type :: type_residual_vector
        private
        integer(int32) :: coupling_mode = -1
        integer(int32) :: num_dofs_per_node = 0
        integer(int32) :: size = 0
        type(type_vector_dp), allocatable :: data(:)
    contains
        ! --- initialize/destroy ---
        procedure, pass(self), public :: initialize => initialize_residual_vector
        procedure, pass(self), public :: destroy => destroy_residual_vector

        ! --- getter ---
        procedure, pass(self), public :: get_size => get_size_residual_vector
        procedure, pass(self), public :: get_data => get_data_residual_vector

        ! --- setter ---
        procedure, pass(self), private :: set_scalar_residual_vector
        procedure, pass(self), private :: set_array_residual_vector
        procedure, pass(self), private :: set_value_at_index_residual_vector
        procedure, pass(self), private :: set_values_at_indices_residual_vector
        generic, public :: set => set_scalar_residual_vector, set_array_residual_vector, &
            set_value_at_index_residual_vector, set_values_at_indices_residual_vector

        ! --- operation ---
        procedure, pass(self), private :: add_value_residual_vector
        procedure, pass(self), private :: add_array_residual_vector
        procedure, pass(self), private :: add_value_at_index_residual_vector
        procedure, pass(self), private :: add_values_at_indices_residual_vector
        generic, public :: add => add_value_residual_vector, add_array_residual_vector, &
            add_value_at_index_residual_vector, add_values_at_indices_residual_vector
        procedure, pass(self), public :: zero => zero_residual_vector
    end type type_residual_vector

contains

    subroutine initialize_residual_vector(self, domain)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        type(type_domain), intent(in) :: domain

        integer(int32) :: i, num_dofs

        self%coupling_mode = domain%get_coupling_mode()
        self%size = domain%get_num_nodes() * domain%get_num_dofs_per_node()
        self%num_dofs_per_node = domain%get_num_dofs_per_node()

        allocate (self%data(self%num_dofs_per_node))
        do i = 1, self%num_dofs_per_node
            call self%data(i)%initialize(domain%get_num_nodes())
        end do

    end subroutine initialize_residual_vector

    pure function get_size_residual_vector(self) result(size)
        implicit none
        class(type_residual_vector), intent(in) :: self
        integer(int32) :: size

        size = self%size

    end function get_size_residual_vector

    function get_data_residual_vector(self, row_dof) result(data)
        implicit none
        class(type_residual_vector), intent(in), target :: self
        integer(int32), intent(in) :: row_dof
        type(type_vector_dp), pointer :: data

        data => self%data(row_dof)

    end function get_data_residual_vector

    subroutine set_scalar_residual_vector(self, row_dof, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        real(real64), intent(in) :: value

        call self%data(row_dof)%set(value)
    end subroutine set_scalar_residual_vector

    subroutine set_array_residual_vector(self, row_dof, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        real(real64), intent(in) :: values(:)

        call self%data(row_dof)%set(values)
    end subroutine set_array_residual_vector

    subroutine set_value_at_index_residual_vector(self, row_dof, global_index, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: global_index
        real(real64), intent(in) :: value

        call self%data(row_dof)%set(global_index, value)
    end subroutine set_value_at_index_residual_vector

    subroutine set_values_at_indices_residual_vector(self, row_dof, global_indices, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: global_indices(:)
        real(real64), intent(in) :: values(:)

        call self%data(row_dof)%set(global_indices, values)
    end subroutine set_values_at_indices_residual_vector

    subroutine add_value_residual_vector(self, row_dof, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        real(real64), intent(in) :: value

        call self%data(row_dof)%add(value)

    end subroutine add_value_residual_vector

    subroutine add_array_residual_vector(self, row_dof, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        real(real64), intent(in) :: values(:)

        call self%data(row_dof)%add(values)
    end subroutine add_array_residual_vector

    subroutine add_value_at_index_residual_vector(self, row_dof, global_index, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: global_index
        real(real64), intent(in) :: value

        call self%data(row_dof)%add(global_index, value)

    end subroutine add_value_at_index_residual_vector

    subroutine add_values_at_indices_residual_vector(self, row_dof, global_indices, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: global_indices(:)
        real(real64), intent(in) :: values(:)

        call self%data(row_dof)%add(global_indices, values)
    end subroutine add_values_at_indices_residual_vector

    subroutine zero_residual_vector(self)
        implicit none
        class(type_residual_vector), intent(inout) :: self

        integer(int32) :: i

        do i = 1, self%num_dofs_per_node
            call self%data(i)%zero()
        end do
    end subroutine zero_residual_vector

    subroutine destroy_residual_vector(self)
        implicit none
        class(type_residual_vector), intent(inout) :: self

        integer(int32) :: i

        if (allocated(self%data)) then
            do i = 1, self%num_dofs_per_node
                call self%data(i)%destroy()
            end do
            deallocate (self%data)
        end if

        self%size = 0
        self%num_dofs_per_node = 0
        self%coupling_mode = -1

    end subroutine destroy_residual_vector

end module field_residual_vector
