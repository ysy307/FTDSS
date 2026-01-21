!> Class for managing the state and history of physical variables in time-dependent simulations.
module core_types_variable
    use, intrinsic :: iso_fortran_env, only: real64, int32
    use :: core_allocate, only:allocate_array
    use :: core_deallocate, only:deallocate_array
    use :: core_types_coordinate_array, only:type_coordinate_array_dp
    use :: core_types_coordinate, only:type_coordinate_dp
    implicit none
    private

    public :: type_variable

    !> Encapsulates the time evolution state of a variable.
    !> Supports non-linear iterative calculations (e.g., Newton-Raphson) and time integration via BDF methods.
    type :: type_variable
        !> Initialization flag
        logical, private :: is_initialized = .false.

        !> Number of past steps to keep in history (Corresponds to BDF order)
        integer(int32), private :: num_history_steps

        !> Degrees of freedom (Array size)
        integer(int32), private :: num_dof

        !> Current value at \( t_{n+1} \).
        !> During Newton-Raphson iterations, this holds the "tentative" value before convergence.
        real(real64), allocatable :: current(:)

        !> Value at the immediately preceding fixed time step \( t_n \).
        real(real64), allocatable :: previous(:)

        !> History values for older steps \( t_{n-1}, t_{n-2}, \dots \).
        !> The second dimension is ordered by age: `(:, 1)` is \( t_{n-1} \), `(:, 2)` is \( t_{n-2} \).
        real(real64), allocatable :: history(:, :)

        !> Change in variable \( \delta u \) (e.g., Newton-Raphson update).
        !> Iteration k: \( \delta = u_{calc}^{(k)} - u_{old}^{(k-1)} \)
        real(real64), allocatable :: delta(:)

        !> Time derivative \( \frac{\partial u}{\partial t} \).
        !> Updated whenever `current` changes during iterations.
        real(real64), allocatable :: diff(:)

        !> Spatial gradient \( \nabla u \).
        type(type_coordinate_array_dp) :: grad

    contains
        !> Lifecycle management
        procedure, public, pass(self) :: initialize => initialize_type_variable
        procedure, public, pass(self) :: destroy => destroy_type_variable

        !> State manipulation
        procedure, public, pass(self) :: advance => advance_time_step_variable
        procedure, public, pass(self) :: restore => restore_previous_step_variable
        procedure, public, pass(self) :: reset => reset_all_states_variable

        !> Setters
        procedure, private, pass(self) :: set_current_array_variable
        procedure, private, pass(self) :: set_current_scalar_variable
        procedure, private, pass(self) :: set_current_scalar_all_variable
        generic, public :: set_current => set_current_array_variable, &
            set_current_scalar_variable, &
            set_current_scalar_all_variable
        procedure, private, pass(self) :: set_previous_array_variable
        procedure, private, pass(self) :: set_previous_scalar_variable
        procedure, private, pass(self) :: set_previous_scalar_all_variable
        generic, public :: set_previous => set_previous_array_variable, &
            set_previous_scalar_variable, &
            set_previous_scalar_all_variable

        procedure, public, pass(self) :: set_delta_array_variable
        procedure, public, pass(self) :: set_delta_scalar_variable
        procedure, public, pass(self) :: set_delta_scalar_all_variable
        generic, public :: set_delta => set_delta_array_variable, &
            set_delta_scalar_variable, &
            set_delta_scalar_all_variable

        !> Getters
        procedure, private, pass(self) :: get_current_array_variable
        procedure, private, pass(self) :: get_current_scalar_variable
        procedure, private, pass(self) :: get_current_gradient_variable
        generic, public :: get_current => get_current_array_variable, &
            get_current_scalar_variable, &
            get_current_gradient_variable

        procedure, private, pass(self) :: get_previous_array
        procedure, private, pass(self) :: get_previous_scalar
        generic, public :: get_previous => get_previous_array, &
            get_previous_scalar

        procedure, private, pass(self) :: get_delta_array
        procedure, private, pass(self) :: get_delta_scalar
        generic, public :: get_delta => get_delta_array, &
            get_delta_scalar

        procedure, public, pass(self) :: get_history => get_history_values_variable

        !> Computation
        procedure, public, pass(self) :: compute_time_derivative => compute_time_derivative_variable
    end type type_variable

contains

    !> Initialize the variable manager and allocate memory.
    subroutine initialize_type_variable(self, num_dof, num_history_steps)
        implicit none
        class(type_variable), intent(inout) :: self
        !> Number of degrees of freedom (e.g., number of nodes)
        integer(int32), intent(in) :: num_dof
        !> Size of history buffer
        integer(int32), intent(in) :: num_history_steps

        self%num_dof = num_dof
        self%num_history_steps = num_history_steps

        call allocate_array(self%current, num_dof)
        call allocate_array(self%previous, num_dof)
        call allocate_array(self%history, num_dof, self%num_history_steps)
        call allocate_array(self%diff, num_dof)
        call allocate_array(self%delta, num_dof)

        call self%grad%initialize(num_dof, 0.0d0)

        ! Zero clear
        self%current(:) = 0.0d0
        self%previous(:) = 0.0d0
        self%history(:, :) = 0.0d0
        self%diff(:) = 0.0d0

        self%is_initialized = .true.
    end subroutine initialize_type_variable

    !> Deallocate memory associated with the variable.
    subroutine destroy_type_variable(self)
        implicit none
        class(type_variable), intent(inout) :: self

        if (self%is_initialized) then
            call deallocate_array(self%current)
            call deallocate_array(self%previous)
            call deallocate_array(self%history)
            call deallocate_array(self%diff)
            call deallocate_array(self%delta)
            call self%grad%destroy()
            self%is_initialized = .false.
        end if
    end subroutine destroy_type_variable

    !> Advance the time step state.
    !> Should be called when the calculation for \( t_{n+1} \) converges and is finalized.
    !> Shifts values: \( u_{previous} \to u_{history} \) and \( u_{current} \to u_{previous} \).
    
    subroutine advance_time_step_variable(self)
        implicit none
        class(type_variable), intent(inout) :: self

        if (.not. self%is_initialized) return

        ! Shift history (Move to older slots)
        if (self%num_history_steps > 1) then
            self%history(:, 2:self%num_history_steps) = self%history(:, 1:self%num_history_steps - 1)
        end if

        ! Move previous (t_n) to history head (t_{n-1})
        if (self%num_history_steps > 0) then
            self%history(:, 1) = self%previous(:)
        end if

        ! Commit current iterative value (t_{n+1}) as the new previous (t_{n+1 becomes n})
        self%previous(:) = self%current(:)

    end subroutine advance_time_step_variable

    !> Restore the state to the previous time step.
    !> Used for step rejection or restarting a step.
    subroutine restore_previous_step_variable(self)
        implicit none
        class(type_variable), intent(inout) :: self

        if (.not. self%is_initialized) return

        if (self%num_history_steps > 0) then
            ! Restore previous from history head
            self%previous(:) = self%history(:, 1)

            ! Inverse shift history
            if (self%num_history_steps > 1) then
                self%history(:, 1:self%num_history_steps - 1) = self%history(:, 2:self%num_history_steps)
            end if

            self%history(:, self%num_history_steps) = 0.0d0
        end if

    end subroutine restore_previous_step_variable

    !> Reset all states (current, previous, history) to a specific initial value.
    subroutine reset_all_states_variable(self, initial_value)
        implicit none
        class(type_variable), intent(inout) :: self
        !> Initial value vector
        real(real64), intent(in) :: initial_value(:)
        integer(int32) :: i

        if (.not. self%is_initialized) then
            error stop "Error: Variable not initialized in reset_all_states_variable."
        end if

        self%current(:) = initial_value(:)
        self%previous(:) = initial_value(:)

        if (self%num_history_steps > 0) then
            do i = 1, self%num_history_steps
                self%history(:, i) = initial_value(:)
            end do
        end if

        self%diff(:) = 0.0d0
        self%delta(:) = 0.0d0
        call self%grad%zero()

    end subroutine reset_all_states_variable

    !> Set current values (Array).
    subroutine set_current_array_variable(self, values)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: values(:)

        if (.not. self%is_initialized) return

        if (size(values) /= self%num_dof) then
            error stop "Error: Dimension mismatch in set_current (array)."
        end if

        self%current(:) = values(:)
    end subroutine set_current_array_variable

    !> Set current value at a specific node.
    subroutine set_current_scalar_variable(self, node_id, value)
        implicit none
        class(type_variable), intent(inout) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(in) :: value

        if (.not. self%is_initialized) return

        if (node_id < 1 .or. node_id > self%num_dof) then
            error stop "Error: Index out of bounds in set_current (scalar)."
        end if

        self%current(node_id) = value
    end subroutine set_current_scalar_variable

    !> Set current value for all nodes (Scalar broadcast).
    subroutine set_current_scalar_all_variable(self, value)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: value

        if (.not. self%is_initialized) return

        self%current(:) = value
    end subroutine set_current_scalar_all_variable

    !> Set previous values (Array).
    subroutine set_previous_array_variable(self, values)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: values(:)

        if (.not. self%is_initialized) return

        if (size(values) /= self%num_dof) then
            error stop "Error: Dimension mismatch in set_current (array)."
        end if

        self%previous(:) = values(:)
    end subroutine set_previous_array_variable

    !> Set previous value at a specific node.
    subroutine set_previous_scalar_variable(self, node_id, value)
        implicit none
        class(type_variable), intent(inout) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(in) :: value

        if (.not. self%is_initialized) return

        if (node_id < 1 .or. node_id > self%num_dof) then
            error stop "Error: Index out of bounds in set_current (scalar)."
        end if

        self%previous(node_id) = value
    end subroutine set_previous_scalar_variable

    !> Set previous value for all nodes (Scalar broadcast).
    subroutine set_previous_scalar_all_variable(self, value)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: value

        if (.not. self%is_initialized) return

        self%previous(:) = value
    end subroutine set_previous_scalar_all_variable

    !> Set delta values (Array).
    subroutine set_delta_array_variable(self, values)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: values(:)

        if (.not. self%is_initialized) return
        if (size(values) /= self%num_dof) then
            error stop "Error: Dimension mismatch in set_delta (array)."
        end if

        self%delta(:) = values(:)
    end subroutine set_delta_array_variable

    !> Set delta value at a specific node.
    subroutine set_delta_scalar_variable(self, node_id, value)
        implicit none
        class(type_variable), intent(inout) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(in) :: value

        if (.not. self%is_initialized) return

        if (node_id < 1 .or. node_id > self%num_dof) then
            error stop "Error: Index out of bounds in set_delta (scalar)."
        end if

        self%delta(node_id) = value
    end subroutine set_delta_scalar_variable

    !> Set delta value for all nodes (Scalar broadcast).
    subroutine set_delta_scalar_all_variable(self, value)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: value

        if (.not. self%is_initialized) return

        self%delta(:) = value
    end subroutine set_delta_scalar_all_variable

    !> Get pointer to current array.
    !> Used mainly for Newton-Raphson updates.
    subroutine get_current_array_variable(self, ptr_values)
        implicit none
        class(type_variable), intent(in), target :: self
        !> Pointer to the current values
        real(real64), intent(inout), pointer, contiguous, dimension(:) :: ptr_values

        if (self%is_initialized) then
            ptr_values => self%current
        else
            ptr_values => null()
        end if
    end subroutine get_current_array_variable

    !> Get current value at a specific node.
    pure subroutine get_current_scalar_variable(self, node_id, scalar_value)
        implicit none
        class(type_variable), intent(in) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(inout) :: scalar_value

        if (self%is_initialized) then
            scalar_value = self%current(node_id)
        else
            scalar_value = 0.0d0
        end if
    end subroutine get_current_scalar_variable

    !> Get current spatial gradient at a specific node.
    pure subroutine get_current_gradient_variable(self, node_id, gradient_value)
        implicit none
        class(type_variable), intent(in) :: self
        integer(int32), intent(in) :: node_id
        type(type_coordinate_dp), intent(inout) :: gradient_value

        if (self%is_initialized) then
            gradient_value%x = self%grad%x(node_id)
            gradient_value%y = self%grad%y(node_id)
            gradient_value%z = self%grad%z(node_id)
        else
            gradient_value%x = 0.0d0
            gradient_value%y = 0.0d0
            gradient_value%z = 0.0d0
        end if
    end subroutine get_current_gradient_variable

    !> Get history values for a specific node.
    !> Returns vector [Current, Previous, History(1), History(2)...].
    pure subroutine get_history_values_variable(self, node_id, output_history)
        implicit none
        class(type_variable), intent(in) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(inout) :: output_history(:)

        integer(int32) :: i, num_out, max_avail

        output_history(:) = 0.0d0
        if (.not. self%is_initialized) return

        max_avail = 2 + self%num_history_steps
        num_out = min(size(output_history), max_avail)

        if (num_out >= 1) output_history(1) = self%current(node_id)
        if (num_out >= 2) output_history(2) = self%previous(node_id)

        if (num_out > 2) then
            do i = 1, num_out - 2
                output_history(i + 2) = self%history(node_id, i)
            end do
        end if

    end subroutine get_history_values_variable

    !> Get pointer to previous array (Fixed at \( t_n \)).
    subroutine get_previous_array(self, ptr_values)
        implicit none
        class(type_variable), intent(in), target :: self
        real(real64), intent(inout), pointer, contiguous, dimension(:) :: ptr_values

        if (self%is_initialized) then
            ptr_values => self%previous
        else
            ptr_values => null()
        end if
    end subroutine get_previous_array

    !> Get previous value at a specific node.
    pure subroutine get_previous_scalar(self, node_id, scalar_value)
        implicit none
        class(type_variable), intent(in) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(inout) :: scalar_value

        if (self%is_initialized) then
            scalar_value = self%previous(node_id)
        else
            scalar_value = 0.0d0
        end if
    end subroutine get_previous_scalar

    !> Get pointer to delta array.
    subroutine get_delta_array(self, ptr_values)
        implicit none
        class(type_variable), intent(in), target :: self
        real(real64), intent(inout), pointer, contiguous, dimension(:) :: ptr_values

        if (self%is_initialized) then
            ptr_values => self%delta
        else
            ptr_values => null()
        end if
    end subroutine get_delta_array

    !> Get delta value at a specific node.
    pure subroutine get_delta_scalar(self, node_id, scalar_value)
        implicit none
        class(type_variable), intent(in) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(inout) :: scalar_value

        if (self%is_initialized) then
            scalar_value = self%delta(node_id)
        else
            scalar_value = 0.0d0
        end if
    end subroutine get_delta_scalar

    !> Compute time derivative \( \frac{\partial u}{\partial t} \) using BDF coefficients.
    !> Must be called whenever `current` is updated during nonlinear iterations.
    !> Formula: \( \frac{\partial u}{\partial t} \approx \alpha_0 u_{n+1} + \alpha_1 u_n + \sum \alpha_{j} u_{n+1-j} \)
    
    subroutine compute_time_derivative_variable(self, bdf_coeffs)
        implicit none
        class(type_variable), intent(inout) :: self
        !> BDF Coefficients (scaled by \( 1/\Delta t \))
        real(real64), intent(in) :: bdf_coeffs(:)

        integer(int32) :: i, hist_idx
        integer(int32) :: n_coeffs

        if (.not. self%is_initialized) return

        n_coeffs = size(bdf_coeffs)
        self%diff(:) = 0.0d0

        ! 1. Term for t_{n+1} (Current/Iterating)
        if (n_coeffs >= 1) then
            self%diff(:) = self%diff(:) + bdf_coeffs(1) * self%current(:)
        end if

        ! 2. Term for t_{n} (Previous/Fixed)
        if (n_coeffs >= 2) then
            self%diff(:) = self%diff(:) + bdf_coeffs(2) * self%previous(:)
        end if

        ! 3. Terms for t_{n-1}... (History/Fixed)
        if (n_coeffs >= 3) then
            do i = 3, n_coeffs
                hist_idx = i - 2
                if (hist_idx > self%num_history_steps) exit

                self%diff(:) = self%diff(:) + bdf_coeffs(i) * self%history(:, hist_idx)
            end do
        end if

    end subroutine compute_time_derivative_variable

end module core_types_variable