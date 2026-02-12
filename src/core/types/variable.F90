module core_types_variable
    use, intrinsic :: iso_fortran_env, only: real64, int32
    use :: core_types_coordinate_array, only:type_coordinate_array_dp
    implicit none
    private

    public :: type_variable

    !> Variable management class
    !> Optimized version for history access using (Time, Space) layout
    type :: type_variable
        private
        logical :: is_initialized = .false.

        integer(int32) :: num_dof = 0
        integer(int32) :: num_history_steps = 0
        integer(int32) :: num_time_slots = 0 ! = history_steps + 2 (Current + Previous + Histories)

        !> Head position of the ring buffer (Index pointing to current time t_{n+1})
        integer(int32) :: head_idx = 1

        !> Main data area
        !> Dim 1: Time (step+2) -> Contiguous in memory (Fastest history access)
        !> Dim 2: Node (dofs)
        real(real64), allocatable :: values(:, :)

        !> Correction amount delta (Newton update) - 1D array for solver
        real(real64), allocatable :: delta(:)

        !> Time derivative du/dt
        real(real64), allocatable :: diff(:)

        !> Spatial gradient
        type(type_coordinate_array_dp) :: grad

    contains
        ! Lifecycle
        procedure, public, pass(self) :: initialize => initialize_type_variable
        procedure, public, pass(self) :: destroy => destroy_type_variable

        ! State manipulation
        procedure, public, pass(self) :: advance => advance_time_step_variable
        procedure, public, pass(self) :: restore => restore_previous_step_variable
        procedure, public, pass(self) :: reset => reset_all_states_variable

        ! Setters
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
        generic, public :: set_delta => set_delta_array_variable

        ! Getters (Pointer / Zero-Copy)
        procedure, public, pass(self) :: get_history ! Pointer to history sequence
        procedure, private, pass(self) :: get_current_values
        procedure, private, pass(self) :: get_current_scalar
        generic, public :: get_current => get_current_values, &
            get_current_scalar

        procedure, private, pass(self) :: get_previous_array
        procedure, private, pass(self) :: get_previous_scalar
        generic, public :: get_previous => get_previous_array, &
            get_previous_scalar

        procedure, public, pass(self) :: get_delta
        procedure, public, pass(self) :: get_diff

        ! Getters (Scalar)
        procedure, public, pass(self) :: get_current_gradient

        ! Computation
        procedure, public, pass(self) :: compute_time_derivative => compute_time_derivative_variable

        ! Utility
        procedure, public, pass(self) :: get_head_index
    end type type_variable

contains

    ! ------------------------------------------------------------------
    ! Initialize
    ! ------------------------------------------------------------------
    subroutine initialize_type_variable(self, num_dof, num_history_steps)
        implicit none
        class(type_variable), intent(inout) :: self
        integer(int32), intent(in) :: num_dof
        integer(int32), intent(in) :: num_history_steps

        call self%destroy()

        self%num_dof = num_dof
        self%num_history_steps = num_history_steps
        self%num_time_slots = num_history_steps + 2
        self%head_idx = 1

        ! Memory allocation: (Time, Space)
        call allocate_array(self%values, self%num_time_slots, num_dof)
        call allocate_array(self%delta, num_dof)
        call allocate_array(self%diff, num_dof)

        call self%grad%initialize(num_dof, 0.0d0)

        ! Zero clear
        self%values(:, :) = 0.0d0
        self%delta(:) = 0.0d0
        self%diff(:) = 0.0d0

        self%is_initialized = .true.
    end subroutine initialize_type_variable

    subroutine destroy_type_variable(self)
        implicit none
        class(type_variable), intent(inout) :: self
        if (self%is_initialized) then
            self%num_dof = 0
            self%num_history_steps = 0
            self%num_time_slots = 0
            self%head_idx = 1
            call deallocate_array(self%values)
            call deallocate_array(self%delta)
            call deallocate_array(self%diff)
            call self%grad%destroy()
            self%is_initialized = .false.
        end if
    end subroutine destroy_type_variable

    ! ------------------------------------------------------------------
    ! State Manipulation (Ring Buffer)
    ! ------------------------------------------------------------------
    subroutine advance_time_step_variable(self)
        implicit none
        class(type_variable), intent(inout) :: self
        integer(int32) :: old_head, new_head

        if (.not. self%is_initialized) return

        old_head = self%head_idx
        ! Advance head by 1 (1 -> 2 -> ... -> Max -> 1)
        new_head = mod(self%head_idx, self%num_time_slots) + 1
        self%head_idx = new_head

        ! Copy previous step value as initial guess for the new step (Predictor)
        ! This is a strided copy (dim 2 access), but negligible compared to total cost
        self%values(new_head, :) = self%values(old_head, :)

    end subroutine advance_time_step_variable

    subroutine restore_previous_step_variable(self)
        implicit none
        class(type_variable), intent(inout) :: self
        if (.not. self%is_initialized) return

        ! Move head back
        if (self%head_idx == 1) then
            self%head_idx = self%num_time_slots
        else
            self%head_idx = self%head_idx - 1
        end if
    end subroutine restore_previous_step_variable

    subroutine reset_all_states_variable(self, initial_value)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: initial_value(:)
        integer(int32) :: i

        if (.not. self%is_initialized) return

        do i = 1, self%num_time_slots
            self%values(i, :) = initial_value(:)
        end do

        self%delta(:) = 0.0d0
        self%diff(:) = 0.0d0
        call self%grad%zero()
        self%head_idx = 1
    end subroutine reset_all_states_variable

    ! ------------------------------------------------------------------
    ! Setters
    ! ------------------------------------------------------------------
    subroutine set_current_array_variable(self, val)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: val(:)
        self%values(self%head_idx, :) = val(:)
    end subroutine set_current_array_variable

    subroutine set_current_scalar_variable(self, node_id, val)
        implicit none
        class(type_variable), intent(inout) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(in) :: val
        self%values(self%head_idx, node_id) = val
    end subroutine set_current_scalar_variable

    subroutine set_current_scalar_all_variable(self, val)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: val
        self%values(self%head_idx, :) = val
    end subroutine set_current_scalar_all_variable

    subroutine set_previous_array_variable(self, val)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: val(:)
        integer(int32) :: prev_idx

        if (.not. self%is_initialized) return

        ! Calculate previous index
        if (self%head_idx == 1) then
            prev_idx = self%num_time_slots
        else
            prev_idx = self%head_idx - 1
        end if

        self%values(prev_idx, :) = val(:)
    end subroutine set_previous_array_variable

    subroutine set_previous_scalar_variable(self, node_id, val)
        implicit none
        class(type_variable), intent(inout) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(in) :: val
        integer(int32) :: prev_idx

        if (.not. self%is_initialized) return

        ! Calculate previous index
        if (self%head_idx == 1) then
            prev_idx = self%num_time_slots
        else
            prev_idx = self%head_idx - 1
        end if

        self%values(prev_idx, node_id) = val
    end subroutine set_previous_scalar_variable

    subroutine set_previous_scalar_all_variable(self, val)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: val
        integer(int32) :: prev_idx

        if (.not. self%is_initialized) return

        ! Calculate previous index
        if (self%head_idx == 1) then
            prev_idx = self%num_time_slots
        else
            prev_idx = self%head_idx - 1
        end if

        self%values(prev_idx, :) = val
    end subroutine set_previous_scalar_all_variable

    subroutine set_delta_array_variable(self, val)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: val(:)
        self%delta(:) = val(:)
    end subroutine set_delta_array_variable

    ! ------------------------------------------------------------------
    ! Getters (Optimized Pointer Access)
    ! ------------------------------------------------------------------

    !> [Important] Get pointer to history data sequence
    !> Returns a contiguous array in memory, providing fastest access in physics loops.
    subroutine get_history(self, node_id, ptr, current_head)
        implicit none
        class(type_variable), intent(in), target :: self
        integer(int32), intent(in) :: node_id

        ! CONTIGUOUS attribute can be applied (fastest)
        real(real64), pointer, intent(inout), contiguous :: ptr(:)
        integer(int32), intent(inout), optional :: current_head

        if (self%is_initialized) then
            ptr => self%values(:, node_id)
            if (present(current_head)) current_head = self%head_idx
        else
            nullify (ptr)
            if (present(current_head)) current_head = 0
        end if
    end subroutine get_history

    !> Get pointer to current value vector
    !> Note: Access involves stride, so contiguous attribute cannot be applied.
    subroutine get_current_values(self, values)
        implicit none
        class(type_variable), intent(in), target :: self
        real(real64), pointer, intent(inout) :: values(:) ! NOT contiguous

        if (self%is_initialized) then
            values => self%values(self%head_idx, :)
        else
            nullify (values)
        end if
    end subroutine get_current_values

    subroutine get_current_scalar(self, node_id, val)
        implicit none
        class(type_variable), intent(in) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(inout) :: val

        if (self%is_initialized) then
            val = self%values(self%head_idx, node_id)
        else
            val = 0.0d0
        end if
    end subroutine get_current_scalar
    !

    !> Get pointer to one step previous (t_n)
    subroutine get_previous_array(self, ptr)
        implicit none
        class(type_variable), intent(in), target :: self
        real(real64), pointer, intent(inout) :: ptr(:)
        integer(int32) :: prev_idx

        if (self%is_initialized) then
            ! If head is 1, previous is tail, otherwise head-1
            if (self%head_idx == 1) then
                prev_idx = self%num_time_slots
            else
                prev_idx = self%head_idx - 1
            end if
            ptr => self%values(prev_idx, :)
        else
            nullify (ptr)
        end if
    end subroutine get_previous_array

    !>
    subroutine get_previous_scalar(self, node_id, val)
        implicit none
        class(type_variable), intent(in) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(inout) :: val
        integer(int32) :: prev_idx

        if (self%is_initialized) then
            ! If head is 1, previous is tail, otherwise head-1
            if (self%head_idx == 1) then
                prev_idx = self%num_time_slots
            else
                prev_idx = self%head_idx - 1
            end if
            val = self%values(prev_idx, node_id)
        else
            val = 0.0d0
        end if
    end subroutine get_previous_scalar

    subroutine get_delta(self, ptr)
        implicit none
        class(type_variable), intent(in), target :: self
        real(real64), pointer, intent(inout), contiguous :: ptr(:)
        if (self%is_initialized) then
            ptr => self%delta
        else
            nullify (ptr)
        end if
    end subroutine get_delta

    subroutine get_diff(self, ptr)
        implicit none
        class(type_variable), intent(in), target :: self
        real(real64), pointer, intent(inout), contiguous :: ptr(:)
        if (self%is_initialized) then
            ptr => self%diff
        else
            nullify (ptr)
        end if
    end subroutine get_diff

    subroutine get_current_gradient(self, ptr)
        implicit none
        class(type_variable), intent(in), target :: self
        type(type_coordinate_array_dp), pointer, intent(inout) :: ptr
        ptr => self%grad
    end subroutine get_current_gradient

    ! ------------------------------------------------------------------
    ! Scalar Access / Helpers
    ! ------------------------------------------------------------------

    pure function get_head_index(self) result(idx)
        class(type_variable), intent(in) :: self
        integer(int32) :: idx
        idx = self%head_idx
    end function get_head_index

    !> Compute time derivative (Ring buffer compatible)
    subroutine compute_time_derivative_variable(self, bdf_coeffs, bdf_order)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: bdf_coeffs(:)
        integer(int32), intent(in) :: bdf_order

        integer(int32) :: i, idx

        if (.not. self%is_initialized) return

        self%diff(:) = 0.0d0

        idx = self%head_idx

        do i = 1, bdf_order + 1
            ! Row access (stride), but sufficient speed as it's equivalent to BLAS level 1
            self%diff(:) = self%diff(:) + bdf_coeffs(i) * self%values(idx, :)

            ! Move index back to past
            if (idx == 1) then
                idx = self%num_time_slots
            else
                idx = idx - 1
            end if
        end do
    end subroutine compute_time_derivative_variable

end module core_types_variable
