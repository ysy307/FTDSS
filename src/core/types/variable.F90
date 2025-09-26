!>
!> Defines a derived type for managing a physical variable and its history
!> over time, which is essential for time-dependent simulations.
!>
module core_types_variable
    use, intrinsic :: iso_fortran_env, only: real64, int32
    use :: core_allocate, only:allocate_array
    implicit none
    private

    public :: type_variable

    !>
    !> Encapsulates a variable's state at different time steps.
    !> This type stores the current value, previous value, historical values,
    !> and the time derivative. It is designed to support time integration
    !> schemes like Backward Differentiation Formulas (BDF).
    !>
    type :: type_variable
        !> The order of the time integration scheme (number of historical steps to store).
        integer(int32) :: rank
        !> The number of degrees of freedom for this variable.
        integer(int32) :: length
        !> The current, most up-to-date value of the variable (time t_{n+1}).
        real(real64), allocatable :: new(:)
        !> The value from the previous time step (time t_n).
        real(real64), allocatable :: pre(:)
        !> A history of values from older time steps (t_{n-1}, t_{n-2}, ...).
        real(real64), allocatable :: old(:, :)
        !> The time derivative of the variable (e.g., du/dt).
        real(real64), allocatable :: dif(:)
    contains
        procedure, pass(self) :: initialize => initialize_type_variable
        procedure, pass(self) :: shift => type_variable_shift
        procedure, pass(self) :: set => type_variable_set
    end type type_variable

contains

    !>
    !> Allocates and initializes the arrays for the variable's state and history.
    !>
    subroutine initialize_type_variable(self, length, rank)
        !> The variable object to initialize.
        class(type_variable), intent(inout) :: self
        !> The number of degrees of freedom for the variable.
        integer(int32), intent(in) :: length
        !> The number of historical time steps to store.
        integer(int32), intent(in) :: rank

        self%rank = rank
        self%length = length

        call allocate_array(self%new, length)
        call allocate_array(self%pre, length)
        call allocate_array(self%old, length, self%rank + 1_int32)
        call allocate_array(self%dif, length)

        self%new(:) = 0.0d0
        self%pre(:) = 0.0d0
        self%old(:, :) = 0.0d0
        self%dif(:) = 0.0d0

    end subroutine initialize_type_variable

    !>
    !> Updates the variable's history by shifting values between time steps.
    !> In a forward step, 'new' becomes 'pre', and 'pre' moves into the 'old' history.
    !> A reverse step can be used to undo this operation.
    !>
    subroutine type_variable_shift(self, reverse)
        !> The variable object whose history is to be shifted.
        class(type_variable), intent(inout) :: self
        !> If present and true, performs a reverse shift to restore the previous state.
        logical, intent(in), optional :: reverse
        logical :: do_reverse

        do_reverse = .false.
        if (present(reverse)) then
            do_reverse = reverse
        end if

        if (do_reverse) then
            ! --- Reverse Shift: Restore state from history ---
            if (self%rank > 0) then
                self%pre(:) = self%old(:, 1)
                ! Shift history to the left (old(:,1) <-- old(:,2), etc.)
                if (self%rank > 1) then
                    self%old(:, 1:self%rank - 1) = self%old(:, 2:self%rank)
                end if
                ! Clear the now-vacant last history entry
                self%old(:, self%rank) = 0.0d0
            end if

        else
            ! --- Forward Shift: Advance time step ---
            self%pre(:) = self%new(:)
            if (self%rank > 0) then
                ! Shift history to the right (old(:,2) <-- old(:,1), etc.)
                if (self%rank > 1) then
                    self%old(:, 2:self%rank) = self%old(:, 1:self%rank - 1)
                end if
                self%old(:, 1) = self%pre(:)
            end if
        end if

    end subroutine type_variable_shift

    !>
    !> Sets all states (new, pre, and all historical values) to a specified value.
    !> This is typically used to set initial conditions for a simulation. The time
    !> derivative term is reset to zero.
    !>
    subroutine type_variable_set(self, value)
        implicit none
        !> The variable object to set.
        class(type_variable), intent(inout) :: self
        !> The array of values to assign to all states.
        real(real64), intent(in) :: value(:)
        integer(int32) :: i

        ! Set current and previous states
        self%new(:) = value(:)
        self%pre(:) = value(:)

        ! Set all historical states
        if (self%rank > 0) then
            do i = 1, self%rank
                self%old(:, i) = value(:)
            end do
        end if

        ! Initialize the time derivative to zero
        self%dif(:) = 0.0d0

    end subroutine type_variable_set

end module core_types_variable
