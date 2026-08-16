!>
!> @brief Continuation (homotopy) parameter controller.
!>
!> Marches a scalar \( \lambda \in [0,1] \) across one time step. The physics
!> evaluates the freezing suction as
!> \[ s_f^{\lambda} = s_f^{ref} + \lambda\,(s_f - s_f^{ref}) \]
!> so \( \lambda = 1 \) is the unmodified governing system and \( \lambda = 0 \)
!> holds the freezing suction at its previous-step value. Stages below
!> \( \lambda = 1 \) only build an initial guess; the acceptance test is taken
!> once, at \( \lambda = 1 \).
!>
module control_homotopy_manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    private

    public :: type_homotopy

    !> Master switch. When .false. the controller reports lambda = 1 forever and
    !> the solve reduces exactly to the non-continuation path.
    logical, parameter, public :: HOMOTOPY_ENABLED = .false.
    !> First increment attempted from lambda = 0.
    real(real64), parameter, public :: HOMOTOPY_DLAMBDA0 = 1.25d-1
    !> Give up the ladder below this increment.
    real(real64), parameter, public :: HOMOTOPY_DLAMBDA_MIN = 1.0d-2
    !> Largest increment the controller may grow to. Stages below the freezing
    !> front always succeed, so an increment allowed to grow freely is coarsest
    !> exactly where lambda starts to bite.
    real(real64), parameter, public :: HOMOTOPY_DLAMBDA_MAX = 1.25d-1
    !> Growth factor applied after a stage that behaved well.
    real(real64), parameter, public :: HOMOTOPY_GROWTH = 1.5d0
    !> Shrink factor applied after a stage that did not.
    real(real64), parameter, public :: HOMOTOPY_SHRINK = 5.0d-1
    !> Nonlinear iterations spent per stage while lambda < 1.
    integer(int32), parameter, public :: HOMOTOPY_STAGE_ITERATIONS = 3
    !> Upper bound on stages, so a pathological step cannot loop forever.
    integer(int32), parameter, public :: HOMOTOPY_MAX_STAGES = 24

    !> Distance below which lambda counts as having reached 1.
    real(real64), parameter :: LAMBDA_COMPLETE_TOLERANCE = 1.0d-12

    !>
    !> @brief State of the continuation ladder within one time step.
    !>
    type :: type_homotopy
        !> Continuation is being marched for this run.
        logical, private :: active = .false.
        !> Value the physics is currently evaluated at.
        real(real64), private :: lambda = 1.0d0
        !> Largest value a stage has completed at.
        real(real64), private :: lambda_accepted = 0.0d0
        !> Increment proposed for the next stage.
        real(real64), private :: dlambda = HOMOTOPY_DLAMBDA0
        !> Stages completed in the current time step.
        integer(int32), private :: stage = 0
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_type_homotopy
        procedure, public, pass(self) :: begin_step => begin_step_homotopy

        ! ---- Mutator ----
        procedure, public, pass(self) :: accept_stage => accept_stage_homotopy
        procedure, public, pass(self) :: reject_stage => reject_stage_homotopy
        procedure, public, pass(self) :: finish => finish_homotopy

        ! ---- Inquiry ----
        procedure, public, pass(self) :: is_active => is_active_homotopy
        procedure, public, pass(self) :: is_complete => is_complete_homotopy
        procedure, public, pass(self) :: is_exhausted => is_exhausted_homotopy

        ! ---- Getter ----
        procedure, public, pass(self) :: get_lambda => get_lambda_homotopy
        procedure, public, pass(self) :: get_stage => get_stage_homotopy
    end type type_homotopy

contains

    !> Arm the controller for a run.
    !>
    !> Assumptions: none. Numerical guarantees: none required.
    !> Computational complexity: O(1) arithmetic and memory.
    !> Failure behavior: none; an inactive controller reports lambda = 1.
    subroutine initialize_type_homotopy(self, active)
        implicit none
        !> Continuation controller
        class(type_homotopy), intent(inout) :: self
        !> Enable the ladder; defaults to the module switch when absent
        logical, intent(in), optional :: active

        self%active = HOMOTOPY_ENABLED
        if (present(active)) self%active = active .and. HOMOTOPY_ENABLED

        self%lambda = 1.0d0
        self%lambda_accepted = 0.0d0
        self%dlambda = HOMOTOPY_DLAMBDA0
        self%stage = 0
    end subroutine initialize_type_homotopy

    !> Restart the ladder at the beginning of a time-step attempt.
    !>
    !> Assumptions: called once per attempt, before any assembly.
    !> Numerical guarantees: lambda is exactly 1 when inactive.
    !> Computational complexity: O(1) arithmetic and memory.
    !> Failure behavior: none.
    subroutine begin_step_homotopy(self)
        implicit none
        !> Continuation controller
        class(type_homotopy), intent(inout) :: self

        self%stage = 0
        self%dlambda = HOMOTOPY_DLAMBDA0
        if (self%active) then
            self%lambda = 0.0d0
            self%lambda_accepted = 0.0d0
        else
            self%lambda = 1.0d0
            self%lambda_accepted = 1.0d0
        end if
    end subroutine begin_step_homotopy

    !> Bank the current stage and propose the next lambda.
    !>
    !> Assumptions: the iterate is a usable initial guess at the current lambda.
    !> Numerical guarantees: lambda never exceeds 1.
    !> Computational complexity: O(1) arithmetic and memory.
    !> Failure behavior: none.
    subroutine accept_stage_homotopy(self)
        implicit none
        !> Continuation controller
        class(type_homotopy), intent(inout) :: self

        self%lambda_accepted = self%lambda
        self%stage = self%stage + 1
        self%dlambda = min(HOMOTOPY_DLAMBDA_MAX, HOMOTOPY_GROWTH * self%dlambda)
        self%lambda = min(1.0d0, self%lambda_accepted + self%dlambda)
    end subroutine accept_stage_homotopy

    !> Retreat to the last banked lambda with a smaller increment.
    !>
    !> Assumptions: the caller has restored the iterate to the banked stage.
    !> Numerical guarantees: lambda never exceeds 1.
    !> Computational complexity: O(1) arithmetic and memory.
    !> Failure behavior: none; exhaustion is reported by is_exhausted.
    subroutine reject_stage_homotopy(self)
        implicit none
        !> Continuation controller
        class(type_homotopy), intent(inout) :: self

        self%dlambda = HOMOTOPY_SHRINK * self%dlambda
        self%lambda = min(1.0d0, self%lambda_accepted + self%dlambda)
    end subroutine reject_stage_homotopy

    !> Force the full physics before the accepted nonlinear solve.
    !>
    !> Assumptions: none. Numerical guarantees: lambda is exactly 1, not a sum
    !> of increments, so the final solve cannot inherit a rounding offset.
    !> Computational complexity: O(1) arithmetic and memory.
    !> Failure behavior: none.
    subroutine finish_homotopy(self)
        implicit none
        !> Continuation controller
        class(type_homotopy), intent(inout) :: self

        self%lambda = 1.0d0
        self%lambda_accepted = 1.0d0
    end subroutine finish_homotopy

    pure function is_active_homotopy(self) result(is_active)
        implicit none
        !> Continuation controller
        class(type_homotopy), intent(in) :: self
        !> True while the ladder is being marched
        logical :: is_active

        is_active = self%active
    end function is_active_homotopy

    pure function is_complete_homotopy(self) result(is_complete)
        implicit none
        !> Continuation controller
        class(type_homotopy), intent(in) :: self
        !> True once a stage has completed at lambda = 1
        logical :: is_complete

        is_complete = self%lambda_accepted >= 1.0d0 - LAMBDA_COMPLETE_TOLERANCE
    end function is_complete_homotopy

    pure function is_exhausted_homotopy(self) result(is_exhausted)
        implicit none
        !> Continuation controller
        class(type_homotopy), intent(in) :: self
        !> True when the increment has collapsed or the stage budget is spent
        logical :: is_exhausted

        is_exhausted = (self%dlambda < HOMOTOPY_DLAMBDA_MIN) .or. &
                       (self%stage >= HOMOTOPY_MAX_STAGES)
    end function is_exhausted_homotopy

    pure function get_lambda_homotopy(self) result(lambda)
        implicit none
        !> Continuation controller
        class(type_homotopy), intent(in) :: self
        !> Value the physics must be evaluated at, in [0, 1]
        real(real64) :: lambda

        lambda = self%lambda
    end function get_lambda_homotopy

    pure function get_stage_homotopy(self) result(stage)
        implicit none
        !> Continuation controller
        class(type_homotopy), intent(in) :: self
        !> Stages completed in the current time step
        integer(int32) :: stage

        stage = self%stage
    end function get_stage_homotopy

end module control_homotopy_manager
