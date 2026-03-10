module control_acceleration
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_linalg, only:vector_dot
    implicit none
    private

    public :: abst_acceleration
    public :: type_acceleration_aitken

    !> Abstract base type for acceleration methods
    type, abstract :: abst_acceleration
        !> Type identifier for the acceleration method
        type(type_constant_id) :: method = type_constant_id("", "", -1)
        !> Flag to indicate if the acceleration method has been initialized
        logical :: initialized = .false.
    contains
        procedure(abst_initialize_acceleration), public, pass(self), deferred :: initialize
        procedure(abst_destroy_acceleration), public, pass(self), deferred :: destroy
        procedure(abst_compute_relaxation_acceleration), public, pass(self), deferred :: compute_relaxation
        procedure(abst_reset_acceleration), public, pass(self), deferred :: reset
        procedure(abst_reach_minimum_relaxation), public, pass(self), deferred :: reach_minimum_relaxation
        procedure(abst_reach_maximum_relaxation), public, pass(self), deferred :: reach_maximum_relaxation
        procedure(abst_get_current_relaxation), public, pass(self), deferred :: get_current_relaxation
        procedure(abst_get_previous_relaxation), public, pass(self), deferred :: get_previous_relaxation
    end type

    abstract interface
        !> Initialize the acceleration method
        !>
        !> Mathematical definition:
        !> - Initializes internal states for acceleration
        !>
        !> Assumptions:
        !> - None
        !>
        !> Numerical guarantee:
        !> - No theoretical error bound available
        !>
        !> Computational complexity:
        !> - Memory: \(O(n)\)
        !> - Arithmetic: \(O(1)\)
        !>
        !> Failure behavior:
        !> - Returns without error
        subroutine abst_initialize_acceleration(self, config)
            import :: abst_acceleration, type_config_acceleration
            implicit none
            !> Acceleration object
            class(abst_acceleration), intent(inout) :: self
            !> Configuration parameters
            type(type_config_acceleration), intent(in) :: config
        end subroutine abst_initialize_acceleration

        !> Destroy the acceleration method
        !>
        !> Mathematical definition:
        !> - Deallocates internal states
        !>
        !> Assumptions:
        !> - None
        !>
        !> Numerical guarantee:
        !> - No theoretical error bound available
        !>
        !> Computational complexity:
        !> - Memory: \(O(1)\)
        !> - Arithmetic: \(O(1)\)
        !>
        !> Failure behavior:
        !> - Returns without error
        subroutine abst_destroy_acceleration(self)
            import :: abst_acceleration
            implicit none
            !> Acceleration object
            class(abst_acceleration), intent(inout) :: self
        end subroutine abst_destroy_acceleration

        !> Apply acceleration to the vector
        !>
        !> \[
        !> u_{k+1} = u_k + \omega \Delta u_k
        !> \]
        !>
        !> Assumptions:
        !> - None
        !>
        !> Numerical guarantee:
        !> - No theoretical error bound available
        !>
        !> Computational complexity:
        !> - Memory: \(O(n)\)
        !> - Arithmetic: \(O(n)\)
        !>
        !> Failure behavior:
        !> - Returns without error
        subroutine abst_compute_relaxation_acceleration(self, physics_type, iter, du, vec)
            import :: abst_acceleration, type_constant_id, int32, real64
            implicit none
            !> Acceleration object
            class(abst_acceleration), intent(inout) :: self
            !> Identifier for the physics type
            type(type_constant_id), intent(in) :: physics_type
            !> Current iteration number
            !> Must satisfy \(iter \ge 1\)
            integer(int32), intent(in) :: iter
            !> Increment vector \(\Delta u_k\)
            !> Not modified
            real(real64), intent(in) :: du(:)
            !> State vector \(u_k\) on entry
            !> Overwritten by updated vector \(u_{k+1}\) on exit
            real(real64), intent(inout) :: vec(:)
        end subroutine abst_compute_relaxation_acceleration

        !> Reset internal states for a new step
        !>
        !> Mathematical definition:
        !> - Resets stored increments and relaxation factors
        !>
        !> Assumptions:
        !> - None
        !>
        !> Numerical guarantee:
        !> - No theoretical error bound available
        !>
        !> Computational complexity:
        !> - Memory: \(O(1)\)
        !> - Arithmetic: \(O(n)\)
        !>
        !> Failure behavior:
        !> - Returns without error
        subroutine abst_reset_acceleration(self)
            import :: abst_acceleration
            implicit none
            !> Acceleration object
            class(abst_acceleration), intent(inout) :: self
        end subroutine abst_reset_acceleration

        !> Check if minimum relaxation factor is reached
        !> Mathematical definition:
        !> - Checks if current relaxation factor is less than or equal to minimum
        !> Assumptions:
        !> - None
        !> Numerical guarantee:
        !> - No theoretical error bound available
        !> Computational complexity:
        !> - Memory: \(O(1)\)
        !> - Arithmetic: \(O(1)\)
        !> Failure behavior:
        !> - Returns .true. if minimum relaxation is reached, .false. otherwise
        pure function abst_reach_minimum_relaxation(self, physics_type) result(reached)
            import :: abst_acceleration, type_constant_id
            implicit none
            !> Acceleration object
            class(abst_acceleration), intent(in) :: self
            !> Identifier for the physics type
            type(type_constant_id), intent(in) :: physics_type
            !> Flag indicating if minimum relaxation is reached
            logical :: reached
        end function abst_reach_minimum_relaxation

        !> Check if maximum relaxation factor is reached
        !> Mathematical definition:
        !> - Checks if current relaxation factor is greater than or equal to maximum
        !> Assumptions:
        !> - None
        !> Numerical guarantee:
        !> - No theoretical error bound available
        !> Computational complexity:
        !> - Memory: \(O(1)\)
        !> - Arithmetic: \(O(1)\)
        !> Failure behavior:
        !> - Returns .true. if maximum relaxation is reached, .false. otherwise
        pure function abst_reach_maximum_relaxation(self, physics_type) result(reached)
            import :: abst_acceleration, type_constant_id
            implicit none
            !> Acceleration object
            class(abst_acceleration), intent(in) :: self
            !> Identifier for the physics type
            type(type_constant_id), intent(in) :: physics_type
            !> Flag indicating if maximum relaxation is reached
            logical :: reached
        end function abst_reach_maximum_relaxation

        !> Get the current relaxation factor
        !> Mathematical definition:
        !> - Returns the current relaxation factor for the given physics type
        !> Assumptions:
        !> - None
        !> Numerical guarantee:
        !> - No theoretical error bound available
        !> Computational complexity:
        !> - Memory: \(O(1)\)
        !> - Arithmetic: \(O(1)\)
        !> Failure behavior:
        !> - Returns the current relaxation factor, or a default value if not initialized
        subroutine abst_get_current_relaxation(self, physics_type, relaxation)
            import :: abst_acceleration, type_constant_id, real64
            implicit none
            !> Acceleration object
            class(abst_acceleration), intent(in) :: self
            !> Identifier for the physics type
            type(type_constant_id), intent(in) :: physics_type
            !> Current relaxation factor
            real(real64), intent(inout) :: relaxation
        end subroutine abst_get_current_relaxation

        !> Get the previous relaxation factor
        !> Mathematical definition:
        !> - Returns the previous relaxation factor for the given physics type
        !> Assumptions:
        !> - None
        !> Numerical guarantee:
        !> - No theoretical error bound available
        !> Computational complexity:
        !> - Memory: \(O(1)\)
        !> - Arithmetic: \(O(1)\)
        !> Failure behavior:
        !> - Returns the previous relaxation factor, or a default value if not initialized
        subroutine abst_get_previous_relaxation(self, physics_type, relaxation)
            import :: abst_acceleration, type_constant_id, real64
            implicit none
            !> Acceleration object
            class(abst_acceleration), intent(in) :: self
            !> Identifier for the physics type
            type(type_constant_id), intent(in) :: physics_type
            !> Previous relaxation factor
            real(real64), intent(inout) :: relaxation
        end subroutine abst_get_previous_relaxation
    end interface

    !> Aitken relaxation method for nonlinear iterations
    type, extends(abst_acceleration) :: type_acceleration_aitken
        !> Maximum relaxation factor \(\omega_{\max}\)
        real(real64) :: max_relaxation = 1.0d0
        !> Minimum relaxation factor \(\omega_{\min}\)
        real(real64) :: min_relaxation = 0.05d0
        !> Current relaxation factor
        real(real64), private :: relaxation_factor(PHYSICS_TYPES%NUM_ID) = 0.5d0
        !> Previous relaxation factor
        real(real64), private :: previous_relaxation_factor(PHYSICS_TYPES%NUM_ID) = 0.5d0
        !> Stored increment vectors from the previous iteration
        real(real64), allocatable, private :: du_raw(:, :)
    contains
        procedure, public, pass(self) :: initialize => initialize_acceleration_aitken
        procedure, public, pass(self) :: destroy => destroy_acceleration_aitken
        procedure, public, pass(self) :: compute_relaxation => compute_relaxation_acceleration_aitken
        procedure, public, pass(self) :: reset => reset_acceleration_aitken
        procedure, public, pass(self) :: reach_minimum_relaxation => reach_minimum_relaxation_aitken
        procedure, public, pass(self) :: reach_maximum_relaxation => reach_maximum_relaxation_aitken
        procedure, public, pass(self) :: get_current_relaxation => get_current_relaxation_aitken
        procedure, public, pass(self) :: get_previous_relaxation => get_previous_relaxation_aitken
    end type type_acceleration_aitken

    interface
        !> Initialize Aitken acceleration
        module subroutine initialize_acceleration_aitken(self, config)
            implicit none
            !> Aitken acceleration object
            class(type_acceleration_aitken), intent(inout) :: self
            !> Configuration parameters
            type(type_config_acceleration), intent(in) :: config
        end subroutine initialize_acceleration_aitken

        !> Destroy Aitken acceleration
        module subroutine destroy_acceleration_aitken(self)
            implicit none
            !> Aitken acceleration object
            class(type_acceleration_aitken), intent(inout) :: self
        end subroutine destroy_acceleration_aitken

        !> Compute Aitken acceleration
        module subroutine compute_relaxation_acceleration_aitken(self, physics_type, iter, du, vec)
            implicit none
            !> Aitken acceleration object
            class(type_acceleration_aitken), intent(inout) :: self
            !> Identifier for the physics type
            type(type_constant_id), intent(in) :: physics_type
            !> Current iteration number
            integer(int32), intent(in) :: iter
            !> Increment vector \(\Delta u_k\)
            real(real64), intent(in) :: du(:)
            !> State vector \(u_k\) on entry
            !> Overwritten by updated vector \(u_{k+1}\) on exit
            real(real64), intent(inout) :: vec(:)
        end subroutine compute_relaxation_acceleration_aitken

        !> Reset Aitken acceleration
        module subroutine reset_acceleration_aitken(self)
            implicit none
            !> Aitken acceleration object
            class(type_acceleration_aitken), intent(inout) :: self
        end subroutine reset_acceleration_aitken

        module pure function reach_minimum_relaxation_aitken(self, physics_type) result(reached)
            implicit none
            !> Aitken acceleration object
            class(type_acceleration_aitken), intent(in) :: self
            !> Identifier for the physics type
            type(type_constant_id), intent(in) :: physics_type
            !> Flag indicating if minimum relaxation is reached
            logical :: reached
        end function reach_minimum_relaxation_aitken

        module pure function reach_maximum_relaxation_aitken(self, physics_type) result(reached)
            implicit none
            !> Aitken acceleration object
            class(type_acceleration_aitken), intent(in) :: self
            !> Identifier for the physics type
            type(type_constant_id), intent(in) :: physics_type
            !> Flag indicating if maximum relaxation is reached
            logical :: reached
        end function reach_maximum_relaxation_aitken

        module subroutine get_current_relaxation_aitken(self, physics_type, relaxation)
            implicit none
            !> Aitken acceleration object
            class(type_acceleration_aitken), intent(in) :: self
            !> Identifier for the physics type
            type(type_constant_id), intent(in) :: physics_type
            !> Current relaxation factor
            real(real64), intent(inout) :: relaxation
        end subroutine get_current_relaxation_aitken

        module subroutine get_previous_relaxation_aitken(self, physics_type, relaxation)
            implicit none
            !> Aitken acceleration object
            class(type_acceleration_aitken), intent(in) :: self
            !> Identifier for the physics type
            type(type_constant_id), intent(in) :: physics_type
            !> Previous relaxation factor
            real(real64), intent(inout) :: relaxation
        end subroutine get_previous_relaxation_aitken
    end interface

end module control_acceleration
