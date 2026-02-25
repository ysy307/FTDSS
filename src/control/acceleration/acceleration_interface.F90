module control_acceleration
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_linalg, only: vector_dot
    implicit none
    private

    public :: abst_acceleration
    public :: type_acceleration_aitken

    !> Abstract base type for acceleration methods
    type, abstract :: abst_acceleration
    contains
        procedure(abst_initialize_acceleration), public, pass(self), deferred :: initialize
        procedure(abst_destory_acceleration), public, pass(self), deferred :: destory
        procedure(abst_compute_acceleration), public, pass(self), deferred :: compute
        procedure(abst_reset_acceleration), public, pass(self), deferred :: reset
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
        subroutine abst_destory_acceleration(self)
            import :: abst_acceleration
            implicit none
            !> Acceleration object
            class(abst_acceleration), intent(inout) :: self
        end subroutine abst_destory_acceleration

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
        subroutine abst_compute_acceleration(self, physics_type, iter, du, vec)
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
        end subroutine abst_compute_acceleration

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
    end interface

    !> Aitken relaxation method for nonlinear iterations
    type, extends(abst_acceleration) :: type_acceleration_aitken
        !> Configuration parameters
        type(type_config_acceleration) :: config
        !> Current relaxation factor
        real(real64), private :: relaxation_factor(PHYSICS_TYPES%NUM_ID) = 0.5d0
        !> Previous relaxation factor
        real(real64), private :: previous_relaxation_factor(PHYSICS_TYPES%NUM_ID) = 0.5d0
        !> Stored increment vectors from the previous iteration
        real(real64), allocatable, private :: du_raw(:, :)
    contains
        procedure, public, pass(self) :: initialize => initialize_acceleration_aitken
        procedure, public, pass(self) :: destory => destory_acceleration_aitken
        procedure, public, pass(self) :: compute => compute_acceleration_aitken
        procedure, public, pass(self) :: reset => reset_acceleration_aitken
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
        module subroutine destory_acceleration_aitken(self)
            implicit none
            !> Aitken acceleration object
            class(type_acceleration_aitken), intent(inout) :: self
        end subroutine destory_acceleration_aitken

        !> Compute Aitken acceleration
        module subroutine compute_acceleration_aitken(self, physics_type, iter, du, vec)
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
        end subroutine compute_acceleration_aitken

        !> Reset Aitken acceleration
        module subroutine reset_acceleration_aitken(self)
            implicit none
            !> Aitken acceleration object
            class(type_acceleration_aitken), intent(inout) :: self
        end subroutine reset_acceleration_aitken
    end interface

end module control_acceleration