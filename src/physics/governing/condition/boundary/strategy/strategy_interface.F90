module condition_boundary_strategy
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
    use :: condition_boundary_data_provider
    implicit none
    private

    public :: abst_bc
    public :: type_bc_result
    public :: type_bc_dirichlet
    public :: type_bc_neumann
    public :: type_bc_robin
    public :: type_bc_cauchy

    type :: type_bc_result
        logical :: is_dirichlet = .false.
        real(real64) :: prescribed_value = 0.0d0
        real(real64) :: flux_value = 0.0d0
        real(real64) :: flux_derivative = 0.0d0
    end type type_bc_result

    type, abstract :: abst_bc
        class(abst_bc_data), pointer, private :: provider => null()
    contains
        procedure, public, pass(self) :: associate_provider => associate_provider_bc
        procedure, public, pass(self) :: destroy => destroy_abst_bc
        procedure(abst_evaluate), public, pass(self), deferred :: evaluate
    end type abst_bc

    abstract interface
        subroutine abst_evaluate(self, current_time, u_curr, result)
            import :: abst_bc, real64, type_bc_result
            implicit none
            class(abst_bc), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: u_curr
            type(type_bc_result), intent(inout) :: result
        end subroutine abst_evaluate
    end interface

    type, extends(abst_bc) :: type_bc_dirichlet
    contains
        procedure, public, pass(self) :: evaluate => evaluate_dirichlet_bc
    end type type_bc_dirichlet

    type, extends(abst_bc) :: type_bc_neumann
    contains
        procedure, public, pass(self) :: evaluate => evaluate_neumann_bc
    end type type_bc_neumann

    type, extends(abst_bc) :: type_bc_robin
    contains
        procedure, public, pass(self) :: evaluate => evaluate_robin_bc
    end type type_bc_robin

    type, extends(abst_bc) :: type_bc_cauchy
    contains
        procedure, public, pass(self) :: evaluate => evaluate_cauchy_bc
    end type type_bc_cauchy

    interface
        module subroutine associate_provider_bc(self, provider)
            implicit none
            class(abst_bc), intent(inout) :: self
            class(abst_bc_data), pointer, intent(in) :: provider
        end subroutine associate_provider_bc

        module subroutine destroy_abst_bc(self)
            implicit none
            class(abst_bc), intent(inout) :: self
        end subroutine destroy_abst_bc

        module subroutine evaluate_dirichlet_bc(self, current_time, u_curr, result)
            implicit none
            class(type_bc_dirichlet), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: u_curr
            type(type_bc_result), intent(inout) :: result
        end subroutine evaluate_dirichlet_bc

        module subroutine evaluate_neumann_bc(self, current_time, u_curr, result)
            implicit none
            class(type_bc_neumann), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: u_curr
            type(type_bc_result), intent(inout) :: result
        end subroutine evaluate_neumann_bc

        module subroutine evaluate_robin_bc(self, current_time, u_curr, result)
            implicit none
            class(type_bc_robin), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: u_curr
            type(type_bc_result), intent(inout) :: result
        end subroutine evaluate_robin_bc

        module subroutine evaluate_cauchy_bc(self, current_time, u_curr, result)
            implicit none
            class(type_bc_cauchy), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: u_curr
            type(type_bc_result), intent(inout) :: result
        end subroutine evaluate_cauchy_bc
    end interface

end module condition_boundary_strategy
