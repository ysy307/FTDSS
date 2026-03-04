module condition_boundary_strategy
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
    ! use :: module_input
    use :: condition_boundary_dto
    use :: condition_boundary_data_provider
    implicit none
    private

    public :: abst_bc
    public :: type_bc_dirichlet
    public :: type_bc_neumann
    public :: type_bc_robin
    public :: type_bc_cauchy

    ! ==========================================================================
    ! Abstract Boundary Condition Strategy
    ! ==========================================================================
    type, abstract :: abst_bc
        type(type_constant_id), private :: physics_type = type_constant_id("", "", -1)
        type(type_constant_id), private :: bc_kind = type_constant_id("", "", -1)
        class(abst_bc_data), private, allocatable :: data_provider
        logical, private :: is_initialized = .false.
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_bc
        procedure, public, pass(self) :: destroy => destroy_bc

        procedure, public, pass(self) :: set_bc_kind => set_bc_kind_abst_bc
        procedure, public, pass(self) :: get_bc_kind => get_bc_kind_abst_bc

        ! ---- Algorithm / Operation ----
        procedure(abst_calc_flux_and_derivative), public, pass(self), deferred :: calc_flux_and_derivative
        procedure(abst_calc_dirichlet_value), public, pass(self), deferred :: calc_dirichlet_value
    end type abst_bc

    abstract interface
        subroutine abst_calc_flux_and_derivative(self, current_time, u_curr, q_flux, dq_du)
            import :: abst_bc, real64
            implicit none
            class(abst_bc), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: u_curr
            real(real64), intent(inout) :: q_flux
            real(real64), intent(inout) :: dq_du
        end subroutine abst_calc_flux_and_derivative

        subroutine abst_calc_dirichlet_value(self, current_time, u_curr, val_fixed, is_active)
            import :: abst_bc, real64
            implicit none
            class(abst_bc), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: u_curr
            real(real64), intent(inout) :: val_fixed
            logical, intent(inout) :: is_active
        end subroutine abst_calc_dirichlet_value
    end interface

    interface
        module subroutine initialize_bc(self, config_bc)
            implicit none
            class(abst_bc), intent(inout) :: self
            type(type_config_bc), intent(in) :: config_bc
        end subroutine initialize_bc

        module subroutine destroy_bc(self)
            implicit none
            class(abst_bc), intent(inout) :: self
        end subroutine destroy_bc

        module subroutine set_bc_kind_abst_bc(self, bc_kind)
            implicit none
            class(abst_bc), intent(inout) :: self
            type(type_constant_id), intent(in) :: bc_kind
        end subroutine set_bc_kind_abst_bc

        module subroutine get_bc_kind_abst_bc(self, bc_kind)
            implicit none
            class(abst_bc), intent(in), target :: self
            type(type_constant_id), intent(inout), pointer :: bc_kind
        end subroutine get_bc_kind_abst_bc
    end interface

    ! ==========================================================================
    ! Concrete Strategies
    ! ==========================================================================
    type, extends(abst_bc) :: type_bc_dirichlet
    contains
        ! ---- Algorithm / Operation ----
        procedure, public, pass(self) :: calc_flux_and_derivative => calc_flux_dirichlet_bc
        procedure, public, pass(self) :: calc_dirichlet_value => calc_dirichlet_dirichlet_bc
    end type type_bc_dirichlet

    interface
        module subroutine calc_flux_dirichlet_bc(self, current_time, u_curr, q_flux, dq_du)
            implicit none
            class(type_bc_dirichlet), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: u_curr
            real(real64), intent(inout) :: q_flux
            real(real64), intent(inout) :: dq_du
        end subroutine calc_flux_dirichlet_bc

        module subroutine calc_dirichlet_dirichlet_bc(self, current_time, u_curr, val_fixed, is_active)
            implicit none
            class(type_bc_dirichlet), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: u_curr
            real(real64), intent(inout) :: val_fixed
            logical, intent(inout) :: is_active
        end subroutine calc_dirichlet_dirichlet_bc
    end interface

    type, extends(abst_bc) :: type_bc_neumann
    contains
        ! ---- Algorithm / Operation ----
        procedure, public, pass(self) :: calc_flux_and_derivative => calc_flux_neumann_bc
        procedure, public, pass(self) :: calc_dirichlet_value => calc_dirichlet_neumann_bc
    end type type_bc_neumann

    interface
        module subroutine calc_flux_neumann_bc(self, current_time, u_curr, q_flux, dq_du)
            implicit none
            class(type_bc_neumann), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: u_curr
            real(real64), intent(inout) :: q_flux
            real(real64), intent(inout) :: dq_du
        end subroutine calc_flux_neumann_bc

        module subroutine calc_dirichlet_neumann_bc(self, current_time, u_curr, val_fixed, is_active)
            implicit none
            class(type_bc_neumann), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: u_curr
            real(real64), intent(inout) :: val_fixed
            logical, intent(inout) :: is_active
        end subroutine calc_dirichlet_neumann_bc
    end interface

    type, extends(abst_bc) :: type_bc_robin
    contains
        ! ---- Algorithm / Operation ----
        procedure, public, pass(self) :: calc_flux_and_derivative => calc_flux_robin_bc
        procedure, public, pass(self) :: calc_dirichlet_value => calc_dirichlet_robin_bc
    end type type_bc_robin

    interface
        module subroutine calc_flux_robin_bc(self, current_time, u_curr, q_flux, dq_du)
            implicit none
            class(type_bc_robin), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: u_curr
            real(real64), intent(inout) :: q_flux
            real(real64), intent(inout) :: dq_du
        end subroutine calc_flux_robin_bc

        module subroutine calc_dirichlet_robin_bc(self, current_time, u_curr, val_fixed, is_active)
            implicit none
            class(type_bc_robin), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: u_curr
            real(real64), intent(inout) :: val_fixed
            logical, intent(inout) :: is_active
        end subroutine calc_dirichlet_robin_bc
    end interface

    type, extends(abst_bc) :: type_bc_cauchy
    contains
        ! ---- Algorithm / Operation ----
        procedure, public, pass(self) :: calc_flux_and_derivative => calc_flux_cauchy_bc
        procedure, public, pass(self) :: calc_dirichlet_value => calc_dirichlet_cauchy_bc
    end type type_bc_cauchy

    interface
        module subroutine calc_flux_cauchy_bc(self, current_time, u_curr, q_flux, dq_du)
            implicit none
            class(type_bc_cauchy), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: u_curr
            real(real64), intent(inout) :: q_flux
            real(real64), intent(inout) :: dq_du
        end subroutine calc_flux_cauchy_bc

        module subroutine calc_dirichlet_cauchy_bc(self, current_time, u_curr, val_fixed, is_active)
            implicit none
            class(type_bc_cauchy), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: u_curr
            real(real64), intent(inout) :: val_fixed
            logical, intent(inout) :: is_active
        end subroutine calc_dirichlet_cauchy_bc
    end interface

end module condition_boundary_strategy
