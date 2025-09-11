module main_hydraulic
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_logger
    use :: stdlib_strings
    use :: module_core, only:allocate_array, deallocate_array, type_variable, type_dp_3d, type_state, type_crs
    use :: module_domain, only:type_domain
    use :: module_properties, only:type_properties_manager
    use :: module_input, only:type_input
    use :: module_boundary, only:type_bc, mode_value, mode_nr
    use :: module_solver
    use :: module_control, only:type_controls
    use :: hydraulic_hydraulic_assemble
    implicit none
    private

    public :: abst_hydraulic
    public :: type_hydraulic_crs

    type, abstract :: abst_hydraulic
        type(type_dp_3d) :: water_flux

        type(type_crs) :: KH_star
        real(real64), allocatable :: FH(:)
        real(real64), allocatable :: PHIH(:)

        !! Nonlinear solver
        character(:), allocatable :: algorithm

        !! Solver
        class(abst_solver), allocatable :: solver
        integer(int32) :: order

        procedure(abst_assemble_global_hydraulic), nopass, pointer :: assemble_global => null()
    contains
        procedure(abst_hydraulic_update), pass(self), deferred :: update
        procedure(abst_shift), pass(self), deferred :: shift
        procedure(abst_solve), pass(self), deferred :: solve
        procedure(abst_compute), pass(self), deferred :: compute
    end type abst_hydraulic

    type, extends(abst_hydraulic) :: type_hydraulic_crs
    contains
        procedure :: update => update_type_hydraulic_crs
        procedure :: shift => shift_type_hydraulic_crs
        procedure :: solve => solve_type_hydraulic_crs
        procedure :: compute => compute_type_hydraulic_crs
    end type type_hydraulic_crs

    abstract interface
        subroutine abst_hydraulic_update(self, domain, property, pressure, porosity)
            import :: abst_hydraulic, type_domain, type_properties_manager, real64
            implicit none
            class(abst_hydraulic), intent(inout) :: self
            type(type_domain), intent(inout), target :: domain
            type(type_properties_manager), intent(inout) :: property
            real(real64), intent(in) :: pressure(:)
            real(real64), intent(in) :: porosity(:)

        end subroutine abst_hydraulic_update

        subroutine abst_shift(self)
            import :: abst_hydraulic
            implicit none
            class(abst_hydraulic), intent(inout) :: self

        end subroutine abst_shift

        subroutine abst_solve(self, pressure, controls)
            import :: abst_hydraulic, type_controls, type_variable
            implicit none
            class(abst_hydraulic), intent(inout) :: self
            type(type_controls), intent(inout) :: controls
            type(type_variable), intent(inout) :: pressure

        end subroutine abst_solve

        subroutine abst_compute(self, domain, property, pressure, temperature, porosity, ice, controls, bc)
            import :: abst_hydraulic, type_domain, type_properties_manager, type_variable, type_controls, type_bc
            implicit none
            class(abst_hydraulic), intent(inout) :: self
            type(type_domain), intent(inout) :: domain
            type(type_properties_manager), intent(in) :: property
            type(type_variable), intent(inout) :: pressure
            type(type_variable), intent(inout) :: temperature
            type(type_variable), intent(inout) :: porosity
            type(type_variable), intent(inout) :: ice
            type(type_controls), intent(inout) :: controls
            type(type_bc), intent(inout) :: bc

        end subroutine abst_compute
    end interface

    interface
        module function construct_type_hydraulic_crs(input, coordinate, domain) result(structure)
            implicit none
            class(abst_hydraulic), allocatable :: structure
            type(type_input), intent(inout) :: input
            type(type_dp_3d), intent(inout), pointer :: coordinate
            type(type_domain), intent(inout) :: domain

        end function construct_type_hydraulic_crs

        module subroutine update_type_hydraulic_crs(self, domain, property, pressure, porosity)
            implicit none
            class(type_hydraulic_crs), intent(inout) :: self
            type(type_domain), intent(inout), target :: domain
            type(type_properties_manager), intent(inout) :: property
            real(real64), intent(in) :: pressure(:)
            real(real64), intent(in) :: porosity(:)

        end subroutine update_type_hydraulic_crs

        module subroutine shift_type_hydraulic_crs(self)
            implicit none
            class(type_hydraulic_crs), intent(inout) :: self

        end subroutine shift_type_hydraulic_crs

        module subroutine solve_type_hydraulic_crs(self, pressure, controls)
            implicit none
            class(type_hydraulic_crs), intent(inout) :: self
            type(type_variable), intent(inout) :: pressure
            type(type_controls), intent(inout) :: controls

        end subroutine solve_type_hydraulic_crs

        module subroutine compute_type_hydraulic_crs(self, domain, property, pressure, temperature, porosity, ice, controls, bc)
            implicit none
            class(type_hydraulic_crs), intent(inout) :: self
            type(type_domain), intent(inout) :: domain
            type(type_properties_manager), intent(in) :: property
            type(type_variable), intent(inout) :: pressure
            type(type_variable), intent(inout) :: temperature
            type(type_variable), intent(inout) :: porosity
            type(type_variable), intent(inout) :: ice
            type(type_controls), intent(inout) :: controls
            type(type_bc), intent(inout) :: bc

        end subroutine compute_type_hydraulic_crs

    end interface

    interface type_hydraulic_crs
        module procedure :: construct_type_hydraulic_crs
    end interface

contains

end module main_hydraulic
