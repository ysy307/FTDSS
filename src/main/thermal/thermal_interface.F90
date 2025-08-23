module main_thermal
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_logger
    use :: stdlib_strings
    use :: module_core, only:allocate_array, deallocate_array, type_variable, type_dp_3d, type_state
    use :: module_domain, only:type_domain
    use :: module_calculate, only:abst_den
    use :: module_properties, only:type_properties_manager, type_phase_property
    use :: module_input, only:type_input
    use :: module_matrix, only:type_crs, gemv, add
    use :: module_boundary, only:type_bc, mode_value, mode_nr
    use :: module_solver
    use :: module_control
    use :: thermal_thermal_assemble
    implicit none
    private

    public :: abst_thermal
    public :: type_thermal_crs

    type, abstract :: abst_thermal
        type(type_variable) :: Qw
        type(type_variable) :: Qice
        type(type_variable) :: Si

        type(type_crs) :: KT_star
        real(real64), allocatable :: FT(:)
        real(real64), allocatable :: PHIT(:)

        !! Nonlinear solver
        character(:), allocatable :: algorithm

        !! Solver
        class(abst_solver), allocatable :: solver
        integer(int32) :: order

        procedure(abst_assemble_global_thermal), nopass, pointer :: assemble_global => null()
    contains
        procedure(abst_update), pass(self), deferred :: update
        procedure(abst_shift), pass(self), deferred :: shift
        procedure(abst_solve), pass(self), deferred :: solve
        procedure(abst_compute), pass(self), deferred :: compute
    end type abst_thermal

    type, extends(abst_thermal) :: type_thermal_crs
    contains
        procedure :: update => update_type_thermal_crs
        procedure :: shift => shift_type_thermal_crs
        procedure :: solve => solve_type_thermal_crs
        procedure :: compute => compute_type_thermal_crs
    end type type_thermal_crs

    abstract interface
        subroutine abst_update(self, domain, property, temperature, porosity, controls)
            import :: abst_thermal, type_domain, type_properties_manager, real64, type_controls
            implicit none
            class(abst_thermal), intent(inout) :: self
            type(type_domain), intent(inout), target :: domain
            type(type_properties_manager), intent(inout) :: property
            real(real64), intent(in) :: temperature(:)
            real(real64), intent(in) :: porosity(:)
            type(type_controls), intent(in) :: controls

        end subroutine abst_update

        subroutine abst_shift(self)
            import :: abst_thermal
            implicit none
            class(abst_thermal), intent(inout) :: self

        end subroutine abst_shift

        subroutine abst_solve(self, temperature, controls)
            import :: abst_thermal, type_controls, type_variable
            implicit none
            class(abst_thermal), intent(inout) :: self
            type(type_variable), intent(inout) :: temperature
            type(type_controls), intent(in) :: controls

        end subroutine abst_solve

        subroutine abst_compute(self, domain, property, temperature, porosity, controls, bc)
            import :: abst_thermal, type_domain, type_properties_manager, type_variable, type_controls, type_bc
            implicit none
            class(abst_thermal), intent(inout) :: self
            type(type_domain), intent(inout) :: domain
            type(type_properties_manager), intent(in) :: property
            type(type_variable), intent(inout) :: temperature
            type(type_variable), intent(inout) :: porosity
            type(type_controls), intent(inout) :: controls
            type(type_bc), intent(inout) :: bc

        end subroutine abst_compute
    end interface

    interface
        module function construct_type_thermal_crs(input, coordinate, domain) result(structure)
            implicit none
            class(abst_thermal), allocatable :: structure
            type(type_input), intent(inout) :: input
            type(type_dp_3d), intent(inout), pointer :: coordinate
            type(type_domain), intent(inout) :: domain

        end function construct_type_thermal_crs

        module subroutine update_type_thermal_crs(self, domain, property, temperature, porosity, controls)
            implicit none
            class(type_thermal_crs), intent(inout) :: self
            type(type_domain), intent(inout), target :: domain
            type(type_properties_manager), intent(inout) :: property
            real(real64), intent(in) :: temperature(:)
            real(real64), intent(in) :: porosity(:)
            type(type_controls), intent(in) :: controls

        end subroutine update_type_thermal_crs

        module subroutine shift_type_thermal_crs(self)
            implicit none
            class(type_thermal_crs), intent(inout) :: self

        end subroutine shift_type_thermal_crs

        module subroutine solve_type_thermal_crs(self, temperature, controls)
            implicit none
            class(type_thermal_crs), intent(inout) :: self
            type(type_variable), intent(inout) :: temperature
            type(type_controls), intent(in) :: controls

        end subroutine solve_type_thermal_crs

        module subroutine compute_type_thermal_crs(self, domain, property, temperature, porosity, controls, bc)
            implicit none
            class(type_thermal_crs), intent(inout) :: self
            type(type_domain), intent(inout) :: domain
            type(type_properties_manager), intent(in) :: property
            type(type_variable), intent(inout) :: temperature
            type(type_variable), intent(inout) :: porosity
            type(type_controls), intent(inout) :: controls
            type(type_bc), intent(inout) :: bc

        end subroutine compute_type_thermal_crs

    end interface

    interface type_thermal_crs
        module procedure :: construct_type_thermal_crs
    end interface

contains

end module main_thermal
