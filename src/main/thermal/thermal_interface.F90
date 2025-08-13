module main_thermal
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_logger
    use :: stdlib_strings
    use :: module_core, only:allocate_array, deallocate_array, type_variable, type_dp_3d, type_state
    use :: module_domain, only:type_domain
    use :: module_properties, only:type_properties_manager
    use :: module_input, only:type_input
    use :: module_matrix, only:type_crs, operator(*), operator(+)
    use :: module_boundary, only:type_bc, mode_value, mode_nr
    use :: module_solver
    use :: module_control, only:type_time, type_iteration
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
        type(type_crs) :: KT_l
        type(type_crs) :: KT_old
        type(type_crs) :: CT_l
        ! type(type_crs), allocatable :: CT_old(:)

        real(real64), allocatable :: FT(:)
        real(real64), allocatable :: FT_old(:)
        real(real64), allocatable :: PHIT(:)
        real(real64), allocatable :: PHIT_old(:)

        !! Nonlinear solver
        character(:), allocatable :: algorithm

        !! Solver
        class(abst_solver), allocatable :: solver
        integer(int32) :: order

        procedure(abst_assemble_global), nopass, pointer :: assemble_global => null()
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
        subroutine abst_update(self, domain, property, temperature, porosity)
            import :: abst_thermal, type_domain, type_properties_manager, real64
            implicit none
            class(abst_thermal), intent(inout) :: self
            type(type_domain), intent(inout), target :: domain
            type(type_properties_manager), intent(inout) :: property
            real(real64), intent(in) :: temperature(:)
            real(real64), intent(in) :: porosity(:)

        end subroutine abst_update

        subroutine abst_shift(self)
            import :: abst_thermal
            implicit none
            class(abst_thermal), intent(inout) :: self

        end subroutine abst_shift

        subroutine abst_solve(self, temperature, time, iteration)
            import :: abst_thermal, type_time, type_iteration, type_variable
            implicit none
            class(abst_thermal), intent(inout) :: self
            type(type_time), intent(inout) :: time
            type(type_iteration), intent(inout) :: iteration
            type(type_variable), intent(inout) :: temperature

        end subroutine abst_solve

        subroutine abst_compute(self, domain, property, temperature, porosity, time, iteration, bc)
            import :: abst_thermal, type_domain, type_properties_manager, type_variable, type_time, type_iteration, type_bc
            implicit none
            class(abst_thermal), intent(inout) :: self
            type(type_domain), intent(inout) :: domain
            type(type_properties_manager), intent(inout) :: property
            type(type_variable), intent(inout) :: temperature
            type(type_variable), intent(inout) :: porosity
            type(type_time), intent(inout) :: time
            type(type_iteration), intent(inout) :: iteration
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

        module subroutine update_type_thermal_crs(self, domain, property, temperature, porosity)
            implicit none
            class(type_thermal_crs), intent(inout) :: self
            type(type_domain), intent(inout), target :: domain
            type(type_properties_manager), intent(inout) :: property
            real(real64), intent(in) :: temperature(:)
            real(real64), intent(in) :: porosity(:)

        end subroutine update_type_thermal_crs

        module subroutine shift_type_thermal_crs(self)
            implicit none
            class(type_thermal_crs), intent(inout) :: self

        end subroutine shift_type_thermal_crs

        module subroutine solve_type_thermal_crs(self, temperature, time, iteration)
            implicit none
            class(type_thermal_crs), intent(inout) :: self
            type(type_variable), intent(inout) :: temperature
            type(type_time), intent(inout) :: time
            type(type_iteration), intent(inout) :: iteration

        end subroutine solve_type_thermal_crs

        module subroutine compute_type_thermal_crs(self, domain, property, temperature, porosity, time, iteration, bc)
            implicit none
            class(type_thermal_crs), intent(inout) :: self
            type(type_domain), intent(inout) :: domain
            type(type_properties_manager), intent(inout) :: property
            type(type_variable), intent(inout) :: temperature
            type(type_variable), intent(inout) :: porosity
            type(type_time), intent(inout) :: time
            type(type_iteration), intent(inout) :: iteration
            type(type_bc), intent(inout) :: bc

        end subroutine compute_type_thermal_crs

    end interface

    interface type_thermal_crs
        module procedure :: construct_type_thermal_crs
    end interface

contains

end module main_thermal
