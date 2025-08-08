module main_thermal
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_logger
    use :: stdlib_strings
    use :: module_core, only:allocate_array, deallocate_array, type_variable, type_dp_3d, type_gauss_point_state
    use :: module_domain, only:type_domain
    use :: module_properties, only:type_properties_manager
    use :: module_input, only:type_input
    use :: module_matrix, only:type_crs, operator(*), operator(+)
    use :: module_boundary
    use :: module_solver
    use :: module_control, only:type_time, type_iteration
    use :: thermal_thermal_assemble
    implicit none
    private

    public :: abst_thermal
    public :: type_thermal_crs

    type, abstract :: abst_thermal
        type(type_variable) :: T
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

        procedure(abst_assemble_local), pointer, nopass :: assemble_mass => null()
        procedure(abst_assemble_local), pointer, nopass :: assemble_diffusive => null()
    contains
        procedure(abst_update), pass(self), deferred :: update
        procedure(abst_shift), pass(self), deferred :: shift
        procedure(abst_assemble), pass(self), deferred :: assemble
        procedure(abst_solve), pass(self), deferred :: solve
    end type abst_thermal

    type, extends(abst_thermal) :: type_thermal_crs
    contains
        procedure :: update => update_type_thermal_crs
        procedure :: shift => shift_type_thermal_crs
        procedure :: assemble => assemble_type_thermal_crs
        procedure :: solve => solve_type_thermal_crs
    end type type_thermal_crs

    abstract interface
        subroutine abst_update(self, domain, property, porosity)
            import :: abst_thermal, type_domain, type_properties_manager, real64
            implicit none
            class(abst_thermal), intent(inout) :: self
            type(type_domain), intent(inout), target :: domain
            type(type_properties_manager), intent(inout) :: property
            real(real64), intent(in) :: porosity(:)

        end subroutine abst_update

        subroutine abst_shift(self)
            import :: abst_thermal
            implicit none
            class(abst_thermal), intent(inout) :: self

        end subroutine abst_shift

        subroutine abst_assemble(self, domain, property, porosity, time, iteration)
            import :: abst_thermal, type_domain, type_properties_manager, type_time, type_iteration, real64
            implicit none
            class(abst_thermal), intent(inout) :: self
            type(type_domain), intent(inout) :: domain
            type(type_properties_manager), intent(inout) :: property
            real(real64), intent(in) :: porosity(:)
            type(type_time), intent(in) :: time
            type(type_iteration), intent(in) :: iteration

        end subroutine abst_assemble

        subroutine abst_solve(self, time, iteration)
            import :: abst_thermal, type_time, type_iteration
            implicit none
            class(abst_thermal), intent(inout) :: self
            type(type_time), intent(inout) :: time
            type(type_iteration), intent(inout) :: iteration
        end subroutine abst_solve
    end interface

    interface
        module function construct_type_thermal_crs(input, coordinate, domain) result(structure)
            implicit none
            class(abst_thermal), allocatable :: structure
            type(type_input), intent(inout) :: input
            type(type_dp_3d), intent(inout), pointer :: coordinate
            type(type_domain), intent(inout) :: domain

        end function construct_type_thermal_crs

        module subroutine update_type_thermal_crs(self, domain, property, porosity)
            implicit none
            class(type_thermal_crs), intent(inout) :: self
            type(type_domain), intent(inout), target :: domain
            type(type_properties_manager), intent(inout) :: property
            real(real64), intent(in) :: porosity(:)

        end subroutine update_type_thermal_crs

        module subroutine shift_type_thermal_crs(self)
            implicit none
            class(type_thermal_crs), intent(inout) :: self

        end subroutine shift_type_thermal_crs

        module subroutine assemble_type_thermal_crs(self, domain, property, porosity, time, iteration)
            implicit none
            class(type_thermal_crs), intent(inout) :: self
            type(type_domain), intent(inout) :: domain
            type(type_properties_manager), intent(inout) :: property
            real(real64), intent(in) :: porosity(:)
            type(type_time), intent(in) :: time
            type(type_iteration), intent(in) :: iteration

        end subroutine assemble_type_thermal_crs

        module subroutine solve_type_thermal_crs(self, time, iteration)
            implicit none
            class(type_thermal_crs), intent(inout) :: self
            type(type_time), intent(inout) :: time
            type(type_iteration), intent(inout) :: iteration

        end subroutine solve_type_thermal_crs

    end interface

    interface type_thermal_crs
        module procedure :: construct_type_thermal_crs
    end interface

contains

end module main_thermal
