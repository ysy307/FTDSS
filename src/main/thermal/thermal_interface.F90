module main_thermal
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_logger
    use :: stdlib_strings
    use :: module_core, only:allocate_array, deallocate_array, type_variable, type_dp_3d
    use :: module_domain, only:type_domain
    use :: module_properties, only:type_proereties_manager
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
        type(type_variable) :: D_Qice
        type(type_variable) :: Si

        type(type_crs) :: KT_star_0
        type(type_crs) :: KT_star
        type(type_crs) :: KT_l
        type(type_crs) :: KT_old
        type(type_crs) :: CT_l
        type(type_crs), allocatable :: CT_old(:)

        real(real64), allocatable :: FT(:)
        real(real64), allocatable :: FT_old(:)
        real(real64), allocatable :: PHIT(:)
        real(real64), allocatable :: PHIT_old(:)

        !! Solver
        class(abst_solver), allocatable :: solver
        integer(int32) :: order

        procedure(abst_assemble_local), pointer, nopass :: assemble_mass => null()
        procedure(abst_assemble_local), pointer, nopass :: assemble_diffusive => null()
    contains
        ! procedure(Abstract_Update), pass(self), deferred :: Update
        procedure(abst_assemble), pass(self), deferred :: assemble
    end type abst_thermal

    type, extends(abst_thermal) :: type_thermal_crs
    contains
        ! procedure :: Update => type_thermal_crs_Update
        procedure :: assemble => assemble_type_thermal_crs
    end type type_thermal_crs

    abstract interface
        ! subroutine Abstract_Update(self, arr_phi)
        !     import :: abst_thermal, real64
        !     implicit none
        !     class(abst_thermal), intent(inout) :: self
        !     ! type(Belonging), intent(inout), optional :: NodeBelonging(:)
        !     real(real64), intent(inout) :: arr_phi(:)

        ! end subroutine Abstract_Update

        subroutine abst_assemble(self, domain, property, porosity, time, iteration)
            import :: abst_thermal, type_domain, type_proereties_manager, type_time, type_iteration, real64
            implicit none
            class(abst_thermal), intent(inout) :: self
            type(type_domain), intent(inout) :: domain
            type(type_proereties_manager), intent(inout) :: property
            real(real64), intent(in) :: porosity(:)
            type(type_time), intent(in) :: time
            type(type_iteration), intent(in) :: iteration

        end subroutine abst_assemble
    end interface

    interface
        module function construct_type_thermal_crs(input, coordinate, domain) result(structure)
            implicit none
            class(abst_thermal), allocatable :: structure
            type(type_input), intent(inout) :: input
            type(type_dp_3d), intent(inout), pointer :: coordinate
            type(type_domain), intent(inout) :: domain

        end function construct_type_thermal_crs

        ! module subroutine type_thermal_crs_Update(self, NodeBelonging, arr_phi)
        !     implicit none
        !     class(type_thermal_crs), intent(inout) :: self
        !     type(Belonging), intent(inout), optional :: NodeBelonging(:)
        !     real(real64), intent(inout) :: arr_phi(:)

        ! end subroutine type_thermal_crs_Update

        module subroutine assemble_type_thermal_crs(self, domain, property, porosity, time, iteration)
            implicit none
            class(type_thermal_crs), intent(inout) :: self
            type(type_domain), intent(inout) :: domain
            type(type_proereties_manager), intent(inout) :: property
            real(real64), intent(in) :: porosity(:)
            type(type_time), intent(in) :: time
            type(type_iteration), intent(in) :: iteration

        end subroutine assemble_type_thermal_crs

    end interface

    interface type_thermal_crs
        module procedure :: construct_type_thermal_crs
    end interface

contains

end module main_thermal
