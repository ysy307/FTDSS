module Main_Thermal
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:allocate_array, deallocate_array, type_variable, type_dp_3d
    ! use :: Core_BaseTypes
    ! use :: Core_Allocate, only:Allocate_Array
    use :: module_domain, only:type_domain
    use :: Properties_Model_Base, only:Proereties_Model_t
    ! use :: Core_Element
    ! use :: Core_Side
    use :: Inout_Input
    ! use :: Calculate_Ice
    ! use :: Calculate_ThermalConductivity
    ! use :: Calculate_Density
    ! use :: Calculate_SpecificHeat
    ! use :: Calculate_HeatCapacity
    ! use :: Matrix_Assemble
    use :: Matrix_CRS, only:type_crs, operator(*), operator(+)
    ! use :: Condition_Initial
    use :: Condition_Boundary
    use :: Solver_Solve
    use :: module_control, only:type_time
    ! use :: Matrix_RCM, only:RCM_Reorder, RCM_Reorder_Inverse

    use :: thermal_thermal_assemble
    ! use :: thermal_thermal_assemble, only:Assemble_Mass_Heat_1_Parallel, Assemble_Diffusion_Heat_1_Parallel
    implicit none

    type, abstract :: Abstract_Thermal
        type(type_variable) :: T
        type(type_variable) :: Qw
        type(type_variable) :: Qice
        type(type_variable) :: D_Qice
        type(type_variable) :: Si

        type(type_crs) :: KT_star_0
        type(type_crs) :: KT_l
        type(type_crs) :: KT_old
        type(type_crs) :: CT_l
        type(type_crs), allocatable :: CT_old(:)

        real(real64), allocatable :: FT(:)
        real(real64), allocatable :: FT_old(:)
        real(real64), allocatable :: PHIT(:)
        real(real64), allocatable :: PHIT_old(:)

        ! ! type(DP3d), pointer :: Coordinate
        ! integer(int32) :: nsize
        ! integer(int32) :: nElement
        ! integer(int32) :: nSide
        ! integer(int32) :: nRegion

        ! type(ElementHolder), allocatable :: Elements(:)
        ! type(SideHolder), allocatable :: Sides(:)
        ! class(Abstract_Condition_BC), allocatable :: BC
        ! class(Abstract_Condition_IC), allocatable :: IC
        !! Thermal properties

        ! class(Abstract_ThermalConductivity), allocatable :: THC
        ! class(Abstract_Density), allocatable :: DEN
        ! class(Abstract_SpecificHeat), allocatable :: SPH
        ! type(HTCHolder), allocatable :: HTC(:)
        ! type(IceHolder), allocatable :: ICE(:)

        !! Solver
        class(Abstract_Solver_CRS), allocatable :: Solver
        integer(int32) :: Order
    contains
        ! procedure(Abstract_Update), pass(self), deferred :: Update
        procedure(Abstract_Assemble), pass(self), deferred :: Assemble
    end type Abstract_Thermal

    type, extends(Abstract_Thermal) :: Type_Thermal_3Phase_2D
    contains
        ! procedure :: Update => Type_Thermal_3Phase_2D_Update
        procedure :: Assemble => Type_Thermal_3Phase_2D_Assemble
    end type Type_Thermal_3Phase_2D

    abstract interface
        ! subroutine Abstract_Update(self, arr_phi)
        !     import :: Abstract_Thermal, real64
        !     implicit none
        !     class(Abstract_Thermal), intent(inout) :: self
        !     ! type(Belonging), intent(inout), optional :: NodeBelonging(:)
        !     real(real64), intent(inout) :: arr_phi(:)

        ! end subroutine Abstract_Update

        subroutine Abstract_Assemble(self, Domain, Property, Porosity, dt, step, iter)
            import :: Abstract_Thermal, int32, real64, type_domain, Proereties_Model_t
            implicit none
            class(Abstract_Thermal), intent(inout) :: self
            type(type_domain), intent(inout) :: Domain
            type(Proereties_Model_t), intent(inout) :: Property
            real(real64), intent(in) :: Porosity(:)
            real(real64), intent(in) :: dt
            integer(int32), intent(in) :: step
            integer(int32), intent(in) :: iter

        end subroutine Abstract_Assemble
    end interface

    interface
        module function Type_Thermal_3Phase_2D_Construct(Input, Coordinate, Domain) result(Structure)
            implicit none
            class(Abstract_Thermal), allocatable :: Structure
            type(Type_Input), intent(inout) :: Input
            type(type_dp_3d), intent(inout), pointer :: Coordinate
            type(type_domain), intent(inout) :: Domain

        end function Type_Thermal_3Phase_2D_Construct

        ! module subroutine Type_Thermal_3Phase_2D_Update(self, NodeBelonging, arr_phi)
        !     implicit none
        !     class(Type_Thermal_3Phase_2D), intent(inout) :: self
        !     type(Belonging), intent(inout), optional :: NodeBelonging(:)
        !     real(real64), intent(inout) :: arr_phi(:)

        ! end subroutine Type_Thermal_3Phase_2D_Update

        module subroutine Type_Thermal_3Phase_2D_Assemble(self, Domain, Property, Porosity, dt, step, iter)
            implicit none
            class(Type_Thermal_3Phase_2D), intent(inout) :: self
            type(type_domain), intent(inout) :: Domain
            type(Proereties_Model_t), intent(inout) :: Property
            real(real64), intent(in) :: Porosity(:)
            real(real64), intent(in) :: dt
            integer(int32), intent(in) :: step
            integer(int32), intent(in) :: iter

        end subroutine Type_Thermal_3Phase_2D_Assemble

    end interface

    interface Type_Thermal_3Phase_2D
        module procedure :: Type_Thermal_3Phase_2D_Construct
    end interface

contains

end module Main_Thermal
