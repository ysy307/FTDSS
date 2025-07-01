module Main_Thermal
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes
    use :: Core_Element
    use :: Core_Side
    use :: Inout_Input
    use :: Calculate_Ice
    use :: Calculate_ThermalConductivity
    use :: Calculate_Density
    use :: Calculate_SpecificHeat
    use :: Calculate_HeatCapacity
    use :: Matrix_Assemble
    use :: Matrix_CRS
    use :: Condition_Boundary
    use :: Condition_Initial
    use :: Solver_Solve
    implicit none

    type, abstract :: Abstract_Thermal
        type(Variables) :: T
        type(Variables) :: Qw
        type(Variables) :: Qice
        type(Variables) :: D_Qice
        type(Variables) :: Si

        type(Type_CRS) :: KT_star_0
        type(Type_CRS) :: KT_l
        type(Type_CRS) :: KT_old
        type(Type_CRS) :: CT_l
        type(Type_CRS), allocatable :: CT_old(:)

        real(real64), allocatable :: FT(:)
        real(real64), allocatable :: FT_old(:)
        real(real64), allocatable :: PHIT(:)
        real(real64), allocatable :: PHIT_old(:)

        ! type(DP3d), pointer :: Coordinate
        integer(int32) :: nsize
        integer(int32) :: nElement
        integer(int32) :: nSide
        integer(int32) :: nRegion
        type(ElementHolder), allocatable :: Elements(:)
        type(SideHolder), allocatable :: Sides(:)
        class(Abstract_Condition_BC), allocatable :: BC
        class(Abstract_Condition_IC), allocatable :: IC
        !! Thermal properties
        class(Abstract_ThermalConductivity), allocatable :: THC
        class(Abstract_Density), allocatable :: DEN
        class(Abstract_SpecificHeat), allocatable :: SPH
        type(HTCHolder), allocatable :: HTC(:)
        type(IceHolder), allocatable :: ICE(:)

        !! Solver
        class(Abstract_Solver_CRS), allocatable :: Solver
        integer(int32) :: Order
    contains
        procedure(Abstract_Update), pass(self), deferred :: Update
        procedure(Abstract_Assemble), pass(self), deferred :: Assemble
    end type Abstract_Thermal

    type, extends(Abstract_Thermal) :: Type_Thermal_3Phase_2D
    contains
        procedure :: Update => Type_Thermal_3Phase_2D_Update
        procedure :: Assemble => Type_Thermal_3Phase_2D_Assemble
    end type Type_Thermal_3Phase_2D

    abstract interface
        subroutine Abstract_Update(self, NodeBelonging, arr_phi)
            import :: Abstract_Thermal, Belonging, real64
            implicit none
            class(Abstract_Thermal), intent(inout) :: self
            type(Belonging), intent(inout), optional :: NodeBelonging(:)
            real(real64), intent(inout) :: arr_phi(:)

        end subroutine Abstract_Update

        subroutine Abstract_Assemble(self, dt, step, iter)
            import :: Abstract_Thermal, int32, real64
            implicit none
            class(Abstract_Thermal), intent(inout) :: self
            real(real64), intent(in) :: dt
            integer(int32), intent(in) :: step
            integer(int32), intent(in) :: iter

        end subroutine Abstract_Assemble
    end interface

    interface
        module function Type_Thermal_3Phase_2D_Construct(Input, Coordinate) result(Structure)
            implicit none
            class(Abstract_Thermal), allocatable :: Structure
            type(Type_Input), intent(inout) :: Input
            type(DP3d), intent(inout), pointer :: Coordinate

        end function Type_Thermal_3Phase_2D_Construct

        module subroutine Type_Thermal_3Phase_2D_Update(self, NodeBelonging, arr_phi)
            implicit none
            class(Type_Thermal_3Phase_2D), intent(inout) :: self
            type(Belonging), intent(inout), optional :: NodeBelonging(:)
            real(real64), intent(inout) :: arr_phi(:)

        end subroutine Type_Thermal_3Phase_2D_Update

        module subroutine Type_Thermal_3Phase_2D_Assemble(self, dt, step, iter)
            implicit none
            class(Type_Thermal_3Phase_2D), intent(inout) :: self
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
