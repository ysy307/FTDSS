module Calculate_HeatConductivity
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Allocate_Allocate, only:Allocate_Array
    use :: Calculate_Ice
    implicit none

    type, abstract :: Abstract_HeatConductivity
        real(real64), allocatable :: value(:)
    end type Abstract_HeatConductivity

    type, extends(Abstract_HeatConductivity) :: Type_HeatConductivity_1Phase
        real(real64) :: lamba1
    end type Type_HeatConductivity_1Phase

    type, extends(Abstract_HeatConductivity) :: Type_HeatConductivity_2Phase
        real(real64) :: lamba1
        real(real64) :: lamba2
    end type Type_HeatConductivity_2Phase

    type, extends(Abstract_HeatConductivity) :: Type_HeatConductivity_3Phase
        real(real64) :: lamba_soil
        real(real64) :: lamba_water
        real(real64) :: lamba_ice
        class(Abstract_Ice), pointer :: Ice
    end type Type_HeatConductivity_3Phase

    type, extends(Abstract_HeatConductivity) :: Type_HeatConductivity_3Phase_Dispersed_2D
        real(real64) :: lamba_soil
        real(real64) :: lamba_water
        real(real64) :: lamba_ice
        class(Abstract_Ice), pointer :: Ice
    end type Type_HeatConductivity_3Phase_Dispersed_2D

    type, extends(Abstract_HeatConductivity) :: Type_HeatConductivity_3Phase_Dispersed_3D
        real(real64) :: lamba_soil
        real(real64) :: lamba_water
        real(real64) :: lamba_ice
        class(Abstract_Ice), pointer :: Ice
    end type Type_HeatConductivity_3Phase_Dispersed_3D

    type, extends(Abstract_HeatConductivity) :: Type_HeatConductivity_4Phase
        real(real64) :: lamba_soil
        real(real64) :: lamba_water
        real(real64) :: lamba_ice
        real(real64) :: lamba_air
        class(Abstract_Ice), pointer :: Ice
    end type Type_HeatConductivity_4Phase

    type, extends(Abstract_HeatConductivity) :: Type_HeatConductivity_4Phase_Dispersed_2D
        real(real64) :: lamba_soil
        real(real64) :: lamba_water
        real(real64) :: lamba_ice
        real(real64) :: lamba_air
        class(Abstract_Ice), pointer :: Ice
    end type Type_HeatConductivity_4Phase_Dispersed_2D

    type, extends(Abstract_HeatConductivity) :: Type_HeatConductivity_4Phase_Dispersed_3D
        real(real64) :: lamba_soil
        real(real64) :: lamba_water
        real(real64) :: lamba_ice
        real(real64) :: lamba_air
        class(Abstract_Ice), pointer :: Ice
    end type Type_HeatConductivity_4Phase_Dispersed_3D

contains
    !----------------------------------------------------------------------------------------------------
    ! Constructe each type of heat conductivity
    !----------------------------------------------------------------------------------------------------

end module Calculate_HeatConductivity
