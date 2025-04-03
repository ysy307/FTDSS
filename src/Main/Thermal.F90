module Main_Thermal
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types, only:Variables, DP3d, Vector2d
    use :: Inout_Input
    use :: Calculate_Ice, only:Abstract_Ice, Type_Ice_TRM, Type_Ice_GCC, Type_Ice_EXP
    use :: Calculate_VolumetricHeatCapacity
    use :: Calculate_ThermalConductivity
    use :: Calculate_Area, only:Update_Area
    use :: Matrix_CRS
    implicit none

    type, abstract :: Abstract_Thermal
        type(Variables) :: T
        type(Type_CRS) :: KT_star_0
        type(Type_CRS) :: KT_l
        type(Type_CRS) :: KT_old
        type(Type_CRS) :: CT
        type(Type_CRS) :: CT_old
        real(real64), allocatable :: FT(:)
        real(real64), allocatable :: FT_old(:)
        real(real64), allocatable :: PHIT(:)
        real(real64), allocatable :: PHIT_old(:)

        real(real64), allocatable :: Area(:)
        integer(int32), allocatable :: Element(:, :)
        type(DP3d) :: Coordinate
        real(real64), allocatable :: Basis(:, :, :)
    end type Abstract_Thermal

    type, extends(Abstract_Thermal) :: Type_Thermal_3Phase
        class(Abstract_Ice), allocatable :: Ice
        type(Type_VolumetricHeatCapacity_3Phase) :: C
        type(Type_ThermalConductivity_3Phase) :: lambda

    end type Type_Thermal_3Phase

contains
    function Type_Thermal_3Phase_Construct(Elements, Coordinate, meshType, Lf, Tf, Input_Ice_Param, Input_Thermal_Params, nsize) result(structure)
        implicit none
        type(Type_Thermal_3Phase) :: structure
        integer(int32), intent(in) :: Elements(:, :)
        type(DP3d), intent(in) :: Coordinate
        integer(int32), intent(in) :: meshType
        real(real64), intent(in) :: Lf
        real(real64), intent(in) :: Tf
        type(Input_Ice), intent(in) :: Input_Ice_Param
        type(Input_Thermal), intent(in) :: Input_Thermal_Params
        integer(int32), intent(in) :: nsize

        allocate (structure%Element, source=Elements)
        call Allocate_Array(structure%Coordinate%x, nsize)
        call Allocate_Array(structure%Coordinate%y, nsize)
        call Allocate_Array(structure%Coordinate%z, nsize)
        structure%Coordinate%x(:) = Coordinate%x(:)
        structure%Coordinate%y(:) = Coordinate%y(:)
        structure%Coordinate%z(:) = Coordinate%z(:)
        call Allocate_Array(structure%Area, nsize)
        call Update_Area(structure%Element, structure%Coordinate, structure%Area)
        if (meshType == 3) then
            allocate (structure%Basis(3, 3, nsize))
            call Calculate_Basis(structure%Element, structure%Coordinate, structure%Basis)
        end if

        structure%KT_star_0 = Type_CRS(Elements, nsize)
        structure%KT_l = structure%KT_star_0%Copy()
        structure%KT_old = structure%KT_star_0%Copy()
        structure%CT = structure%KT_star_0%Copy()
        structure%CT_old = structure%KT_star_0%Copy()
        call Allocate_Array(structure%FT, nsize)
        call Allocate_Array(structure%FT_old, nsize)
        call Allocate_Array(structure%PHIT, nsize)
        call Allocate_Array(structure%PHIT_old, nsize)

        call Allocate_Array(structure%T%pre, nsize)
        call Allocate_Array(structure%T%old, nsize)
        call Allocate_Array(structure%T%new, nsize)

        select case (Input_Ice_Param%QiceType)
        case (1)
            structure%Ice = Type_Ice_TRM(Lf, Tf, nsize)
        case (2)
            if (Input_Ice_Param%isSegregation) then
                !!! TBI
            else
                select case (Input_Ice_Param%c_unit)
                case ("m")
                    select case (Input_Ice_Param%ModelType)
                    case (1:3)
                        structure%Ice = Type_Ice_GCC(ModelType=Input_Ice_Param%ModelType, &
                                                     isSegregation=Input_Ice_Param%isSegregation, &
                                                     c_unit=Input_Ice_Param%c_unit, &
                                                     nsize=nsize, &
                                                     thetaR=Input_Ice_Param%thetaR, &
                                                     thetaS=Input_Ice_Param%thetaS, &
                                                     alpha1=Input_Ice_Param%alpha1, &
                                                     n1=Input_Ice_Param%n1, &
                                                     Lf=Lf, &
                                                     Tf=Tf)
                    case (4)
                        structure%Ice = Type_Ice_GCC(ModelType=Input_Ice_Param%ModelType, &
                                                     isSegregation=Input_Ice_Param%isSegregation, &
                                                     c_unit=Input_Ice_Param%c_unit, &
                                                     nsize=nsize, &
                                                     thetaS=Input_Ice_Param%thetaS, &
                                                     thetaR=Input_Ice_Param%thetaR, &
                                                     alpha1=Input_Ice_Param%alpha1, &
                                                     hcrit=Input_Ice_Param%hcrit, &
                                                     n1=Input_Ice_Param%n1, &
                                                     Lf=Lf, &
                                                     Tf=Tf)
                    case (5)
                        structure%Ice = Type_Ice_GCC(ModelType=Input_Ice_Param%ModelType, &
                                                     isSegregation=Input_Ice_Param%isSegregation, &
                                                     c_unit=Input_Ice_Param%c_unit, &
                                                     nsize=nsize, &
                                                     thetaS=Input_Ice_Param%thetaS, &
                                                     thetaR=Input_Ice_Param%thetaR, &
                                                     alpha1=Input_Ice_Param%alpha1, &
                                                     n1=Input_Ice_Param%n1, &
                                                     w1=Input_Ice_Param%w1, &
                                                     alpha2=Input_Ice_Param%alpha2, &
                                                     n2=Input_Ice_Param%n2, &
                                                     Lf=Lf, &
                                                     Tf=Tf)
                    case (6)
                        structure%Ice = Type_Ice_GCC(ModelType=Input_Ice_Param%ModelType, &
                                                     isSegregation=Input_Ice_Param%isSegregation, &
                                                     c_unit=Input_Ice_Param%c_unit, &
                                                     nsize=nsize, &
                                                     thetaS=Input_Ice_Param%thetaS, &
                                                     thetaR=Input_Ice_Param%thetaR, &
                                                     alpha1=Input_Ice_Param%alpha1, &
                                                     n1=Input_Ice_Param%n1, &
                                                     n2=Input_Ice_Param%n2, &
                                                     Lf=Lf, &
                                                     Tf=Tf)
                    end select
                case ("Pa")
                !! TBD
                end select
            end if
        case (3)
            structure%Ice = Type_Ice_EXP(Lf, Input_Ice_Param%phi, Tf, Input_Ice_Param%a, nsize)
        end select

        structure%C = Type_VolumetricHeatCapacity_3Phase(structure%Ice, &
                                                         Input_Thermal_Params%Cp(1), &
                                                         Input_Thermal_Params%Cp(2), &
                                                         Input_Thermal_Params%Cp(3), &
                                                         Input_Thermal_Params%rho(3), &
                                                         Input_Thermal_Params%rho(2), &
                                                         nsize)

        structure%lambda = Type_ThermalConductivity_3Phase(Input_Thermal_Params%lambda(1), &
                                                           Input_Thermal_Params%lambda(2), &
                                                           Input_Thermal_Params%lambda(3), &
                                                           nsize)

    end function Type_Thermal_3Phase_Construct
end module Main_Thermal
