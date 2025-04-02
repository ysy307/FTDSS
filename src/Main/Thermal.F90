module Main_Thermal
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types, only:Variables
    use :: Inout_Input
    use :: Calculate_Ice, only:Abstract_Ice, Type_Ice_TRM, Type_Ice_GCC, Type_Ice_EXP
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
    end type Abstract_Thermal

    type, extends(Abstract_Thermal) :: Type_Thermal_3Phase
        class(Abstract_Ice), allocatable :: Ice

    end type Type_Thermal_3Phase

contains
    function Type_Thermal_3Phase_Construct(Input_Ice_Param) result(structure)
        implicit none
        type(Type_Thermal_3Phase) :: structure
        type(Input_Ice), intent(in) :: Input_Ice_Param

        select case (Input_Ice_Param%QiceType)
        case (1)
            structure%Ice = Type_Ice_TRM(Input_Ice_Param%Lf, Input_Ice_Param%Tf, Input_Ice_Param%nsize)
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
                                                     nsize=Input_Ice_Param%nsize, &
                                                     thetaR=Input_Ice_Param%thetaR, &
                                                     thetaS=Input_Ice_Param%thetaS, &
                                                     alpha1=Input_Ice_Param%alpha1, &
                                                     n1=Input_Ice_Param%n1, &
                                                     Lf=Input_Ice_Param%Lf, &
                                                     Tf=Input_Ice_Param%Tf)
                    case (4)
                        structure%Ice = Type_Ice_GCC(ModelType=Input_Ice_Param%ModelType, &
                                                     isSegregation=Input_Ice_Param%isSegregation, &
                                                     c_unit=Input_Ice_Param%c_unit, &
                                                     nsize=Input_Ice_Param%nsize, &
                                                     thetaS=Input_Ice_Param%thetaS, &
                                                     thetaR=Input_Ice_Param%thetaR, &
                                                     alpha1=Input_Ice_Param%alpha1, &
                                                     hcrit=Input_Ice_Param%hcrit, &
                                                     n1=Input_Ice_Param%n1, &
                                                     Lf=Input_Ice_Param%Lf, &
                                                     Tf=Input_Ice_Param%Tf)
                    case (5)
                        structure%Ice = Type_Ice_GCC(ModelType=Input_Ice_Param%ModelType, &
                                                     isSegregation=Input_Ice_Param%isSegregation, &
                                                     c_unit=Input_Ice_Param%c_unit, &
                                                     nsize=Input_Ice_Param%nsize, &
                                                     thetaS=Input_Ice_Param%thetaS, &
                                                     thetaR=Input_Ice_Param%thetaR, &
                                                     alpha1=Input_Ice_Param%alpha1, &
                                                     n1=Input_Ice_Param%n1, &
                                                     w1=Input_Ice_Param%w1, &
                                                     alpha2=Input_Ice_Param%alpha2, &
                                                     n2=Input_Ice_Param%n2, &
                                                     Lf=Input_Ice_Param%Lf, &
                                                     Tf=Input_Ice_Param%Tf)
                    case (6)
                        structure%Ice = Type_Ice_GCC(ModelType=Input_Ice_Param%ModelType, &
                                                     isSegregation=Input_Ice_Param%isSegregation, &
                                                     c_unit=Input_Ice_Param%c_unit, &
                                                     nsize=Input_Ice_Param%nsize, &
                                                     thetaS=Input_Ice_Param%thetaS, &
                                                     thetaR=Input_Ice_Param%thetaR, &
                                                     alpha1=Input_Ice_Param%alpha1, &
                                                     n1=Input_Ice_Param%n1, &
                                                     n2=Input_Ice_Param%n2, &
                                                     Lf=Input_Ice_Param%Lf, &
                                                     Tf=Input_Ice_Param%Tf)
                    end select
                case ("Pa")
                !! TBD
                end select
            end if
        case (3)
            structure%Ice = Type_Ice_EXP(Input_Ice_Param%Lf, Input_Ice_Param%phi, Input_Ice_Param%Tf, Input_Ice_Param%a, Input_Ice_Param%nsize)
        end select

    end function
end module Main_Thermal
