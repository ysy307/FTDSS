module Allocate_Structure
    use :: error
    use :: Types
    use :: allocate
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    private

    public :: Allocate_DF
    public :: Allocate_DP2d
    public :: Allocate_BCinfo
    public :: Allocate_Solver
    public :: Allocate_Structure_Thermal_Type
    public :: Allocate_Structure_Ice_Type
    public :: Allocate_Structure_WRF_Type
    public :: Allocate_Structure_Hydraulic_Type

contains

    subroutine Allocate_DF(ar_DF, n)
        implicit none
        type(DF), intent(inout) :: ar_DF
        integer(int32), intent(in) :: n

        call Allocate_Vector(ar_DF%old, n)
        call Allocate_Vector(ar_DF%pre, n)
        call Allocate_Vector(ar_DF%new, n)
        call Allocate_Vector(ar_DF%dif, n)
        call Allocate_Vector(ar_DF%div, n)
        call Allocate_Vector(ar_DF%tmp, n)

    end subroutine Allocate_DF

    subroutine Allocate_DP2d(ar_DP2d, n)
        implicit none
        type(DP2d), intent(inout) :: ar_DP2d
        integer(int32), intent(in) :: n

        call Allocate_Vector(ar_DP2d%x, n)
        call Allocate_Vector(ar_DP2d%y, n)

    end subroutine Allocate_DP2d

    subroutine Allocate_INT2d(ar_INT2d, n)
        implicit none
        type(INT2d), intent(inout) :: ar_INT2d
        integer(int32), intent(in) :: n

        call Allocate_Vector(ar_INT2d%x, n)
        call Allocate_Vector(ar_INT2d%y, n)

    end subroutine Allocate_INT2d

    subroutine Allocate_BCinfo(BCinfo, nNode, nType, nEdge)
        implicit none
        type(BoudaryConditionInfo), intent(inout) :: BCinfo
        integer(int32), intent(in) :: nNode, nType
        integer(int32), intent(in), optional :: nEdge

        call Allocate_Vector(BCinfo%Node, nNode)
        call Allocate_Vector(BCinfo%TypeKey, nNode)
        call Allocate_Vector(BCinfo%type, nType)
        call Allocate_Vector(BCinfo%value, nType)
        if (present(nEdge)) then
            ! print*, nEdge
            call Allocate_INT2d(BCinfo%Edges, nEdge)
            call Allocate_Vector(BCinfo%EdgesDirection, nEdge)
            call Allocate_Vector(BCinfo%EdgesDistance, nEdge)
        end if

    end subroutine Allocate_BCinfo

    subroutine Allocate_Solver(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver

        !* Allocate Geometry2d
        call Allocate_Matrix(Solver%N%pElement, Solver%N%shape, Solver%N%element)
        call Allocate_Vector(Solver%N%vCood%x, Solver%N%node)
        call Allocate_Vector(Solver%N%vCood%y, Solver%N%node)
        call Allocate_Vector(Solver%N%eArea, Solver%N%element)
        call Allocate_Matrix(Solver%N%Basis%a, Solver%N%ShCoe, Solver%N%element)
        call Allocate_Matrix(Solver%N%Basis%b, Solver%N%ShCoe, Solver%N%element)
        call Allocate_Matrix(Solver%N%Basis%c, Solver%N%ShCoe, Solver%N%element)
        if (Solver%N%ShCoe == 4) call Allocate_Matrix(Solver%N%Basis%d, Solver%N%ShCoe, Solver%N%element)

        call Allocate_Vector(Solver%mWater%old, Solver%N%node)
        call Allocate_Vector(Solver%mWater%pre, Solver%N%node)
        call Allocate_Vector(Solver%mIce%old, Solver%N%node)
        call Allocate_Vector(Solver%mIce%pre, Solver%N%node)
        call Allocate_Vector(Solver%mIce%dif, Solver%N%node)

        call Allocate_DF(Solver%Si, Solver%N%node)

        if (Solver%isHeat) then
            call Allocate_DF(Solver%Heat%Variables%Cs, Solver%N%node)
            call Allocate_DF(Solver%Heat%Variables%Cp, Solver%N%node)
            call Allocate_DF(Solver%Heat%Variables%lambda, Solver%N%node)
            call Allocate_DF(Solver%Heat%Variables%rho, Solver%N%node)
            call Allocate_DF(Solver%Heat%Variables%Ca, Solver%N%node)

            call Allocate_DF(Solver%T, Solver%N%node)
            call Allocate_Vector(Solver%Heat%Rhs, Solver%N%node)
            call Allocate_Vector(Solver%Heat%Variables%Phase, Solver%N%node)
            call Allocate_Matrix(Solver%Heat%RA, Solver%N%node, Solver%N%node)
            call Allocate_DP2d(Solver%Heat%Variables%Tgrad, Solver%N%node)
            call Allocate_DP2d(Solver%Heat%Variables%TFlux, Solver%N%node)
        end if
        if (Solver%isWater) then
            call Allocate_DF(Solver%Water%Variables%Klh, Solver%N%node)
            call Allocate_DP2d(Solver%Water%Variables%wFlux, Solver%N%node)
            call Allocate_DP2d(Solver%Water%Variables%hGrad, Solver%N%node)

            call Allocate_DF(Solver%P, Solver%N%node)
            call Allocate_Vector(Solver%Water%Rhs, Solver%N%node)
            call Allocate_Matrix(Solver%Water%RA, Solver%N%node, Solver%N%node)
        end if

    end subroutine Allocate_Solver

    subroutine Allocate_Structure_Thermal_Type(Structure_Thermal, Flags)
        ! Allocate thermal structure type
        implicit none
        type(Type_Thermal), intent(inout) :: Structure_Thermal ! Thermal structure
        type(Type_Region_Flags), intent(in) :: Flags ! Region flags

        if (allocated(Structure_Thermal%Density)) deallocate (Structure_Thermal%Density)
        if (allocated(Structure_Thermal%SpecificHeat)) deallocate (Structure_Thermal%SpecificHeat)
        if (allocated(Structure_Thermal%ThermalConductivity)) deallocate (Structure_Thermal%ThermalConductivity)

        if (Flags%is1Phase) then
            allocate (Type_Density_1Phase :: Structure_Thermal%Density)
            allocate (Type_SpecificHeat_1Phase :: Structure_Thermal%SpecificHeat)
            allocate (Type_ThermalConductivity_1Phase :: Structure_Thermal%ThermalConductivity)
        else if (Flags%is2Phase) then
            allocate (Type_Density_2Phase :: Structure_Thermal%Density)
            allocate (Type_SpecificHeat_2Phase :: Structure_Thermal%SpecificHeat)
            allocate (Type_ThermalConductivity_2Phase :: Structure_Thermal%ThermalConductivity)
        else if (Flags%is3Phase) then
            allocate (Type_Density_3Phase :: Structure_Thermal%Density)
            allocate (Type_SpecificHeat_3Phase :: Structure_Thermal%SpecificHeat)
            if (Flags%isDispersity) then
                allocate (Type_ThermalConductivity_3Phase_Dispersity_2D :: Structure_Thermal%ThermalConductivity)
                ! allocate (Type_ThermalConductivity_3Phase_Dispersity_3D :: Structure_Thermal%ThermalConductivity)
            else
                allocate (Type_ThermalConductivity_3Phase :: Structure_Thermal%ThermalConductivity)
            end if
        end if

    end subroutine Allocate_Structure_Thermal_Type

    subroutine Allocate_Structure_Ice_Type(Structure_Thermal, QiceModelType)
        ! Allocate ice model type
        implicit none
        type(Type_Thermal), intent(inout) :: Structure_Thermal ! Thermal structure
        integer(int32), intent(in) :: QiceModelType ! Ice model type

        if (allocated(Structure_Thermal%Ice)) deallocate (Structure_Thermal%Ice)

        if (QiceModelType == 1) then
            allocate (Type_Ice_TRM :: Structure_Thermal%Ice)
        else if (QiceModelType == 2) then
            allocate (Type_Ice_GCC :: Structure_Thermal%Ice)
        else if (QiceModelType == 3) then
            allocate (Type_Ice_EXP :: Structure_Thermal%Ice)
        end if

    end subroutine Allocate_Structure_Ice_Type

    subroutine Allocate_Structure_WRF_Type(Structure_Thermal, WRFModelType)
        ! Allocate WRF model type
        implicit none
        type(Type_Thermal), intent(inout) :: Structure_Thermal ! Thermal structure
        integer(int32), intent(in) :: WRFModelType ! WRF model type

        if (.not. allocated(Structure_Thermal%Ice)) then
            print *, "Error: Ice structure is not allocated."
            return
        end if
        select type (Ice => Structure_Thermal%Ice)
        type is (Type_Ice_GCC)
            select case (WRFModelType)
            case (1)
                allocate (Type_WRF_BC :: Ice%WRF)
            case (2)
                allocate (Type_WRF_VG :: Ice%WRF)
            case (3)
                allocate (Type_WRF_KO :: Ice%WRF)
            case (4)
                allocate (Type_WRF_MVG :: Ice%WRF)
            case (5)
                allocate (Type_WRF_Durner :: Ice%WRF)
            case (6)
                allocate (Type_WRF_DVGCH :: Ice%WRF)
            case default
                print *, "Error: WRFModelType is not defined."
            end select
        end select

    end subroutine Allocate_Structure_WRF_Type

    subroutine Allocate_Structure_Hydraulic_Type(Structure_Hydraulic)
        ! Allocate hydraulic structure type
        implicit none
        type(Type_Hydraulic), intent(inout) :: Structure_Hydraulic ! Hydraulic structure

        if (Structure_Hydraulic%useHCF > 0) then
            call Allocate_Structure_HCF_Type(Structure_Hydraulic)
        end if
        if (Structure_Hydraulic%useImpedance) then
            allocate (Type_Impedance :: Structure_Hydraulic%Impedance)
        end if

        select case (Structure_Hydraulic%useKTDynamics)
        case (1:2)
            allocate (Type_KTDynamics :: Structure_Hydraulic%KTDynamics)
        end select

    end subroutine Allocate_Structure_Hydraulic_Type

    subroutine Allocate_Structure_HCF_Type(Structure_Hydraulic)
        ! Allocate WRF model type
        implicit none
        type(Type_Hydraulic), intent(inout) :: Structure_Hydraulic ! Hydraulic structure

        select case (Structure_Hydraulic%useHCF)
        case (1)
            allocate (Type_HCF_BC :: Structure_Hydraulic%HCF)
        case (2)
            allocate (Type_HCF_VG :: Structure_Hydraulic%HCF)
        case (3)
            allocate (Type_HCF_KO :: Structure_Hydraulic%HCF)
        case (4)
            allocate (Type_HCF_MVG :: Structure_Hydraulic%HCF)
        case (5)
            allocate (Type_HCF_Durner :: Structure_Hydraulic%HCF)
        case (6)
            allocate (Type_HCF_DVGCH :: Structure_Hydraulic%HCF)
        case default
            print *, "Error: HCFModelType is not defined."
        end select

    end subroutine Allocate_Structure_HCF_Type

end module Allocate_Structure
