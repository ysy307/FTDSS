module Main_Thermal
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types, only:Variables, DP3d, Vector2d
    use :: Inout_Input
    use :: Calculate_Ice, only:Abstract_Ice, Type_Ice_TRM, Type_Ice_GCC, Type_Ice_EXP
    use :: Calculate_VolumetricHeatCapacity
    use :: Calculate_ThermalConductivity
    use :: Calculate_Area, only:Update_Area
    use :: Calculate_Shape, only:Calculate_Basis
    use :: Matrix_Assemble
    use :: Matrix_CRS
    use :: Condition_Fix_Boundary_Conditions, only:Type_BC_Thermal
    use :: Solver_Solve
    implicit none

    type, abstract :: Abstract_Thermal
        type(Variables) :: T
        type(Type_CRS) :: KT_star_0
        type(Type_CRS) :: KT_l
        type(Type_CRS) :: KT_old
        type(Type_CRS) :: CT_l
        type(Type_CRS), allocatable :: CT_old(:)

        real(real64), allocatable :: FT(:)
        real(real64), allocatable :: FT_old(:)
        real(real64), allocatable :: PHIT(:)
        real(real64), allocatable :: PHIT_old(:)

        integer(int32) :: nsize
        integer(int32) :: nElement
        real(real64), allocatable :: Area(:)
        integer(int32), allocatable :: Element(:, :)
        type(DP3d) :: Coordinate
        real(real64), allocatable :: Basis(:, :, :)
        type(Type_BC_Thermal) :: BC
        class(Abstract_Solver_CRS), allocatable :: Solver
    end type Abstract_Thermal

    type, extends(Abstract_Thermal) :: Type_Thermal_3Phase
        class(Abstract_Ice), allocatable :: Ice
        type(Type_VolumetricHeatCapacity_3Phase) :: C
        type(Type_ThermalConductivity_3Phase) :: lambda
    contains
        procedure :: Assemble => Type_Thermal_3Phase_Assemble
        procedure :: Update => Type_Thermal_3Phase_Update
    end type Type_Thermal_3Phase

    interface Type_Thermal_3Phase
        module procedure Type_Thermal_3Phase_Construct
    end interface

contains
    function Type_Thermal_3Phase_Construct(Input, Elements, Coordinate, meshType, Lf, Tf, Input_Ice_Param, Input_Thermal_Params, Input_Thermal_BC, nsize, Input_VTK) result(structure)
        implicit none
        type(Type_Thermal_3Phase) :: structure
        type(Type_Input), intent(in) :: Input

        integer(int32), intent(in) :: Elements(:, :)
        type(DP3d), intent(in) :: Coordinate
        integer(int32), intent(in) :: meshType
        real(real64), intent(in) :: Lf
        real(real64), intent(in) :: Tf
        type(Input_Ice), intent(in) :: Input_Ice_Param
        type(Input_Thermal), intent(in) :: Input_Thermal_Params
        type(Type_BC_Thermal), intent(in) :: Input_Thermal_BC
        integer(int32), intent(in) :: nsize
        type(Type_VTK), intent(in) :: Input_VTK
        integer(int32) :: nElement

        structure%nsize = nsize
        structure%nElement = size(Elements, 2)

        allocate (structure%Element, source=Elements)
        call structure%Coordinate%allocate(nsize)
        structure%Coordinate = Coordinate
        nElement = size(Elements, 2)
        call Allocate_Array(structure%Area, nElement)
        call Update_Area(structure%Element, structure%Coordinate, structure%Area)
        if (meshType == 3) then
            allocate (structure%Basis(3, 3, nElement))
            call Calculate_Basis(structure%Element, structure%Coordinate, structure%Basis)
        end if

        structure%KT_star_0 = Type_CRS(Elements, nsize)
        structure%KT_l = structure%KT_star_0%Copy()
        structure%KT_old = structure%KT_star_0%Copy()
        structure%CT_l = structure%KT_star_0%Copy()
        allocate (structure%CT_old(3))
        structure%CT_old(1) = structure%KT_star_0%Copy()
        structure%CT_old(2) = structure%KT_star_0%Copy()
        structure%CT_old(3) = structure%KT_star_0%Copy()
        call Allocate_Array(structure%FT, nsize)
        call Allocate_Array(structure%FT_old, nsize)
        call Allocate_Array(structure%PHIT, nsize)
        call Allocate_Array(structure%PHIT_old, nsize)

        call structure%T%allocate(nsize, 2)

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
                                                         Input_Ice_Param%phi, &
                                                         nsize)

        structure%lambda = Type_ThermalConductivity_3Phase(Input_Thermal_Params%lambda(1), &
                                                           Input_Thermal_Params%lambda(2), &
                                                           Input_Thermal_Params%lambda(3), &
                                                           nsize)

        structure%BC = Type_BC_Thermal(Input_Thermal_BC%BCGroup, Input_Thermal_BC%BC_Info, Input_Thermal_BC%Edge, structure%Coordinate, Input_VTK)

        structure%Solver = Solver_CRS_LU_Constructor(nsize, 1, 1, 11, 13, 1, 0, structure%KT_star_0)
        ! structure%Solver = Solver_CRS_BiCGSTAB_Constructor(nsize, 1.0d-8, 100000_int32, 1_int32)

    end function Type_Thermal_3Phase_Construct

    subroutine Type_Thermal_3Phase_Assemble(self, dt, step, iter)
        implicit none
        class(Type_Thermal_3Phase), intent(inout) :: self
        real(real64), intent(in) :: dt
        integer(int32), intent(in) :: step
        integer(int32), intent(in) :: iter

        if (step >= 2) then
            self%CT_old(2)%Val(:) = self%CT_old(1)%Val(:)
            self%CT_old(1)%Val(:) = self%CT_l%Val(:)
        end if

        self%CT_l%Val(:) = 0.0d0
        self%KT_l%Val(:) = 0.0d0
        self%KT_star_0%Val(:) = 0.0d0
        ! if (step == 1) then

        ! end if
        call Assemble_Mass_Lumped_231(self%CT_l, self%Element, self%Area, self%C%Apparent, self%nElement)
        call Assemble_Diffusion_231(self%KT_l, self%Element, self%Basis, self%Area, self%lambda%value, self%nElement)
        if (step == 1) then
            self%KT_star_0%Val(:) = dt * self%KT_l%Val(:) + self%CT_l%Val(:)
            if (iter == 1) then
                self%CT_old(1)%Val(:) = self%CT_l%Val(:)
                self%KT_old%Val(:) = self%KT_l%Val(:)
                self%PHIT(:) = 0.0d0
                self%PHIT_old(:) = -self%CT_old(1) * self%T%old(:, 1)
            end if
            self%PHIT(:) = dt * (self%KT_l * self%T%pre(:)) + self%CT_l * self%T%pre(:) + self%PHIT_old(:)
        else
            self%KT_star_0%Val(:) = 2.0d0 * dt * self%KT_l%Val(:) + 3.0d0 * self%CT_l%Val(:)
            if (iter == 1) then
                self%PHIT_old(:) = -4.0d0 * (self%CT_old(1) * self%T%old(:, 1)) + self%CT_old(2) * self%T%old(:, 2)
            end if
            self%PHIT(:) = 2.0d0 * dt * (self%KT_l * self%T%pre(:)) + 3.0d0 * (self%CT_l * self%T%pre(:)) + self%PHIT_old(:)
        end if
        ! self%PHIT(:) = self%PHIT_old(:)
    end subroutine Type_Thermal_3Phase_Assemble

    subroutine Type_Thermal_3Phase_Update(self, phi_soil, rho_ice, iiter)
        implicit none
        class(Type_Thermal_3Phase), intent(inout) :: self
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: rho_ice
        integer(int32), intent(in) :: iiter

        select type (Ice => self%Ice)
        type is (Type_Ice_GCC)
            call Ice%Update_Ice(self%T%pre(:))
        end select
        call self%lambda%Update(phi_soil, self%Ice%Qw%pre, self%Ice%Qice%pre)
        call self%C%Update(phi_soil, self%Ice%Qw%pre, self%Ice%Qice%pre)
        ! if (iiter == 1) then
        !     call self%C%Update_Ca_Revise(structure_Ice=self%Ice, rho_ice=ThermalInput%rho(3), arr_Temperature=self%T%pre(:), arr_Temperature_old=self%T%old(:))
        ! else
        call self%C%Update_Ca(structure_Ice=self%Ice, rho_ice=rho_ice, arr_Temperature=self%T%pre(:))
        ! end if

    end subroutine Type_Thermal_3Phase_Update

end module Main_Thermal
