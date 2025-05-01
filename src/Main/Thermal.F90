module Main_Thermal
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes, only:Variables, DP3d, Vector2d
    use :: Core_Element
    use :: Inout_Input
    use :: Calculate_Ice, only:Abstract_Ice, Type_Ice_TRM, Type_Ice_GCC, Type_Ice_EXP
    use :: Calculate_VolumetricHeatCapacity
    use :: Calculate_ThermalConductivity
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
        type(DP3d), pointer :: Coordinate
        type(ElementHolder), allocatable :: Elements(:)
        type(Type_BC_Thermal) :: BC
        class(Abstract_Solver_CRS), allocatable :: Solver
        class(Abstract_Ice), allocatable :: Ice
    contains
        procedure(Abstract_Assemble), pass(self), deferred :: Assemble
    end type Abstract_Thermal

    type, extends(Abstract_Thermal) :: Type_Thermal_3Phase_2D
        type(Type_VolumetricHeatCapacity_3Phase) :: C
        type(Type_ThermalConductivity_3Phase) :: lambda
    contains
        procedure, pass(self) :: Assemble => Type_Thermal_3Phase_2D_Assemble
        procedure :: Update => Type_Thermal_3Phase_2D_Update
    end type Type_Thermal_3Phase_2D

    interface Type_Thermal_3Phase_2D
        module procedure Type_Thermal_3Phase_2D_Construct
    end interface

    abstract interface
        subroutine Abstract_Assemble(self, dt, step, iter)
            import :: Abstract_Thermal, int32, real64
            implicit none
            class(Abstract_Thermal), intent(inout) :: self
            real(real64), intent(in) :: dt
            integer(int32), intent(in) :: step
            integer(int32), intent(in) :: iter

        end subroutine Abstract_Assemble
    end interface

contains
    function Type_Thermal_3Phase_2D_Construct(Structure_Input) result(structure)
        implicit none
        type(Type_Thermal_3Phase_2D) :: structure
        type(Type_Input), intent(in) :: Structure_Input
        integer(int32) :: CountElements
        integer(int32) :: iCell, idx
        integer(int32) :: i
        ! Initialize the structure
        allocate (structure%Coordinate)
        call structure%Coordinate%allocate(Structure_Input%VTK%numPoints)
        structure%Coordinate = Structure_Input%VTK%POINTS

        ! Count the number of elements (e.g., triangles)
        CountElements = 0
        if (Structure_Input%VTK%numTotalCells == 0) then
            print *, "Error: No cells found in the VTK structure."
            return
        end if
        if (Structure_Input%Basic%DimensionType == 1) then
            do iCell = 1, Structure_Input%VTK%numTotalCells
                if (Structure_Input%VTK%CELLS(iCell)%CellType == Structure_Input%VTK%Names%VTK_TRIANGLE .or. &
                    Structure_Input%VTK%CELLS(iCell)%CellType == Structure_Input%VTK%Names%VTK_TRIANGLE_STRIP .or. &
                    Structure_Input%VTK%CELLS(iCell)%CellType == Structure_Input%VTK%Names%VTK_POLYGON .or. &
                    Structure_Input%VTK%CELLS(iCell)%CellType == Structure_Input%VTK%Names%VTK_PIXEL .or. &
                    Structure_Input%VTK%CELLS(iCell)%CellType == Structure_Input%VTK%Names%VTK_QUAD .or. &
                    Structure_Input%VTK%CELLS(iCell)%CellType == Structure_Input%VTK%Names%VTK_QUADRATIC_TRIANGLE .or. &
                    Structure_Input%VTK%CELLS(iCell)%CellType == Structure_Input%VTK%Names%VTK_QUADRATIC_QUAD) then
                    CountElements = CountElements + 1
                end if
            end do

            structure%nElement = CountElements
            structure%nsize = Structure_Input%VTK%numPoints

            allocate (structure%Elements(structure%nElement)) ! pointer 多形配列にディスクリプタを確保

            ! 各要素ごとに具象オブジェクトをポインタで割り当て
            idx = 0
            do iCell = 1, Structure_Input%VTK%numTotalCells
                select case (Structure_Input%VTK%CELLS(iCell)%CellType)
                case (5) ! VTK_TRIANGLE
                    idx = idx + 1
                    structure%Elements(idx)%e = TriangleFirst(iCell, structure%Coordinate, Structure_Input%VTK%CELLS(iCell)%CONNECTIVITY)
                case (9) ! VTK_QUAD
                    idx = idx + 1
                    structure%Elements(idx)%e = SquareFirst(iCell, structure%Coordinate, Structure_Input%VTK%CELLS(iCell)%CONNECTIVITY)
                end select
            end do
        end if

        structure%KT_star_0 = Type_CRS(structure%Elements, structure%nsize)
        structure%KT_l = structure%KT_star_0%Copy()
        structure%KT_old = structure%KT_star_0%Copy()
        structure%CT_l = structure%KT_star_0%Copy()
        allocate (structure%CT_old(Structure_Input%Basic%Order))
        do i = 1, Structure_Input%Basic%Order
            structure%CT_old(i) = structure%KT_star_0%Copy()
        end do

        call Allocate_Array(structure%FT, structure%nsize)
        call Allocate_Array(structure%FT_old, structure%nsize)
        call Allocate_Array(structure%PHIT, structure%nsize)
        call Allocate_Array(structure%PHIT_old, structure%nsize)

        call structure%T%allocate(structure%nsize, Structure_Input%Basic%Order)

        do i = 1, Structure_Input%Basic%numRegion
            select case (Structure_Input%Regions(i)%Ice%QiceType)
            case (1)
                structure%Ice = Type_Ice_TRM(Structure_Input%Regions(i)%Thermal%LatentHeat, Structure_Input%Regions(i)%Ice%Tf, structure%nsize)
            case (2)
                if (Structure_Input%Regions(i)%Flag%isFrostHeavePressure) then
                !!! TBI
                else
                    select case (Structure_Input%Regions(i)%Ice%c_unit)
                    case ("m")
                        select case (Structure_Input%Regions(i)%Ice%ModelType)
                        case (1:3)
                            structure%Ice = Type_Ice_GCC(ModelType=Structure_Input%Regions(i)%Ice%ModelType, &
                                                         isSegregation=Structure_Input%Regions(i)%Flag%isFrostHeavePressure, &
                                                         c_unit=Structure_Input%Regions(i)%Ice%c_unit, &
                                                         nsize=structure%nsize, &
                                                         thetaR=Structure_Input%Regions(i)%Ice%thetaR, &
                                                         thetaS=Structure_Input%Regions(i)%Ice%thetaS, &
                                                         alpha1=Structure_Input%Regions(i)%Ice%alpha1, &
                                                         n1=Structure_Input%Regions(i)%Ice%n1, &
                                                         Lf=Structure_Input%Regions(i)%Thermal%LatentHeat, &
                                                         Tf=Structure_Input%Regions(i)%Ice%Tf)
                        case (4)
                            structure%Ice = Type_Ice_GCC(ModelType=Structure_Input%Regions(i)%Ice%ModelType, &
                                                         isSegregation=Structure_Input%Regions(i)%Flag%isFrostHeavePressure, &
                                                         c_unit=Structure_Input%Regions(i)%Ice%c_unit, &
                                                         nsize=structure%nsize, &
                                                         thetaS=Structure_Input%Regions(i)%Ice%thetaS, &
                                                         thetaR=Structure_Input%Regions(i)%Ice%thetaR, &
                                                         alpha1=Structure_Input%Regions(i)%Ice%alpha1, &
                                                         hcrit=Structure_Input%Regions(i)%Ice%hcrit, &
                                                         n1=Structure_Input%Regions(i)%Ice%n1, &
                                                         Lf=Structure_Input%Regions(i)%Thermal%LatentHeat, &
                                                         Tf=Structure_Input%Regions(i)%Ice%Tf)
                        case (5)
                            structure%Ice = Type_Ice_GCC(ModelType=Structure_Input%Regions(i)%Ice%ModelType, &
                                                         isSegregation=Structure_Input%Regions(i)%Flag%isFrostHeavePressure, &
                                                         c_unit=Structure_Input%Regions(i)%Ice%c_unit, &
                                                         nsize=structure%nsize, &
                                                         thetaS=Structure_Input%Regions(i)%Ice%thetaS, &
                                                         thetaR=Structure_Input%Regions(i)%Ice%thetaR, &
                                                         alpha1=Structure_Input%Regions(i)%Ice%alpha1, &
                                                         n1=Structure_Input%Regions(i)%Ice%n1, &
                                                         w1=Structure_Input%Regions(i)%Ice%w1, &
                                                         alpha2=Structure_Input%Regions(i)%Ice%alpha2, &
                                                         n2=Structure_Input%Regions(i)%Ice%n2, &
                                                         Lf=Structure_Input%Regions(i)%Thermal%LatentHeat, &
                                                         Tf=Structure_Input%Regions(i)%Ice%Tf)
                        case (6)
                            structure%Ice = Type_Ice_GCC(ModelType=Structure_Input%Regions(i)%Ice%ModelType, &
                                                         isSegregation=Structure_Input%Regions(i)%Flag%isFrostHeavePressure, &
                                                         c_unit=Structure_Input%Regions(i)%Ice%c_unit, &
                                                         nsize=structure%nsize, &
                                                         thetaS=Structure_Input%Regions(i)%Ice%thetaS, &
                                                         thetaR=Structure_Input%Regions(i)%Ice%thetaR, &
                                                         alpha1=Structure_Input%Regions(i)%Ice%alpha1, &
                                                         n1=Structure_Input%Regions(i)%Ice%n1, &
                                                         n2=Structure_Input%Regions(i)%Ice%n2, &
                                                         Lf=Structure_Input%Regions(i)%Thermal%LatentHeat, &
                                                         Tf=Structure_Input%Regions(i)%Ice%Tf)
                        end select
                    case ("Pa")
                !! TBD
                    end select
                end if
            case (3)
                structure%Ice = Type_Ice_EXP(Structure_Input%Regions(i)%Thermal%LatentHeat, Structure_Input%Regions(i)%Ice%phi, Structure_Input%Regions(i)%Ice%Tf, Structure_Input%Regions(i)%Ice%a, structure%nsize)
            end select

            structure%C = Type_VolumetricHeatCapacity_3Phase(structure%Ice, &
                                                             Structure_Input%Regions(i)%Thermal%Cp(1), &
                                                             Structure_Input%Regions(i)%Thermal%Cp(2), &
                                                             Structure_Input%Regions(i)%Thermal%Cp(3), &
                                                             Structure_Input%Regions(i)%Thermal%rho(3), &
                                                             Structure_Input%Regions(i)%Thermal%rho(2), &
                                                             Structure_Input%Regions(i)%Ice%phi, &
                                                             structure%nsize)

            structure%lambda = Type_ThermalConductivity_3Phase(Structure_Input%Regions(i)%Thermal%lambda(1), &
                                                               Structure_Input%Regions(i)%Thermal%lambda(2), &
                                                               Structure_Input%Regions(i)%Thermal%lambda(3), &
                                                               structure%nsize)
        end do

        structure%BC = Type_BC_Thermal(Structure_Input%Conditions, Structure_Input%VTK)

        if (Structure_Input%Solver_Thermal%useSolver == 1) then
            structure%Solver = Solver_CRS_LU_Constructor(N=structure%nsize, &
                                                         MAXFCT=1, &
                                                         MNUM=1, &
                                                         MTYPE=11, &
                                                         PHASE=33, &
                                                         NRHS=1, &
                                                         MSGVLV=0, &
                                                         a=structure%KT_star_0)
        else if (Structure_Input%Solver_Thermal%useSolver == 2) then
            if (Structure_Input%Solver_Thermal%useSolverType == 4) then
                structure%Solver = Solver_CRS_BiCGSTAB_Constructor(N=structure%nsize, &
                                                                   tol=Structure_Input%Solver_Thermal%tolerance, &
                                                                   maxiter=Structure_Input%Solver_Thermal%maxIteration, &
                                                                   Preconditioner=Structure_Input%Solver_Thermal%usePreconditionerType)
            end if
        end if
    end function Type_Thermal_3Phase_2D_Construct

    subroutine Type_Thermal_3Phase_2D_Assemble(self, dt, step, iter)
        implicit none
        class(Type_Thermal_3Phase_2D), intent(inout) :: self
        real(real64), intent(in) :: dt
        integer(int32), intent(in) :: step
        integer(int32), intent(in) :: iter

        ! if (step >= 2) then
        !     self%CT_old(2)%Val(:) = self%CT_old(1)%Val(:)
        !     self%CT_old(1)%Val(:) = self%CT_l%Val(:)
        ! end if

        self%CT_l%Val(:) = 0.0d0
        self%KT_l%Val(:) = 0.0d0
        self%KT_star_0%Val(:) = 0.0d0
        ! ! if (step == 1) then

        ! ! end if
        call Assemble_Mass_1(self%CT_l, self%Elements, self%C%Apparent)
        ! stop
        ! print *, "Assembled1"
        call Assemble_Diffusion_1_Isotropic(self%KT_l, self%Elements, self%lambda%value)
        ! print *, "Assembled2"
        ! if (step == 1) then
        self%KT_star_0 = dt * self%KT_l + self%CT_l
        !     if (iter == 1) then
        ! self%CT_old(1)%Val(:) = self%CT_l%Val(:)
        !         self%KT_old%Val(:) = self%KT_l%Val(:)
        !         self%PHIT(:) = 0.0d0
        ! self%PHIT_old(:) = -self%CT_old(1) * self%T%old(:, 1)
        !     end if
        ! print *, size(self%T%old(:, 1))
        self%PHIT(:) = self%CT_l * self%T%old(:, 1)
        ! stop
        ! self%PHIT(:) = -self%CT_old(1) * self%T%old(:, 1)
        ! self%PHIT(:) = dt * (self%KT_l * self%T%pre(:)) + self%CT_l * self%T%pre(:) + self%PHIT_old(:)
        ! else
        !     self%KT_star_0%Val(:) = 2.0d0 * dt * self%KT_l%Val(:) + 3.0d0 * self%CT_l%Val(:)
        !     if (iter == 1) then
        !         self%PHIT_old(:) = -4.0d0 * (self%CT_old(1) * self%T%old(:, 1)) + self%CT_old(2) * self%T%old(:, 2)
        !     end if
        !     self%PHIT(:) = 2.0d0 * dt * (self%KT_l * self%T%pre(:)) + 3.0d0 * (self%CT_l * self%T%pre(:)) + self%PHIT_old(:)
        ! end if
        ! self%PHIT(:) = self%PHIT_old(:)
    end subroutine Type_Thermal_3Phase_2D_Assemble

    subroutine Type_Thermal_3Phase_2D_Update(self, phi_soil, rho_ice, iiter)
        implicit none
        class(Type_Thermal_3Phase_2D), intent(inout) :: self
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

    end subroutine Type_Thermal_3Phase_2D_Update

end module Main_Thermal
