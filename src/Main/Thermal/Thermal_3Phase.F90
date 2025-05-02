submodule(Main_Thermal) Main_Thermal_3Phase
    implicit none
contains
    module function Type_Thermal_3Phase_2D_Construct(Input, Coordinate) result(Structure)
        implicit none
        class(Abstract_Thermal), allocatable :: Structure
        type(Type_Input), intent(in) :: Input
        type(DP3d), intent(inout), pointer :: Coordinate

        integer(int32) :: CountElements
        integer(int32) :: iCell, idx
        integer(int32) :: i
        ! Initialize the Structure
        ! allocate (Coordinate)
        ! call Coordinate%allocate(Input%VTK%numPoints)
        ! Coordinate = Input%VTK%POINTS

        ! Count the number of elements (e.g., triangles)
        CountElements = 0
        if (Input%VTK%numTotalCells == 0) then
            print *, "Error: No cells found in the VTK Structure."
            return
        end if
        if (Input%Basic%DimensionType == 1) then
            do iCell = 1, Input%VTK%numTotalCells
                if (Input%VTK%CELLS(iCell)%CellType == Input%VTK%Names%VTK_TRIANGLE .or. &
                    Input%VTK%CELLS(iCell)%CellType == Input%VTK%Names%VTK_TRIANGLE_STRIP .or. &
                    Input%VTK%CELLS(iCell)%CellType == Input%VTK%Names%VTK_POLYGON .or. &
                    Input%VTK%CELLS(iCell)%CellType == Input%VTK%Names%VTK_PIXEL .or. &
                    Input%VTK%CELLS(iCell)%CellType == Input%VTK%Names%VTK_QUAD .or. &
                    Input%VTK%CELLS(iCell)%CellType == Input%VTK%Names%VTK_QUADRATIC_TRIANGLE .or. &
                    Input%VTK%CELLS(iCell)%CellType == Input%VTK%Names%VTK_QUADRATIC_QUAD) then
                    CountElements = CountElements + 1
                end if
            end do

            Structure%nElement = CountElements
            Structure%nsize = Input%VTK%numPoints

            allocate (Structure%Elements(Structure%nElement)) ! pointer 多形配列にディスクリプタを確保

            ! 各要素ごとに具象オブジェクトをポインタで割り当て
            idx = 0
            do iCell = 1, Input%VTK%numTotalCells
                select case (Input%VTK%CELLS(iCell)%CellType)
                case (5) ! VTK_TRIANGLE
                    idx = idx + 1
                    Structure%Elements(idx)%e = TriangleFirst(iCell, Coordinate, Input%VTK%CELLS(iCell)%CONNECTIVITY)
                case (9) ! VTK_QUAD
                    idx = idx + 1
                    Structure%Elements(idx)%e = SquareFirst(iCell, Coordinate, Input%VTK%CELLS(iCell)%CONNECTIVITY)
                end select
            end do
        end if

        Structure%KT_star_0 = Type_CRS(Structure%Elements, Structure%nsize)
        Structure%KT_l = Structure%KT_star_0%Copy()
        Structure%KT_old = Structure%KT_star_0%Copy()
        Structure%CT_l = Structure%KT_star_0%Copy()
        Structure%Order = Input%Basic%Order
        allocate (Structure%CT_old(Input%Basic%Order))
        do i = 1, Input%Basic%Order
            Structure%CT_old(i) = Structure%KT_star_0%Copy()
        end do

        call Allocate_Array(Structure%FT, Structure%nsize)
        call Allocate_Array(Structure%FT_old, Structure%nsize)
        call Allocate_Array(Structure%PHIT, Structure%nsize)
        call Allocate_Array(Structure%PHIT_old, Structure%nsize)

        call Structure%T%allocate(Structure%nsize, Input%Basic%Order)

        ! allocate (Structure%Ice(Input%Basic%numRegion))

        ! do i = 1, Input%Basic%numRegion
        !     select case (Input%Regions(i)%Ice%QiceType)
        !     case (1)
        !         Structure%Ice = Type_Ice_TRM(Input%Regions(i)%Thermal%LatentHeat, Input%Regions(i)%Ice%Tf, Structure%nsize)
        !     case (2)
        !         if (Input%Regions(i)%Flag%isFrostHeavePressure) then
        !         !!! TBI
        !         else
        !             select case (Input%Regions(i)%Ice%c_unit)
        !             case ("m")
        !                 select case (Input%Regions(i)%Ice%ModelType)
        !                 case (1:3)
        !                     Structure%Ice = Type_Ice_GCC(ModelType=Input%Regions(i)%Ice%ModelType, &
        !                                                  isSegregation=Input%Regions(i)%Flag%isFrostHeavePressure, &
        !                                                  c_unit=Input%Regions(i)%Ice%c_unit, &
        !                                                  nsize=Structure%nsize, &
        !                                                  thetaR=Input%Regions(i)%Ice%thetaR, &
        !                                                  thetaS=Input%Regions(i)%Ice%thetaS, &
        !                                                  alpha1=Input%Regions(i)%Ice%alpha1, &
        !                                                  n1=Input%Regions(i)%Ice%n1, &
        !                                                  Lf=Input%Regions(i)%Thermal%LatentHeat, &
        !                                                  Tf=Input%Regions(i)%Ice%Tf)
        !                 case (4)
        !                     Structure%Ice = Type_Ice_GCC(ModelType=Input%Regions(i)%Ice%ModelType, &
        !                                                  isSegregation=Input%Regions(i)%Flag%isFrostHeavePressure, &
        !                                                  c_unit=Input%Regions(i)%Ice%c_unit, &
        !                                                  nsize=Structure%nsize, &
        !                                                  thetaS=Input%Regions(i)%Ice%thetaS, &
        !                                                  thetaR=Input%Regions(i)%Ice%thetaR, &
        !                                                  alpha1=Input%Regions(i)%Ice%alpha1, &
        !                                                  hcrit=Input%Regions(i)%Ice%hcrit, &
        !                                                  n1=Input%Regions(i)%Ice%n1, &
        !                                                  Lf=Input%Regions(i)%Thermal%LatentHeat, &
        !                                                  Tf=Input%Regions(i)%Ice%Tf)
        !                 case (5)
        !                     Structure%Ice = Type_Ice_GCC(ModelType=Input%Regions(i)%Ice%ModelType, &
        !                                                  isSegregation=Input%Regions(i)%Flag%isFrostHeavePressure, &
        !                                                  c_unit=Input%Regions(i)%Ice%c_unit, &
        !                                                  nsize=Structure%nsize, &
        !                                                  thetaS=Input%Regions(i)%Ice%thetaS, &
        !                                                  thetaR=Input%Regions(i)%Ice%thetaR, &
        !                                                  alpha1=Input%Regions(i)%Ice%alpha1, &
        !                                                  n1=Input%Regions(i)%Ice%n1, &
        !                                                  w1=Input%Regions(i)%Ice%w1, &
        !                                                  alpha2=Input%Regions(i)%Ice%alpha2, &
        !                                                  n2=Input%Regions(i)%Ice%n2, &
        !                                                  Lf=Input%Regions(i)%Thermal%LatentHeat, &
        !                                                  Tf=Input%Regions(i)%Ice%Tf)
        !                 case (6)
        !                     Structure%Ice = Type_Ice_GCC(ModelType=Input%Regions(i)%Ice%ModelType, &
        !                                                  isSegregation=Input%Regions(i)%Flag%isFrostHeavePressure, &
        !                                                  c_unit=Input%Regions(i)%Ice%c_unit, &
        !                                                  nsize=Structure%nsize, &
        !                                                  thetaS=Input%Regions(i)%Ice%thetaS, &
        !                                                  thetaR=Input%Regions(i)%Ice%thetaR, &
        !                                                  alpha1=Input%Regions(i)%Ice%alpha1, &
        !                                                  n1=Input%Regions(i)%Ice%n1, &
        !                                                  n2=Input%Regions(i)%Ice%n2, &
        !                                                  Lf=Input%Regions(i)%Thermal%LatentHeat, &
        !                                                  Tf=Input%Regions(i)%Ice%Tf)
        !                 end select
        !             case ("Pa")
        !         !! TBD
        !             end select
        !         end if
        !     case (3)
        !         Structure%Ice = Type_Ice_EXP(Input%Regions(i)%Thermal%LatentHeat, Input%Regions(i)%Ice%phi, Input%Regions(i)%Ice%Tf, Input%Regions(i)%Ice%a, Structure%nsize)
        !     end select

        !     ! Structure%C = Type_VolumetricHeatCapacity_3Phase(Structure%Ice, &
        !     !                                                  Input%Regions(i)%Thermal%Cp(1), &
        !     !                                                  Input%Regions(i)%Thermal%Cp(2), &
        !     !                                                  Input%Regions(i)%Thermal%Cp(3), &
        !     !                                                  Input%Regions(i)%Thermal%rho(3), &
        !     !                                                  Input%Regions(i)%Thermal%rho(2), &
        !     !                                                  Input%Regions(i)%Ice%phi, &
        !     !                                                  Structure%nsize)

        ! end do

        !! Thermal properties
        Structure%THC = Type_ThermalConductivity_3Phase(Input)
        Structure%DEN = Type_Density_3Phase(Input)
        Structure%SPH = Type_SpecificHeat_3Phase(Input)

        if (any(Input%Regions(:)%isFrozen)) then
            Structure%HTC = Type_HeatCapacity_3Phase_Apparent(Input)
        end if

        Structure%BC = Type_BC_Thermal(Input%Conditions, Input%VTK)

        if (Input%Solver_Thermal%useSolver == 1) then
            Structure%Solver = Solver_CRS_LU_Constructor(N=Structure%nsize, &
                                                         MAXFCT=1, &
                                                         MNUM=1, &
                                                         MTYPE=11, &
                                                         PHASE=33, &
                                                         NRHS=1, &
                                                         MSGVLV=0, &
                                                         a=Structure%KT_star_0)
        else if (Input%Solver_Thermal%useSolver == 2) then
            if (Input%Solver_Thermal%useSolverType == 4) then
                Structure%Solver = Solver_CRS_BiCGSTAB_Constructor(N=Structure%nsize, &
                                                                   tol=Input%Solver_Thermal%tolerance, &
                                                                   maxiter=Input%Solver_Thermal%maxIteration, &
                                                                   Preconditioner=Input%Solver_Thermal%usePreconditionerType)
            end if
        end if
    end function Type_Thermal_3Phase_2D_Construct

    module subroutine Type_Thermal_3Phase_2D_Assemble(self, dt, step, iter)
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
        !!!-----------------------------------------------------------------------
        ! call Assemble_Mass_1(self%CT_l, self%Elements, self%C%Apparent)
        !
        ! call Assemble_Diffusion_1_Isotropic(self%KT_l, self%Elements, self%lambda%value)
         !!!-----------------------------------------------------------------------
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

end submodule Main_Thermal_3Phase
