submodule(Main_Thermal) Main_Thermal_3Phase
    implicit none
contains
    module function Type_Thermal_3Phase_2D_Construct(Input, Coordinate) result(Structure)
        implicit none
        class(Abstract_Thermal), allocatable :: Structure
        type(Type_Input), intent(inout) :: Input
        type(DP3d), intent(inout), pointer :: Coordinate

        integer(int32) :: CountElements, CountSides
        integer(int32) :: iCell, iElem, iSide, idx
        integer(int32) :: i
        integer(int32) :: iRegion

        if (allocated(Structure)) deallocate (Structure)
        allocate (Type_Thermal_3Phase_2D :: Structure)

        ! Count the number of elements (e.g., triangles)
        CountElements = 0
        CountSides = 0
        if (Input%VTK%numTotalCells == 0) then
            print *, "Error: No cells found in the VTK Structure."
            return
        end if

        if (Input%Basic%DimensionType == 1) then
            do iCell = 1, Input%VTK%numTotalCells
                if (Input%VTK%CELLS(iCell)%CellType == Input%VTK%Names%VTK_TRIANGLE .or. &
                    Input%VTK%CELLS(iCell)%CellType == Input%VTK%Names%VTK_PIXEL .or. &
                    Input%VTK%CELLS(iCell)%CellType == Input%VTK%Names%VTK_QUAD .or. &
                    Input%VTK%CELLS(iCell)%CellType == Input%VTK%Names%VTK_QUADRATIC_TRIANGLE .or. &
                    Input%VTK%CELLS(iCell)%CellType == Input%VTK%Names%VTK_QUADRATIC_QUAD &
                    ) then
                    CountElements = CountElements + 1
                end if
                if (Input%VTK%CELLS(iCell)%CellType == Input%VTK%Names%VTK_LINE .or. &
                    Input%VTK%CELLS(iCell)%CellType == Input%VTK%Names%VTK_QUADRATIC_EDGE &
                    ) then
                    CountSides = CountSides + 1
                end if
            end do

            Structure%nElement = CountElements
            Structure%nSide = CountSides
            Structure%nsize = Input%VTK%numPoints
            Structure%nRegion = Input%Basic%numRegion

            allocate (Structure%Elements(Structure%nElement)) ! pointer 多形配列にディスクリプタを確保
            allocate (Structure%Sides(Structure%nSide)) ! pointer 多形配列にディスクリプタを確保
            ! 各要素ごとに具象オブジェクトをポインタで割り当て

            iElem = 1
            iSide = 1
            do iCell = 1, Input%VTK%numTotalCells
                if (Input%VTK%Is_In(Input%VTK%CELLS(iCell)%CellType, 1)) then
                    call Structure%Sides(iSide)%allocate(Input%VTK%CELLS(iCell)%CellType, &
                                                         iCell, &
                                                         Coordinate, &
                                                         Input%VTK%CELLS(iCell)%CONNECTIVITY, &
                                                         Input%VTK%CELLS(iCell)%CellEntityId)
                    iSide = iSide + 1
                end if
                if (Input%VTK%Is_In(Input%VTK%CELLS(iCell)%CellType, 2)) then
                    call Structure%Elements(iElem)%allocate(Input%VTK%CELLS(iCell)%CellType, &
                                                            iCell, &
                                                            Coordinate, &
                                                            Input%VTK%CELLS(iCell)%CONNECTIVITY)
                    iElem = iElem + 1
                end if

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

        allocate (Structure%Ice(Input%Basic%numRegion))

        do iRegion = 1, Input%Basic%numRegion
            select case (Input%Regions(iRegion)%Ice%QiceType)
            case (1)
                Structure%Ice(iRegion)%f = Type_Ice_TRM(Input%Regions(iRegion), Structure%nsize)
            case (2)
                Structure%Ice(iRegion)%f = Type_Ice_GCC(Input%Regions(iRegion), Structure%nsize)
            case (3)
                Structure%Ice(iRegion)%f = Type_Ice_EXP(Input%Regions(iRegion), Structure%nsize)
            end select
        end do

        !! Thermal properties
        Structure%THC = Type_ThermalConductivity_3Phase(Input)
        Structure%DEN = Type_Density_3Phase(Input)
        Structure%SPH = Type_SpecificHeat_3Phase(Input)

        Structure%HTC = Type_HeatCapacity_3Phase_Apparent(Input)

        call Structure%Qw%allocate(Structure%nsize, Input%Basic%Order)
        call Structure%Qice%allocate(Structure%nsize, Input%Basic%Order)
        call Structure%D_Qice%allocate(Structure%nsize, Input%Basic%Order)
        call Structure%Si%allocate(Structure%nsize, Input%Basic%Order)

        Structure%BC = Type_BC_Thermal_CRS(Input)

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

    module subroutine Type_Thermal_3Phase_2D_Update(self, NodeBelonging, arr_phi)
        implicit none
        class(Type_Thermal_3Phase_2D), intent(inout) :: self
        type(Belonging), intent(inout), optional :: NodeBelonging(:)
        real(real64), intent(inout) :: arr_phi(:)

        call self%Ice(1)%f%Update_Ice(NodeBelonging=NodeBelonging, &
                                      arr_T=self%T%pre(:), &
                                      arr_phi=arr_phi(:), &
                                      Density=self%DEN, &
                                      arr_Cp=self%HTC%value(:, 1), &
                                      arr_Qw=self%Qw%pre(:), &
                                      arr_Qice=self%Qice%pre(:), &
                                      arr_Si=self%Si)

        call self%THC%Update(NodeBelonging, 1.0d0 - arr_phi(:), self%Qw%pre, self%Qice%pre)
        call self%SPH%Update(NodeBelonging, 1.0d0 - arr_phi(:), self%Qw%pre, self%Qice%pre)
        call self%DEN%Update(NodeBelonging, 1.0d0 - arr_phi(:), self%Qw%pre, self%Qice%pre)
        call self%HTC%Update(NodeBelonging=NodeBelonging, arr_phi1=1.0d0 - arr_phi(:), arr_phi2=self%Qw%pre, arr_phi3=self%Qice%pre, &
                             Ice=self%Ice(1)%f, Temperature=self%T%pre(:), Density=self%DEN)
    end subroutine Type_Thermal_3Phase_2D_Update

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
        call Assemble_Mass_1(self%CT_l, self%Elements, self%HTC%value(:, 2))
        !
        call Assemble_Diffusion_1_Isotropic(self%KT_l, self%Elements, self%THC%value(:, 1))
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
