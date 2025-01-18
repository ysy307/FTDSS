module Inout_Inout
    use omp_lib
    use :: Types
    use :: error
    use :: Allocate
    use :: Allocate_Structure
    use, intrinsic :: iso_fortran_env, only : int32, real64
    implicit none
    private


    type :: IO
        private
        character(256) :: dir_Path
        character(256) :: in_Basic_FileName, in_BC_FileName, in_BCtype_FileName, in_Obs_FileName, in_ObsFlag_FileName, in_Top_FileName, in_COO_FileName, in_IC_FileName
        character(256) :: out_T_FileName, out_Fr_FileName, out_TC_FileName, out_C_FileName, out_P_FileName, out_Flux_FileName, out_K_FileName
    contains
        procedure :: Input_Parameters   => Inout_Input_Parameters
        procedure :: Input_Flags        => Inout_Input_Flags
        procedure :: Input_BC           => Inout_Input_BC
        procedure :: Input_IC           => Inout_Input_IC
        procedure :: Input_Vertices     => Inout_Input_Vertices
        procedure :: Input_Coodinates   => Inout_Input_Coodinates
        procedure :: Input_Observation  => Inout_Input_Observation
        procedure :: Output_All         => Inout_Output_All_vtk
        ! procedure :: Output_All         => Inout_Output_All
        procedure :: Output_Observation => Inout_Output_Observation
    end type IO

    interface IO
        module procedure Inout_Initialize_Path_Setting
    end interface

    public :: IO


    contains

    type(IO) function Inout_Initialize_Path_Setting
        implicit none
        character(64), parameter :: dName = "ProjectPath.dir"
        integer(int32) :: access, status, len_path, unit_num
        integer :: i, j

        status = access(dName, "r")
        if (status /= 0) call error_message(901)
        open(newunit=unit_num, file=dName, iostat=status, status="old")
        if (status /= 0) call error_message(901)

        read(unit_num, '(a)') Inout_Initialize_Path_Setting%dir_Path
        close(unit_num)
        len_path = len_trim(Inout_Initialize_Path_Setting%dir_Path)
        Inout_Initialize_Path_Setting%dir_Path = adjustl(Inout_Initialize_Path_Setting%dir_Path)

        ! For windows, replace "\\" with "/"
        i = index(Inout_Initialize_Path_Setting%dir_Path, "\\")
        do while (i > 0)
            Inout_Initialize_Path_Setting%dir_Path(i:i+1) = "/"
            if (i+2 <= len_path) then
                Inout_Initialize_Path_Setting%dir_Path(i+1:) = Inout_Initialize_Path_Setting%dir_Path(i+2:) // " "
            endif
            len_path = len_path - 1
            i = index(Inout_Initialize_Path_Setting%dir_Path, "\\")
        end do

        ! For UNIX, replace "\" with "/"
        i = index(Inout_Initialize_Path_Setting%dir_Path, "\")
        do while (i > 0)
            Inout_Initialize_Path_Setting%dir_Path(i:i) = "/"
            len_path = len_trim(Inout_Initialize_Path_Setting%dir_Path)
            i = index(Inout_Initialize_Path_Setting%dir_Path, "\")
        end do

        ! Add "/" to end to path
        if (len_path > 0 .and. Inout_Initialize_Path_Setting%dir_Path(len_path:len_path) /= "/") then
            Inout_Initialize_Path_Setting%dir_Path = trim(adjustl(Inout_Initialize_Path_Setting%dir_Path)) // "/"
        endif

        Inout_Initialize_Path_Setting%in_Basic_FileName   = trim(Inout_Initialize_Path_Setting%dir_Path) // "Input/Basic.in"
        Inout_Initialize_Path_Setting%in_BC_FileName      = trim(Inout_Initialize_Path_Setting%dir_Path) // "Input/BC.in"
        Inout_Initialize_Path_Setting%in_BCtype_FileName  = trim(Inout_Initialize_Path_Setting%dir_Path) // "Input/BCtype.in"
        Inout_Initialize_Path_Setting%in_IC_FileName      = trim(Inout_Initialize_Path_Setting%dir_Path) // "Input/IC.in"
        Inout_Initialize_Path_Setting%in_Obs_FileName     = trim(Inout_Initialize_Path_Setting%dir_Path) // "Input/Obs.in"
        Inout_Initialize_Path_Setting%in_ObsFlag_FileName = trim(Inout_Initialize_Path_Setting%dir_Path) // "Input/printobs.in"
        Inout_Initialize_Path_Setting%in_Top_FileName     = trim(Inout_Initialize_Path_Setting%dir_Path) // "Input/top.in"
        Inout_Initialize_Path_Setting%in_COO_FileName     = trim(Inout_Initialize_Path_Setting%dir_Path) // "Input/coordinate.in"


        if (access(Inout_Initialize_Path_Setting%in_Basic_FileName,   "r") /= 0) stop
        if (access(Inout_Initialize_Path_Setting%in_BC_FileName,      "r") /= 0) stop
        if (access(Inout_Initialize_Path_Setting%in_BCtype_FileName,  "r") /= 0) stop
        if (access(Inout_Initialize_Path_Setting%in_IC_FileName,      "r") /= 0) stop
        if (access(Inout_Initialize_Path_Setting%in_Obs_FileName,     "r") /= 0) stop
        if (access(Inout_Initialize_Path_Setting%in_ObsFlag_FileName, "r") /= 0) stop
        if (access(Inout_Initialize_Path_Setting%in_Top_FileName,     "r") /= 0) stop
        if (access(Inout_Initialize_Path_Setting%in_COO_FileName,     "r") /= 0) stop

        Inout_Initialize_Path_Setting%out_T_FileName    = trim(Inout_Initialize_Path_Setting%dir_Path) // "Output/obsf_T.dat"
        Inout_Initialize_Path_Setting%out_Fr_FileName   = trim(Inout_Initialize_Path_Setting%dir_Path) // "Output/obsf_Fr.dat"
        Inout_Initialize_Path_Setting%out_TC_FileName   = trim(Inout_Initialize_Path_Setting%dir_Path) // "Output/obsf_TC.dat"
        Inout_Initialize_Path_Setting%out_C_FileName    = trim(Inout_Initialize_Path_Setting%dir_Path) // "Output/obsf_C.dat"
        Inout_Initialize_Path_Setting%out_P_FileName    = trim(Inout_Initialize_Path_Setting%dir_Path) // "Output/obsf_P.dat"
        Inout_Initialize_Path_Setting%out_Flux_FileName = trim(Inout_Initialize_Path_Setting%dir_Path) // "Output/obsf_Flux.dat"
        Inout_Initialize_Path_Setting%out_K_FileName    = trim(Inout_Initialize_Path_Setting%dir_Path) // "Output/obsf_K.dat"

    end function Inout_Initialize_Path_Setting

    subroutine Inout_Input_Parameters(self, Solver)
        implicit none
        class(IO) :: self
        type(SolverInfo), intent(inout) :: Solver
        integer(int32) :: status, unit_num

        open(newunit=unit_num, file=self%in_Basic_FileName, status="old", action="read", iostat=status)
        if (status /= 0) call error_message(902)

        read(unit_num, *)
        read(unit_num, *)
        read(unit_num, *)
        read(unit_num, *)
        read(unit_num, *) Solver%N%element, Solver%N%node, Solver%N%shape, Solver%N%dim
        read(unit_num, *)
        read(unit_num, *) Solver%nAnalysis, Solver%nFrTreat, Solver%nTimeDisc, Solver%isStdOut, Solver%outputFile
        read(unit_num, *)
        read(unit_num, *) Solver%Time%tUnit, Solver%Time%cTime, Solver%Time%cdt, Solver%Time%cinterval
        read(unit_num, *)
        read(unit_num, *)
        read(unit_num, *)
        read(unit_num, *)
        read(unit_num, *) Solver%Heat%Constants%Porosity, Solver%Heat%Constants%LatentHeat
        read(unit_num, *)
        read(unit_num, *) Solver%Heat%Constants%Density%soil, Solver%Heat%Constants%Density%water, Solver%Heat%Constants%Density%ice
        read(unit_num, *)
        read(unit_num, *) Solver%Heat%Constants%ThermalConductivity%soil, Solver%Heat%Constants%ThermalConductivity%water, Solver%Heat%Constants%ThermalConductivity%ice, Solver%Heat%Constants%dispersity%x, Solver%Heat%Constants%dispersity%y
        read(unit_num, *)
        read(unit_num, *) Solver%Heat%Constants%SpecificHeat%soil, Solver%Heat%Constants%SpecificHeat%water, Solver%Heat%Constants%SpecificHeat%ice
        read(unit_num, *)
        read(unit_num, *) Solver%Heat%Latent%GCC%thetaS, Solver%Heat%Latent%GCC%thetaR, Solver%Heat%Latent%GCC%alpha, Solver%Heat%Latent%GCC%n
        read(unit_num, *)
        read(unit_num, *) Solver%Heat%Latent%Power%Tf, Solver%Heat%Latent%Power%a
        read(unit_num, *)
        read(unit_num, *)
        read(unit_num, *)
        read(unit_num, *)
        read(unit_num, *) Solver%Water%Constants%HydraulicConductivity%soil, Solver%Water%Constants%HydraulicConductivity%ice
        read(unit_num, *)
        read(unit_num, *)
        read(unit_num, *)
        read(unit_num, *)
        read(unit_num, *) Solver%Lis%TSolver, Solver%Lis%TOption, Solver%Lis%PSolver, Solver%Lis%POption
        read(unit_num, *)
        read(unit_num, *) Solver%Lis%Maxiter, Solver%Lis%Tol, Solver%Lis%isOMP
        close(unit_num)

        Solver%N%ShCoe                           = Solver%N%shape * Solver%N%dim
        Solver%Heat%Constants%HeatCapacity%soil  = Solver%Heat%Constants%Density%soil  * Solver%Heat%Constants%SpecificHeat%soil
        Solver%Heat%Constants%HeatCapacity%water = Solver%Heat%Constants%Density%water * Solver%Heat%Constants%SpecificHeat%water
        Solver%Heat%Constants%HeatCapacity%ice   = Solver%Heat%Constants%Density%ice   * Solver%Heat%Constants%SpecificHeat%ice

        Solver%Water%Constants%zeta              = Solver%Heat%Constants%Density%ice  / Solver%Heat%Constants%Density%water - 1.d0
        ! print*,Solver%Water%Constants%zeta
        ! stop

        Solver%isHeat   = mod(Solver%nAnalysis, 8) >= 4
        Solver%isWater  = mod(Solver%nAnalysis, 4) >= 2
        Solver%isStress = mod(Solver%nAnalysis, 2) == 1

        if (Solver%nFrTreat == 20 .or. Solver%nFrTreat == 30) then
            Solver%Heat%Latent%useModel  = Solver%nFrTreat
            Solver%Heat%Latent%Lf        = Solver%Heat%Constants%LatentHeat
            Solver%Heat%Latent%rhoI      = Solver%Heat%Constants%Density%ice
            if (Solver%nFrTreat == 20) then
                Solver%Heat%Latent%GCC%m     = 1.d0 - 1.d0 / Solver%Heat%Latent%GCC%n
                Solver%Heat%Latent%GCC%Tf    = Solver%Heat%Latent%Power%Tf
            else if (Solver%nFrTreat == 30) then
                Solver%Heat%Latent%Power%phi = Solver%Heat%Constants%Porosity
            end if
        end if

    end subroutine Inout_Input_Parameters

    subroutine Inout_Input_Flags(self, Solver)
        implicit none
        class(IO)                           :: self
        type(SolverInfo), intent(inout) :: Solver
        integer(int32)                      :: unit_num, status
        integer(int32)                      :: i, num, num_flag
        character(32)                       :: dummy_variable

        open(newunit=unit_num, file=self%in_ObsFlag_FileName, status="old", action="read", iostat=status)
        if (status /= 0) call error_message(902)

        read(unit_num, *)
        read(unit_num, *) num
        read(unit_num, *)

        call Allocate_Vector(Solver%Flags%outOBS, num)
        do i = 1, num
            read(unit_num, *) dummy_variable, num_flag
            if (num_flag == 1) then
                Solver%Flags%outOBS(i) = .true.
            else
                Solver%Flags%outOBS(i) = .false.
            end if
        end do

        close(unit_num)

        if (.not. Solver%isHeat) then
            Solver%Flags%outOBS(1) = .false.
            Solver%Flags%outOBS(2) = .false.
            Solver%Flags%outOBS(3) = .false.
            Solver%Flags%outOBS(4) = .false.
        end if
        if (.not. Solver%isWater) then
            Solver%Flags%outOBS(5) = .false.
            Solver%Flags%outOBS(6) = .false.
            Solver%Flags%outOBS(7) = .false.
        end if

        ! 潜熱の取り扱いのフラグ
		if (10 <= Solver%nFrTreat .and. Solver%nFrTreat <= 13) then
			Solver%Flags%isTRM   = .true.
			Solver%Flags%isGCC   = .false.
			Solver%Flags%isPower = .false.
            if (Solver%nFrTreat == 10) then
                Solver%Flags%isSwitchTRM     = .false.
                Solver%Flags%isSwitchOnceTRM = .false.
            else if (Solver%nFrTreat == 11) then
                Solver%Flags%isSwitchTRM     = .true.
                Solver%Flags%isSwitchOnceTRM = .false.
            else if (Solver%nFrTreat == 12) then
                Solver%Flags%isSwitchTRM     = .false.
                Solver%Flags%isSwitchOnceTRM = .true.
            else if (Solver%nFrTreat == 13) then
                Solver%Flags%isSwitchTRM     = .true.
                Solver%Flags%isSwitchOnceTRM = .true.
            end if
		else if (Solver%nFrTreat == 20) then
				Solver%Flags%isTRM   = .false.
				Solver%Flags%isGCC   = .true.
				Solver%Flags%isPower = .false.
        else if (Solver%nFrTreat == 30) then
				Solver%Flags%isTRM   = .false.
				Solver%Flags%isGCC   = .false.
				Solver%Flags%isPower = .true.
		end if

		Solver%Flags%isOutput      = .true.
		Solver%Flags%isPrintLisMem = .true.
		if (Solver%isStdOut == 1) then
			Solver%Flags%isStdOut  = .true.
		else
			Solver%Flags%isStdOut  = .false.
		end if
		if (Solver%outputFile == 1) then
			Solver%Flags%isOutputAll = .true.
		else
			Solver%Flags%isOutputAll = .false.
		end if

    end subroutine Inout_Input_Flags

    subroutine Inout_Input_BC(self, Solver)
        implicit none
        class(IO)                           :: self
        type(SolverInfo), intent(inout)    :: Solver
        integer(int32)                     :: status, unit_num, i

        type(BoudaryConditionInfo)          :: tmpBC
        integer(int32), allocatable         :: tmpType(:,:), tmpEdges(:,:)
        real(real64),   allocatable         :: tmpValue(:,:)
        character(32)                       :: dummy_variable

        open(newunit=unit_num, file=self%in_BC_FileName, status="old", action="read", iostat=status)
        read(unit_num, *)
        read(unit_num, *) Solver%BC%numNode, Solver%BC%numType
        read(unit_num, *)

        call Allocate_BCinfo(tmpBC, Solver%BC%numNode, Solver%BC%numType)
        do i = 1,  Solver%BC%numNode
            read(unit_num, *) tmpBC%Node(i), tmpBC%TypeKey(i)
        end do
        read(unit_num, *) dummy_variable
        if (dummy_variable == "BCEdges") then
            read(unit_num, *) Solver%BC%numEdges
            read(unit_num, *)
            call Allocate_Matrix(tmpEdges, 3, Solver%BC%numEdges)
            do i = 1, Solver%BC%numEdges
                read(unit_num, *) tmpEdges(1, i), tmpEdges(2, i), tmpEdges(3, i)
            end do
        end if
        close(unit_num)
        ! print*,tmpEdges(3,:)

        call Allocate_Matrix(tmpType,  3, Solver%BC%numType)
        call Allocate_Matrix(tmpValue, 3, Solver%BC%numType)
        open(newunit=unit_num, file=self%in_BCtype_FileName, status="old", action="read", iostat=status)
        read(unit_num, *)
        do i = 1, Solver%BC%numType
            read(unit_num, *)   tmpType(Temperature,  i), tmpType(Pressure,  i), tmpType(Stress,  i), &
                              & tmpValue(Temperature, i), tmpValue(Pressure, i), tmpValue(Stress, i)
        end do
        close(unit_num)
        ! if any()
        if (Solver%isHeat) then
            if (Solver%BC%numEdges > 0) then
                call Allocate_BCinfo(Solver%BC%Heat, Solver%BC%numNode, Solver%BC%numType, Solver%BC%numEdges)
            else
                call Allocate_BCinfo(Solver%BC%Heat, Solver%BC%numNode, Solver%BC%numType)
            end if
            Solver%BC%Heat%Node(:)    = tmpBC%Node(:)
            Solver%BC%Heat%TypeKey(:) = tmpBC%TypeKey(:)
            Solver%BC%Heat%Type(:)    = tmpType(Temperature, :)
            Solver%BC%Heat%Value(:)   = tmpValue(Temperature, :)
            if (Solver%BC%numEdges > 0) then
                Solver%BC%Heat%Edges%x(:) = tmpEdges(1,:)
                Solver%BC%Heat%Edges%y(:) = tmpEdges(2,:)
                Solver%BC%Heat%EdgesDirection(:) = tmpEdges(3,:)
            end if
        end if
        if (Solver%isWater) then
            if (Solver%BC%numEdges > 0) then
                call Allocate_BCinfo(Solver%BC%Water, Solver%BC%numNode, Solver%BC%numType, Solver%BC%numEdges)
            else
                call Allocate_BCinfo(Solver%BC%Water, Solver%BC%numNode, Solver%BC%numType)
            end if
            ! call Allocate_BCinfo(Solver%BC%Water, Solver%BC%numNode, Solver%BC%numType)
            Solver%BC%Water%Node(:)    = tmpBC%Node(:)
            Solver%BC%Water%TypeKey(:) = tmpBC%TypeKey(:)
            Solver%BC%Water%Type(:)    = tmpType(Pressure, :)
            Solver%BC%Water%Value(:)   = tmpValue(Pressure, :)
            if (Solver%BC%numEdges > 0) then
                Solver%BC%Water%Edges%x(:) = tmpEdges(1,:)
                Solver%BC%Water%Edges%y(:) = tmpEdges(2,:)
                Solver%BC%Water%EdgesDirection(:) = tmpEdges(3,:)
            end if
        end if
        if (Solver%isStress) then
            call Allocate_BCinfo(Solver%BC%Stress, Solver%BC%numNode, Solver%BC%numType)
            Solver%BC%Stress%Node(:)    = tmpBC%Node(:)
            Solver%BC%Stress%TypeKey(:) = tmpBC%TypeKey(:)
            Solver%BC%Stress%Type(:)    = tmpType(Stress, :)
            Solver%BC%Stress%Value(:)   = tmpValue(Stress, :)
        end if
        if (allocated(tmpBC%Node))    deallocate(tmpBC%Node)
        if (allocated(tmpBC%TypeKey)) deallocate(tmpBC%TypeKey)
        if (allocated(tmpType))       deallocate(tmpType)
        if (allocated(tmpValue))      deallocate(tmpValue)
        if (allocated(tmpEdges))      deallocate(tmpEdges)

    end subroutine Inout_Input_BC

    subroutine Inout_Input_IC(self, Solver)
        implicit none
        class(IO)                        :: self
        type(SolverInfo), intent(inout) :: Solver
        integer(int32)                  :: status, unit_num

        open(newunit=unit_num, file=self%in_IC_FileName, status="old", action="read", iostat=status)
        if (status /= 0) call error_message(903)

        read(unit_num, *)
        read(unit_num, *) Solver%IC%Heat%Type,   Solver%IC%Heat%Value
        read(unit_num, *) Solver%IC%Water%Type,  Solver%IC%Water%Value
        read(unit_num, *) Solver%IC%Stress%Type, Solver%IC%Stress%Value

        close(unit_num)

        Solver%IC%Heat%isSet   = Solver%IC%Heat%Type   == 2
        Solver%IC%Water%isSet  = Solver%IC%Water%Type  == 2
        Solver%IC%Stress%isSet = Solver%IC%Stress%Type == 2

    end subroutine Inout_Input_IC

    subroutine Inout_Input_Vertices(self, Solver)
        implicit none
        class(IO)                        :: self
        type(SolverInfo), intent(inout) :: Solver
        integer(int32)                  :: status, unit_num, iElem

        open(newunit=unit_num, file=self%in_Top_FileName, status="old", action="read", iostat=status)
        if (status /= 0) call error_message(905)

        if (Solver%N%ShCoe == 3) then
            do iElem = 1, Solver%N%element
                read(unit_num, *) Solver%N%pElement(1, iElem), Solver%N%pElement(2, iElem), Solver%N%pElement(3, iElem)
            end do
        end if
        close(unit_num)

    end subroutine Inout_Input_Vertices

    subroutine Inout_Input_Coodinates(self, Solver)
        implicit none
        class(IO)                        :: self
        type(SolverInfo), intent(inout) :: Solver
        integer(int32) :: status, unit_num, iN

        open(newunit=unit_num, file=self%in_COO_FileName, status="old", action="read", iostat=status)
        if (status /= 0) call error_message(906)

        do iN = 1, Solver%N%node
            read(unit_num, *) Solver%N%vCood%x(iN), Solver%N%vCood%y(iN)
        end do

        close(unit_num)
    end subroutine Inout_Input_Coodinates

    subroutine Inout_Input_Observation(self, Solver)
        implicit none
        class(IO)                        :: self
        type(SolverInfo), intent(inout) :: Solver
        integer(int32)                  :: status, unit_num, iObs

        open(newunit=unit_num, file=self%in_Obs_FileName, status="old", action="read", iostat=status)
        if (status /= 0) call error_message(905)

        read(unit_num, *)
        read(unit_num, *) Solver%Obs%nObsType
        read(unit_num, *)
        read(unit_num, *) Solver%Obs%nObs
        read(unit_num, *)

        if (Solver%Obs%nObsType == 1) then
            call Allocate_Vector(Solver%Obs%obsPoint, Solver%Obs%nObs)
            do iObs = 1, Solver%Obs%nObs
                read(unit_num, *) Solver%Obs%obsPoint(iObs)
            end do
        else if (Solver%Obs%nObsType == 2) then
            call Allocate_DP2d(Solver%Obs%obsCOO, Solver%Obs%nObs)
            do iObs = 1, Solver%Obs%nObs
                read(unit_num, *) Solver%Obs%obsCOO%x(iObs), Solver%Obs%obsCOO%y(iObs)
            end do
        end if
        close(unit_num)
    end subroutine Inout_Input_Observation

    subroutine Inout_Output_All(self, Solver, num)
        implicit none
        class(IO)                        :: self
        type(SolverInfo), intent(inout) :: Solver
        integer(int32),   intent(in)    :: num

        character(256)                  :: oName, fmt
        integer(int32)                  :: ios, unit_num, iN

        write(oName, Solver%fmt_Fileout) trim(self%dir_Path), "Output/DATFILE/Output_", num ,".dat"
        open(newunit=unit_num, file=oName, status='replace', action='write', iostat=ios)
        if (ios /= 0) call error_message(931)

        select case (Solver%nAnalysis)
        case (1)
            ! do iN = 1, Solver%N%node
            !     write(unit_num, '(es15.7,a,es15.7)') Solver%T%pre(iN), ', ', Solver%T%Si(iN)
            ! end do
        case (2)
        case (3)
        case (4)
            fmt = '(2es13.5,2es13.5)'
            do iN = 1, Solver%N%node
                write(unit_num, fmt) Solver%N%vCood%x(iN), Solver%N%vCood%y(iN), Solver%T%pre(iN), Solver%Si%pre(iN)
            end do
        case (5)
        case (6)
            fmt = '(2es13.5,5es13.5)'
            do iN = 1, Solver%N%node
                write(unit_num, fmt) Solver%N%vCood%x(iN), Solver%N%vCood%y(iN),Solver%T%pre(iN), Solver%P%pre(iN), Solver%Si%pre(iN), Solver%Water%Variables%wFlux%x(iN), + Solver%Water%Variables%wFlux%y(iN)
            end do
        case (7)

        end select
        close(unit_num)

    end subroutine Inout_Output_All

    subroutine Inout_Output_Observation(self, Solver, time)
        implicit none
        class(IO)                        :: self
        type(SolverInfo), intent(inout) :: Solver
        real(real64),     intent(in)    :: time

        integer(int32) :: unit_num, ios, iObs, nObs, iS, dim
        real(real64)   :: obsValue(Solver%Obs%nObs), obsValue2d(2 * Solver%Obs%nObs)
        real(real64)   :: tmpValue

        character(64) :: ofmt

        nObs = Solver%Obs%nObs

        if (Solver%Flags%outOBS(1)) then
            write(ofmt, '(a, i0, a)') '(es15.7,', nObs, 'es16.8)'
            if (time == 0.0d0) then
                open(newunit=unit_num, file=self%out_T_FileName, status='replace', action='write', form='formatted', position='append', iostat=ios)
            else
                open(newunit=unit_num, file=self%out_T_FileName, status='old', action='write', form='formatted', position='append', iostat=ios)
            end if
            if (Solver%Obs%nObsType == 1) then
                write(unit_num, ofmt) time, (Solver%T%pre(Solver%Obs%obsPoint(iObs)), iObs = 1, nObs)
            else if (Solver%Obs%nObsType == 2) then
                do iObs = 1, nObs
                    tmpValue = 0.0d0
                    do iS = 1, Solver%N%Shcoe
                        tmpValue = tmpValue + Solver%Obs%vAreaObs(iS, iObs) * Solver%T%pre(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs)))
                    end do
                    obsValue(iObs) = tmpValue
                end do
                write(unit_num, ofmt) time, (obsValue(iObs), iObs = 1, nObs)
            end if
            close(unit_num)
        end if
        if (Solver%Flags%outOBS(2)) then
            write(ofmt, '(a, i0, a)') '(es15.7,', nObs, 'es16.8)'
            if (time == 0.0d0) then
                open(newunit=unit_num, file=self%out_Fr_FileName, status='replace', action='write', form='formatted', position='append', iostat=ios)
            else
                open(newunit=unit_num, file=self%out_Fr_FileName, status='old', action='write', form='formatted', position='append', iostat=ios)
            end if
            if (Solver%Obs%nObsType == 1) then
                write(unit_num, ofmt) time, (Solver%Si%pre(Solver%Obs%obsPoint(iObs)), iObs = 1, nObs)
            else if (Solver%Obs%nObsType == 2) then
                do iObs = 1, nObs
                    tmpValue = 0.0d0
                    do iS = 1, Solver%N%Shcoe
                        tmpValue = tmpValue + Solver%Obs%vAreaObs(iS, iObs) * Solver%Si%pre(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs)))
                    end do
                    obsValue(iObs) = tmpValue
                end do
                write(unit_num, ofmt) time, (obsValue(iObs), iObs = 1, nObs)
            end if
            close(unit_num)
        end if
        if (Solver%Flags%outOBS(3)) then
            write(ofmt, '(a, i0, a)') '(es15.7,', 2*nObs, 'es16.8)'
            if (time == 0.0d0) then
                open(newunit=unit_num, file=self%out_TC_FileName, status='replace', action='write', form='formatted', position='append', iostat=ios)
            else
                open(newunit=unit_num, file=self%out_TC_FileName, status='old', action='write', form='formatted', position='append', iostat=ios)
            end if
            if (Solver%Obs%nObsType == 1) then
                write(unit_num, ofmt) time, (Solver%Heat%Variables%Tgrad%x(Solver%Obs%obsPoint(iObs)),Solver%Heat%Variables%Tgrad%y(Solver%Obs%obsPoint(iObs)), iObs = 1, nObs)
            else if (Solver%Obs%nObsType == 2) then
                do iObs = 1, nObs
                    tmpValue = 0.0d0
                    do iS = 1, Solver%N%Shcoe
                        tmpValue = tmpValue + (Solver%Obs%vAreaObs(iS, iObs) * Solver%Heat%Variables%Tgrad%x(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs))))**2
                        ! tmpValue = tmpValue + sqrt((Solver%Obs%vAreaObs(iS, iObs) * Solver%Heat%Variables%Tgrad%x(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs))))**2+(Solver%Obs%vAreaObs(iS, iObs) * Solver%Heat%Variables%Tgrad%y(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs))))**2)
                    end do
                    obsValue(iObs) = tmpValue
                end do
                write(unit_num, ofmt) time, (obsValue(iObs), iObs = 1, nObs)
            end if
            close(unit_num)
        end if
        if (Solver%Flags%outOBS(4)) then
            write(ofmt, '(a, i0, a)') '(es15.7,', nObs, 'es16.8)'
            if (time == 0.0d0) then
                open(newunit=unit_num, file=self%out_C_FileName, status='replace', action='write', form='formatted', position='append', iostat=ios)
            else
                open(newunit=unit_num, file=self%out_C_FileName, status='old', action='write', form='formatted', position='append', iostat=ios)
            end if
            if (Solver%Flags%isTRM) then
                if (Solver%Obs%nObsType == 1) then
                    write(unit_num, ofmt) time, (Solver%Heat%Variables%Cp%pre(Solver%Obs%obsPoint(iObs)), iObs = 1, nObs)
                else if (Solver%Obs%nObsType == 2) then
                    do iObs = 1, nObs
                        tmpValue = 0.0d0
                        do iS = 1, Solver%N%Shcoe
                            tmpValue = tmpValue + Solver%Obs%vAreaObs(iS, iObs) * Solver%Heat%Variables%Cp%pre(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs)))
                        end do
                        obsValue(iObs) = tmpValue
                    end do
                    write(unit_num, ofmt) time, (obsValue(iObs), iObs = 1, nObs)
                end if
            else
                if (Solver%Obs%nObsType == 1) then
                    write(unit_num, ofmt) time, (Solver%Heat%Variables%Ca%pre(Solver%Obs%obsPoint(iObs)), iObs = 1, nObs)
                else if (Solver%Obs%nObsType == 2) then
                    do iObs = 1, nObs
                        tmpValue = 0.0d0
                        do iS = 1, Solver%N%Shcoe
                            tmpValue = tmpValue + Solver%Obs%vAreaObs(iS, iObs) * Solver%Heat%Variables%Ca%pre(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs)))
                        end do
                        obsValue(iObs) = tmpValue
                    end do
                    write(unit_num, ofmt) time, (obsValue(iObs), iObs = 1, nObs)
                end if
            end if
            close(unit_num)
        end if
        if (Solver%Flags%outOBS(5)) then
            write(ofmt, '(a, i0, a)') '(es15.7,', nObs, 'es16.8)'
            if (time == 0.0d0) then
                open(newunit=unit_num, file=self%out_P_FileName, status='replace', action='write', form='formatted', position='append', iostat=ios)
            else
                open(newunit=unit_num, file=self%out_P_FileName, status='old', action='write', form='formatted', position='append', iostat=ios)
            end if
            if (Solver%Obs%nObsType == 1) then
                write(unit_num, ofmt) time, (Solver%P%pre(Solver%Obs%obsPoint(iObs)), iObs = 1, nObs)
            else if (Solver%Obs%nObsType == 2) then
                do iObs = 1, nObs
                    tmpValue = 0.0d0
                    do iS = 1, Solver%N%Shcoe
                        tmpValue = tmpValue + Solver%Obs%vAreaObs(iS, iObs) * Solver%P%pre(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs)))
                    end do
                    obsValue(iObs) = tmpValue
                end do
                write(unit_num, ofmt) time, (obsValue(iObs), iObs = 1, nObs)
            end if
            close(unit_num)
        end if
        if (Solver%Flags%outOBS(6)) then
            write(ofmt, '(a, i0, a)') '(es15.7,', 2*nObs, 'es16.8)'
            if (time == 0.0d0) then
                open(newunit=unit_num, file=self%out_Flux_FileName, status='replace', action='write', form='formatted', position='append', iostat=ios)
            else
                open(newunit=unit_num, file=self%out_Flux_FileName, status='old', action='write', form='formatted', position='append', iostat=ios)
            end if
            if (Solver%Obs%nObsType == 1) then
                write(unit_num, ofmt) time, (Solver%Water%Variables%wFlux%x(Solver%Obs%obsPoint(iObs)), Solver%Water%Variables%wFlux%y(Solver%Obs%obsPoint(iObs)), iObs = 1, nObs)
            else if (Solver%Obs%nObsType == 2) then
                do iObs = 1, nObs
                    do dim = 1, 2
                        if (dim == 1) then
                            tmpValue = 0.0d0
                            do iS = 1, Solver%N%Shcoe
                                tmpValue = tmpValue + Solver%Obs%vAreaObs(iS, iObs) * Solver%Water%Variables%wFlux%x(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs)))
                            end do
                        else
                            do iS = 1, Solver%N%Shcoe
                                tmpValue = tmpValue + Solver%Obs%vAreaObs(iS, iObs) * Solver%Water%Variables%wFlux%y(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs)))
                            end do
                        end if
                        obsValue(2 * (iObs-1) + dim) = tmpValue
                    end do

                end do
                write(unit_num, ofmt) time, (obsValue(iObs), iObs = 1, 2 * nObs)
            end if
            close(unit_num)
        end if
        if (Solver%Flags%outOBS(7)) then
            write(ofmt, '(a, i0, a)') '(es15.7,', nObs, 'es16.8)'
            if (time == 0.0d0) then
                open(newunit=unit_num, file=self%out_K_FileName, status='replace', action='write', form='formatted', position='append', iostat=ios)
            else
                open(newunit=unit_num, file=self%out_K_FileName, status='old', action='write', form='formatted', position='append', iostat=ios)
            end if
            if (Solver%Obs%nObsType == 1) then
                write(unit_num, ofmt) time, (Solver%Water%Variables%Klh%pre(Solver%Obs%obsPoint(iObs)), iObs = 1, nObs)
            else if (Solver%Obs%nObsType == 2) then
                do iObs = 1, nObs
                    tmpValue = 0.0d0
                    do iS = 1, Solver%N%Shcoe
                        tmpValue = tmpValue + Solver%Obs%vAreaObs(iS, iObs) * Solver%Water%Variables%Klh%pre(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs)))
                    end do
                    obsValue(iObs) = tmpValue
                end do
                write(unit_num, ofmt) time, (obsValue(iObs), iObs = 1, nObs)
            end if
            close(unit_num)
        end if
    end subroutine Inout_Output_Observation


    subroutine Output_time(ar_secsum, ttime, dxmin, dt, tmax, nSType, nLFType)
        implicit none
        real(real64), intent(in) :: ar_secsum(:), ttime, dxmin, dt, tmax
        integer(int32), intent(in) :: nSType, nLFType
        integer(int32) :: ios, tdata(8)
        character(len=100) :: oPath, oName
        character(len=12) :: real_clock(3)
        character(len=5) :: SType, LFType

        call date_and_time(real_clock(1), real_clock(2), real_clock(3), tdata)
        if (nSType == 1) then
        SType = "LU"
        else if (nSType == 2) then
        SType = "GE"
        else if (nSType == 3) then
        SType = "CG"
        else
        SType= "ELSE"
        end if
        if (nLFType == 1) then
        LFType = "TRM"
        else if (nLFType == 2) then
        LFType = "ASBM"
        else
        LFType = "ELSE"
        end if
        call get_path(oPath)
        ! write(oName, '(a,f6.4,a,f4.1,a,f3.1,5a)') trim(oPath) // 'Output/TIME/' // 'Time_', dxmin, 'm_', dt, 's_', tmax/86400, 'day_',&
        ! & trim(SType),'_', trim(LFType), '.dat'
        ! print*, trim(oName)
        
        oName = trim(oPath) // 'Output/TIME/time.dat'
        open(100, file = oName, status='replace', action="write", iostat=ios)
        if (ios /= 0) call error_message(931)

        write(100,'(a,i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)') 'Date : ', tdata(1), '/', tdata(2), '/', tdata(3), '   Time : ',&
        & tdata(5), ':', tdata(6), ':', tdata(7)
        write(100,'(a)') '--------------------------------------------------'
        write(100,'(a,f6.4,a)') 'Minimum Distance  : ', dxmin, 'm'
        write(100,'(a,f6.2,a)') 'Initial time step : ', dt, 's'
        write(100,'(a,i0,a)')   'Termination time  : ', nint(tmax), 's'
        write(100,'(a)') '--------------------------------------------------'
        write(100,'(a,f13.5,a)') 'Total time      :', ttime, 's'
        write(100,'(a,f13.5,a)') 'Assemble Section:', ar_secsum(1), 's'
        write(100,'(a,f13.5,a)') 'Matrix Calculate:', ar_secsum(2), 's'
        write(100,'(a,f13.5,a)') 'Solve linear Eq.:', ar_secsum(3), 's'
        write(100,'(a,f13.5,a)') 'Latent Heat Clac:', ar_secsum(4), 's'
        write(100,'(a)', advance='no') '--------------------------------------------------'

        close(100)

        return
    end subroutine Output_time

    subroutine Inout_Output_All_vtk(self, Solver, num)
            
        implicit none
        class(IO)                        :: self
        type(SolverInfo), intent(inout) :: Solver
        integer(int32),   intent(in)    :: num

        character(256)                  :: oName, fmt
        integer(int32)                  :: ios, unit_num, iN
        real(real64)                    :: wFlux_all(3, Solver%N%node)

        write(oName, Solver%fmt_Fileout) trim(self%dir_Path), "Output/DATFILE/Output_", num ,".vtk"
        open(newunit=unit_num, file=oName, status='replace', action='write', iostat=ios)
        if (ios /= 0) call error_message(931)

        
        write(unit_num, '(a)') "# vtk DataFile Version 2.0"
        write(unit_num, '(a)') "Analysis ASCII VTK file"
        write(unit_num, '(a)') "ASCII"
        write(unit_num, '(a)') "DATASET UNSTRUCTURED_GRID"
        write(unit_num, '(a,i0,a)') "POINTS ", Solver%N%node, " double"
        do iN = 1, Solver%N%node
            write(unit_num, '(3f18.13)') Solver%N%vCood%x(iN), Solver%N%vCood%y(iN), 0
        end do
        write(unit_num, '(a)') ""
        write(unit_num, '(a,i0,a,i0,a)') "CELLS ", Solver%N%element, " ", Solver%N%element * 4
        do iN = 1, Solver%N%element
            write(unit_num, '(i0,a,i0,a,i0,a,i0)') Solver%N%ShCoe, " ", Solver%N%pElement(1, iN)-1, " ", Solver%N%pElement(2, iN)-1, " ", Solver%N%pElement(3, iN)-1
        end do
        write(unit_num, '(a,i0,a)') "CELL_TYPES ", Solver%N%element
        do iN = 1, Solver%N%element
            write(unit_num, '(i0)') 5
        end do


        select case (Solver%nAnalysis)
        case (1)
            ! do iN = 1, Solver%N%node
            !     write(unit_num, '(es15.7,a,es15.7)') Solver%T%pre(iN), ', ', Solver%T%Si(iN)
            ! end do
        case (2)
        case (3)
        case (4)
            write(unit_num, '(a, i0)') "POINT_DATA ", Solver%N%node
            write(unit_num, '(a)') "SCALARS Temperature double 1"
            write(unit_num, '(a)') "LOOKUP_TABLE default"
            write(unit_num, '(es13.5)') Solver%T%pre(:)
            write(unit_num, '(a)') "SCALARS Pressure double 1"
            write(unit_num, '(a)') "LOOKUP_TABLE default"
            write(unit_num, '(es13.5)') Solver%P%pre(:)
            write(unit_num, '(a)') "SCALARS Si double 1"
            write(unit_num, '(a)') "LOOKUP_TABLE default"
            write(unit_num, '(es13.5)') Solver%Si%pre(:)
        case (6)
            wFlux_all(1, :) = Solver%Water%Variables%wFlux%x(:)
            wFlux_all(2, :) = Solver%Water%Variables%wFlux%y(:)
            wFlux_all(3, :) = 0.0d0
            write(unit_num, '(a, i0)') "POINT_DATA ", Solver%N%node
            write(unit_num, '(a)') "SCALARS Temperature double 1"
            write(unit_num, '(a)') "LOOKUP_TABLE default"
            write(unit_num, '(es13.5)') Solver%T%pre(:)
            write(unit_num, '(a)') ""
            write(unit_num, '(a)') "SCALARS Pressure double 1"
            write(unit_num, '(a)') "LOOKUP_TABLE default"
            write(unit_num, '(es13.5)') Solver%P%pre(:)
            write(unit_num, '(a)') ""
            write(unit_num, '(a)') "SCALARS Si double 1"
            write(unit_num, '(a)') "LOOKUP_TABLE default"
            write(unit_num, '(es13.5)') Solver%Si%pre(:)
            write(unit_num, '(a)') ""
            write(unit_num, '(a)') "VECTORS WaterFlux double"
            write(unit_num, '(3es13.5)') wFlux_all

        case (7)

        end select
        close(unit_num)

    end subroutine Inout_Output_All_vtk

    subroutine Measure_Time(nsec, ar_sec, ar_secsum)
        implicit none
        integer(int32), intent(in) :: nsec
        real(real64), intent(inout) :: ar_sec(:), ar_secsum(:)

        ar_sec(nsec+1) = omp_get_wtime()
        ar_secsum(nsec) = ar_secsum(nsec) + ar_sec(nsec+1) - ar_sec(nsec)
        return
    end subroutine Measure_Time
end module Inout_Inout