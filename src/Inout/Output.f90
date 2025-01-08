module Inout_Output
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Inout_SetProjectPath, only:GetProjectPath => Inout_SetProjectPath_GetProjectPath
    use :: error
    use :: allocate
    use :: Inout_Input
    use :: types

#ifdef _MPI
    use mpi
#endif
    implicit none
    private

    type :: Output
        private
        integer(int32), allocatable :: Output_Observation_Flag(:)
        character(256) :: T_FileName, Fr_FileName, TC_FileName, C_FileName, P_FileName, Flux_FileName, K_FileName
        character(256) :: dir_Path
        logical, allocatable :: is_Output(:)
        logical :: is_Output_Dat, is_Output_VTK
    contains
        ! procedure :: Output_All         => Inout_Output_All_vtk
        procedure :: Output_All => Inout_Output_All
        procedure :: Output_Observation => Inout_Output_Observation
    end type Output

    interface Output
        module procedure Output_Constructor
    end interface

    public :: Output

contains

    type(Output) function Output_Constructor(Inputs)
        implicit none
        type(Input), intent(in) :: Inputs
        integer(int32) :: Flag_Size
        integer(int32), allocatable :: Output_Observation_Flag(:)
        ! character(256) :: dir_Path
        integer :: i

        integer(int32) :: Output_File_Type

        ! Path settings
        Output_Constructor%dir_Path = GetProjectPath()

        Output_File_Type = Inputs%Input_Get_Output_File()
        if (Output_File_Type == 1) then
            Output_Constructor%is_Output_Dat = .true.
            Output_Constructor%is_Output_VTK = .false.
        else if (Output_File_Type == 2) then
            Output_Constructor%is_Output_Dat = .false.
            Output_Constructor%is_Output_VTK = .true.
        end if

        Output_Constructor%T_FileName = trim(Output_Constructor%dir_Path)//"Output/obsf_T.dat"
        Output_Constructor%Fr_FileName = trim(Output_Constructor%dir_Path)//"Output/obsf_Fr.dat"
        Output_Constructor%TC_FileName = trim(Output_Constructor%dir_Path)//"Output/obsf_TC.dat"
        Output_Constructor%C_FileName = trim(Output_Constructor%dir_Path)//"Output/obsf_C.dat"
        Output_Constructor%P_FileName = trim(Output_Constructor%dir_Path)//"Output/obsf_P.dat"
        Output_Constructor%Flux_FileName = trim(Output_Constructor%dir_Path)//"Output/obsf_Flux.dat"
        Output_Constructor%K_FileName = trim(Output_Constructor%dir_Path)//"Output/obsf_K.dat"

        Output_Observation_Flag = Inputs%Input_Get_Observation_Flag()
        Flag_Size = size(Output_Observation_Flag)
        call Allocate_Vector(Output_Constructor%is_Output, Flag_Size)
        do i = 1, Flag_Size
            call Set_Output_Flag(Output_Observation_Flag(i), Output_Constructor%is_Output(i))
        end do

        if (allocated(Output_Observation_Flag)) deallocate (Output_Observation_Flag)

    end function Output_Constructor

    subroutine Inout_Output_All(self, Solver, num)
        implicit none
        class(Output)                        :: self
        type(SolverInfo), intent(inout) :: Solver
        integer(int32), intent(in)    :: num

        if (self%is_Output_Dat) call Inout_Output_All_Dat(self, Solver, num)
        if (self%is_Output_VTK) call Inout_Output_All_vtk(self, Solver, num)

    end subroutine Inout_Output_All

    subroutine Inout_Output_All_Dat(self, Solver, num)
        implicit none
        class(Output)                        :: self
        type(SolverInfo), intent(inout) :: Solver
        integer(int32), intent(in)    :: num

        character(256)                  :: oName, fmt
        integer(int32)                  :: ios, unit_num, iN

        write (oName, Solver%fmt_Fileout) trim(self%dir_Path), "Output/DATFILE/Output_", num, ".dat"
        open (newunit=unit_num, file=oName, status='replace', action='write', iostat=ios)
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
                write (unit_num, fmt) Solver%N%vCood%x(iN), Solver%N%vCood%y(iN), Solver%T%pre(iN), Solver%Si%pre(iN)
            end do
        case (5)
        case (6)
            fmt = '(2es13.5,5es13.5)'
            do iN = 1, Solver%N%node
                write (unit_num, fmt) Solver%N%vCood%x(iN), Solver%N%vCood%y(iN), Solver%T%pre(iN), Solver%P%pre(iN), Solver%Si%pre(iN), Solver%Water%Variables%wFlux%x(iN), +Solver%Water%Variables%wFlux%y(iN)
            end do
        case (7)

        end select
        close (unit_num)

    end subroutine Inout_Output_All_Dat

    subroutine Inout_Output_Observation(self, Solver, time)
        implicit none
        class(Output)                        :: self
        type(SolverInfo), intent(inout) :: Solver
        real(real64), intent(in)    :: time

        integer(int32) :: unit_num, ios, iObs, nObs, iS, dim
        real(real64)   :: obsValue(Solver%Obs%nObs), obsValue2d(2 * Solver%Obs%nObs)
        real(real64)   :: tmpValue

        character(64) :: ofmt

        nObs = Solver%Obs%nObs

        if (Solver%Flags%outOBS(1)) then
            write (ofmt, '(a, i0, a)') '(es15.7,', nObs, 'es16.8)'
            if (time == 0.0d0) then
                open (newunit=unit_num, file=self%T_FileName, status='replace', action='write', form='formatted', position='append', iostat=ios)
            else
                open (newunit=unit_num, file=self%T_FileName, status='old', action='write', form='formatted', position='append', iostat=ios)
            end if
            if (Solver%Obs%nObsType == 1) then
                write (unit_num, ofmt) time, (Solver%T%pre(Solver%Obs%obsPoint(iObs)), iObs=1, nObs)
            else if (Solver%Obs%nObsType == 2) then
                do iObs = 1, nObs
                    tmpValue = 0.0d0
                    do iS = 1, Solver%N%Shcoe
                        tmpValue = tmpValue + Solver%Obs%vAreaObs(iS, iObs) * Solver%T%pre(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs)))
                    end do
                    obsValue(iObs) = tmpValue
                end do
                write (unit_num, ofmt) time, (obsValue(iObs), iObs=1, nObs)
            end if
            close (unit_num)
        end if
        if (Solver%Flags%outOBS(2)) then
            write (ofmt, '(a, i0, a)') '(es15.7,', nObs, 'es16.8)'
            if (time == 0.0d0) then
                open (newunit=unit_num, file=self%Fr_FileName, status='replace', action='write', form='formatted', position='append', iostat=ios)
            else
                open (newunit=unit_num, file=self%Fr_FileName, status='old', action='write', form='formatted', position='append', iostat=ios)
            end if
            if (Solver%Obs%nObsType == 1) then
                write (unit_num, ofmt) time, (Solver%Si%pre(Solver%Obs%obsPoint(iObs)), iObs=1, nObs)
            else if (Solver%Obs%nObsType == 2) then
                do iObs = 1, nObs
                    tmpValue = 0.0d0
                    do iS = 1, Solver%N%Shcoe
                        tmpValue = tmpValue + Solver%Obs%vAreaObs(iS, iObs) * Solver%Si%pre(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs)))
                    end do
                    obsValue(iObs) = tmpValue
                end do
                write (unit_num, ofmt) time, (obsValue(iObs), iObs=1, nObs)
            end if
            close (unit_num)
        end if
        if (Solver%Flags%outOBS(3)) then
            write (ofmt, '(a, i0, a)') '(es15.7,', 2 * nObs, 'es16.8)'
            if (time == 0.0d0) then
                open (newunit=unit_num, file=self%TC_FileName, status='replace', action='write', form='formatted', position='append', iostat=ios)
            else
                open (newunit=unit_num, file=self%TC_FileName, status='old', action='write', form='formatted', position='append', iostat=ios)
            end if
            if (Solver%Obs%nObsType == 1) then
                write (unit_num, ofmt) time, (Solver%Heat%Variables%Tgrad%x(Solver%Obs%obsPoint(iObs)), Solver%Heat%Variables%Tgrad%y(Solver%Obs%obsPoint(iObs)), iObs=1, nObs)
            else if (Solver%Obs%nObsType == 2) then
                do iObs = 1, nObs
                    tmpValue = 0.0d0
                    do iS = 1, Solver%N%Shcoe
                        tmpValue = tmpValue + (Solver%Obs%vAreaObs(iS, iObs) * Solver%Heat%Variables%Tgrad%x(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs))))**2
                        ! tmpValue = tmpValue + sqrt((Solver%Obs%vAreaObs(iS, iObs) * Solver%Heat%Variables%Tgrad%x(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs))))**2+(Solver%Obs%vAreaObs(iS, iObs) * Solver%Heat%Variables%Tgrad%y(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs))))**2)
                    end do
                    obsValue(iObs) = tmpValue
                end do
                write (unit_num, ofmt) time, (obsValue(iObs), iObs=1, nObs)
            end if
            close (unit_num)
        end if
        if (Solver%Flags%outOBS(4)) then
            write (ofmt, '(a, i0, a)') '(es15.7,', nObs, 'es16.8)'
            if (time == 0.0d0) then
                open (newunit=unit_num, file=self%C_FileName, status='replace', action='write', form='formatted', position='append', iostat=ios)
            else
                open (newunit=unit_num, file=self%C_FileName, status='old', action='write', form='formatted', position='append', iostat=ios)
            end if
            if (Solver%Flags%isTRM) then
                if (Solver%Obs%nObsType == 1) then
                    write (unit_num, ofmt) time, (Solver%Heat%Variables%Cp%pre(Solver%Obs%obsPoint(iObs)), iObs=1, nObs)
                else if (Solver%Obs%nObsType == 2) then
                    do iObs = 1, nObs
                        tmpValue = 0.0d0
                        do iS = 1, Solver%N%Shcoe
                            tmpValue = tmpValue + Solver%Obs%vAreaObs(iS, iObs) * Solver%Heat%Variables%Cp%pre(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs)))
                        end do
                        obsValue(iObs) = tmpValue
                    end do
                    write (unit_num, ofmt) time, (obsValue(iObs), iObs=1, nObs)
                end if
            else
                if (Solver%Obs%nObsType == 1) then
                    write (unit_num, ofmt) time, (Solver%Heat%Variables%Ca%pre(Solver%Obs%obsPoint(iObs)), iObs=1, nObs)
                else if (Solver%Obs%nObsType == 2) then
                    do iObs = 1, nObs
                        tmpValue = 0.0d0
                        do iS = 1, Solver%N%Shcoe
                            tmpValue = tmpValue + Solver%Obs%vAreaObs(iS, iObs) * Solver%Heat%Variables%Ca%pre(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs)))
                        end do
                        obsValue(iObs) = tmpValue
                    end do
                    write (unit_num, ofmt) time, (obsValue(iObs), iObs=1, nObs)
                end if
            end if
            close (unit_num)
        end if
        if (Solver%Flags%outOBS(5)) then
            write (ofmt, '(a, i0, a)') '(es15.7,', nObs, 'es16.8)'
            if (time == 0.0d0) then
                open (newunit=unit_num, file=self%P_FileName, status='replace', action='write', form='formatted', position='append', iostat=ios)
            else
                open (newunit=unit_num, file=self%P_FileName, status='old', action='write', form='formatted', position='append', iostat=ios)
            end if
            if (Solver%Obs%nObsType == 1) then
                write (unit_num, ofmt) time, (Solver%P%pre(Solver%Obs%obsPoint(iObs)), iObs=1, nObs)
            else if (Solver%Obs%nObsType == 2) then
                do iObs = 1, nObs
                    tmpValue = 0.0d0
                    do iS = 1, Solver%N%Shcoe
                        tmpValue = tmpValue + Solver%Obs%vAreaObs(iS, iObs) * Solver%P%pre(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs)))
                    end do
                    obsValue(iObs) = tmpValue
                end do
                write (unit_num, ofmt) time, (obsValue(iObs), iObs=1, nObs)
            end if
            close (unit_num)
        end if
        if (Solver%Flags%outOBS(6)) then
            write (ofmt, '(a, i0, a)') '(es15.7,', 2 * nObs, 'es16.8)'
            if (time == 0.0d0) then
                open (newunit=unit_num, file=self%Flux_FileName, status='replace', action='write', form='formatted', position='append', iostat=ios)
            else
                open (newunit=unit_num, file=self%Flux_FileName, status='old', action='write', form='formatted', position='append', iostat=ios)
            end if
            if (Solver%Obs%nObsType == 1) then
                write (unit_num, ofmt) time, (Solver%Water%Variables%wFlux%x(Solver%Obs%obsPoint(iObs)), Solver%Water%Variables%wFlux%y(Solver%Obs%obsPoint(iObs)), iObs=1, nObs)
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
                        obsValue(2 * (iObs - 1) + dim) = tmpValue
                    end do

                end do
                write (unit_num, ofmt) time, (obsValue(iObs), iObs=1, 2 * nObs)
            end if
            close (unit_num)
        end if
        if (Solver%Flags%outOBS(7)) then
            write (ofmt, '(a, i0, a)') '(es15.7,', nObs, 'es16.8)'
            if (time == 0.0d0) then
                open (newunit=unit_num, file=self%K_FileName, status='replace', action='write', form='formatted', position='append', iostat=ios)
            else
                open (newunit=unit_num, file=self%K_FileName, status='old', action='write', form='formatted', position='append', iostat=ios)
            end if
            if (Solver%Obs%nObsType == 1) then
                write (unit_num, ofmt) time, (Solver%Water%Variables%Klh%pre(Solver%Obs%obsPoint(iObs)), iObs=1, nObs)
            else if (Solver%Obs%nObsType == 2) then
                do iObs = 1, nObs
                    tmpValue = 0.0d0
                    do iS = 1, Solver%N%Shcoe
                        tmpValue = tmpValue + Solver%Obs%vAreaObs(iS, iObs) * Solver%Water%Variables%Klh%pre(Solver%N%pElement(iS, Solver%Obs%nAreaObs(iObs)))
                    end do
                    obsValue(iObs) = tmpValue
                end do
                write (unit_num, ofmt) time, (obsValue(iObs), iObs=1, nObs)
            end if
            close (unit_num)
        end if
    end subroutine Inout_Output_Observation

    ! subroutine Output_time(ar_secsum, ttime, dxmin, dt, tmax, nSType, nLFType)
    !     implicit none
    !     real(real64), intent(in) :: ar_secsum(:), ttime, dxmin, dt, tmax
    !     integer(int32), intent(in) :: nSType, nLFType
    !     integer(int32) :: ios, tdata(8)
    !     character(len=100) :: oPath, oName
    !     character(len=12) :: real_clock(3)
    !     character(len=5) :: SType, LFType

    !     call date_and_time(real_clock(1), real_clock(2), real_clock(3), tdata)
    !     if (nSType == 1) then
    !     SType = "LU"
    !     else if (nSType == 2) then
    !     SType = "GE"
    !     else if (nSType == 3) then
    !     SType = "CG"
    !     else
    !     SType= "ELSE"
    !     end if
    !     if (nLFType == 1) then
    !     LFType = "TRM"
    !     else if (nLFType == 2) then
    !     LFType = "ASBM"
    !     else
    !     LFType = "ELSE"
    !     end if
    !     call get_path(oPath)
    !     ! write(oName, '(a,f6.4,a,f4.1,a,f3.1,5a)') trim(oPath) // 'Output/TIME/' // 'Time_', dxmin, 'm_', dt, 's_', tmax/86400, 'day_',&
    !     ! & trim(SType),'_', trim(LFType), '.dat'
    !     ! print*, trim(oName)

    !     oName = trim(oPath) // 'Output/TIME/time.dat'
    !     open(100, file = oName, status='replace', action="write", iostat=ios)
    !     if (ios /= 0) call error_message(931)

    !     write(100,'(a,i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)') 'Date : ', tdata(1), '/', tdata(2), '/', tdata(3), '   Time : ',&
    !     & tdata(5), ':', tdata(6), ':', tdata(7)
    !     write(100,'(a)') '--------------------------------------------------'
    !     write(100,'(a,f6.4,a)') 'Minimum Distance  : ', dxmin, 'm'
    !     write(100,'(a,f6.2,a)') 'Initial time step : ', dt, 's'
    !     write(100,'(a,i0,a)')   'Termination time  : ', nint(tmax), 's'
    !     write(100,'(a)') '--------------------------------------------------'
    !     write(100,'(a,f13.5,a)') 'Total time      :', ttime, 's'
    !     write(100,'(a,f13.5,a)') 'Assemble Section:', ar_secsum(1), 's'
    !     write(100,'(a,f13.5,a)') 'Matrix Calculate:', ar_secsum(2), 's'
    !     write(100,'(a,f13.5,a)') 'Solve linear Eq.:', ar_secsum(3), 's'
    !     write(100,'(a,f13.5,a)') 'Latent Heat Clac:', ar_secsum(4), 's'
    !     write(100,'(a)', advance='no') '--------------------------------------------------'

    !     close(100)

    !     return
    ! end subroutine Output_time

    subroutine Inout_Output_All_vtk(self, Solver, num)

        implicit none
        class(Output)                        :: self
        type(SolverInfo), intent(inout) :: Solver
        integer(int32), intent(in)    :: num

        character(256)                  :: oName, fmt
        integer(int32)                  :: ios, unit_num, iN
        real(real64)                    :: wFlux_all(3, Solver%N%node)

        write (oName, Solver%fmt_Fileout) trim(self%dir_Path), "Output/DATFILE/Output_", num, ".vtk"
        open (newunit=unit_num, file=oName, status='replace', action='write', iostat=ios)
        if (ios /= 0) call error_message(931)

        write (unit_num, '(a)') "# vtk DataFile Version 2.0"
        write (unit_num, '(a)') "Analysis ASCII VTK file"
        write (unit_num, '(a)') "ASCII"
        write (unit_num, '(a)') "DATASET UNSTRUCTURED_GRID"
        write (unit_num, '(a,i0,a)') "POINTS ", Solver%N%node, " double"
        do iN = 1, Solver%N%node
            write (unit_num, '(3f18.13)') Solver%N%vCood%x(iN), Solver%N%vCood%y(iN), 0
        end do
        write (unit_num, '(a)') ""
        write (unit_num, '(a,i0,a,i0,a)') "CELLS ", Solver%N%element, " ", Solver%N%element * 4
        do iN = 1, Solver%N%element
            write (unit_num, '(i0,a,i0,a,i0,a,i0)') Solver%N%ShCoe, " ", Solver%N%pElement(1, iN) - 1, " ", Solver%N%pElement(2, iN) - 1, " ", Solver%N%pElement(3, iN) - 1
        end do
        write (unit_num, '(a,i0,a)') "CELL_TYPES ", Solver%N%element
        do iN = 1, Solver%N%element
            write (unit_num, '(i0)') 5
        end do

        select case (Solver%nAnalysis)
        case (1)
            ! do iN = 1, Solver%N%node
            !     write(unit_num, '(es15.7,a,es15.7)') Solver%T%pre(iN), ', ', Solver%T%Si(iN)
            ! end do
        case (2)
        case (3)
        case (4)
            write (unit_num, '(a, i0)') "POINT_DATA ", Solver%N%node
            write (unit_num, '(a)') "SCALARS Temperature double 1"
            write (unit_num, '(a)') "LOOKUP_TABLE default"
            write (unit_num, '(es13.5)') Solver%T%pre(:)
            write (unit_num, '(a)') "SCALARS Pressure double 1"
            write (unit_num, '(a)') "LOOKUP_TABLE default"
            write (unit_num, '(es13.5)') Solver%P%pre(:)
            write (unit_num, '(a)') "SCALARS Si double 1"
            write (unit_num, '(a)') "LOOKUP_TABLE default"
            write (unit_num, '(es13.5)') Solver%Si%pre(:)
        case (6)
            wFlux_all(1, :) = Solver%Water%Variables%wFlux%x(:)
            wFlux_all(2, :) = Solver%Water%Variables%wFlux%y(:)
            wFlux_all(3, :) = 0.0d0
            write (unit_num, '(a, i0)') "POINT_DATA ", Solver%N%node
            write (unit_num, '(a)') "SCALARS Temperature double 1"
            write (unit_num, '(a)') "LOOKUP_TABLE default"
            write (unit_num, '(es13.5)') Solver%T%pre(:)
            write (unit_num, '(a)') ""
            write (unit_num, '(a)') "SCALARS Pressure double 1"
            write (unit_num, '(a)') "LOOKUP_TABLE default"
            write (unit_num, '(es13.5)') Solver%P%pre(:)
            write (unit_num, '(a)') ""
            write (unit_num, '(a)') "SCALARS Si double 1"
            write (unit_num, '(a)') "LOOKUP_TABLE default"
            write (unit_num, '(es13.5)') Solver%Si%pre(:)
            write (unit_num, '(a)') ""
            write (unit_num, '(a)') "VECTORS WaterFlux double"
            write (unit_num, '(3es13.5)') wFlux_all

        case (7)

        end select
        close (unit_num)

    end subroutine Inout_Output_All_vtk

    ! subroutine Measure_Time(nsec, ar_sec, ar_secsum)
    !     implicit none
    !     integer(int32), intent(in) :: nsec
    !     real(real64), intent(inout) :: ar_sec(:), ar_secsum(:)

    !     ar_sec(nsec+1) = omp_get_wtime()
    !     ar_secsum(nsec) = ar_secsum(nsec) + ar_sec(nsec+1) - ar_sec(nsec)
    !     return
    ! end subroutine Measure_Time

    subroutine Set_Output_Flag(iValue, Flag)
        implicit none
        integer(int32), intent(in) :: iValue
        logical, intent(inout) :: Flag

        if (iValue == 1) then
            Flag = .true.
        else
            Flag = .false.
        end if

    end subroutine Set_Output_Flag
end module Inout_Output
