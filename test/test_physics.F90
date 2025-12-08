program test_physics
    use, intrinsic :: iso_fortran_env
    use :: stdlib_io, only:loadtxt
    use :: iapws, only:type_iapws06, type_iapws97
#ifdef _MPI
    use :: mpi_f08
#endif
    use :: module_core
    use :: module_physics
    implicit none

    integer(int32) :: unit
    integer(int32) :: ierr, myrank

    ! MPI初期化
#ifdef _MPI
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
#else
    myrank = 0
#endif

    if (myrank == 0) then
        open (newunit=unit, file="log/test/physics.log", status="replace", action="write", iostat=ierr)
        if (ierr /= 0) then
            print *, "Error opening log file"
            stop
        end if

        write (unit, '(a)') "# Physics module tests"
        write (unit, '(a)') "---"
        call test_density()
        call test_specific_heat()
        write (unit, '(a)') "---"
        write (unit, '(a)') "## Completed"
        close (unit)
    end if

#ifdef _MPI
    call MPI_Finalize(ierr)
#endif

contains
    subroutine test_density()
        implicit none
        type(type_state) :: state
        type(type_physics_info) :: physics_info
        type(type_iapws97) :: water
        type(type_iapws06) :: ice
        type(holder_dens) :: denstiy

        real(real64) :: computed_density, expected_density

        ! Initialize IAPWS models
        call water%initialize()
        call ice%initialize()
        physics_info%num_phases = 4
        physics_info%solid = 2650.0d0 ! soil density [kg/m3]
        physics_info%water = 1000.0d0 ! water density [kg/m3]
        physics_info%ice = 917.0d0 ! ice density [kg/m3]
        physics_info%vapor = 0.6d0 ! vapor density [kg/m3]
        call denstiy%initialize(1, physics_info, water, ice)

        ! Test case 1
        state%temperature = 10.0d0 ! [C]
        state%pressure = 101325.0d0 ! [Pa]
        state%porosity = 0.4d0
        state%water_content = 0.2d0
        state%ice_content = 0.1d0
        state%relative_humidity = 0.6d0
        call denstiy%p%calc(state, computed_density)
        expected_density = 1881.464474792440d0
        call check_variable(computed_density, expected_density, "Density Test")

    end subroutine test_density

    subroutine test_specific_heat()
        implicit none
        type(type_state) :: state
        type(type_physics_info) :: physics_info
        type(type_iapws97) :: water
        type(type_iapws06) :: ice
        type(holder_sphs) :: specific_heat

        real(real64) :: computed_specific_heat, expected_specific_heat

        ! Initialize IAPWS models
        call water%initialize()
        call ice%initialize()
        physics_info%num_phases = 4
        physics_info%solid = 800d0
        physics_info%water = 4180d0
        physics_info%ice = 2100.0d0
        physics_info%vapor = 1200.d0
        call specific_heat%initialize(1, physics_info, water, ice)

        ! Test case 1
        state%temperature = 10.0d0 ! [C]
        state%pressure = 101325.0d0 ! [Pa]
        state%porosity = 0.4d0
        state%water_content = 0.2d0
        state%ice_content = 0.1d0
        state%relative_humidity = 0.6d0
        call specific_heat%p%calc(state, computed_specific_heat)
        expected_specific_heat = 1.725720228875d3
        call check_variable(computed_specific_heat, expected_specific_heat, "Specific Heat Test")

    end subroutine test_specific_heat

    ! ======================================================================
    ! Check Utilities
    ! ======================================================================
    subroutine check_variable(v, v_exa, v_name, id)
        implicit none
        real(real64), intent(in) :: v, v_exa
        character(len=*), intent(in) :: v_name
        integer(int32), intent(in), optional :: id
        real(real64), parameter :: tol = 1.0d-8
        real(real64) :: rel_diff

        if (abs(v_exa) > 0.0d0) then
            rel_diff = abs(v - v_exa) / abs(v_exa)
        else
            rel_diff = abs(v - v_exa)
        end if

        if (rel_diff > tol) then
            write (unit, '(a)') "**FAIL**: `"//v_name//"`"
            write (unit, '(a)') ""
            write (unit, '("|",a6,"|",a20,"|",a20,"|",a20,"|")') "ID", "computed", "expected", "rel_diff"
            write (unit, '("|",a6,"|",a20,"|",a20,"|",a20,"|")') &
                repeat('-', 6), repeat('-', 20), repeat('-', 20), repeat('-', 20)
            if (present(id)) then
                write (unit, '("|",i6,"|",es20.10,"|",es20.10,"|",es20.10,"|")') id, v, v_exa, rel_diff
            else
                write (unit, '("|",a6,"|",es20.10,"|",es20.10,"|",es20.10,"|")') "-", v, v_exa, rel_diff
            end if
            write (unit, '(a)') ""
        else
            write (unit, '(a)') "PASS: `"//v_name//"`"
            write (unit, '(a)') ""
        end if
    end subroutine check_variable

    subroutine check_variables(v, v_exa, v_name, ids)
        implicit none
        real(real64), intent(in) :: v(:)
        real(real64), intent(in) :: v_exa(:)
        character(len=*), intent(in) :: v_name
        integer(int32), intent(in), optional :: ids(:)

        real(real64), parameter :: tol = 1.0d-8
        real(real64), allocatable :: rel_diff(:)
        integer(int32) :: i, n

        n = size(v)
        allocate (rel_diff(n))

        where (abs(v_exa) > 0.0d0)
            rel_diff = abs(v - v_exa) / abs(v_exa)
        elsewhere
            rel_diff = abs(v - v_exa)
        end where

        if (any(rel_diff > tol)) then
            write (unit, '(a)') "**FAIL**: `"//v_name//"`"
            write (unit, '(a)') ""
            write (unit, '("|",a6,"|",a20,"|",a20,"|",a20,"|")') "ID", "computed", "expected", "rel_diff"
            write (unit, '("|",a6,"|",a20,"|",a20,"|",a20,"|")') &
                repeat('-', 6), repeat('-', 20), repeat('-', 20), repeat('-', 20)
            do i = 1, n
                if (present(ids)) then
                    write (unit, '("|",i6,"|",es20.10,"|",es20.10,"|",es20.10,"|")') ids(i), v(i), v_exa(i), rel_diff(i)
                else
                    write (unit, '("|",i6,"|",es20.10,"|",es20.10,"|",es20.10,"|")') i, v(i), v_exa(i), rel_diff(i)
                end if
            end do
            write (unit, '(a)') ""
        else
            write (unit, '(a)') "PASS: `"//v_name//"`"
            write (unit, '(a)') ""
        end if
    end subroutine check_variables

end program test_physics
