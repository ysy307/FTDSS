program test_physics
    use, intrinsic :: iso_fortran_env
    use :: stdlib_io, only:loadtxt
    use :: iapws, only:type_iapws06, type_iapws_property
#ifdef _MPI
    use :: mpi_f08
#endif
    use :: module_core
    ! use :: module_physics
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

        call test_iapws()

        write (unit, '(a)') "---"
        write (unit, '(a)') "## Completed"
        close (unit)
    end if

#ifdef _MPI
    call MPI_Finalize(ierr)
#endif

contains

    subroutine test_iapws()
        implicit none
        ! write (unit, '(a)') "### IAPWS-IF97 Auxiliary functions"
        ! call test_iapws97_auxiliary()

        ! write (unit, '(a)') "### IAPWS-IF97 Region 1"
        ! call test_iapws97_region1()

        ! write (unit, '(a)') "### IAPWS-IF97 Region 2"
        ! call test_iapws97_region2()

        ! write (unit, '(a)') "### IAPWS-IF97 Region 3"
        ! call test_iapws97_region3()

        ! write (unit, '(a)') "### IAPWS-IF97 Region 4"
        ! call test_iapws97_region4()

        ! write (unit, '(a)') "### IAPWS-IF97 Region 5"
        ! call test_iapws97_region5()

        write (unit, '(a)') "### IAPWS-06 Ice Ih"
        call test_iapws06_Ih()

        ! write (unit, '(a)') "### IAPWS-08 Ice Phase Boundaries"
        ! call test_iapws08()
    end subroutine test_iapws

    subroutine test_iapws97_auxiliary()
        implicit none
        real(real64), parameter :: T_test = 0.623150000d3 ! K
        real(real64), parameter :: p_test = 0.165291643d8 ! Pa
        real(real64) :: p_boundary, T_boundary

        ! 必要に応じて実装
        ! p_boundary = calc_p_boundary_iapws97_region23(T_test)
        ! T_boundary = calc_t_boundary_iapws97_region23(p_test)
    end subroutine test_iapws97_auxiliary

    ! ! ======================================================================
    ! ! Region 1
    ! ! ======================================================================
    ! subroutine test_iapws97_region1()
    !     implicit none
    !     real(real64), allocatable :: data_table(:, :)
    !     integer :: n_points
    !     real(real64), allocatable :: T(:), p(:)
    !     real(real64), allocatable :: nu_exact(:), h_exact(:), u_exact(:)
    !     real(real64), allocatable :: s_exact(:), cp_exact(:), w_exact(:)
    !     type(type_iapws_property), allocatable :: properties(:)

    !     ! ヘッダーなし、転置データ（行＝物理量、列＝テスト点）として読み込む
    !     call loadtxt("/workspaces/FTDSS/test/data/physics/iapws97_r1.dat", data_table, fmt="*")

    !     n_points = size(data_table, 2) ! 列数がテスト点の数

    !     ! 行ごとに物理量が並んでいるので、(行番号, :) でスライス
    !     T = data_table(1, :)
    !     p = data_table(2, :)
    !     nu_exact = data_table(3, :)
    !     h_exact = data_table(4, :)
    !     u_exact = data_table(5, :)
    !     s_exact = data_table(6, :)
    !     cp_exact = data_table(7, :)
    !     w_exact = data_table(8, :)

    !     allocate (properties(n_points))
    !     call calc_iapws_properties(T, p, properties, IAPWS97_R1_LIQ)

    !     call check_variables(properties(:)%nu, nu_exact, &
    !                          "IAPWS Region 1 specific volume", properties(:)%region_id)
    !     call check_variables(properties(:)%h, h_exact, &
    !                          "IAPWS Region 1 enthalpy", properties(:)%region_id)
    !     call check_variables(properties(:)%u, u_exact, &
    !                          "IAPWS Region 1 internal energy", properties(:)%region_id)
    !     call check_variables(properties(:)%s, s_exact, &
    !                          "IAPWS Region 1 entropy", properties(:)%region_id)
    !     call check_variables(properties(:)%cp, cp_exact, &
    !                          "IAPWS Region 1 isobaric heat capacity", properties(:)%region_id)
    !     call check_variables(properties(:)%w, w_exact, &
    !                          "IAPWS Region 1 speed of sound", properties(:)%region_id)
    ! end subroutine test_iapws97_region1

    ! ! ======================================================================
    ! ! Region 2
    ! ! ======================================================================
    ! subroutine test_iapws97_region2()
    !     implicit none
    !     real(real64), allocatable :: data_table(:, :)
    !     integer :: n_points
    !     real(real64), allocatable :: T(:), p(:)
    !     real(real64), allocatable :: nu_exact(:), h_exact(:), u_exact(:)
    !     real(real64), allocatable :: s_exact(:), cp_exact(:), w_exact(:)
    !     type(type_iapws_property), allocatable :: properties(:)

    !     call loadtxt("/workspaces/FTDSS/test/data/physics/iapws97_r2.dat", data_table, fmt="*")
    !     n_points = size(data_table, 2)

    !     T = data_table(1, :)
    !     p = data_table(2, :)
    !     nu_exact = data_table(3, :)
    !     h_exact = data_table(4, :)
    !     u_exact = data_table(5, :)
    !     s_exact = data_table(6, :)
    !     cp_exact = data_table(7, :)
    !     w_exact = data_table(8, :)

    !     allocate (properties(n_points))
    !     call calc_iapws_properties(T, p, properties, IAPWS97_R2_VAP)

    !     call check_variables(properties(:)%nu, nu_exact, &
    !                          "IAPWS Region 2 specific volume", properties(:)%region_id)
    !     call check_variables(properties(:)%h, h_exact, &
    !                          "IAPWS Region 2 enthalpy", properties(:)%region_id)
    !     call check_variables(properties(:)%u, u_exact, &
    !                          "IAPWS Region 2 internal energy", properties(:)%region_id)
    !     call check_variables(properties(:)%s, s_exact, &
    !                          "IAPWS Region 2 entropy", properties(:)%region_id)
    !     call check_variables(properties(:)%cp, cp_exact, &
    !                          "IAPWS Region 2 isobaric heat capacity", properties(:)%region_id)
    !     call check_variables(properties(:)%w, w_exact, &
    !                          "IAPWS Region 2 speed of sound", properties(:)%region_id)
    ! end subroutine test_iapws97_region2

    ! ! ======================================================================
    ! ! Region 3
    ! ! ======================================================================
    ! subroutine test_iapws97_region3()
    !     implicit none
    !     real(real64), allocatable :: data_table(:, :)
    !     integer :: n_points
    !     real(real64), allocatable :: T(:), rho(:)
    !     real(real64), allocatable :: p_exact(:), h_exact(:), u_exact(:)
    !     real(real64), allocatable :: s_exact(:), cp_exact(:), w_exact(:)
    !     type(type_iapws_property), allocatable :: properties(:)

    !     call loadtxt("/workspaces/FTDSS/test/data/physics/iapws97_r3.dat", data_table, fmt="*")
    !     n_points = size(data_table, 2)

    !     T = data_table(1, :)
    !     rho = data_table(2, :)
    !     p_exact = data_table(3, :)
    !     h_exact = data_table(4, :)
    !     u_exact = data_table(5, :)
    !     s_exact = data_table(6, :)
    !     cp_exact = data_table(7, :)
    !     w_exact = data_table(8, :)

    !     allocate (properties(n_points))
    !     call calc_iapws_properties(T, p_exact, properties, IAPWS97_R3_CRIT)

    !     call check_variables(properties(:)%p, p_exact, &
    !                          "IAPWS Region 3 Pressure", properties(:)%region_id)
    !     call check_variables(properties(:)%h, h_exact, &
    !                          "IAPWS Region 3 Enthalpy", properties(:)%region_id)
    !     call check_variables(properties(:)%u, u_exact, &
    !                          "IAPWS Region 3 Internal Energy", properties(:)%region_id)
    !     call check_variables(properties(:)%s, s_exact, &
    !                          "IAPWS Region 3 Entropy", properties(:)%region_id)
    !     call check_variables(properties(:)%cp, cp_exact, &
    !                          "IAPWS Region 3 Isobaric Heat Capacity", properties(:)%region_id)
    !     call check_variables(properties(:)%w, w_exact, &
    !                          "IAPWS Region 3 Speed of Sound", properties(:)%region_id)
    ! end subroutine test_iapws97_region3

    ! ! ======================================================================
    ! ! Region 4
    ! ! ======================================================================
    ! subroutine test_iapws97_region4()
    !     implicit none
    !     real(real64), allocatable :: data_table(:, :)
    !     integer :: n_points, i
    !     real(real64), allocatable :: T_in(:), P_sat_exact(:), P_sat_calc(:)
    !     real(real64), allocatable :: P_in(:), T_sat_exact(:), T_sat_calc(:)

    !     call loadtxt("/workspaces/FTDSS/test/data/physics/iapws97_r4.dat", data_table, fmt="*")
    !     n_points = size(data_table, 2)

    !     ! 1-2行目を Test 1用、3-4行目を Test 2用とみなす
    !     T_in = data_table(1, :)
    !     P_sat_exact = data_table(2, :)
    !     P_in = data_table(3, :)
    !     T_sat_exact = data_table(4, :)

    !     allocate (P_sat_calc(n_points))
    !     allocate (T_sat_calc(n_points))

    !     ! Test 1: Saturation Pressure (T -> P)
    !     do i = 1, n_points
    !         P_sat_calc(i) = calc_psat_iapws97_region4(T_in(i))
    !     end do
    !     call check_variables(P_sat_calc, P_sat_exact, &
    !                          "IAPWS Region 4 Saturation Pressure")

    !     ! Test 2: Saturation Temperature (P -> T)
    !     do i = 1, n_points
    !         T_sat_calc(i) = calc_tsat_iapws97_region4(P_in(i))
    !     end do
    !     call check_variables(T_sat_calc, T_sat_exact, &
    !                          "IAPWS Region 4 Saturation Temperature")
    ! end subroutine test_iapws97_region4

    ! ! ======================================================================
    ! ! Region 5
    ! ! ======================================================================
    ! subroutine test_iapws97_region5()
    !     implicit none
    !     real(real64), allocatable :: data_table(:, :)
    !     integer :: n_points
    !     real(real64), allocatable :: T(:), p(:)
    !     real(real64), allocatable :: nu_exact(:), h_exact(:), u_exact(:)
    !     real(real64), allocatable :: s_exact(:), cp_exact(:), w_exact(:)
    !     type(type_iapws_property), allocatable :: properties(:)

    !     call loadtxt("/workspaces/FTDSS/test/data/physics/iapws97_r5.dat", data_table, fmt="*")
    !     n_points = size(data_table, 2)

    !     T = data_table(1, :)
    !     p = data_table(2, :)
    !     nu_exact = data_table(3, :)
    !     h_exact = data_table(4, :)
    !     u_exact = data_table(5, :)
    !     s_exact = data_table(6, :)
    !     cp_exact = data_table(7, :)
    !     w_exact = data_table(8, :)

    !     allocate (properties(n_points))
    !     call calc_iapws_properties(T, p, properties, IAPWS97_R5_GAS)

    !     call check_variables(properties(:)%nu, nu_exact, &
    !                          "IAPWS Region 5 Specific Volume", properties(:)%region_id)
    !     call check_variables(properties(:)%h, h_exact, &
    !                          "IAPWS Region 5 Enthalpy", properties(:)%region_id)
    !     call check_variables(properties(:)%u, u_exact, &
    !                          "IAPWS Region 5 Internal Energy", properties(:)%region_id)
    !     call check_variables(properties(:)%s, s_exact, &
    !                          "IAPWS Region 5 Entropy", properties(:)%region_id)
    !     call check_variables(properties(:)%cp, cp_exact, &
    !                          "IAPWS Region 5 Isobaric Heat Capacity", properties(:)%region_id)
    !     call check_variables(properties(:)%w, w_exact, &
    !                          "IAPWS Region 5 Speed of Sound", properties(:)%region_id)
    ! end subroutine test_iapws97_region5

    ! ======================================================================
    ! Ice Ih
    ! ======================================================================
    subroutine test_iapws06_Ih()
        implicit none
        real(real64), allocatable :: data_table(:, :)
        integer :: n_points
        real(real64), allocatable :: T(:), p(:)
        real(real64), allocatable :: nu_exact(:), h_exact(:), u_exact(:)
        real(real64), allocatable :: s_exact(:), cp_exact(:)
        real(real64), allocatable :: alpha_exact(:), beta_exact(:)
        real(real64), allocatable :: kappa_T_exact(:), kappa_s_exact(:)
        type(type_iapws_property), allocatable :: properties(:)
        type(type_iapws06) :: Ice

        call Ice%initialize()

        call loadtxt("/workspaces/FTDSS/test/data/physics/iapws06_Ih.dat", data_table, fmt="*")
        n_points = size(data_table, 2)

        T = data_table(1, :)
        p = data_table(2, :)
        nu_exact = data_table(3, :)
        h_exact = data_table(4, :)
        u_exact = data_table(5, :)
        s_exact = data_table(6, :)
        cp_exact = data_table(7, :)
        alpha_exact = data_table(8, :)
        beta_exact = data_table(9, :)
        kappa_T_exact = data_table(10, :)
        kappa_s_exact = data_table(11, :)

        allocate (properties(n_points))
        print *, T, P
        call Ice%calc_properties(T, p, properties)
        print *, properties(1)%nu
        print *, properties(1)%rho
        print *, properties(1)%drho_dT
        print *, properties(1)%drho_dP
        print *, properties(1)%u
        print *, properties(1)%h
        print *, properties(1)%s
        print *, properties(1)%cp
        print *, properties(1)%cv
        print *, properties(1)%w
        print *, properties(1)%alpha
        print *, properties(1)%beta
        print *, properties(1)%kappa_T
        print *, properties(1)%kappa_s

        call check_variables(properties(:)%nu, nu_exact, "IAPWS-06 Ice Ih specific volume", properties(:)%region_id)
        call check_variables(properties(:)%h, h_exact, "IAPWS-06 Ice Ih enthalpy", properties(:)%region_id)
        call check_variables(properties(:)%u, u_exact, "IAPWS-06 Ice Ih internal energy", properties(:)%region_id)
        call check_variables(properties(:)%s, s_exact, "IAPWS-06 Ice Ih entropy", properties(:)%region_id)
        call check_variables(properties(:)%cp, cp_exact, "IAPWS-06 Ice Ih isobaric heat capacity", properties(:)%region_id)
        call check_variables(properties(:)%alpha, alpha_exact, "IAPWS-06 Ice Ih cubic expansion coefficient", properties(:)%region_id)
        call check_variables(properties(:)%beta, beta_exact, "IAPWS-06 Ice Ih pressure coefficient", properties(:)%region_id)
        call check_variables(properties(:)%kappa_T, kappa_T_exact, "IAPWS-06 Ice Ih isothermal compressibility", properties(:)%region_id)
        call check_variables(properties(:)%kappa_s, kappa_s_exact, "IAPWS-06 Ice Ih isentropic compressibility", properties(:)%region_id)
    end subroutine test_iapws06_Ih

    ! ! ======================================================================
    ! ! IAPWS-08 (Phase Boundaries)
    ! ! ======================================================================
    ! subroutine test_iapws08()
    !     implicit none
    !     real(real64), allocatable :: data_table(:, :)
    !     integer :: n_points, i, id
    !     real(real64) :: T_test, p_ref, p_calc
    !     character(len=64) :: test_name

    !     call loadtxt("/workspaces/FTDSS/test/data/physics/iapws08.dat", data_table, fmt="*")
    !     n_points = size(data_table, 2)

    !     do i = 1, n_points
    !         ! data_table(行, 列) なので i列目を取得
    !         T_test = data_table(1, i)
    !         p_ref = data_table(2, i)

    !         select case (i)
    !         case (1)
    !             p_calc = calc_p_boundary_iapws08_iceIh_melting(T_test)
    !             test_name = "IAPWS-08 Eq(1) Ice Ih Melting"
    !         case (2)
    !             p_calc = calc_p_boundary_iapws08_iceIII_melting(T_test)
    !             test_name = "IAPWS-08 Eq(2) Ice III Melting"
    !         case (3)
    !             p_calc = calc_p_boundary_iapws08_iceV_melting(T_test)
    !             test_name = "IAPWS-08 Eq(3) Ice V Melting"
    !         case (4)
    !             p_calc = calc_p_boundary_iapws08_iceVI_melting(T_test)
    !             test_name = "IAPWS-08 Eq(4) Ice VI Melting"
    !         case (5)
    !             p_calc = calc_p_boundary_iapws08_iceVII_melting(T_test)
    !             test_name = "IAPWS-08 Eq(5) Ice VII Melting"
    !         case (6)
    !             p_calc = calc_p_boundary_iapws08_iceIh_sublimation(T_test)
    !             test_name = "IAPWS-08 Eq(6) Ice Ih Sublimation"
    !         case default
    !             test_name = "Unknown ID"
    !             p_calc = 0.0d0
    !         end select

    !         call check_variable(p_calc, p_ref, trim(test_name), i)
    !     end do
    ! end subroutine test_iapws08

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
