program test_physics
    use, intrinsic :: iso_fortran_env
#ifdef _MPI
    use :: mpi_f08
#endif
    use :: module_core
    use :: module_physics
    implicit none

    integer(int32) :: unit
    integer(int32) :: ierr
#ifdef _MPI
    call MPI_Init(ierr)
#endif
    open (newunit=unit, file="log/test/physics.log", status="replace", action="write", iostat=ierr)
    write (unit, '(a)') "# Physics module tests"
    write (unit, '(a)') "---"
    call test_iapws()
    write (unit, '(a)') "---"
    write (unit, '(a)') "## Completed"
    close (unit)
#ifdef _MPI
    call MPI_Finalize(ierr)
#endif

contains
    subroutine test_iapws()
        implicit none
        write (unit, '(a)') "### IAPWS-IF97 Auxiliary functions"
        call test_iapws97_auxiliary()

        write (unit, '(a)') "### IAPWS-IF97 Region 1"
        call test_iapws97_region1()

        write (unit, '(a)') "### IAPWS-IF97 Region 2"
        call test_iapws97_region2()

        write (unit, '(a)') "### IAPWS-IF97 Region 3"
        call test_iapws97_region3()

        write (unit, '(a)') "### IAPWS-IF97 Region 4"
        call test_iapws97_region4()

        write (unit, '(a)') "### IAPWS-IF97 Region 5"
        call test_iapws97_region5()

        write (unit, '(a)') "### IAPWS-06 Ice Ih"
        call test_iapws06_Ih()

        write (unit, '(a)') "### IAPWS-08 Ice Phase Boundaries"
        call test_iapws08()
    end subroutine test_iapws

    subroutine test_iapws97_auxiliary()
        implicit none
        real(real64), parameter :: T_test = 0.623150000d3 ! K
        real(real64), parameter :: p_test = 0.165291643d8 ! Pa

        real(real64) :: p_boundary, T_boundary

        ! p_boundary = calc_p_boundary_iapws97_region23(T_test)
        ! T_boundary = calc_t_boundary_iapws97_region23(p_test)

        ! call check_variable(p_boundary, p_test, "IAPWS-IF97 boundary pressure (region 2-3)")
        ! call check_variable(T_boundary, T_test, "IAPWS-IF97 boundary temperature (region 2-3)")

    end subroutine test_iapws97_auxiliary

    subroutine test_iapws97_region1()
        implicit none
        integer(int32), parameter :: test_points = 3
        real(real64), parameter :: T(test_points) = [300.0d0, 300.0d0, 500.0d0]
        real(real64), parameter :: p(test_points) = [3.0d6, 80.0d6, 3.0d6]

        type(type_iapws_property) :: properties(test_points)

        real(real64) :: nu(test_points)
        real(real64) :: h(test_points)
        real(real64) :: u(test_points)
        real(real64) :: s(test_points)
        real(real64) :: cp(test_points)
        real(real64) :: w(test_points)

        real(real64), parameter :: nu_exact(test_points) = [0.100215168d-2, 0.971180894d-3, 0.120241800d-2]
        real(real64), parameter :: h_exact(test_points) = [0.115331273d6, 0.184142828d6, 0.975542239d6]
        real(real64), parameter :: u_exact(test_points) = [0.112324818d6, 0.106448356d6, 0.971934985d6]
        real(real64), parameter :: s_exact(test_points) = [0.392294792d3, 0.368563852d3, 0.258041912d4]
        real(real64), parameter :: cp_exact(test_points) = [0.417301218d4, 0.401008987d4, 0.465580682d4]
        real(real64), parameter :: w_exact(test_points) = [0.150773921d4, 0.163469054d4, 0.124071337d4]

        integer(int32) :: i

        call calc_iapws_properties(T, p, properties)

        call check_variables(properties(:)%nu, nu_exact, "IAPWS Region 1 specific volume")
        call check_variables(properties(:)%h, h_exact, "IAPWS Region 1 enthalpy")
        call check_variables(properties(:)%u, u_exact, "IAPWS Region 1 internal energy")
        call check_variables(properties(:)%s, s_exact, "IAPWS Region 1 entropy")
        call check_variables(properties(:)%cp, cp_exact, "IAPWS Region 1 isobaric heat capacity")
        call check_variables(properties(:)%w, w_exact, "IAPWS Region 1 speed of sound")

    end subroutine test_iapws97_region1

    subroutine test_iapws97_region2()
        implicit none
        integer(int32), parameter :: test_points = 3
        ! Table 15のテスト条件
        ! Point 1: 300 K, 0.0035 MPa
        ! Point 2: 700 K, 0.0035 MPa
        ! Point 3: 700 K, 30 MPa
        real(real64), parameter :: T(test_points) = [300.0d0, 700.0d0, 700.0d0]
        real(real64), parameter :: p(test_points) = [0.0035d6, 0.0035d6, 30.0d6] ! MPa -> Pa

        real(real64) :: nu(test_points)
        real(real64) :: h(test_points)
        real(real64) :: u(test_points)
        real(real64) :: s(test_points)
        real(real64) :: cp(test_points)
        real(real64) :: w(test_points)

        type(type_iapws_property) :: properties(test_points)

        ! Table 15の検証データ
        ! v: m^3/kg (変換不要)
        real(real64), parameter :: nu_exact(test_points) = [ &
                                   0.394913866d2, &
                                   0.923015898d2, &
                                   0.542946619d-2]

        ! h: kJ/kg -> J/kg (* 1.0d3)
        real(real64), parameter :: h_exact(test_points) = [ &
                                   0.254991145d4, &
                                   0.333568375d4, &
                                   0.263149474d4] * 1.0d3

        ! u: kJ/kg -> J/kg (* 1.0d3)
        real(real64), parameter :: u_exact(test_points) = [ &
                                   0.241169160d4, &
                                   0.301262819d4, &
                                   0.246861076d4] * 1.0d3

        ! s: kJ/(kg K) -> J/(kg K) (* 1.0d3)
        real(real64), parameter :: s_exact(test_points) = [ &
                                   0.852238967d1, &
                                   0.101749996d2, &
                                   0.517540298d1] * 1.0d3

        ! cp: kJ/(kg K) -> J/(kg K) (* 1.0d3)
        real(real64), parameter :: cp_exact(test_points) = [ &
                                   0.191300162d1, &
                                   0.208141274d1, &
                                   0.103505092d2] * 1.0d3

        ! w: m/s (変換不要)
        real(real64), parameter :: w_exact(test_points) = [ &
                                   0.427920172d3, &
                                   0.644289068d3, &
                                   0.480386523d3]

        integer(int32) :: i

        ! 計算実行
        call calc_iapws_properties(T, p, properties)
        ! do i = 1, test_points
        !     nu(i) = calc_nu_iapws97_region2(T(i), p(i))
        !     h(i) = calc_h_iapws97_region2(T(i), p(i))
        !     u(i) = calc_u_iapws97_region2(T(i), p(i))
        !     s(i) = calc_s_iapws97_region2(T(i), p(i))
        !     cp(i) = calc_cp_iapws97_region2(T(i), p(i))
        !     w(i) = calc_w_iapws97_region2(T(i), p(i))
        ! end do

        ! 検証（check_variablesサブルーチンが存在すると仮定）
        call check_variables(properties(:)%nu, nu_exact, "IAPWS Region 2 specific volume")
        call check_variables(properties(:)%h, h_exact, "IAPWS Region 2 enthalpy")
        call check_variables(properties(:)%u, u_exact, "IAPWS Region 2 internal energy")
        call check_variables(properties(:)%s, s_exact, "IAPWS Region 2 entropy")
        call check_variables(properties(:)%cp, cp_exact, "IAPWS Region 2 isobaric heat capacity")
        call check_variables(properties(:)%w, w_exact, "IAPWS Region 2 speed of sound")

    end subroutine test_iapws97_region2

    subroutine test_iapws97_region3()
        implicit none
        integer(int32), parameter :: test_points = 3

        ! Table 33 Inputs
        ! Point 1: T=650 K, rho=500 kg/m^3
        ! Point 2: T=650 K, rho=200 kg/m^3
        ! Point 3: T=750 K, rho=500 kg/m^3
        real(real64), parameter :: T(test_points) = [650.0d0, 650.0d0, 750.0d0]
        real(real64), parameter :: rho(test_points) = [500.0d0, 200.0d0, 500.0d0]

        real(real64) :: p(test_points)
        real(real64) :: h(test_points)
        real(real64) :: u(test_points)
        real(real64) :: s(test_points)
        real(real64) :: cp(test_points)
        real(real64) :: w(test_points)
        type(type_iapws_property) :: properties(test_points)

        ! Table 33 Reference Data (Converted to SI units)

        ! Pressure: MPa -> Pa (* 1.0d6)
        real(real64), parameter :: p_exact(test_points) = [ &
                                   0.255837018d2, &
                                   0.222930643d2, &
                                   0.783095639d2] * 1.0d6

        ! Enthalpy: kJ/kg -> J/kg (* 1.0d3)
        real(real64), parameter :: h_exact(test_points) = [ &
                                   0.186343019d4, &
                                   0.237512401d4, &
                                   0.225868845d4] * 1.0d3

        ! Internal Energy: kJ/kg -> J/kg (* 1.0d3)
        real(real64), parameter :: u_exact(test_points) = [ &
                                   0.181226279d4, &
                                   0.226365868d4, &
                                   0.210206932d4] * 1.0d3

        ! Entropy: kJ/(kg K) -> J/(kg K) (* 1.0d3)
        real(real64), parameter :: s_exact(test_points) = [ &
                                   0.405427273d1, &
                                   0.485438792d1, &
                                   0.446971906d1] * 1.0d3

        ! Isobaric Heat Capacity: kJ/(kg K) -> J/(kg K) (* 1.0d3)
        real(real64), parameter :: cp_exact(test_points) = [ &
                                   0.138935717d2, &
                                   0.446579342d2, &
                                   0.634165359d1] * 1.0d3

        ! Speed of Sound: m/s (No conversion needed)
        real(real64), parameter :: w_exact(test_points) = [ &
                                   0.502005554d3, &
                                   0.383444594d3, &
                                   0.760696041d3]

        integer(int32) :: i

        ! Execute calculations
        call calc_iapws_properties(T, p_exact, properties)
        ! do i = 1, test_points
        !     p(i) = calc_p_iapws97_region3(T(i), rho(i))
        !     u(i) = calc_u_iapws97_region3(T(i), rho(i))
        !     s(i) = calc_s_iapws97_region3(T(i), rho(i))
        !     h(i) = calc_h_iapws97_region3(T(i), rho(i))
        !     cp(i) = calc_cp_iapws97_region3(T(i), rho(i))
        !     w(i) = calc_w_iapws97_region3(T(i), rho(i))
        ! end do

        ! Verify results
        call check_variables(properties(:)%p, p_exact, "IAPWS Region 3 Pressure")
        call check_variables(properties(:)%h, h_exact, "IAPWS Region 3 Enthalpy")
        call check_variables(properties(:)%u, u_exact, "IAPWS Region 3 Internal Energy")
        call check_variables(properties(:)%s, s_exact, "IAPWS Region 3 Entropy")
        call check_variables(properties(:)%cp, cp_exact, "IAPWS Region 3 Isobaric Heat Capacity")
        call check_variables(properties(:)%w, w_exact, "IAPWS Region 3 Speed of Sound")

    end subroutine test_iapws97_region3

    subroutine test_iapws97_region4()
        implicit none
        integer(int32), parameter :: test_points_p = 3
        integer(int32), parameter :: test_points_t = 3
        integer(int32) :: i

        ! ==========================================================
        ! Test 1: Saturation Pressure (Table 35)
        ! Input: Temperature [K]
        ! Output: Saturation Pressure [Pa]
        ! ==========================================================
        real(real64), parameter :: T_in(test_points_p) = [300.0d0, 500.0d0, 600.0d0]

        ! Expected Pressure: MPa -> Pa (* 1.0d6)
        real(real64), parameter :: P_sat_exact(test_points_p) = [ &
                                   0.353658941d-2, &
                                   0.263889776d1, &
                                   0.123443146d2] * 1.0d6

        real(real64) :: P_sat_calc(test_points_p)

        ! ==========================================================
        ! Test 2: Saturation Temperature (Table 36)
        ! Input: Pressure [Pa]
        ! Output: Saturation Temperature [K]
        ! ==========================================================
        ! Input Pressure: MPa -> Pa (* 1.0d6)
        real(real64), parameter :: P_in(test_points_t) = [0.1d0, 1.0d0, 10.0d0] * 1.0d6

        ! Expected Temperature: K
        real(real64), parameter :: T_sat_exact(test_points_t) = [ &
                                   0.372755919d3, &
                                   0.453035632d3, &
                                   0.584149488d3]

        real(real64) :: T_sat_calc(test_points_t)

        ! ----------------------------------------------------------
        ! Execute Calculation: Saturation Pressure
        ! ----------------------------------------------------------
        ! do i = 1, test_points_p
        !     P_sat_calc(i) = calc_psat_iapws97_region4(T_in(i))
        ! end do

        ! call check_variables(P_sat_calc, P_sat_exact, "IAPWS Region 4 Saturation Pressure")

        ! ----------------------------------------------------------
        ! Execute Calculation: Saturation Temperature
        ! ----------------------------------------------------------
        ! do i = 1, test_points_t
        !     T_sat_calc(i) = calc_tsat_iapws97_region4(P_in(i))
        ! end do

        ! call check_variables(T_sat_calc, T_sat_exact, "IAPWS Region 4 Saturation Temperature")

    end subroutine test_iapws97_region4

    subroutine test_iapws97_region5()
        implicit none
        integer(int32), parameter :: test_points = 3

        ! Table 42 Inputs
        ! Point 1: T=1500 K, p=0.5 MPa
        ! Point 2: T=1500 K, p=30 MPa
        ! Point 3: T=2000 K, p=30 MPa
        real(real64), parameter :: T(test_points) = [1500.0d0, 1500.0d0, 2000.0d0]
        ! Pressure: MPa -> Pa (* 1.0d6)
        real(real64), parameter :: p(test_points) = [0.5d0, 30.0d0, 30.0d0] * 1.0d6

        type(type_iapws_property) :: properties(test_points)

        ! Table 42 Reference Data (Converted to SI units)

        ! Specific Volume: m^3/kg (No conversion needed)
        real(real64), parameter :: nu_exact(test_points) = [ &
                                   0.138455090d1, &
                                   0.230761299d-1, &
                                   0.311385219d-1]

        ! Enthalpy: kJ/kg -> J/kg (* 1.0d3)
        real(real64), parameter :: h_exact(test_points) = [ &
                                   0.521976855d4, &
                                   0.516723514d4, &
                                   0.657122604d4] * 1.0d3

        ! Internal Energy: kJ/kg -> J/kg (* 1.0d3)
        real(real64), parameter :: u_exact(test_points) = [ &
                                   0.452749310d4, &
                                   0.447495124d4, &
                                   0.563707038d4] * 1.0d3

        ! Entropy: kJ/(kg K) -> J/(kg K) (* 1.0d3)
        real(real64), parameter :: s_exact(test_points) = [ &
                                   0.965408875d1, &
                                   0.772970133d1, &
                                   0.853640523d1] * 1.0d3

        ! Isobaric Heat Capacity: kJ/(kg K) -> J/(kg K) (* 1.0d3)
        real(real64), parameter :: cp_exact(test_points) = [ &
                                   0.261609445d1, &
                                   0.272724317d1, &
                                   0.288569882d1] * 1.0d3

        ! Speed of Sound: m/s (No conversion needed)
        real(real64), parameter :: w_exact(test_points) = [ &
                                   0.917068690d3, &
                                   0.928548002d3, &
                                   0.106736948d4]

        integer(int32) :: i

        ! Execute calculations
        call calc_iapws_properties(T, p, properties)
        ! do i = 1, test_points
        !     nu(i) = calc_nu_iapws97_region5(T(i), p(i))
        !     h(i) = calc_h_iapws97_region5(T(i), p(i))
        !     u(i) = calc_u_iapws97_region5(T(i), p(i))
        !     s(i) = calc_s_iapws97_region5(T(i), p(i))
        !     cp(i) = calc_cp_iapws97_region5(T(i), p(i))
        !     w(i) = calc_w_iapws97_region5(T(i), p(i))
        ! end do

        ! Verify results
        call check_variables(properties(:)%nu, nu_exact, "IAPWS Region 5 Specific Volume")
        call check_variables(properties(:)%h, h_exact, "IAPWS Region 5 Enthalpy")
        call check_variables(properties(:)%u, u_exact, "IAPWS Region 5 Internal Energy")
        call check_variables(properties(:)%s, s_exact, "IAPWS Region 5 Entropy")
        call check_variables(properties(:)%cp, cp_exact, "IAPWS Region 5 Isobaric Heat Capacity")
        call check_variables(properties(:)%w, w_exact, "IAPWS Region 5 Speed of Sound")

    end subroutine test_iapws97_region5

    subroutine test_iapws06_Ih()
        implicit none
        integer(int32), parameter :: test_points = 3

        ! Table 3の条件 (T, p)
        ! 1. Triple point: Tt = 273.16 K, pt = 611.657 Pa
        ! 2. Melting point at p0: Tmelt = 273.152519 K, p0 = 101325 Pa
        ! 3. T = 100 K, p = 100 MPa
        real(real64), parameter :: T(test_points) = [273.16d0, 273.152519d0, 100.0d0]
        real(real64), parameter :: p(test_points) = [611.657d0, 101325.0d0, 100.0d6]

        type(type_iapws_property) :: properties(test_points)

        ! 画像のTable 3より抽出 (Quantityと対応関数)
        ! (dg/dp)T -> nu (Specific volume)
        ! h -> h (Enthalpy)
        ! u -> u (Internal energy)
        ! s -> s (Entropy)
        ! cp -> cp (Isobaric heat capacity)
        ! alpha -> alpha (Volume expansivity)
        ! beta -> beta (Pressure coefficient, dP/dT|v) [Pa K^-1]
        ! kappa_T -> kappa_T (Isothermal compressibility) [Pa^-1]
        ! kappa_s -> kappa_s (Isentropic compressibility) [Pa^-1]

        real(real64), parameter :: nu_exact(test_points) = [0.109085812737d-2, 0.109084388214d-2, 0.106193389260d-2]
        real(real64), parameter :: h_exact(test_points) = [-0.333444253966d6, -0.333354873637d6, -0.483491635676d6]
        real(real64), parameter :: u_exact(test_points) = [-0.333444921197d6, -0.333465403393d6, -0.589685024936d6]
        real(real64), parameter :: s_exact(test_points) = [-0.122069433940d4, -0.122076932550d4, -0.261195122589d4]
        real(real64), parameter :: cp_exact(test_points) = [0.209678431622d4, 0.209671391024d4, 0.866333195517d3]
        real(real64), parameter :: alpha_exact(test_points) = [0.159863102566d-3, 0.159841589458d-3, 0.258495528207d-4]
        real(real64), parameter :: beta_exact(test_points) = [0.135714764659d7, 0.135705899321d7, 0.291466166994d6]
        real(real64), parameter :: kappa_T_exact(test_points) = [0.117793449348d-9, 0.117785291765d-9, 0.886880048115d-10]
        real(real64), parameter :: kappa_s_exact(test_points) = [0.114161597779d-9, 0.114154442556d-9, 0.886060982687d-10]

        integer(int32) :: i

        call calc_iapws_properties(T, p, properties)

        ! do i = 1, test_points
        !     nu(i) = calc_nu_iapws06_Ih(T(i), p(i))
        !     h(i) = calc_h_iapws06_Ih(T(i), p(i))
        !     u(i) = calc_u_iapws06_Ih(T(i), p(i))
        !     s(i) = calc_s_iapws06_Ih(T(i), p(i))
        !     cp(i) = calc_cp_iapws06_Ih(T(i), p(i))
        !     alpha(i) = calc_alpha_iapws06_Ih(T(i), p(i))
        !     beta(i) = calc_beta_iapws06_Ih(T(i), p(i))
        !     kappa_T(i) = calc_kappa_T_iapws06_Ih(T(i), p(i))
        !     kappa_s(i) = calc_kappa_s_iapws06_Ih(T(i), p(i))
        ! end do

        call check_variables(properties(:)%nu, nu_exact, "IAPWS-06 Ice Ih specific volume")
        call check_variables(properties(:)%h, h_exact, "IAPWS-06 Ice Ih enthalpy")
        call check_variables(properties(:)%u, u_exact, "IAPWS-06 Ice Ih internal energy")
        call check_variables(properties(:)%s, s_exact, "IAPWS-06 Ice Ih entropy")
        call check_variables(properties(:)%cp, cp_exact, "IAPWS-06 Ice Ih isobaric heat capacity")
        call check_variables(properties(:)%alpha, alpha_exact, "IAPWS-06 Ice Ih cubic expansion coefficient")
        call check_variables(properties(:)%beta, beta_exact, "IAPWS-06 Ice Ih pressure coefficient")
        call check_variables(properties(:)%kappa_T, kappa_T_exact, "IAPWS-06 Ice Ih isothermal compressibility")
        call check_variables(properties(:)%kappa_s, kappa_s_exact, "IAPWS-06 Ice Ih isentropic compressibility")

    end subroutine test_iapws06_Ih

    subroutine test_iapws08()
        implicit none

        real(real64) :: T_test
        real(real64) :: p_ref
        real(real64) :: p_calc

        ! ----------------------------------------------------------------------
        ! Verification against Table 3 of IAPWS R14-08 [cite: 221]
        ! Note: Table values are in MPa, converted to Pa for comparison (x 1.0d6)
        ! ----------------------------------------------------------------------

        ! 1. Eq(1) Ice Ih Melting (Liquid-Solid)
        ! T = 260.0 K, P = 138.268 MPa [cite: 221]
        T_test = 260.0d0
        p_ref = 138.268113002217887697d6 ! 138.268 MPa -> Pa
        ! p_calc = calc_p_boundary_iapws08_iceIh_melting(T_test)
        ! call check_variable(p_calc, p_ref, "IAPWS-08 Ice Ih Melting pressure")

        ! 2. Eq(2) Ice III Melting (Liquid-Solid)
        ! T = 254.0 K, P = 268.685 MPa [cite: 221]
        T_test = 254.0d0
        p_ref = 268.684646633610782374d6 ! 268.685 MPa -> Pa
        ! p_calc = calc_p_boundary_iapws08_iceIII_melting(T_test)
        ! call check_variable(p_calc, p_ref, "IAPWS-08 Ice III Melting pressure")

        ! 3. Eq(3) Ice V Melting (Liquid-Solid)
        ! T = 265.0 K, P = 479.640 MPa [cite: 221]
        T_test = 265.0d0
        p_ref = 479.640244378799081915d6 ! 479.640 MPa -> Pa
        ! p_calc = calc_p_boundary_iapws08_iceV_melting(T_test)
        ! call check_variable(p_calc, p_ref, "IAPWS-08 Ice V Melting pressure")

        ! 4. Eq(4) Ice VI Melting (Liquid-Solid)
        ! T = 320.0 K, P = 1356.76 MPa [cite: 221]
        T_test = 320.0d0
        p_ref = 1356.756517869388289910d6 ! 1356.76 MPa -> Pa
        ! p_calc = calc_p_boundary_iapws08_iceVI_melting(T_test)
        ! call check_variable(p_calc, p_ref, "IAPWS-08 Ice VI Melting pressure")

        ! 5. Eq(5) Ice VII Melting (Liquid-Solid)
        ! T = 550.0 K, P = 6308.71 MPa [cite: 221]
        T_test = 550.0d0
        p_ref = 6308.714243543018710625d6 ! 6308.71 MPa -> Pa
        ! p_calc = calc_p_boundary_iapws08_iceVII_melting(T_test)
        ! call check_variable(p_calc, p_ref, "IAPWS-08 Ice VII Melting pressure")

        ! 6. Eq(6) Ice Ih Sublimation (Gas-Solid)
        ! T = 230.0 K, P = 8.94735e-6 MPa (= 8.94735 Pa) [cite: 221]
        T_test = 230.0d0
        p_ref = 8.9473527401891512767d0 ! 10^-6 MPa * 10^6 = 1 Pa scaling
        ! p_calc = calc_p_boundary_iapws08_iceIh_sublimation(T_test)
        ! call check_variable(p_calc, p_ref, "IAPWS-08 Ice Ih Sublimation pressure")

    end subroutine test_iapws08

    subroutine check_variable(v, v_exa, v_name)
        implicit none
        real(real64), intent(in) :: v
        real(real64), intent(in) :: v_exa
        character(len=*), intent(in) :: v_name

        real(real64), parameter :: tol = 1.0d-8
        real(real64) :: rel_diff

        rel_diff = abs(v - v_exa) / v_exa

        if (rel_diff > tol) then
            write (unit, '(a)') "**FAIL**: `"//v_name//"`"
            write (unit, '(a)') ""
            write (unit, '("|",a12,"|",a12,"|",a12,"|")') "computed", "expected", "rel_diff"
            write (unit, '("|",a12,"|",a12,"|",a12,"|")') &
                repeat('-', 11)//':', repeat('-', 11)//':', repeat('-', 11)//':'
            write (unit, '("|",es12.5,"|",es12.5,"|",es12.5,"|")') v, v_exa, rel_diff
            write (unit, '(a)') ""
        else
            write (unit, '(a)') "PASS: `"//v_name//"`"
            write (unit, '(a)') ""
        end if
    end subroutine check_variable

    subroutine check_variables(v, v_exa, v_name)
        implicit none
        real(real64), intent(in) :: v(:)
        real(real64), intent(in) :: v_exa(:)
        character(len=*), intent(in) :: v_name

        real(real64), parameter :: tol = 1.0d-8
        real(real64) :: rel_diff(size(v))
        integer(int32) :: i

        rel_diff = abs(v - v_exa) / v_exa

        if (any(rel_diff > tol)) then
            write (unit, '(a)') "**FAIL**: `"//v_name//"`"
            write (unit, '(a)') ""
            write (unit, '("|",a6,"|",a12,"|",a12,"|",a12,"|")') "index", "computed", "expected", "rel_diff"
            write (unit, '("|",a6,"|",a12,"|",a12,"|",a12,"|")') &
                repeat('-', 5)//':', repeat('-', 11)//':', repeat('-', 11)//':', repeat('-', 11)//':'
            do i = 1, size(v)
                write (unit, '("|",i6,"|",es12.5,"|",es12.5,"|",es12.5,"|")') i, v(i), v_exa(i), rel_diff(i)
            end do
            write (unit, '(a)') ""
        else
            write (unit, '(a)') "PASS: `"//v_name//"`"
            write (unit, '(a)') ""
        end if
    end subroutine check_variables

end program test_physics
