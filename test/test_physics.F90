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
        call test_auxiliary()

        write (unit, '(a)') "### IAPWS-IF97 Region 1"
        call test_iapws_region1()

        write (unit, '(a)') "### IAPWS-IF97 Region 2"
        call test_iapws_region2()

        write (unit, '(a)') "### IAPWS-IF97 Region 3"
        call test_iapws_region3()

        write (unit, '(a)') "### IAPWS-IF97 Region 4"
        call test_iapws_region4()

        write (unit, '(a)') "### IAPWS-IF97 Region 5"
        call test_iapws_region5()
    end subroutine test_iapws

    subroutine test_auxiliary()
        implicit none
        real(real64), parameter :: T_test = 0.623150000d3 ! K
        real(real64), parameter :: p_test = 0.165291643d8 ! Pa

        real(real64) :: p_boundary, T_boundary

        p_boundary = get_boundary_pressure_region23(T_test)
        T_boundary = get_boundary_temperature_region23(p_test)

        call check_variable(p_boundary, p_test, "IAPWS-IF97 boundary pressure (region 2-3)")
        call check_variable(T_boundary, T_test, "IAPWS-IF97 boundary temperature (region 2-3)")

    end subroutine test_auxiliary

    subroutine test_iapws_region1()
        implicit none
        integer(int32), parameter :: test_points = 3
        real(real64), parameter :: T(test_points) = [300.0d0, 300.0d0, 500.0d0]
        real(real64), parameter :: p(test_points) = [3.0d6, 80.0d6, 3.0d6]

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

        do i = 1, test_points
            nu(i) = get_nu_iapws97_region1(T(i), p(i))
            h(i) = get_h_iapws97_region1(T(i), p(i))
            u(i) = get_u_iapws97_region1(T(i), p(i))
            s(i) = get_s_iapws97_region1(T(i), p(i))
            cp(i) = get_cp_iapws97_region1(T(i), p(i))
            w(i) = get_w_iapws97_region1(T(i), p(i))
        end do

        call check_variables(nu, nu_exact, "IAPWS Region 1 specific volume")
        call check_variables(h, h_exact, "IAPWS Region 1 enthalpy")
        call check_variables(u, u_exact, "IAPWS Region 1 internal energy")
        call check_variables(s, s_exact, "IAPWS Region 1 entropy")
        call check_variables(cp, cp_exact, "IAPWS Region 1 isobaric heat capacity")
        call check_variables(w, w_exact, "IAPWS Region 1 speed of sound")

    end subroutine test_iapws_region1

    subroutine test_iapws_region2()
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
        do i = 1, test_points
            nu(i) = get_nu_iapws97_region2(T(i), p(i))
            h(i) = get_h_iapws97_region2(T(i), p(i))
            u(i) = get_u_iapws97_region2(T(i), p(i))
            s(i) = get_s_iapws97_region2(T(i), p(i))
            cp(i) = get_cp_iapws97_region2(T(i), p(i))
            w(i) = get_w_iapws97_region2(T(i), p(i))
        end do

        ! 検証（check_variablesサブルーチンが存在すると仮定）
        call check_variables(nu, nu_exact, "IAPWS Region 2 specific volume")
        call check_variables(h, h_exact, "IAPWS Region 2 enthalpy")
        call check_variables(u, u_exact, "IAPWS Region 2 internal energy")
        call check_variables(s, s_exact, "IAPWS Region 2 entropy")
        call check_variables(cp, cp_exact, "IAPWS Region 2 isobaric heat capacity")
        call check_variables(w, w_exact, "IAPWS Region 2 speed of sound")

    end subroutine test_iapws_region2

    subroutine test_iapws_region3()
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
        do i = 1, test_points
            p(i) = get_p_iapws97_region3(T(i), rho(i))
            u(i) = get_u_iapws97_region3(T(i), rho(i))
            s(i) = get_s_iapws97_region3(T(i), rho(i))
            h(i) = get_h_iapws97_region3(T(i), rho(i))
            cp(i) = get_cp_iapws97_region3(T(i), rho(i))
            w(i) = get_w_iapws97_region3(T(i), rho(i))
        end do

        ! Verify results
        call check_variables(p, p_exact, "IAPWS Region 3 Pressure")
        call check_variables(h, h_exact, "IAPWS Region 3 Enthalpy")
        call check_variables(u, u_exact, "IAPWS Region 3 Internal Energy")
        call check_variables(s, s_exact, "IAPWS Region 3 Entropy")
        call check_variables(cp, cp_exact, "IAPWS Region 3 Isobaric Heat Capacity")
        call check_variables(w, w_exact, "IAPWS Region 3 Speed of Sound")

    end subroutine test_iapws_region3

    subroutine test_iapws_region4()
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
        do i = 1, test_points_p
            P_sat_calc(i) = get_sat_pressure_region4(T_in(i))
        end do

        call check_variables(P_sat_calc, P_sat_exact, "IAPWS Region 4 Saturation Pressure")

        ! ----------------------------------------------------------
        ! Execute Calculation: Saturation Temperature
        ! ----------------------------------------------------------
        do i = 1, test_points_t
            T_sat_calc(i) = get_sat_temperature_region4(P_in(i))
        end do

        call check_variables(T_sat_calc, T_sat_exact, "IAPWS Region 4 Saturation Temperature")

    end subroutine test_iapws_region4

    subroutine test_iapws_region5()
        implicit none
        integer(int32), parameter :: test_points = 3

        ! Table 42 Inputs
        ! Point 1: T=1500 K, p=0.5 MPa
        ! Point 2: T=1500 K, p=30 MPa
        ! Point 3: T=2000 K, p=30 MPa
        real(real64), parameter :: T(test_points) = [1500.0d0, 1500.0d0, 2000.0d0]
        ! Pressure: MPa -> Pa (* 1.0d6)
        real(real64), parameter :: p(test_points) = [0.5d0, 30.0d0, 30.0d0] * 1.0d6

        real(real64) :: nu(test_points)
        real(real64) :: h(test_points)
        real(real64) :: u(test_points)
        real(real64) :: s(test_points)
        real(real64) :: cp(test_points)
        real(real64) :: w(test_points)

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
        do i = 1, test_points
            nu(i) = get_nu_iapws97_region5(T(i), p(i))
            h(i) = get_h_iapws97_region5(T(i), p(i))
            u(i) = get_u_iapws97_region5(T(i), p(i))
            s(i) = get_s_iapws97_region5(T(i), p(i))
            cp(i) = get_cp_iapws97_region5(T(i), p(i))
            w(i) = get_w_iapws97_region5(T(i), p(i))
        end do

        ! Verify results
        call check_variables(nu, nu_exact, "IAPWS Region 5 Specific Volume")
        call check_variables(h, h_exact, "IAPWS Region 5 Enthalpy")
        call check_variables(u, u_exact, "IAPWS Region 5 Internal Energy")
        call check_variables(s, s_exact, "IAPWS Region 5 Entropy")
        call check_variables(cp, cp_exact, "IAPWS Region 5 Isobaric Heat Capacity")
        call check_variables(w, w_exact, "IAPWS Region 5 Speed of Sound")

    end subroutine test_iapws_region5

    subroutine check_variable(v, v_exa, v_name)
        implicit none
        real(real64), intent(in) :: v
        real(real64), intent(in) :: v_exa
        character(len=*), intent(in) :: v_name

        real(real64), parameter :: tol = 1.0d-8
        real(real64) :: diff

        diff = abs(v - v_exa) / v_exa

        if (diff > tol) then
            write (unit, '(a)') "**FAIL**: `"//v_name//"`"
            write (unit, '(a)') ""
            write (unit, '(a)') "| value | exact | rel_diff |"
            write (unit, '(a)') "|------:|------:|----------:|"
            write (unit, '("|",i6,"|",es12.4,"|",es12.4,"|",es12.4,"|")') v, v_exa, diff
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
        integer(int32) :: i

        if (any(abs(v - v_exa) / v_exa > tol)) then
            write (unit, '(a)') "**FAIL**: `"//v_name//"`"
            write (unit, '(a)') ""
            write (unit, '(a)') "| index | computed | expected | diff |"
            write (unit, '(a)') "|------:|---------:|---------:|------:|"

            do i = 1, size(v)
                write (unit, '("|",i6,"|",es12.4,"|",es12.4,"|",es12.4,"|")') i, v(i), v_exa(i), v(i) - v_exa(i)
            end do
            write (unit, '(a)') ""
        else
            write (unit, '(a)') "PASS: `"//v_name//"`"
            write (unit, '(a)') ""
        end if
    end subroutine check_variables

end program test_physics
