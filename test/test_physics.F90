program test_physics
    use, intrinsic :: iso_fortran_env
    use :: stdlib_io, only:loadtxt
    use :: iapws, only:type_iapws06, type_iapws97
#ifdef _MPI
    use :: mpi_f08
#endif
    use :: module_core
    use :: module_physics, g => gravity_acceleration, rho_std => reference_water_density
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
        call test_wrf()
        call test_hcf()
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
        state%pressure = 0.0d0 ! [Pa]
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
        state%pressure = 0.0d0 ! [Pa]
        state%porosity = 0.4d0
        state%water_content = 0.2d0
        state%ice_content = 0.1d0
        state%relative_humidity = 0.6d0
        call specific_heat%p%calc(state, computed_specific_heat)
        expected_specific_heat = 1.725720228875d3
        call check_variable(computed_specific_heat, expected_specific_heat, "Specific Heat Test")

    end subroutine test_specific_heat

    subroutine test_wrf()
        implicit none
        ! 6つのモデルを保持する配列
        type(holder_wrfs) :: wrf_models(6)
        type(type_wrf_params) :: params(6)

        integer(int32) :: i, j, n_steps
        integer(int32) :: file_unit

        real(real64) :: h_cm, h_pa ! 入力(cm)と計算用(Pa)を分離
        real(real64) :: theta_vals(6)
        real(real64) :: h_min, h_max, dh
        real(real64) :: conv_factor ! 単位変換係数

        !-------------------------------------------------
        ! 0. 変換係数の計算 (cm -> Pa)
        !-------------------------------------------------
        ! 親モジュールで定義されている g (重力加速度) と rho_std (水の密度) を使用
        ! 以前のコードの「9.80655d1」のような定数打ち込みによる誤差を排除
        conv_factor = (rho_std * g) / 100.0d0

        !-------------------------------------------------
        ! 1. 各モデルのパラメータ設定
        !-------------------------------------------------
        ! 共通設定
        do j = 1, 6
            call params(j)%reset()
            params(j)%unit_id = PHYSICS_UNIT_CM
            params(j)%theta_s = 0.5d0
            params(j)%theta_r = 0.1d0
        end do

        ! --- (1) Brooks-Corey (BC) ---
        params(1)%model_number = 1 ! WRF_BC
        params(1)%alpha1 = -10.0d0
        params(1)%n1 = 2.0d0

        ! --- (2) Van Genuchten (VG) ---
        params(2)%model_number = 2 ! WRF_VG
        params(2)%alpha1 = 0.01d0
        params(2)%n1 = 2.0d0
        params(2)%m1 = 0.5d0

        ! --- (3) Kosugi (KO) ---
        params(3)%model_number = 3 ! WRF_KO
        params(3)%alpha1 = -100.0d0
        params(3)%n1 = 1.0d0

        ! --- (4) Modified VG (MVG) ---
        params(4)%model_number = 4 ! WRF_MVG
        params(4)%alpha1 = 0.01d0
        params(4)%n1 = 2.0d0
        params(4)%m1 = 0.5d0
        params(4)%h_crit = -5.0d0

        ! --- (5) Durner (Dual VG) ---
        params(5)%model_number = 5 ! WRF_DURNER
        params(5)%w1 = 0.4d0
        params(5)%alpha1 = 0.01d0
        params(5)%n1 = 2.0d0
        params(5)%m1 = 0.5d0
        params(5)%w2 = 0.6d0
        params(5)%alpha2 = 0.05d0
        params(5)%n2 = 3.0d0
        params(5)%m2 = 1.0d0 - 1.0d0 / 3.0d0

        ! --- (6) Dual VG Common Alpha (DVGCH) ---
        params(6)%model_number = 6 ! WRF_DVGCH
        params(6)%w1 = 0.4d0
        params(6)%alpha1 = 0.01d0
        params(6)%n1 = 2.0d0
        params(6)%m1 = 0.5d0
        params(6)%w2 = 0.6d0
        params(6)%n2 = 3.0d0
        params(6)%m2 = 1.0d0 - 1.0d0 / 3.0d0

        !-------------------------------------------------
        ! 2. 初期化
        !-------------------------------------------------
        do j = 1, 6
            call wrf_models(j)%initialize(material_id=1, params=params(j))
        end do

        !-------------------------------------------------
        ! 3. 計算ループとCSV出力
        !-------------------------------------------------
        open (newunit=file_unit, file='log/test/wrf.csv', status='replace', action='write')

        ! ヘッダー出力
        write (file_unit, '(A)') 'h,theta_bc,theta_vg,theta_ko,theta_mvg,theta_durner,theta_dvgch'

        h_min = -1000.0d0
        h_max = 0.0d0
        n_steps = 100
        dh = (h_max - h_min) / real(n_steps, real64)

        do i = 0, n_steps
            ! h_cm: CSV出力用の水頭 (cm)
            h_cm = h_min + real(i, real64) * dh

            ! h_pa: 計算モデル入力用の圧力 (Pa)
            ! 内部パラメータと同じ係数を使って変換することで整合性を保証
            h_pa = h_cm * conv_factor

            ! 全モデル計算 (引数は Pa)
            do j = 1, 6
                call wrf_models(j)%p%calc(h_pa, theta_vals(j))
            end do

            ! 出力
            ! h_cm をそのまま出し、theta は倍精度(ES24.16E3)で出力して桁落ちを防ぐ
            write (file_unit, '(ES24.16E3, 6(",", ES24.16E3))') h_cm, theta_vals(1:6)
        end do

        close (file_unit)

    end subroutine test_wrf
    subroutine test_hcf()
        implicit none

        ! 6つのモデルを保持する配列
        type(holder_hcfs) :: hcf_models(6)
        type(type_hcf_params) :: params(6)
        type(type_state) :: state
        type(type_iapws97) :: water

        integer(int32) :: i, j, n_steps
        integer(int32) :: file_unit

        real(real64) :: h_cm, h_pa ! 入力(cm)と計算用(Pa)を分離
        real(real64) :: kr_vals(6)
        real(real64) :: h_min, h_max, dh
        real(real64) :: conv_factor ! 単位変換係数

        call water%initialize()

        !-------------------------------------------------
        ! 0. 変換係数の計算 (cm -> Pa)
        !-------------------------------------------------
        ! 親モジュール等で定義されている g (重力加速度) と rho_std (水の密度) を使用
        conv_factor = (rho_std * g) / 100.0d0

        !-------------------------------------------------
        ! 1. 各モデルのパラメータ設定
        !-------------------------------------------------
        ! 共通設定 (HCF_BASE type)
        do j = 1, 6
            call params(j)%reset()
            params(j)%unit_id = PHYSICS_UNIT_CM
            params(j)%model_number = 1 ! 1 = HCF_BASE
            params(j)%water_viscosity_model = 1 ! 1 = NONE (or default)
            params(j)%k_s = 1.0d0 ! Ks = 1.0
            params(j)%l = 0.5d0 ! Mualem parameter
            ! デフォルトのtheta (MVG用)
            params(j)%theta_s = 0.5d0
            params(j)%theta_r = 0.1d0
        end do

        ! --- (1) Brooks-Corey (BC) ---
        params(1)%hcf_model_number = 1
        params(1)%alpha1 = -10.0d0
        params(1)%n1 = 2.0d0

        ! --- (2) Van Genuchten (VG) ---
        params(2)%hcf_model_number = 2
        params(2)%alpha1 = 0.01d0
        params(2)%n1 = 2.0d0
        params(2)%m1 = 0.5d0

        ! --- (3) Kosugi (KO) ---
        params(3)%hcf_model_number = 3
        params(3)%alpha1 = -100.0d0
        params(3)%n1 = 1.0d0

        ! --- (4) Modified VG (MVG) ---
        params(4)%hcf_model_number = 4
        params(4)%alpha1 = 0.01d0
        params(4)%n1 = 2.0d0
        params(4)%m1 = 0.5d0
        params(4)%h_crit = -5.0d0

        ! --- (5) Durner (Dual VG) ---
        params(5)%hcf_model_number = 5
        params(5)%w1 = 0.4d0
        params(5)%alpha1 = 0.01d0
        params(5)%n1 = 2.0d0
        params(5)%m1 = 0.5d0
        params(5)%w2 = 0.6d0
        params(5)%alpha2 = 0.05d0
        params(5)%n2 = 3.0d0
        params(5)%m2 = 1.0d0 - 1.0d0 / 3.0d0

        ! --- (6) Dual VG Common Alpha (DVGCH) ---
        params(6)%hcf_model_number = 6
        params(6)%w1 = 0.4d0
        params(6)%alpha1 = 0.01d0 ! 共通alpha
        params(6)%n1 = 2.0d0
        params(6)%m1 = 0.5d0
        params(6)%w2 = 0.6d0
        params(6)%n2 = 3.0d0
        params(6)%m2 = 1.0d0 - 1.0d0 / 3.0d0

        !-------------------------------------------------
        ! 2. 初期化
        !-------------------------------------------------
        do j = 1, 6
            call hcf_models(j)%initialize(material_id=1, params=params(j), water=water)
        end do

        !-------------------------------------------------
        ! 3. 計算ループとCSV出力
        !-------------------------------------------------
        open (newunit=file_unit, file='log/test/hcf.csv', status='replace', action='write')

        ! ヘッダー出力 (7列)
        write (file_unit, '(A)') 'h,kr_bc,kr_vg,kr_ko,kr_mvg,kr_durner,kr_dvgch'

        h_min = -1000.0d0
        h_max = 0.0d0
        n_steps = 100
        dh = (h_max - h_min) / real(n_steps, real64)

        do i = 0, n_steps
            ! h_cm: CSV出力用の水頭 (cm)
            h_cm = h_min + real(i, real64) * dh

            ! h_pa: 計算モデル入力用の圧力 (Pa)
            ! 内部パラメータと同じ係数を使って変換することで整合性を保証
            h_pa = h_cm * conv_factor
            state%pressure = h_pa

            ! 全モデル計算
            do j = 1, 6
                call hcf_models(j)%p%calc_kflh(state, kr_vals(j))
            end do

            ! 出力
            ! h_cm をそのまま出し、kr_vals は倍精度(ES24.16E3)で出力して桁落ちを防ぐ
            write (file_unit, '(ES24.16E3, 6(",", ES24.16E3))') h_cm, kr_vals(1:6)
        end do

        close (file_unit)

    end subroutine test_hcf

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
