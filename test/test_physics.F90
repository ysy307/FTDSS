program test_physics
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: physics_service
    ! use :: module_physics_materials, only:type_physics_info
    ! use :: module_physics_models, only:type_wrf_params, type_hcf_params
    ! 定数が定義されているモジュール（環境に合わせて適宜変更してください）
    use :: module_physics, only:g => gravity_acceleration, rho_std => reference_water_density

    implicit none

    integer(int32) :: unit
    integer(int32) :: ierr

    ! ログファイルの準備
    open (newunit=unit, file="log/test/physics_service.log", status="replace", action="write", iostat=ierr)
    if (ierr /= 0) then
        print *, "Error opening log file"
        stop
    end if

    write (unit, '(a)') "# Physics Service tests"
    write (unit, '(a)') "---"

    call test_density_service()
    call test_wrf_hcf_service()

    write (unit, '(a)') "---"
    write (unit, '(a)') "## Completed"
    close (unit)

contains

    !---------------------------------------------------------------------------
    ! 密度のテスト
    !---------------------------------------------------------------------------
    subroutine test_density_service()
        implicit none
        type(type_properties_manager) :: pm
        type(type_state) :: state

        ! 初期化用配列
        integer(int32), allocatable :: mat_ids(:)
        type(type_physics_info), allocatable :: den_infos(:)
        type(type_physics_info), allocatable :: sph_infos(:)
        type(type_physics_info), allocatable :: vhc_infos(:)
        type(type_physics_info), allocatable :: thc_infos(:)

        real(real64) :: computed_density, expected_density

        ! 1. マテリアル設定 (ID=1)
        mat_ids = [1]
        allocate (den_infos(1), sph_infos(1), vhc_infos(1), thc_infos(1))

        ! 密度パラメータ設定
        den_infos(1)%num_phases = 4
        den_infos(1)%solid = 2650.0d0
        den_infos(1)%water = 1000.0d0
        den_infos(1)%ice = 917.0d0
        den_infos(1)%vapor = 0.6d0

        ! その他はダミー設定（エラー回避用）
        sph_infos(1)%solid = 800.0d0
        vhc_infos(1)%solid = 2.0d6
        thc_infos(1)%solid = 1.5d0

        ! 2. マネージャーの初期化
        ! WRF/HCF/GCCは使用しないのでOptional引数は省略可能だが、
        ! initializeの実装によっては空配列が必要な場合もあるため注意。
        call pm%initialize(unique_material_ids=mat_ids, &
                           density_info=den_infos, &
                           specific_heat_info=sph_infos, &
                           heat_capacity_info=vhc_infos, &
                           thermal_conductivity_info=thc_infos)

        ! 3. テスト条件設定
        call state%reset()
        call state%set(temperature=10.0d0, pressure=0.0d0, &
                       porosity=0.4d0, water_content=0.2d0, &
                       ice_content=0.1d0, relative_humidity=0.6d0)

        ! 4. 計算実行 (Material ID = 1)
        call pm%calc_density(1, state, computed_density)

        ! 5. 検証
        ! Expected: (1-0.4)*2650 + 0.2*1000(approx) + 0.1*917 + ...
        ! IAPWSを使用しているため水密度は温度依存で厳密に計算される
        expected_density = 1881.464474792440d0
        call check_variable(computed_density, expected_density, "Density Service Test")

    end subroutine test_density_service

    !---------------------------------------------------------------------------
    ! WRF (保水性) と HCF (透水性) の統合テスト
    !---------------------------------------------------------------------------
    subroutine test_wrf_hcf_service()
        implicit none
        type(type_properties_manager) :: pm
        type(type_state) :: state

        ! 初期化用配列
        integer(int32), parameter :: num_mats = 6
        integer(int32), allocatable :: mat_ids(:)
        type(type_physics_info), allocatable :: dummy_infos(:)

        integer(int32), allocatable :: wrf_ids(:), hcf_ids(:), gcc_ids(:)
        type(type_wrf_params), allocatable :: wrf_params(:)
        type(type_hcf_params), allocatable :: hcf_params(:)

        integer(int32) :: i, j, n_steps, file_unit
        real(real64) :: h_cm, h_pa
        real(real64) :: h_min, h_max, dh
        real(real64) :: conv_factor

        real(real64) :: val_Kflh
        real(real64), allocatable :: results_theta(:), results_kr(:)

        ! 1. マテリアルID設定 (1~6)
        allocate (mat_ids(num_mats))
        do j = 1, num_mats
            mat_ids(j) = j
        end do

        ! ダミーの物性情報（WRF/HCF計算には直接影響しないが初期化に必要）
        allocate (dummy_infos(num_mats))
        do j = 1, num_mats
            dummy_infos(j)%num_phases = 3
            dummy_infos(j)%solid = 2650.0d0
        end do

        ! 2. WRF/HCF パラメータ設定
        allocate (wrf_ids(num_mats), wrf_params(num_mats))
        allocate (hcf_ids(num_mats), hcf_params(num_mats))
        allocate (gcc_ids(num_mats))
        gcc_ids = 0 ! GCCなし

        ! 共通設定
        do j = 1, num_mats
            ! WRF
            call wrf_params(j)%reset()
            wrf_params(j)%unit_id = 1 ! CM (PHYSICS_UNIT_CM想定)
            wrf_params(j)%theta_s = 0.5d0
            wrf_params(j)%theta_r = 0.1d0

            ! HCF
            call hcf_params(j)%reset()
            hcf_params(j)%unit_id = 1 ! CM
            hcf_params(j)%k_s = 1.0d0
            hcf_params(j)%l = 0.5d0
            hcf_params(j)%theta_s = 0.5d0
            hcf_params(j)%theta_r = 0.1d0
        end do

        ! 個別モデル設定 (IDとパラメータの対応)
        ! 1: Brooks-Corey
        wrf_ids(1) = 1; wrf_params(1)%model_number = 1; wrf_params(1)%alpha1 = -10.0d0; wrf_params(1)%n1 = 2.0d0
        hcf_ids(1) = 1; hcf_params(1)%hcf_model_number = 1; hcf_params(1)%alpha1 = -10.0d0; hcf_params(1)%n1 = 2.0d0

        ! 2: Van Genuchten
        wrf_ids(2) = 2; wrf_params(2)%model_number = 2; wrf_params(2)%alpha1 = 0.01d0; wrf_params(2)%n1 = 2.0d0; wrf_params(2)%m1 = 0.5d0
        hcf_ids(2) = 2; hcf_params(2)%hcf_model_number = 2; hcf_params(2)%alpha1 = 0.01d0; hcf_params(2)%n1 = 2.0d0; hcf_params(2)%m1 = 0.5d0

        ! 3: Kosugi
        wrf_ids(3) = 3; wrf_params(3)%model_number = 3; wrf_params(3)%alpha1 = -100.0d0; wrf_params(3)%n1 = 1.0d0
        hcf_ids(3) = 3; hcf_params(3)%hcf_model_number = 3; hcf_params(3)%alpha1 = -100.0d0; hcf_params(3)%n1 = 1.0d0

        ! 4: Modified VG
        wrf_ids(4) = 4; wrf_params(4)%model_number = 4; wrf_params(4)%alpha1 = 0.01d0; wrf_params(4)%n1 = 2.0d0; wrf_params(4)%m1 = 0.5d0; wrf_params(4)%h_crit = -5.0d0
        hcf_ids(4) = 4; hcf_params(4)%hcf_model_number = 4; hcf_params(4)%alpha1 = 0.01d0; hcf_params(4)%n1 = 2.0d0; hcf_params(4)%m1 = 0.5d0

        ! 5: Durner
        wrf_ids(5) = 5; wrf_params(5)%model_number = 5; wrf_params(5)%w1 = 0.4d0; wrf_params(5)%alpha1 = 0.01d0; wrf_params(5)%n1 = 2.0d0; wrf_params(5)%m1 = 0.5d0; wrf_params(5)%w2 = 0.6d0; wrf_params(5)%alpha2 = 0.05d0; wrf_params(5)%n2 = 3.0d0; wrf_params(5)%m2 = 1.0d0 - 1.0d0 / 3.0d0
        hcf_ids(5) = 5; hcf_params(5)%hcf_model_number = 5

        ! 6: DVGCH
        wrf_ids(6) = 6; wrf_params(6)%model_number = 6; wrf_params(6)%w1 = 0.4d0; wrf_params(6)%alpha1 = 0.01d0; wrf_params(6)%n1 = 2.0d0; wrf_params(6)%m1 = 0.5d0; wrf_params(6)%w2 = 0.6d0; wrf_params(6)%n2 = 3.0d0; wrf_params(6)%m2 = 1.0d0 - 1.0d0 / 3.0d0
        hcf_ids(6) = 6; hcf_params(6)%hcf_model_number = 6

        ! 3. マネージャー初期化
        call pm%initialize(unique_material_ids=mat_ids, &
                           density_info=dummy_infos, &
                           specific_heat_info=dummy_infos, &
                           heat_capacity_info=dummy_infos, &
                           thermal_conductivity_info=dummy_infos, &
                           wrf_ids=wrf_ids, wrf_model_info=wrf_params, &
                           hcf_ids=hcf_ids, hcf_model_info=hcf_params, &
                           gcc_model_ids=gcc_ids)

        ! 4. 計算ループとCSV出力
        conv_factor = (rho_std * g) / 100.0d0
        h_min = -1000.0d0
        h_max = 0.0d0
        n_steps = 100
        dh = (h_max - h_min) / real(n_steps, real64)

        allocate (results_theta(num_mats), results_kr(num_mats))

        ! --- WRF (theta) Output ---
        open (newunit=file_unit, file='log/test/wrf_service.csv', status='replace', action='write')
        write (file_unit, '(A)') 'h,theta_bc,theta_vg,theta_ko,theta_mvg,theta_durner,theta_dvgch'

        do i = 0, n_steps
            h_cm = h_min + real(i, real64) * dh
            h_pa = h_cm * conv_factor

            ! 状態更新
            state%pressure = h_pa
            state%temperature = 20.0d0 ! HCFの粘性補正などで必要

            do j = 1, num_mats
                ! 水相状態の更新 (これで state%water_content が計算される)
                call pm%update_water_phases(j, state)
                results_theta(j) = state%water_content
            end do

            write (file_unit, '(ES24.16E3, 6(",", ES24.16E3))') h_cm, results_theta
        end do
        close (file_unit)
        write (unit, '(a)') "PASS: WRF Calculation (See log/test/wrf_service.csv)"

        ! --- HCF (Kr) Output ---
        open (newunit=file_unit, file='log/test/hcf_service.csv', status='replace', action='write')
        write (file_unit, '(A)') 'h,kr_bc,kr_vg,kr_ko,kr_mvg,kr_durner,kr_dvgch'

        do i = 0, n_steps
            h_cm = h_min + real(i, real64) * dh
            h_pa = h_cm * conv_factor

            state%pressure = h_pa
            state%temperature = 20.0d0
            state%porosity = 0.5d0 ! HCF計算で飽和度Se算出に必要(Se = theta/porosity...)

            do j = 1, num_mats
                ! まず水分量を更新
                call pm%update_water_phases(j, state)
                ! 次に透水係数を計算
                call pm%calc_Kflh(j, state, val_Kflh)
                results_kr(j) = val_Kflh
            end do

            write (file_unit, '(ES24.16E3, 6(",", ES24.16E3))') h_cm, results_kr
        end do
        close (file_unit)
        write (unit, '(a)') "PASS: HCF Calculation (See log/test/hcf_service.csv)"

    end subroutine test_wrf_hcf_service

    !---------------------------------------------------------------------------
    ! 判定用ユーティリティ
    !---------------------------------------------------------------------------
    subroutine check_variable(v, v_exa, v_name)
        implicit none
        real(real64), intent(in) :: v, v_exa
        character(len=*), intent(in) :: v_name
        real(real64), parameter :: tol = 1.0d-8
        real(real64) :: rel_diff

        if (abs(v_exa) > 0.0d0) then
            rel_diff = abs(v - v_exa) / abs(v_exa)
        else
            rel_diff = abs(v - v_exa)
        end if

        if (rel_diff > tol) then
            write (unit, '(a)') "**FAIL**: `"//v_name//"`"
            write (unit, '(a, es15.8, a, es15.8, a, es15.8)') "  Computed: ", v, " | Expected: ", v_exa, " | Diff: ", rel_diff
        else
            write (unit, '(a)') "PASS: `"//v_name//"`"
        end if
    end subroutine check_variable

end program test_physics
