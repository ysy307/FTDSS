program test_physics_models
    use, intrinsic :: iso_fortran_env
    use :: module_core, only:type_state, PHYSICS_UNIT_PA, &
        WRF_BC, WRF_VG, WRF_KO, WRF_MVG, WRF_DURNER, WRF_DVGCH, &
        HCF_BASE, HCF_BC, HCF_VG, HCF_KO, HCF_MVG, HCF_DURNER, HCF_DVGCH
    use :: physics_models_wrf, only:holder_wrfs, type_wrf_params
    use :: physics_models_hcf, only:holder_hcfs, type_hcf_params
    use :: iapws, only:type_iapws97 ! HCFの初期化に必要
    implicit none

    ! --- 変数定義 ---
    type(holder_wrfs) :: wrf
    type(type_wrf_params) :: wrf_params
    type(holder_hcfs) :: hcf
    type(type_hcf_params) :: hcf_params

    ! HCF計算用のダミー水オブジェクト（初期化インターフェースに必要）
    type(type_iapws97) :: water_dummy

    ! HCF計算用の状態変数（圧力を渡すために使用）
    type(type_state) :: state

    integer(int32) :: i, j
    integer(int32), parameter :: num_steps = 100
    integer(int32), parameter :: num_models = 6
    real(real64) :: h_val, log_h
    real(real64) :: val_theta, val_kr

    ! 結果格納用配列
    real(real64) :: h_values(num_steps)
    real(real64) :: results_theta(num_steps, num_models)
    real(real64) :: results_kr(num_steps, num_models)

    ! --- 共通パラメータ (Pythonコード準拠) ---
    real(real64), parameter :: ts = 0.5d0 ! theta_s
    real(real64), parameter :: tr = 0.1d0 ! theta_r
    real(real64), parameter :: l_param = 0.5d0

    ! ========================================================================
    ! 1. 水分保持曲線 (WRF) の計算
    ! ========================================================================
    print *, "Calculating WRF (Water Retention Functions)..."

    do j = 1, num_models
        ! パラメータのリセット
        call wrf_params%reset()
        wrf_params%unit_id = PHYSICS_UNIT_PA
        wrf_params%theta_s = ts
        wrf_params%theta_r = tr

        select case (j)
        case (1) ! Brooks-Corey (BC)
            wrf_params%model_number = WRF_BC
            wrf_params%alpha1 = -10.0d0
            wrf_params%n1 = 2.0d0

        case (2) ! Van Genuchten (VG)
            wrf_params%model_number = WRF_VG
            wrf_params%alpha1 = 0.01d0
            wrf_params%n1 = 2.0d0
            wrf_params%m1 = 0.5d0

        case (3) ! Kosugi (KO)
            wrf_params%model_number = WRF_KO
            wrf_params%alpha1 = -100.0d0
            wrf_params%n1 = 1.0d0

        case (4) ! Modified VG (MVG)
            wrf_params%model_number = WRF_MVG
            wrf_params%alpha1 = 0.01d0
            wrf_params%n1 = 2.0d0
            wrf_params%m1 = 0.5d0
            wrf_params%h_crit = -5.0d0

        case (5) ! Durner (Dual VG)
            wrf_params%model_number = WRF_DURNER
            wrf_params%alpha1 = 0.01d0
            wrf_params%n1 = 2.0d0
            wrf_params%m1 = 0.5d0
            wrf_params%w1 = 0.4d0
            wrf_params%alpha2 = 0.05d0
            wrf_params%n2 = 3.0d0
            wrf_params%m2 = 1.0d0 - 1.0d0 / 3.0d0
            wrf_params%w2 = 0.6d0

        case (6) ! DVGCH (Common Alpha)
            wrf_params%model_number = WRF_DVGCH
            wrf_params%alpha1 = 0.01d0
            wrf_params%n1 = 2.0d0
            wrf_params%m1 = 0.5d0
            wrf_params%w1 = 0.4d0
            wrf_params%n2 = 3.0d0
            wrf_params%m2 = 1.0d0 - 1.0d0 / 3.0d0
            wrf_params%w2 = 0.6d0
        end select

        ! モデルの初期化
        call wrf%initialize(1, wrf_params)

        ! 計算ループ (h: -0.01 -> -10000)
        do i = 1, num_steps
            ! ログスケールで h を生成 (-0.01 ~ -10000)
            log_h = -2.0d0 + (6.0d0 * real(i - 1, real64) / real(num_steps - 1, real64))
            h_val = -(10.0d0**log_h)

            if (j == 1) h_values(i) = h_val ! 初回のみhを保存

            ! WRF計算実行
            call wrf%p%calc(h_val, val_theta)
            results_theta(i, j) = val_theta
        end do
    end do

    ! ========================================================================
    ! 2. 透水係数 (HCF) の計算
    ! ========================================================================
    print *, "Calculating HCF (Hydraulic Conductivity Functions)..."

    do j = 1, num_models
        call hcf_params%reset()
        hcf_params%unit_id = PHYSICS_UNIT_PA
        hcf_params%model_number = HCF_BASE ! 基本モデル(Base)を使用
        hcf_params%k_s = 1.0d0 ! 相対値を見るため Ks=1.0
        hcf_params%l = l_param

        ! HCFパラメータの設定 (WRFと同じ物理パラメータを使用)
        select case (j)
        case (1) ! BC
            hcf_params%hcf_model_number = HCF_BC
            hcf_params%alpha1 = -10.0d0
            hcf_params%n1 = 2.0d0
        case (2) ! VG
            hcf_params%hcf_model_number = HCF_VG
            hcf_params%alpha1 = 0.01d0
            hcf_params%n1 = 2.0d0
            hcf_params%m1 = 0.5d0
        case (3) ! KO
            hcf_params%hcf_model_number = HCF_KO
            hcf_params%alpha1 = -100.0d0
            hcf_params%n1 = 1.0d0
        case (4) ! MVG
            hcf_params%hcf_model_number = HCF_MVG
            hcf_params%theta_s = ts
            hcf_params%theta_r = tr
            hcf_params%alpha1 = 0.01d0
            hcf_params%n1 = 2.0d0
            hcf_params%m1 = 0.5d0
            hcf_params%h_crit = -5.0d0
        case (5) ! Durner
            hcf_params%hcf_model_number = HCF_DURNER
            hcf_params%alpha1 = 0.01d0
            hcf_params%n1 = 2.0d0
            hcf_params%m1 = 0.5d0
            hcf_params%w1 = 0.4d0
            hcf_params%alpha2 = 0.05d0
            hcf_params%n2 = 3.0d0
            hcf_params%m2 = 1.0d0 - 1.0d0 / 3.0d0
            hcf_params%w2 = 0.6d0
        case (6) ! DVGCH
            hcf_params%hcf_model_number = HCF_DVGCH
            hcf_params%alpha1 = 0.01d0
            hcf_params%n1 = 2.0d0
            hcf_params%m1 = 0.5d0
            hcf_params%w1 = 0.4d0
            hcf_params%n2 = 3.0d0
            hcf_params%m2 = 1.0d0 - 1.0d0 / 3.0d0
            hcf_params%w2 = 0.6d0
        end select

        ! HCF初期化
        call hcf%initialize(1, hcf_params, water_dummy)

        ! 計算ループ
        do i = 1, num_steps
            h_val = h_values(i)

            ! HCF計算にはState経由で圧力(h)を渡す
            call state%pressure%set(h_val)

            ! calc_Kflh は (Ks * Kr) を返す
            call hcf%p%calc_Kflh(state, val_kr)
            results_kr(i, j) = val_kr
        end do
    end do

    ! ========================================================================
    ! 3. CSV出力
    ! ========================================================================
    print *, "Writing results to verification_results.csv..."

    open (unit=10, file='log/test/wrf.csv', status='replace', action='write')

    ! ヘッダー出力
    write (10, '(A)') 'h,theta_bc,theta_vg,theta_ko,theta_mvg,theta_durner,theta_dvgch'

    do i = 1, num_steps
        ! カンマ区切りでデータ出力
        write (10, '(E24.16, 6(",", E24.16))') &
            h_values(i), &
            results_theta(i, 1), results_theta(i, 2), results_theta(i, 3), &
            results_theta(i, 4), results_theta(i, 5), results_theta(i, 6)
    end do

    close (10)

    open (unit=10, file='log/test/hcf.csv', status='replace', action='write')

    ! ヘッダー出力
    write (10, '(A)') 'h,kr_bc,kr_vg,kr_ko,kr_mvg,kr_durner,kr_dvgch'

    do i = 1, num_steps
        ! カンマ区切りでデータ出力
        write (10, '(E24.16, 6(",", E24.16))') &
            h_values(i), &
            results_kr(i, 1), results_kr(i, 2), results_kr(i, 3), &
            results_kr(i, 4), results_kr(i, 5), results_kr(i, 6)
    end do

end program test_physics_models
