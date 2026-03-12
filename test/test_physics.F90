program test_physics_models
    use, intrinsic :: iso_fortran_env
    use :: module_core, only:type_state, PHYSICS_UNIT_PA, &
        WRF_BC, WRF_VG, WRF_KO, WRF_MVG, WRF_DURNER, WRF_DVGCH, &
        HCF_BASE, HCF_BC, HCF_VG, HCF_KO, HCF_MVG, HCF_DURNER, HCF_DVGCH
    use :: physics_models_wrf, only:holder_wrfs, type_wrf_params
    use :: physics_models_hcf, only:holder_hcfs, type_hcf_params
    use :: iapws, only:type_iapws97 ! Required for HCF initialization
    implicit none

    ! --- Variable definitions ---
    type(holder_wrfs) :: wrf
    type(type_wrf_params) :: wrf_params
    type(holder_hcfs) :: hcf
    type(type_hcf_params) :: hcf_params

    ! Dummy water object for HCF initialization interface
    type(type_iapws97) :: water_dummy

    ! State variable for HCF (used to pass pressure)
    type(type_state) :: state

    integer(int32) :: i, j
    integer(int32), parameter :: num_steps = 100
    integer(int32), parameter :: num_models = 6
    real(real64) :: h_val, log_h
    real(real64) :: val_theta, val_kr

    ! Result arrays
    real(real64) :: h_values(num_steps)
    real(real64) :: results_theta(num_steps, num_models)
    real(real64) :: results_kr(num_steps, num_models)

    ! --- Common parameters (matching Python reference code) ---
    real(real64), parameter :: ts = 0.5d0 ! theta_s
    real(real64), parameter :: tr = 0.1d0 ! theta_r
    real(real64), parameter :: l_param = 0.5d0

    ! ========================================================================
    ! 1. Water Retention Function (WRF) Calculation
    ! ========================================================================
    print *, "Calculating WRF (Water Retention Functions)..."

    do j = 1, num_models
        ! Reset parameters
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

        ! Initialize model
        call wrf%initialize(1, wrf_params)

        ! Computation loop (h: -0.01 -> -10000)
        do i = 1, num_steps
            ! Generate h on log scale (-0.01 to -10000)
            log_h = -2.0d0 + (6.0d0 * real(i - 1, real64) / real(num_steps - 1, real64))
            h_val = -(10.0d0**log_h)

            if (j == 1) h_values(i) = h_val ! Store h only on the first model

            ! Compute WRF
            call wrf%p%calc(h_val, val_theta)
            results_theta(i, j) = val_theta
        end do
    end do

    ! ========================================================================
    ! 2. Hydraulic Conductivity Function (HCF) Calculation
    ! ========================================================================
    print *, "Calculating HCF (Hydraulic Conductivity Functions)..."

    do j = 1, num_models
        call hcf_params%reset()
        hcf_params%unit_id = PHYSICS_UNIT_PA
        hcf_params%model_number = HCF_BASE ! Use Base model
        hcf_params%k_s = 1.0d0 ! Ks=1.0 to obtain relative values
        hcf_params%l = l_param

        ! Set HCF parameters (same physical parameters as WRF)
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

        ! Initialize HCF
        call hcf%initialize(1, hcf_params, water_dummy)

        ! Computation loop
        do i = 1, num_steps
            h_val = h_values(i)

            ! Pass pressure (h) to HCF via State
            call state%pressure%set(h_val)

            ! calc_Kflh returns (Ks * Kr)
            call hcf%p%calc_Kflh(state, val_kr)
            results_kr(i, j) = val_kr
        end do
    end do

    ! ========================================================================
    ! 3. CSV Output
    ! ========================================================================
    print *, "Writing results to verification_results.csv..."

    open (unit=10, file='log/test/wrf.csv', status='replace', action='write')

    ! Write header
    write (10, '(A)') 'h,theta_bc,theta_vg,theta_ko,theta_mvg,theta_durner,theta_dvgch'

    do i = 1, num_steps
        write (10, '(E24.16, 6(",", E24.16))') &
            h_values(i), &
            results_theta(i, 1), results_theta(i, 2), results_theta(i, 3), &
            results_theta(i, 4), results_theta(i, 5), results_theta(i, 6)
    end do

    close (10)

    open (unit=10, file='log/test/hcf.csv', status='replace', action='write')

    ! Write header
    write (10, '(A)') 'h,kr_bc,kr_vg,kr_ko,kr_mvg,kr_durner,kr_dvgch'

    do i = 1, num_steps
        write (10, '(E24.16, 6(",", E24.16))') &
            h_values(i), &
            results_kr(i, 1), results_kr(i, 2), results_kr(i, 3), &
            results_kr(i, 4), results_kr(i, 5), results_kr(i, 6)
    end do

end program test_physics_models
