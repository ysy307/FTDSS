module test_constitutive_suite
    use, intrinsic :: iso_fortran_env, only: error_unit, int32, int64, output_unit, real64
    use :: iapws, only: type_iapws06, type_iapws97
    use :: module_core, only: GCC_TYPES, HCF_MODES, PHYSICS_UNITS, SWCC_MODELS, &
        type_config_gcc, type_config_hcf, type_config_wrf, type_state
    use :: module_input, only: input_translator, type_input
    use :: models_hcf, only: holder_hcfs
    use :: models_phase_change_vaporization, only: type_evaporation
    use :: models_phase_change_gcc, only: holder_gccs
    use :: models_phase_change_manager, only: type_phase_manager
    use :: models_wrf, only: holder_wrfs
    use :: numerical_special_functions_mkl, only: type_mkl_regularized_incomplete_beta
    implicit none
    private

    public :: type_constitutive_test_suite

    type :: type_constitutive_test_suite
        integer(int32), private :: failures = 0
    contains
        procedure, public :: run => run_constitutive_tests
        procedure, private :: test_translator_units
        procedure, private :: test_wrf_derivatives
        procedure, private :: test_pressure_capacity_units
        procedure, private :: test_special_functions
        procedure, private :: test_hcf_vg_dispatch
        procedure, private :: test_incomplete_beta_thread_safety
        procedure, private :: test_vapor_combined_evaluation
        procedure, private :: test_saturation_pressure
        procedure, private :: benchmark_hot_paths
        procedure, private :: report_benchmark
        procedure, private :: configure_wrf
        procedure, private :: check_close
        procedure, private :: check_true
    end type type_constitutive_test_suite

contains

    subroutine run_constitutive_tests(self)
        implicit none
        class(type_constitutive_test_suite), intent(inout) :: self

        call self%test_translator_units()
        call self%test_wrf_derivatives()
        call self%test_pressure_capacity_units()
        call self%test_special_functions()
        call self%test_hcf_vg_dispatch()
        call self%test_incomplete_beta_thread_safety()
        call self%test_vapor_combined_evaluation()
        call self%test_saturation_pressure()
        call self%benchmark_hot_paths()

        if (self%failures > 0) then
            write (error_unit, '(A,I0)') "FAILED test_constitutive checks: ", self%failures
            error stop 1
        end if
        write (output_unit, '(A)') "All test_constitutive checks passed."
    end subroutine run_constitutive_tests

    subroutine test_translator_units(self)
        implicit none
        class(type_constitutive_test_suite), intent(inout) :: self

        type(type_input) :: input
        type(type_config_wrf) :: config
        real(real64), parameter :: rho_g = 1000.0d0 * 9.80665d0

        input%basic%num_materials = 1
        allocate (input%basic%materials(1))

        input%basic%materials(1)%water_characteristic_curve%model_number = SWCC_MODELS%VG%ID
        input%basic%materials(1)%water_characteristic_curve%theta_s = 0.45d0
        input%basic%materials(1)%water_characteristic_curve%theta_r = 0.05d0
        input%basic%materials(1)%water_characteristic_curve%n1 = 2.0d0
        input%basic%materials(1)%water_characteristic_curve%m1 = 0.5d0
        input%basic%materials(1)%water_characteristic_curve%l = 0.5d0

        input%basic%materials(1)%water_characteristic_curve%unit = PHYSICS_UNITS%M%ID
        input%basic%materials(1)%water_characteristic_curve%alpha1 = 2.0d0
        call input_translator%execute(input, 1_int32, config)
        call self%check_close("VG alpha: m to 1/m", config%alpha1, 2.0d0, 1.0d-14)

        input%basic%materials(1)%water_characteristic_curve%unit = PHYSICS_UNITS%CM%ID
        input%basic%materials(1)%water_characteristic_curve%alpha1 = 0.02d0
        call input_translator%execute(input, 1_int32, config)
        call self%check_close("VG alpha: 1/cm to 1/m", config%alpha1, 2.0d0, 1.0d-14)

        input%basic%materials(1)%water_characteristic_curve%unit = PHYSICS_UNITS%PA%ID
        input%basic%materials(1)%water_characteristic_curve%alpha1 = 2.0d0 / rho_g
        call input_translator%execute(input, 1_int32, config)
        call self%check_close("VG alpha: 1/Pa to 1/m", config%alpha1, 2.0d0, 1.0d-12)

        input%basic%materials(1)%water_characteristic_curve%model_number = SWCC_MODELS%BC%ID
        input%basic%materials(1)%water_characteristic_curve%n1 = 2.0d0

        input%basic%materials(1)%water_characteristic_curve%unit = PHYSICS_UNITS%M%ID
        input%basic%materials(1)%water_characteristic_curve%alpha1 = -0.5d0
        call input_translator%execute(input, 1_int32, config)
        call self%check_close("BC air-entry: m to m", config%alpha1, -0.5d0, 1.0d-14)

        input%basic%materials(1)%water_characteristic_curve%unit = PHYSICS_UNITS%CM%ID
        input%basic%materials(1)%water_characteristic_curve%alpha1 = -50.0d0
        call input_translator%execute(input, 1_int32, config)
        call self%check_close("BC air-entry: cm to m", config%alpha1, -0.5d0, 1.0d-14)

        input%basic%materials(1)%water_characteristic_curve%unit = PHYSICS_UNITS%PA%ID
        input%basic%materials(1)%water_characteristic_curve%alpha1 = -0.5d0 * rho_g
        call input_translator%execute(input, 1_int32, config)
        call self%check_close("BC air-entry: Pa to m", config%alpha1, -0.5d0, 1.0d-12)

        input%basic%materials(1)%water_characteristic_curve%model_number = SWCC_MODELS%KO%ID
        input%basic%materials(1)%water_characteristic_curve%alpha1 = -0.25d0 * rho_g
        call input_translator%execute(input, 1_int32, config)
        call self%check_close("KO median head: Pa to m", config%alpha1, -0.25d0, 1.0d-12)

        input%basic%materials(1)%water_characteristic_curve%model_number = SWCC_MODELS%MVG%ID
        input%basic%materials(1)%water_characteristic_curve%alpha1 = 1.5d0 / rho_g
        input%basic%materials(1)%water_characteristic_curve%h_crit = -0.1d0 * rho_g
        call input_translator%execute(input, 1_int32, config)
        call self%check_close("MVG alpha: 1/Pa to 1/m", config%alpha1, 1.5d0, 1.0d-12)
        call self%check_close("MVG critical head: Pa to m", config%h_crit, -0.1d0, 1.0d-12)

        input%basic%materials(1)%water_characteristic_curve%model_number = SWCC_MODELS%DURNER%ID
        input%basic%materials(1)%water_characteristic_curve%alpha1 = 2.0d0 / rho_g
        input%basic%materials(1)%water_characteristic_curve%alpha2 = 0.4d0 / rho_g
        call input_translator%execute(input, 1_int32, config)
        call self%check_close("Durner alpha1: 1/Pa to 1/m", config%alpha1, 2.0d0, 1.0d-12)
        call self%check_close("Durner alpha2: 1/Pa to 1/m", config%alpha2, 0.4d0, 1.0d-12)

        input%basic%materials(1)%water_characteristic_curve%model_number = SWCC_MODELS%DVGCH%ID
        input%basic%materials(1)%water_characteristic_curve%alpha1 = 0.8d0 / rho_g
        call input_translator%execute(input, 1_int32, config)
        call self%check_close("DVGCH alpha: 1/Pa to 1/m", config%alpha1, 0.8d0, 1.0d-12)
    end subroutine test_translator_units

    subroutine test_wrf_derivatives(self)
        implicit none
        class(type_constitutive_test_suite), intent(inout) :: self

        type(type_config_wrf) :: config
        type(holder_wrfs) :: wrf
        integer(int32) :: model_id
        real(real64) :: h, dh, theta_minus, theta_plus
        real(real64) :: derivative, derivative_fd
        character(len=64) :: label

        h = -1.0d0
        dh = 1.0d-6

        do model_id = SWCC_MODELS%BC%ID, SWCC_MODELS%DVGCH%ID
            call self%configure_wrf(config, model_id)
            call wrf%initialize(config)

            call wrf%p%calc(h - dh, theta_minus)
            call wrf%p%calc(h + dh, theta_plus)
            call wrf%p%deriv(h, derivative)
            derivative_fd = (theta_plus - theta_minus) / (2.0d0 * dh)

            write (label, '(A,I0)') "WRF dtheta/dh finite difference, model ", model_id
            call self%check_close(trim(label), derivative, derivative_fd, 2.0d-6)
            call self%check_true(trim(label)//" is non-negative", derivative >= 0.0d0)
        end do
    end subroutine test_wrf_derivatives

    subroutine test_pressure_capacity_units(self)
        implicit none
        class(type_constitutive_test_suite), intent(inout) :: self

        type(type_config_wrf) :: config
        type(holder_wrfs) :: wrf
        real(real64), parameter :: rho_g = 1000.0d0 * 9.80665d0
        real(real64) :: h, pressure, dp
        real(real64) :: theta_minus, theta_plus
        real(real64) :: dtheta_dh, dtheta_dP, dtheta_dP_fd
        real(real64) :: physical_capacity_saturated, lscheme_capacity

        call self%configure_wrf(config, SWCC_MODELS%VG%ID)
        call wrf%initialize(config)

        h = -1.0d0
        pressure = rho_g * h
        dp = 1.0d-2
        call wrf%p%deriv(h, dtheta_dh)
        dtheta_dP = dtheta_dh / rho_g
        call wrf%p%calc((pressure - dp) / rho_g, theta_minus)
        call wrf%p%calc((pressure + dp) / rho_g, theta_plus)
        dtheta_dP_fd = (theta_plus - theta_minus) / (2.0d0 * dp)
        call self%check_close("dtheta/dP conversion from pressure head", dtheta_dP, dtheta_dP_fd, 2.0d-6)

        call wrf%p%deriv(0.0d0, dtheta_dh)
        physical_capacity_saturated = dtheta_dh / rho_g
        call wrf%p%calc_lscheme_capacity(lscheme_capacity)

        call self%check_close("physical saturated capacity", physical_capacity_saturated, 0.0d0, 1.0d-14)
        call self%check_true("L-scheme capacity is positive at saturation", lscheme_capacity > 0.0d0)
        write (output_unit, '(A,ES24.16)') "physical saturated dtheta/dP [1/Pa] = ", physical_capacity_saturated
        write (output_unit, '(A,ES24.16)') "L-scheme capacity floor [1/Pa]       = ", lscheme_capacity
    end subroutine test_pressure_capacity_units

    subroutine test_special_functions(self)
        implicit none
        class(type_constitutive_test_suite), intent(inout) :: self

        type(type_mkl_regularized_incomplete_beta) :: incomplete_beta
        real(real64) :: value
        logical :: converged

        value = 0.0d0
        converged = .false.
        call incomplete_beta%initialize(2.0d0, 3.0d0)
        call incomplete_beta%evaluate(0.5d0, value, converged)
        call self%check_true("MKL incomplete beta continued fraction converged", converged)
        call self%check_close("regularized incomplete beta I_0.5(2,3)", value, 0.6875d0, 2.0d-14)

        value = 0.0d0
        converged = .false.
        call incomplete_beta%initialize(0.5d0, 0.5d0)
        call incomplete_beta%evaluate(0.5d0, value, converged)
        call self%check_true("MKL incomplete beta symmetric case converged", converged)
        call self%check_close("regularized incomplete beta I_0.5(0.5,0.5)", value, 0.5d0, 2.0d-14)
    end subroutine test_special_functions

    subroutine test_hcf_vg_dispatch(self)
        implicit none
        class(type_constitutive_test_suite), intent(inout) :: self

        type(type_config_hcf) :: config
        type(type_state) :: state
        type(holder_hcfs) :: hcf
        type(type_iapws97), target :: water
        type(type_iapws06), target :: ice
        type(type_mkl_regularized_incomplete_beta) :: incomplete_beta
        real(real64), parameter :: rho_g = 1000.0d0 * 9.80665d0
        real(real64) :: effective_saturation, zeta, beta_value
        real(real64) :: conductivity, expected_conductivity
        logical :: converged

        call config%reset()
        config%model = HCF_MODES%BASE
        config%swcc_model = SWCC_MODELS%VG
        config%k_sat = 1.0d0
        config%l = 0.5d0
        config%alpha1 = 2.0d0
        config%n1 = 1.48d0
        config%m1 = 0.2d0
        call hcf%initialize(config, water, ice)

        call state%pressure%set(-rho_g)
        conductivity = 0.0d0
        call hcf%p%calc_Kflh(state, conductivity)

        effective_saturation = (1.0d0 + config%alpha1**config%n1)**(-config%m1)
        zeta = effective_saturation**(1.0d0 / config%m1)
        call incomplete_beta%initialize(config%m1 + 1.0d0 / config%n1, 1.0d0 - 1.0d0 / config%n1)
        beta_value = 0.0d0
        converged = .false.
        call incomplete_beta%evaluate(zeta, beta_value, converged)
        expected_conductivity = config%k_sat * effective_saturation**config%l * beta_value**2

        call self%check_true("HCF VG incomplete beta converged through bound model", converged)
        call self%check_close("HCF VG polymorphic calc_Kflh dispatch", conductivity, expected_conductivity, 2.0d-14)
    end subroutine test_hcf_vg_dispatch

    subroutine test_incomplete_beta_thread_safety(self)
        implicit none
        class(type_constitutive_test_suite), intent(inout) :: self

        integer(int32), parameter :: num_evaluations = 100000
        type(type_mkl_regularized_incomplete_beta) :: incomplete_beta
        real(real64), allocatable :: serial_values(:), parallel_values(:)
        integer(int32) :: i
        real(real64) :: x, value
        logical :: converged

        allocate (serial_values(num_evaluations), parallel_values(num_evaluations))
        call incomplete_beta%initialize(0.2d0 + 1.0d0 / 1.48d0, 1.0d0 - 1.0d0 / 1.48d0)

        do i = 1, num_evaluations
            x = 0.01d0 + 0.98d0 * real(mod(i, 997), real64) / 996.0d0
            value = 0.0d0
            converged = .false.
            call incomplete_beta%evaluate(x, value, converged)
            serial_values(i) = value
        end do

        !$omp parallel do default(none) shared(incomplete_beta, parallel_values) &
        !$omp private(x, value, converged) schedule(static)
        do i = 1, num_evaluations
            x = 0.01d0 + 0.98d0 * real(mod(i, 997), real64) / 996.0d0
            value = 0.0d0
            converged = .false.
            call incomplete_beta%evaluate(x, value, converged)
            parallel_values(i) = value
        end do
        !$omp end parallel do

        call self%check_true("shared incomplete beta evaluator is thread-safe", &
                             all(serial_values == parallel_values))
    end subroutine test_incomplete_beta_thread_safety

    subroutine test_vapor_combined_evaluation(self)
        implicit none
        class(type_constitutive_test_suite), intent(inout) :: self

        type(type_evaporation) :: evaporation
        type(type_iapws97), target :: water
        type(type_state) :: state
        real(real64) :: rh
        real(real64) :: vapor_separate, dP_separate, dT_separate
        real(real64) :: vapor_combined, dP_combined, dT_combined

        call water%initialize()
        call evaporation%initialize(water)
        call state%temperature%set(-1.0d0)
        call state%pressure%set(-10000.0d0)
        call state%air_content%set(0.2d0)
        call state%dQa_dP%set(-1.0d-7)
        call state%dQa_dT%set(1.0d-3)
        call evaporation%calc_relative_humidity(state, rh)
        call state%relative_humidity%set(rh)

        call evaporation%calc_vapor_content(state, vapor_separate)
        call evaporation%calc_vapor_content_derivatives(state, dP_separate, dT_separate)
        call evaporation%calc_vapor_content_with_derivatives(state, vapor_combined, dP_combined, dT_combined)

        call self%check_close("combined vapor content", vapor_combined, vapor_separate, 2.0d-14)
        call self%check_close("combined vapor pressure derivative", dP_combined, dP_separate, 2.0d-14)
        call self%check_close("combined vapor temperature derivative", dT_combined, dT_separate, 2.0d-14)
    end subroutine test_vapor_combined_evaluation

    subroutine test_saturation_pressure(self)
        implicit none
        class(type_constitutive_test_suite), intent(inout) :: self

        type(type_config_wrf) :: wrf_config
        type(type_config_gcc) :: gcc_config
        type(holder_wrfs) :: wrf
        type(holder_gccs) :: gcc
        type(type_phase_manager) :: phase_manager
        type(type_iapws97), target :: water
        type(type_iapws06), target :: ice
        type(type_state) :: state
        real(real64) :: saturation_pressure, Qw, Qi, Qa
        logical :: is_saturated

        call self%configure_wrf(wrf_config, SWCC_MODELS%VG%ID)
        wrf_config%theta_s = 0.535d0
        wrf_config%theta_r = 0.05d0
        wrf_config%alpha1 = 1.11d0
        wrf_config%n1 = 1.48d0
        wrf_config%m1 = 0.2d0
        call wrf%initialize(wrf_config)

        call gcc_config%reset()
        gcc_config%gcc_model = GCC_TYPES%NON_SEGREGATION
        call water%initialize()
        call ice%initialize()
        call gcc%initialize(1_int32, gcc_config, water, ice)
        call phase_manager%initialize(gcc%p, wrf%p, water, ice)

        call state%temperature%set(-1.0d0)
        call state%pressure%set(0.0d0)
        call state%porosity%set(0.535d0)
        saturation_pressure = 0.0d0
        is_saturated = .false.
        call phase_manager%calc_saturation_pressure(state, saturation_pressure, is_saturated)
        call self%check_true("frozen saturation pressure is negative", saturation_pressure < 0.0d0)
        call self%check_true("zero gauge pressure is saturated while frozen", is_saturated)

        call state%pressure%set(saturation_pressure - 10.0d0)
        call phase_manager%calc_saturation_pressure(state, saturation_pressure, is_saturated)
        call self%check_true("state below saturation pressure contains gas", .not. is_saturated)
        call phase_manager%update_water_phases(state)
        call state%water_content%get(Qw)
        call state%ice_content%get(Qi)
        call state%air_content%get(Qa)
        call self%check_true("state below saturation pressure has positive gas volume", Qa > 0.0d0)
        call self%check_close("unsaturated phase volumes close the pore volume", Qw + Qi + Qa, 0.535d0, 2.0d-14)

        call state%pressure%set(saturation_pressure + 10.0d0)
        call phase_manager%calc_saturation_pressure(state, saturation_pressure, is_saturated)
        call self%check_true("state above saturation pressure is gas-free", is_saturated)
        call phase_manager%update_water_phases(state)
        call state%water_content%get(Qw)
        call state%ice_content%get(Qi)
        call state%air_content%get(Qa)
        call self%check_close("saturated phase volumes close the pore volume", Qw + Qi + Qa, 0.535d0, 2.0d-14)
        call self%check_true("saturated state has zero gas volume", abs(Qa) < 1.0d-12)
    end subroutine test_saturation_pressure

    subroutine benchmark_hot_paths(self)
        implicit none
        class(type_constitutive_test_suite), intent(inout) :: self

        integer(int32), parameter :: beta_evaluations = 2000000
        integer(int32), parameter :: hcf_evaluations = 1000000
        integer(int32), parameter :: capacity_gets = 10000000
        integer(int32), parameter :: capacity_scans = 100000
        integer(int32), parameter :: wrf_initializations = 2000
        integer(int32), parameter :: vapor_evaluations = 20000
        real(real64), parameter :: rho_g = 1000.0d0 * 9.80665d0
        type(type_mkl_regularized_incomplete_beta) :: incomplete_beta
        type(type_config_wrf) :: wrf_config
        type(type_config_hcf) :: hcf_config
        type(holder_wrfs) :: wrf
        type(holder_hcfs) :: hcf_beta, hcf_closed
        type(type_state) :: state
        type(type_iapws97), target :: water
        type(type_iapws06), target :: ice
        type(type_evaporation) :: evaporation
        integer(int64) :: clock_start, clock_end, clock_rate
        integer(int32) :: i, j
        real(real64) :: value, x, pressure, checksum, elapsed_seconds
        real(real64) :: h_abs_min, h_abs_max, h_abs, fraction
        real(real64) :: derivative, maximum_derivative, x_peak
        real(real64) :: rh, dvapor_dP, dvapor_dT
        logical :: converged

        call system_clock(count_rate=clock_rate)
        call water%initialize()

        call incomplete_beta%initialize(0.2d0 + 1.0d0 / 1.48d0, 1.0d0 - 1.0d0 / 1.48d0)
        checksum = 0.0d0
        call system_clock(clock_start)
        do i = 1, beta_evaluations
            x = 0.01d0 + 0.98d0 * real(mod(i, 997), real64) / 996.0d0
            value = 0.0d0
            converged = .false.
            call incomplete_beta%evaluate(x, value, converged)
            if (converged) checksum = checksum + value
        end do
        call system_clock(clock_end)
        elapsed_seconds = real(clock_end - clock_start, real64) / real(clock_rate, real64)
        call self%report_benchmark("incomplete beta evaluate", elapsed_seconds, beta_evaluations, checksum)

        call self%configure_wrf(wrf_config, SWCC_MODELS%VG%ID)
        call wrf%initialize(wrf_config)
        checksum = 0.0d0
        call system_clock(clock_start)
        do i = 1, capacity_gets
            value = 0.0d0
            call wrf%p%calc_lscheme_capacity(value)
            checksum = checksum + value
        end do
        call system_clock(clock_end)
        elapsed_seconds = real(clock_end - clock_start, real64) / real(clock_rate, real64)
        call self%report_benchmark("cached capacity get", elapsed_seconds, capacity_gets, checksum)

        h_abs_min = min(abs(wrf_config%h_crit), 1.0d0 / wrf_config%alpha1, 1.0d0 / wrf_config%alpha2) * 1.0d-4
        h_abs_max = max(abs(wrf_config%h_crit), 1.0d0 / wrf_config%alpha1, 1.0d0 / wrf_config%alpha2) * 1.0d4
        checksum = 0.0d0
        call system_clock(clock_start)
        do i = 1, capacity_scans
            maximum_derivative = 0.0d0
            do j = 0, 96
                fraction = real(j, real64) / 96.0d0
                h_abs = exp(log(h_abs_min) + fraction * (log(h_abs_max) - log(h_abs_min)))
                derivative = 0.0d0
                call wrf%p%deriv(-h_abs, derivative)
                maximum_derivative = max(maximum_derivative, derivative)
            end do
            x_peak = ((wrf_config%n1 - 1.0d0) / &
                      (wrf_config%m1 * wrf_config%n1 + 1.0d0))**(1.0d0 / wrf_config%n1)
            derivative = 0.0d0
            call wrf%p%deriv(-x_peak / wrf_config%alpha1, derivative)
            maximum_derivative = max(maximum_derivative, derivative)
            x_peak = ((wrf_config%n2 - 1.0d0) / &
                      (wrf_config%m2 * wrf_config%n2 + 1.0d0))**(1.0d0 / wrf_config%n2)
            derivative = 0.0d0
            call wrf%p%deriv(-x_peak / wrf_config%alpha2, derivative)
            maximum_derivative = max(maximum_derivative, derivative)
            derivative = 0.0d0
            call wrf%p%deriv(wrf_config%h_crit * (1.0d0 + 1.0d-8), derivative)
            maximum_derivative = max(maximum_derivative, derivative)
            checksum = checksum + maximum_derivative / rho_g
        end do
        call system_clock(clock_end)
        elapsed_seconds = real(clock_end - clock_start, real64) / real(clock_rate, real64)
        call self%report_benchmark("legacy-equivalent capacity scan", elapsed_seconds, capacity_scans, checksum)

        checksum = 0.0d0
        call system_clock(clock_start)
        do i = 1, wrf_initializations
            call wrf%initialize(wrf_config)
            value = 0.0d0
            call wrf%p%calc_lscheme_capacity(value)
            checksum = checksum + value
        end do
        call system_clock(clock_end)
        elapsed_seconds = real(clock_end - clock_start, real64) / real(clock_rate, real64)
        call self%report_benchmark("WRF initialize and capacity scan", elapsed_seconds, wrf_initializations, checksum)

        call evaporation%initialize(water)
        call state%temperature%set(-1.0d0)
        call state%pressure%set(-10000.0d0)
        call state%air_content%set(0.2d0)
        call state%dQa_dP%set(-1.0d-7)
        call state%dQa_dT%set(1.0d-3)
        call evaporation%calc_relative_humidity(state, rh)
        call state%relative_humidity%set(rh)

        checksum = 0.0d0
        call system_clock(clock_start)
        do i = 1, vapor_evaluations
            value = 0.0d0
            dvapor_dP = 0.0d0
            dvapor_dT = 0.0d0
            call evaporation%calc_vapor_content(state, value)
            call evaporation%calc_vapor_content_derivatives(state, dvapor_dP, dvapor_dT)
            checksum = checksum + value + dvapor_dP + dvapor_dT
        end do
        call system_clock(clock_end)
        elapsed_seconds = real(clock_end - clock_start, real64) / real(clock_rate, real64)
        call self%report_benchmark("separate vapor value and derivatives", elapsed_seconds, vapor_evaluations, checksum)

        checksum = 0.0d0
        call system_clock(clock_start)
        do i = 1, vapor_evaluations
            value = 0.0d0
            dvapor_dP = 0.0d0
            dvapor_dT = 0.0d0
            call evaporation%calc_vapor_content_with_derivatives(state, value, dvapor_dP, dvapor_dT)
            checksum = checksum + value + dvapor_dP + dvapor_dT
        end do
        call system_clock(clock_end)
        elapsed_seconds = real(clock_end - clock_start, real64) / real(clock_rate, real64)
        call self%report_benchmark("combined vapor value and derivatives", elapsed_seconds, vapor_evaluations, checksum)

        call hcf_config%reset()
        hcf_config%model = HCF_MODES%BASE
        hcf_config%swcc_model = SWCC_MODELS%VG
        hcf_config%k_sat = 1.0d0
        hcf_config%l = 0.5d0
        hcf_config%alpha1 = 2.0d0
        hcf_config%n1 = 1.48d0
        hcf_config%m1 = 0.2d0
        call hcf_beta%initialize(hcf_config, water, ice)

        hcf_config%n1 = 2.0d0
        hcf_config%m1 = 0.5d0
        call hcf_closed%initialize(hcf_config, water, ice)

        pressure = -rho_g
        call state%pressure%set(pressure)
        checksum = 0.0d0
        call system_clock(clock_start)
        do i = 1, hcf_evaluations
            value = 0.0d0
            call hcf_closed%p%calc_Kflh(state, value)
            checksum = checksum + value
        end do
        call system_clock(clock_end)
        elapsed_seconds = real(clock_end - clock_start, real64) / real(clock_rate, real64)
        call self%report_benchmark("HCF VG closed form", elapsed_seconds, hcf_evaluations, checksum)

        checksum = 0.0d0
        call system_clock(clock_start)
        do i = 1, hcf_evaluations
            value = 0.0d0
            call hcf_beta%p%calc_Kflh(state, value)
            checksum = checksum + value
        end do
        call system_clock(clock_end)
        elapsed_seconds = real(clock_end - clock_start, real64) / real(clock_rate, real64)
        call self%report_benchmark("HCF VG incomplete beta", elapsed_seconds, hcf_evaluations, checksum)
    end subroutine benchmark_hot_paths

    subroutine report_benchmark(self, name, elapsed_seconds, evaluations, checksum)
        implicit none
        class(type_constitutive_test_suite), intent(in) :: self
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: elapsed_seconds
        integer(int32), intent(in) :: evaluations
        real(real64), intent(in) :: checksum

        real(real64) :: nanoseconds_per_evaluation

        nanoseconds_per_evaluation = elapsed_seconds * 1.0d9 / real(evaluations, real64)
        write (output_unit, '(A,A,A,F10.6,A,F12.3,A,ES16.8)') &
            "BENCH: ", trim(name), " total[s]=", elapsed_seconds, &
            " ns/eval=", nanoseconds_per_evaluation, " checksum=", checksum
    end subroutine report_benchmark

    subroutine configure_wrf(self, config, model_id)
        implicit none
        class(type_constitutive_test_suite), intent(in) :: self
        type(type_config_wrf), intent(inout) :: config
        integer(int32), intent(in) :: model_id

        call config%reset()
        config%material_id = 1
        config%swcc_model = SWCC_MODELS%to_object(model_id)
        config%theta_s = 0.45d0
        config%theta_r = 0.05d0
        config%alpha1 = 2.0d0
        config%n1 = 2.0d0
        config%m1 = 0.5d0
        config%h_crit = -0.05d0
        config%alpha2 = 0.2d0
        config%n2 = 3.0d0
        config%m2 = 2.0d0 / 3.0d0
        config%w1 = 0.4d0
        config%w2 = 0.6d0

        if (model_id == SWCC_MODELS%BC%ID .or. model_id == SWCC_MODELS%KO%ID) then
            config%alpha1 = -0.5d0
            config%n1 = 2.0d0
        end if
    end subroutine configure_wrf

    subroutine check_close(self, name, actual, expected, relative_tolerance)
        implicit none
        class(type_constitutive_test_suite), intent(inout) :: self
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: actual
        real(real64), intent(in) :: expected
        real(real64), intent(in) :: relative_tolerance

        real(real64) :: scale

        scale = max(1.0d-14, abs(actual), abs(expected))
        if (abs(actual - expected) <= relative_tolerance * scale) then
            write (output_unit, '(A)') "PASS: "//trim(name)
        else
            self%failures = self%failures + 1
            write (error_unit, '(A)') "FAIL: "//trim(name)
            write (error_unit, '(A,ES24.16)') "  actual   = ", actual
            write (error_unit, '(A,ES24.16)') "  expected = ", expected
        end if
    end subroutine check_close

    subroutine check_true(self, name, condition)
        implicit none
        class(type_constitutive_test_suite), intent(inout) :: self
        character(len=*), intent(in) :: name
        logical, intent(in) :: condition

        if (condition) then
            write (output_unit, '(A)') "PASS: "//trim(name)
        else
            self%failures = self%failures + 1
            write (error_unit, '(A)') "FAIL: "//trim(name)
        end if
    end subroutine check_true

end module test_constitutive_suite

program test_constitutive
    use :: test_constitutive_suite, only: type_constitutive_test_suite
    implicit none

    type(type_constitutive_test_suite) :: suite

    call suite%run()
end program test_constitutive
