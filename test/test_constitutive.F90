module test_constitutive_suite
    use, intrinsic :: iso_fortran_env, only: error_unit, int32, output_unit, real64
    use :: iapws, only: type_iapws06, type_iapws97
    use :: module_core, only: HCF_MODES, PHYSICS_UNITS, SWCC_MODELS, type_config_hcf, type_config_wrf, type_state
    use :: module_input, only: input_translator, type_input
    use :: models_hcf, only: holder_hcfs
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
