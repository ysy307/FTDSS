program test_ats
    use, intrinsic :: iso_fortran_env, only:error_unit, output_unit, real64
    use :: module_core, only:type_config_time_ats
    use :: control_time_ats, only:type_ats
    use :: control_acceleration_aa1_coupled, only:compute_aa1_coefficient, compute_coupled_aa1_coefficient
    implicit none

    type(type_config_time_ats) :: config
    type(type_ats) :: ats
    real(real64) :: next_dt, expected, gamma
    real(real64) :: g_temperature(1), g_pressure(1)
    real(real64) :: g_temperature_prev(1), g_pressure_prev(1)
    logical :: aa_usable
    integer :: failures

    failures = 0
    config%active = .true.
    config%use_error_control = .true.
    config%safety_factor = 0.9d0
    config%max_growth_rate = 2.0d0
    config%dt_min = 1.0d-6
    config%dt_max = 1.0d6
    config%pi_k_i = 0.15d0
    config%pi_k_p = 0.20d0
    config%error_rtol = 1.0d-2
    call ats%initialize(config)

    next_dt = -1.0d0
    call ats%pi_controller_dt(10.0d0, 1.0d0, next_dt)
    call check_close("normalized PI error is not divided by rtol twice", next_dt, 9.0d0, 1.0d-13, failures)

    call ats%initialize(config)
    next_dt = -1.0d0
    call ats%lte_retry_dt(10.0d0, 4.0d0, next_dt)
    expected = 10.0d0 * min(0.8d0, 0.9d0 / sqrt(4.0d0))
    call check_close("BDF1 LTE rejection uses the square-root controller", next_dt, expected, 1.0d-13, failures)

    next_dt = -1.0d0
    call ats%pi_controller_dt(10.0d0, 1.0d0, next_dt)
    call check_close("rejected LTE step does not advance PI history", next_dt, 9.0d0, 1.0d-13, failures)

    call ats%initialize(config)
    next_dt = -1.0d0
    call ats%lte_retry_dt(10.0d0, 100.0d0, next_dt)
    call check_close("LTE retry reduction is bounded below", next_dt, 1.0d0, 1.0d-13, failures)

    next_dt = -1.0d0
    call ats%lte_retry_dt(10.0d0, 1.01d0, next_dt)
    call check_close("LTE retry reduction is bounded above", next_dt, 8.0d0, 1.0d-13, failures)

    g_temperature = -2.0d0
    g_temperature_prev = 2.0d0
    g_pressure = 0.0d0
    g_pressure_prev = 0.0d0
    gamma = 0.0d0
    aa_usable = .false.
    call compute_coupled_aa1_coefficient(g_temperature, g_pressure, g_temperature_prev, g_pressure_prev, &
                                         1.0d0, 2.0d0, gamma, aa_usable)
    call check_true("AA(1) accepts a period-two fixed-point residual", aa_usable, failures)
    call check_close("AA(1) cancels a period-two residual with gamma one half", &
                     gamma, 0.5d0, 1.0d-13, failures)

    gamma = 0.0d0
    aa_usable = .false.
    call compute_aa1_coefficient(g_temperature, g_temperature_prev, 2.0d0, gamma, aa_usable)
    call check_true("single-field AA(1) accepts a phase period-two residual", aa_usable, failures)
    call check_close("single-field AA(1) cancels a phase period-two residual", &
                     gamma, 0.5d0, 1.0d-13, failures)

    if (failures > 0) then
        write (error_unit, '(A,I0)') "FAILED test_ats checks: ", failures
        error stop 1
    end if
    write (output_unit, '(A)') "All test_ats checks passed."

contains

    subroutine check_close(label, actual, reference, tolerance, count)
        implicit none
        character(len=*), intent(in) :: label
        real(real64), intent(in) :: actual, reference, tolerance
        integer, intent(inout) :: count

        if (abs(actual - reference) <= tolerance * max(1.0d0, abs(reference))) then
            write (output_unit, '(A)') "PASS: "//trim(label)
        else
            count = count + 1
            write (error_unit, '(A,2(1X,ES16.8))') "FAIL: "//trim(label), actual, reference
        end if
    end subroutine check_close

    subroutine check_true(label, actual, count)
        implicit none
        character(len=*), intent(in) :: label
        logical, intent(in) :: actual
        integer, intent(inout) :: count

        if (actual) then
            write (output_unit, '(A)') "PASS: "//trim(label)
        else
            count = count + 1
            write (error_unit, '(A)') "FAIL: "//trim(label)
        end if
    end subroutine check_true

end program test_ats
