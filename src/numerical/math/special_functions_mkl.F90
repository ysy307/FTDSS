!> Numerical special functions constructed from oneMKL primitives.
module numerical_special_functions_mkl
    use, intrinsic :: iso_c_binding, only: c_double, c_int
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    private

    public :: type_mkl_regularized_incomplete_beta

    !> oneMKL-normalized regularized incomplete-beta evaluator for fixed \(a,b\).
    type :: type_mkl_regularized_incomplete_beta
        private
        real(real64) :: a = 0.0d0
        real(real64) :: b = 0.0d0
        real(real64) :: log_normalization = 0.0d0
        logical :: initialized = .false.
    contains
        procedure, pass(self), public :: initialize
        procedure, pass(self), public :: evaluate
        procedure, nopass, private :: beta_continued_fraction
    end type type_mkl_regularized_incomplete_beta

    interface
        !> oneMKL VML logarithm of the absolute gamma function.
        subroutine mkl_vd_lgamma(n, input, result) bind(C, name="vdLGamma")
            import :: c_double, c_int
            implicit none
            integer(c_int), value, intent(in) :: n
            real(c_double), intent(in) :: input(*)
            real(c_double), intent(inout) :: result(*)
        end subroutine mkl_vd_lgamma
    end interface

contains

    !> Cache the oneMKL VML LGamma normalization for fixed \(a,b\).
    subroutine initialize(self, a, b)
        implicit none
        class(type_mkl_regularized_incomplete_beta), intent(inout) :: self
        real(real64), intent(in) :: a
        real(real64), intent(in) :: b

        real(c_double) :: gamma_input(3)
        real(c_double) :: log_gamma_values(3)

        self%a = a
        self%b = b
        self%log_normalization = 0.0d0
        self%initialized = a > 0.0d0 .and. b > 0.0d0
        if (.not. self%initialized) return

        gamma_input = [a + b, a, b]
        log_gamma_values = 0.0d0
        call mkl_vd_lgamma(3_c_int, gamma_input, log_gamma_values)
        self%log_normalization = log_gamma_values(1) - log_gamma_values(2) - log_gamma_values(3)
    end subroutine initialize

    !> Compute the regularized incomplete beta function \(I_x(a,b)\).
    subroutine evaluate(self, x, result, converged)
        implicit none
        class(type_mkl_regularized_incomplete_beta), intent(in) :: self
        real(real64), intent(in) :: x
        real(real64), intent(inout) :: result
        logical, intent(inout), optional :: converged

        real(real64) :: beta_factor
        real(real64) :: continued_fraction
        logical :: fraction_converged

        if (present(converged)) converged = .true.
        if (x <= 0.0d0) then
            result = 0.0d0
            return
        end if
        if (x >= 1.0d0) then
            result = 1.0d0
            return
        end if
        if (.not. self%initialized) then
            result = 0.0d0
            if (present(converged)) converged = .false.
            return
        end if

        beta_factor = exp(self%log_normalization + self%a * log(x) + self%b * log(1.0d0 - x))
        if (x < (self%a + 1.0d0) / (self%a + self%b + 2.0d0)) then
            call self%beta_continued_fraction(self%a, self%b, x, continued_fraction, fraction_converged)
            result = beta_factor * continued_fraction / self%a
        else
            call self%beta_continued_fraction(self%b, self%a, 1.0d0 - x, continued_fraction, fraction_converged)
            result = 1.0d0 - beta_factor * continued_fraction / self%b
        end if
        result = min(1.0d0, max(0.0d0, result))
        if (present(converged)) converged = fraction_converged
    end subroutine evaluate

    !> Evaluate the incomplete-beta continued fraction using modified Lentz iteration.
    subroutine beta_continued_fraction(a, b, x, result, converged)
        implicit none
        real(real64), intent(in) :: a
        real(real64), intent(in) :: b
        real(real64), intent(in) :: x
        real(real64), intent(inout) :: result
        logical, intent(inout) :: converged

        integer(int32), parameter :: max_iterations = 200
        real(real64), parameter :: convergence_tolerance = 3.0d-15
        real(real64), parameter :: minimum_fraction = 1.0d-300
        real(real64) :: a_plus_b, a_plus_one, a_minus_one
        real(real64) :: coefficient, numerator, denominator, delta
        integer(int32) :: iteration, twice_iteration

        converged = .false.
        a_plus_b = a + b
        a_plus_one = a + 1.0d0
        a_minus_one = a - 1.0d0
        numerator = 1.0d0
        denominator = 1.0d0 - a_plus_b * x / a_plus_one
        if (abs(denominator) < minimum_fraction) denominator = minimum_fraction
        denominator = 1.0d0 / denominator
        result = denominator

        do iteration = 1, max_iterations
            twice_iteration = 2 * iteration
            coefficient = real(iteration, real64) * (b - real(iteration, real64)) * x / &
                          ((a_minus_one + real(twice_iteration, real64)) * &
                           (a + real(twice_iteration, real64)))
            denominator = 1.0d0 + coefficient * denominator
            if (abs(denominator) < minimum_fraction) denominator = minimum_fraction
            numerator = 1.0d0 + coefficient / numerator
            if (abs(numerator) < minimum_fraction) numerator = minimum_fraction
            denominator = 1.0d0 / denominator
            result = result * denominator * numerator

            coefficient = -(a + real(iteration, real64)) * &
                          (a_plus_b + real(iteration, real64)) * x / &
                          ((a + real(twice_iteration, real64)) * &
                           (a_plus_one + real(twice_iteration, real64)))
            denominator = 1.0d0 + coefficient * denominator
            if (abs(denominator) < minimum_fraction) denominator = minimum_fraction
            numerator = 1.0d0 + coefficient / numerator
            if (abs(numerator) < minimum_fraction) numerator = minimum_fraction
            denominator = 1.0d0 / denominator
            delta = denominator * numerator
            result = result * delta
            if (abs(delta - 1.0d0) < convergence_tolerance) then
                converged = .true.
                return
            end if
        end do
    end subroutine beta_continued_fraction

end module numerical_special_functions_mkl
