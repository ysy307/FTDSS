!> Numerical kernel for depth-one Anderson acceleration of a coupled pair.
module control_acceleration_aa1_coupled
    use, intrinsic :: iso_fortran_env, only:real64
    implicit none
    private

    public :: compute_coupled_aa1_coefficient
    public :: compute_aa1_coefficient
    public :: coupled_weighted_norm

contains

    !> Compute the AA(1) least-squares coefficient for one field.
    pure subroutine compute_aa1_coefficient(g, g_prev, gamma_limit, gamma, usable)
        implicit none
        real(real64), intent(in) :: g(:), g_prev(:)
        real(real64), intent(in) :: gamma_limit
        real(real64), intent(inout) :: gamma
        logical, intent(inout) :: usable

        real(real64) :: current_norm_sq, predicted_norm_sq
        real(real64) :: numerator, denominator
        real(real64) :: delta(size(g))

        gamma = 0.0d0
        usable = .false.
        if (size(g) /= size(g_prev)) return

        delta = g - g_prev
        numerator = dot_product(g, delta)
        denominator = dot_product(delta, delta)
        current_norm_sq = dot_product(g, g)
        if (denominator <= epsilon(1.0d0) * max(current_norm_sq, 1.0d0)) return

        gamma = numerator / denominator
        if (.not. (gamma == gamma .and. abs(gamma) < huge(1.0d0))) then
            gamma = 0.0d0
            return
        end if
        gamma = max(-abs(gamma_limit), min(abs(gamma_limit), gamma))

        predicted_norm_sq = dot_product(g - gamma * delta, g - gamma * delta)
        if (predicted_norm_sq > (1.0d0 + 100.0d0 * epsilon(1.0d0)) * current_norm_sq) then
            gamma = 0.0d0
            return
        end if
        usable = .true.
    end subroutine compute_aa1_coefficient

    !> Compute the AA(1) least-squares coefficient for two scaled fields.
    pure subroutine compute_coupled_aa1_coefficient(g_first, g_second, g_first_prev, g_second_prev, &
                                                    second_scale, gamma_limit, gamma, usable)
        implicit none
        real(real64), intent(in) :: g_first(:), g_second(:)
        real(real64), intent(in) :: g_first_prev(:), g_second_prev(:)
        real(real64), intent(in) :: second_scale, gamma_limit
        real(real64), intent(inout) :: gamma
        logical, intent(inout) :: usable

        real(real64) :: current_norm_sq, predicted_norm_sq
        real(real64) :: numerator, denominator, delta
        integer :: i

        gamma = 0.0d0
        usable = .false.
        if (size(g_first) /= size(g_first_prev)) return
        if (size(g_second) /= size(g_second_prev)) return

        numerator = 0.0d0
        denominator = 0.0d0
        current_norm_sq = 0.0d0
        do i = 1, size(g_first)
            delta = g_first(i) - g_first_prev(i)
            numerator = numerator + g_first(i) * delta
            denominator = denominator + delta * delta
            current_norm_sq = current_norm_sq + g_first(i) * g_first(i)
        end do
        do i = 1, size(g_second)
            delta = (g_second(i) - g_second_prev(i)) * second_scale
            numerator = numerator + g_second(i) * second_scale * delta
            denominator = denominator + delta * delta
            current_norm_sq = current_norm_sq + (g_second(i) * second_scale)**2
        end do

        if (denominator <= epsilon(1.0d0) * max(current_norm_sq, 1.0d0)) return
        gamma = numerator / denominator
        if (.not. (gamma == gamma .and. abs(gamma) < huge(1.0d0))) then
            gamma = 0.0d0
            return
        end if
        gamma = max(-abs(gamma_limit), min(abs(gamma_limit), gamma))

        predicted_norm_sq = 0.0d0
        do i = 1, size(g_first)
            delta = g_first(i) - gamma * (g_first(i) - g_first_prev(i))
            predicted_norm_sq = predicted_norm_sq + delta * delta
        end do
        do i = 1, size(g_second)
            delta = (g_second(i) - gamma * (g_second(i) - g_second_prev(i))) * second_scale
            predicted_norm_sq = predicted_norm_sq + delta * delta
        end do

        ! Clipping toward gamma=0 should preserve the least-squares decrease.
        ! Reject only round-off or invalid arithmetic, not an oscillatory input.
        if (predicted_norm_sq > (1.0d0 + 100.0d0 * epsilon(1.0d0)) * current_norm_sq) then
            gamma = 0.0d0
            return
        end if
        usable = .true.
    end subroutine compute_coupled_aa1_coefficient

    !> Euclidean norm of a pair after scaling its second field.
    pure function coupled_weighted_norm(first, second, second_scale) result(norm)
        implicit none
        real(real64), intent(in) :: first(:), second(:), second_scale
        real(real64) :: norm

        norm = sqrt(sum(first * first) + sum((second * second_scale)**2))
    end function coupled_weighted_norm

end module control_acceleration_aa1_coupled
