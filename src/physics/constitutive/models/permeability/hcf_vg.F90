submodule(models_hcf) hcf_vg
    implicit none
contains

    !> Cache the fixed incomplete-beta normalization for this material.
    module subroutine initialize_hcf_base_vg(self)
        implicit none
        class(type_hcf_base_vg), intent(inout) :: self

        real(real64) :: p, q

        associate (params => self%parent%config)
            if (params%n1 > 1.0d0) then
                p = params%m1 + 1.0d0 / params%n1
                q = 1.0d0 - 1.0d0 / params%n1
            else
                p = 0.0d0
                q = 0.0d0
            end if
            call self%incomplete_beta%initialize(p, q)
        end associate
    end subroutine initialize_hcf_base_vg

    !> @brief Mualem relative permeability for the van Genuchten retention model.
    !>
    !> Mathematical definition (van Genuchten et al., 1991; Hansson et al., 2004,
    !> Eq. [3]) for INDEPENDENT \(m\) and \(n\):
    !> \[ k_r = S_e^{l}\,\bigl[I_{\zeta}(m + 1/n,\; 1 - 1/n)\bigr]^2, \qquad
    !>    \zeta = S_e^{1/m} \]
    !> where \(I_x(a,b)\) is the regularized incomplete beta function.  Only when
    !> \(m = 1 - 1/n\) does this reduce to the closed form
    !> \(k_r = S_e^{l}[1-(1-S_e^{1/m})^m]^2\); using the closed form with an
    !> independent \(m\) (e.g. the Kanagawa sandy loam fit m=0.2, n=1.48)
    !> underestimates \(k_r\) by up to an order of magnitude in the wet-to-medium
    !> suction range and correspondingly suppresses cryosuction redistribution.
    !>
    !> Assumptions: \(n > 1\) (otherwise the Mualem integral is ill-defined and
    !> the closed form is used as a fallback).  Computational complexity: O(1);
    !> the continued fraction converges in a few dozen terms.  Failure behavior:
    !> returns the closed-form value if the beta continued fraction fails to
    !> converge (does not abort).
    module subroutine calc_kr_base_vg(self, h, kr)
        implicit none
        class(type_hcf_base_vg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        real(real64), parameter :: m_consistent_tolerance = 1.0d-9
        real(real64) :: effective_saturation, zeta, beta_value
        logical :: converged

        associate (params => self%parent%config)
            if (h < 0.0d0) then
                effective_saturation = (1.0d0 + (-params%alpha1 * h)**params%n1)**(-params%m1)
            else
                effective_saturation = 1.0d0
            end if

            if (abs(params%m1 - (1.0d0 - 1.0d0 / params%n1)) <= m_consistent_tolerance .or. &
                params%n1 <= 1.0d0) then
                kr = effective_saturation**params%l * &
                     (1.0d0 - (1.0d0 - effective_saturation**(1.0d0 / params%m1))**params%m1)**2
            else
                zeta = effective_saturation**(1.0d0 / params%m1)
                beta_value = 0.0d0
                converged = .false.
                call self%incomplete_beta%evaluate(zeta, beta_value, converged)
                if (converged) then
                    kr = effective_saturation**params%l * beta_value**2
                else
                    kr = effective_saturation**params%l * &
                         (1.0d0 - (1.0d0 - effective_saturation**(1.0d0 / params%m1))**params%m1)**2
                end if
            end if
        end associate
    end subroutine calc_kr_base_vg

end submodule hcf_vg
