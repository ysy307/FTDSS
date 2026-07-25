module models_phase_change_fusion
    use, intrinsic :: iso_fortran_env
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state
    use :: constitutive_constants, only: &
        Tf0 => water_freezing_point_at_standard_atmospheric_pressure, &
        g => gravity_acceleration, rho_std => reference_water_density, &
        SUCTION_BLEND_EPS => suction_blend_epsilon, &
        Lf0 => latent_heat_fusion_water_0C, &
        TtoK => celsius_to_kelvin
    use :: physics_constitutive_base, only:abst_constitutive
    use :: models_wrf, only:abst_wrf
    use :: models_phase_change_gcc, only:abst_gcc
    implicit none
    private

    public :: type_fusion

    !>
    !> @brief Model for fusion (melting/freezing) physics.
    !>
    type, extends(abst_constitutive) :: type_fusion
        private
        class(abst_wrf), pointer :: wrf
        class(abst_gcc), pointer :: gcc
    contains
        procedure, pass(self), public :: initialize => initialize_type_fusion
        procedure, pass(self), public :: calc_ice_content
        procedure, pass(self), public :: calc_ice_content_derivatives
        procedure, pass(self), public :: calc_water_content
        procedure, pass(self), public :: calc_water_content_derivatives
        procedure, pass(self), public :: calc_cryo_head_dT
        procedure, pass(self), public :: calc_effective_suction
        procedure, pass(self), public :: project_ice_content
        procedure, pass(self), public :: calc_conserved_target
        procedure, pass(self), public :: solve_local_conserved_equilibrium
        procedure, pass(self), public :: calc_saturation_pressure
        procedure, pass(self), public :: deriv_pressure_ice_water

    end type type_fusion

contains

    pure subroutine compute_effective_suction(psi_cap, psi_cryo, psi_eff, dpsi_eff_dpsi_cap, dpsi_eff_dpsi_cryo)
        implicit none
        real(real64), intent(in) :: psi_cap, psi_cryo
        real(real64), intent(inout) :: psi_eff
        real(real64), intent(inout), optional :: dpsi_eff_dpsi_cap, dpsi_eff_dpsi_cryo

        ! Total soil-water suction as the SUPERPOSITION of the two
        ! chemical-potential lowerings acting on the liquid: the air-water
        ! capillary suction psi_cap and the ice-water cryogenic suction
        ! psi_cryo add. The water chemical potential is
        !   mu_w = mu_w^sat - v_w * psi_eff,   psi_eff = psi_cap + psi_cryo,
        ! so the single retention relation S_w = F_WRF(psi_eff) holds for both
        ! unfrozen and frozen states, and - crucially - the Darcy flux is
        ! driven by grad(mu_w) ~ grad(psi_eff), which carries BOTH the
        ! pressure gradient and the cryogenic (temperature) gradient. A max()
        ! would drop whichever term is smaller: in the frozen zone that
        ! discards the pore-pressure contribution, decoupling the pressure
        ! from transport (singular d/dp) and removing the cryosuction driving
        ! force. Addition keeps d psi_eff/d psi_cap = 1 everywhere, so the
        ! pressure diagonal stays well posed while the cryogenic gradient
        ! migrates water to the freezing front. Above freezing psi_cryo = 0
        ! and psi_eff reduces to the ordinary capillary suction.
        psi_eff = psi_cap + psi_cryo

        if (present(dpsi_eff_dpsi_cap)) dpsi_eff_dpsi_cap = 1.0d0
        if (present(dpsi_eff_dpsi_cryo)) dpsi_eff_dpsi_cryo = 1.0d0
    end subroutine compute_effective_suction

    !>
    !> @brief Initialize fusion model.
    !>
    subroutine initialize_type_fusion(self, wrf, gcc, water, ice)
        implicit none
        class(type_fusion), intent(inout) :: self
        class(abst_wrf), intent(in), target :: wrf
        class(abst_gcc), intent(in), target :: gcc
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        self%wrf => wrf
        self%gcc => gcc
        self%water => water
        self%ice => ice
    end subroutine initialize_type_fusion

    !---------------------------------------------------------------------------
    ! Ice Calculations
    !---------------------------------------------------------------------------

    !>
    !> @brief Calculate ice content based on thermodynamic state.
    !>
    !> \[ \theta_{tot} = \theta_{WRF}(-\psi_{cap}) + \frac{\rho_i}{\rho_l}\theta_i \]
    !> \[ \theta_{l,new} = \theta_{WRF}(-\psi_{eff}) \]
    !> \[ \theta_i = \left(\theta_{tot} - \theta_{l,new}\right)\frac{\rho_l}{\rho_i} \]
    !> \(\theta_{WRF}(-\psi_{cap})\) approximates the post-hydraulic-solve liquid content
    !> at current pressure without cryogenic suction, avoiding dependence on the stale
    !> nodal water content field (updated only after convergence).
    !> Assumptions: \(T < 0\) for phase change; \(\theta_i\) from state is the previous-iteration value.
    !> Numerical guarantee: No theoretical error bound available.
    !> Computational complexity: \(O(1)\) arithmetic and memory.
    !> Failure behavior: returns zero ice content when \(T \ge 0\) or \(\theta_{l,new} \ge \theta_{tot}\).
    subroutine calc_ice_content(self, state, ice_content)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: ice_content

        real(real64) :: pressure, temperature
        real(real64) :: psi_cap, psi_cryo, psi_eff
        real(real64) :: theta_l_cap, theta_l_new
        real(real64) :: rho_w, rho_i
        real(real64) :: theta_tot, phi

        call state%temperature%get(temperature)
        call state%pressure%get(pressure)

        if (temperature < 0.0d0) then
            call self%calc_rho_water(state, rho_w)
            call self%calc_rho_ice(state, rho_i)

            if (rho_w <= tiny(1.0d0) .or. rho_i <= tiny(1.0d0)) then
                ice_content = 0.0d0
                return
            end if

            psi_cap = max(0.0d0, -pressure)
            call self%wrf%calc(-psi_cap / (rho_std * g), theta_l_cap)

            call self%gcc%calc(state, psi_cryo)
            call compute_effective_suction(psi_cap, psi_cryo, psi_eff)
            call self%wrf%calc(-psi_eff / (rho_std * g), theta_l_new)

            ! Pore-volume bound on the water-equivalent total theta_tot.
            !
            ! The physical constraint is the instantaneous phase-volume balance
            !   theta_l + theta_i <= phi,
            ! which, with theta_i = (theta_tot - theta_l) * rho_w/rho_i, is
            !   theta_tot <= phi * (rho_i/rho_w) + theta_l * (1 - rho_i/rho_w).
            ! The bound therefore relaxes continuously from phi*(rho_i/rho_w)
            ! (pore entirely ice, theta_l = 0) to phi (pore entirely liquid).
            ! The previous form capped theta_tot at phi*(rho_i/rho_w) uncondition-
            ! ally, i.e. it demanded that the water present could still fit after
            ! freezing ALL of it - far too strict whenever liquid remains. Because
            ! that cap made theta_tot a constant, it also drove the storage
            ! capacity dTheta/dP to zero, so any inflow into a capped node turned
            ! into unbounded pressure growth instead of storage (the +MPa pockets).
            theta_tot = theta_l_cap
            call state%porosity%get(phi)
            if (phi > 0.0d0) then
                theta_tot = min(theta_tot, phi * (rho_i / rho_w) + theta_l_new * (1.0d0 - rho_i / rho_w))
            end if

            if (theta_l_new < theta_tot) then
                ice_content = (theta_tot - theta_l_new) * (rho_w / rho_i)
            else
                ice_content = 0.0d0
            end if
        else
            ice_content = 0.0d0
        end if

    end subroutine calc_ice_content

    !>
    !> @brief Calculate derivatives of ice content w.r.t pressure and temperature.
    !>
    !> \[ \frac{\partial \theta_i}{\partial P} = \frac{\rho_l}{\rho_i}
    !>    \left(\frac{\partial \theta_{l,cap}}{\partial P}
    !>          - \frac{\partial \theta_{l,new}}{\partial P}\right) \]
    !> \[ \frac{\partial \theta_i}{\partial T} = -\frac{\rho_l}{\rho_i}
    !>    \frac{\partial \theta_{l,new}}{\partial T} \]
    !> \(\theta_i\) from state is treated as fixed (previous-iteration value).
    !> Numerical guarantee: No theoretical error bound available.
    !> Computational complexity: \(O(1)\) arithmetic and memory.
    !> Failure behavior: returns zero derivatives when \(T \ge 0\) or \(\theta_{l,new} \ge \theta_{tot}\).
    subroutine calc_ice_content_derivatives(self, state, dice_dP, dice_dT)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: dice_dP
        real(real64), intent(inout) :: dice_dT

        real(real64) :: pressure, temperature
        real(real64) :: psi_cap, psi_cryo, psi_eff
        real(real64) :: d_psi_eff_dpsi_cap, d_psi_eff_dpsi_cryo
        real(real64) :: d_psi_cap_dP, d_psi_cryo_dP, d_psi_cryo_dT
        real(real64) :: d_psi_eff_dP, d_psi_eff_dT
        real(real64) :: theta_l_cap, theta_l_new
        real(real64) :: dtheta_dPin_cap, dtheta_dPin_eff
        real(real64) :: d_theta_cap_dP, d_theta_eff_dP, d_theta_eff_dT
        real(real64) :: rho_w, rho_i
        real(real64) :: theta_tot, phi, d_theta_tot_dP, d_theta_tot_dT, theta_bound

        call state%temperature%get(temperature)
        call state%pressure%get(pressure)

        if (temperature < 0.0d0) then
            call self%calc_rho_water(state, rho_w)
            call self%calc_rho_ice(state, rho_i)

            if (rho_w <= tiny(1.0d0) .or. rho_i <= tiny(1.0d0)) then
                dice_dP = 0.0d0
                dice_dT = 0.0d0
                return
            end if

            if (pressure < 0.0d0) then
                psi_cap = -pressure
                d_psi_cap_dP = -1.0d0
            else
                psi_cap = 0.0d0
                d_psi_cap_dP = 0.0d0
            end if

            call self%wrf%calc(-psi_cap / (rho_std * g), theta_l_cap)
            call self%wrf%deriv(-psi_cap / (rho_std * g), dtheta_dPin_cap)
            d_theta_cap_dP = dtheta_dPin_cap * (-d_psi_cap_dP) / (rho_std * g)

            call self%gcc%calc(state, psi_cryo)
            call self%gcc%deriv_pressure(state, d_psi_cryo_dP)
            call self%gcc%deriv_temperature(state, d_psi_cryo_dT)

            call compute_effective_suction(psi_cap, psi_cryo, psi_eff, d_psi_eff_dpsi_cap, d_psi_eff_dpsi_cryo)
            d_psi_eff_dP = d_psi_eff_dpsi_cap * d_psi_cap_dP + d_psi_eff_dpsi_cryo * d_psi_cryo_dP
            d_psi_eff_dT = d_psi_eff_dpsi_cryo * d_psi_cryo_dT

            call self%wrf%calc(-psi_eff / (rho_std * g), theta_l_new)
            call self%wrf%deriv(-psi_eff / (rho_std * g), dtheta_dPin_eff)

            d_theta_eff_dP = dtheta_dPin_eff * (-d_psi_eff_dP) / (rho_std * g)
            d_theta_eff_dT = dtheta_dPin_eff * (-d_psi_eff_dT) / (rho_std * g)

            ! Pore-volume bound, differentiated consistently with calc_ice_content:
            !   theta_tot <= phi*(rho_i/rho_w) + theta_l*(1 - rho_i/rho_w).
            ! On the bound the total follows the liquid, so
            !   d(theta_tot) = (1 - rho_i/rho_w) * d(theta_l),
            ! and the ice derivatives collapse to dQi = -d(theta_l): further ice
            ! can only replace liquid one-for-one in volume. Off the bound the
            ! total is theta(psi_cap), which has no direct T dependence.
            theta_tot = theta_l_cap
            call state%porosity%get(phi)
            d_theta_tot_dP = d_theta_cap_dP
            d_theta_tot_dT = 0.0d0
            if (phi > 0.0d0) then
                theta_bound = phi * (rho_i / rho_w) + theta_l_new * (1.0d0 - rho_i / rho_w)
                if (theta_l_cap >= theta_bound) then
                    theta_tot = theta_bound
                    d_theta_tot_dP = (1.0d0 - rho_i / rho_w) * d_theta_eff_dP
                    d_theta_tot_dT = (1.0d0 - rho_i / rho_w) * d_theta_eff_dT
                end if
            end if

            if (theta_l_new < theta_tot) then
                dice_dP = (d_theta_tot_dP - d_theta_eff_dP) * (rho_w / rho_i)
                dice_dT = (d_theta_tot_dT - d_theta_eff_dT) * (rho_w / rho_i)
            else
                dice_dP = 0.0d0
                dice_dT = 0.0d0
            end if
        else
            dice_dP = 0.0d0
            dice_dT = 0.0d0
        end if

    end subroutine calc_ice_content_derivatives

    !---------------------------------------------------------------------------
    ! Liquid Water Calculations
    !---------------------------------------------------------------------------

    !>
    !> @brief Calculate liquid water content based on thermodynamic state.
    !>
    subroutine calc_water_content(self, state, water_content)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: water_content

        real(real64) :: pressure
        real(real64) :: psi_cap, psi_cryo, psi_eff

        call state%pressure%get(pressure)

        if (pressure < 0.0d0) then
            psi_cap = -pressure
        else
            psi_cap = 0.0d0
        end if

        ! Generalized-suction (freezing = drying analogy): the unfrozen liquid
        ! content follows the retention curve evaluated at the generalized
        ! suction psi_eff = max(psi_cap, psi_cryo). Below freezing the
        ! cryogenic suction psi_cryo(T) lowers the unfrozen content, which is
        ! what makes theta_l temperature-dependent and supplies the apparent
        ! heat capacity to the energy equation.
        call self%gcc%calc(state, psi_cryo)
        call compute_effective_suction(psi_cap, psi_cryo, psi_eff)
        call self%wrf%calc(-psi_eff / (rho_std * g), water_content)

    end subroutine calc_water_content

    !>
    !> @brief Calculate derivatives of liquid water content w.r.t pressure and temperature.
    !>
    subroutine calc_water_content_derivatives(self, state, dwater_dP, dwater_dT)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: dwater_dP !> d(theta_l)/dP
        real(real64), intent(inout) :: dwater_dT !> d(theta_l)/dT

        real(real64) :: pressure
        real(real64) :: psi_cap, psi_cryo, psi_eff
        real(real64) :: d_psi_cap_dP, d_psi_cryo_dP, d_psi_cryo_dT
        real(real64) :: d_psi_eff_dpsi_cap, d_psi_eff_dpsi_cryo
        real(real64) :: d_psi_eff_dP, d_psi_eff_dT
        real(real64) :: d_theta_liquid_dPress

        call state%pressure%get(pressure)

        ! Capillary suction
        if (pressure < 0.0d0) then
            psi_cap = -pressure
            d_psi_cap_dP = -1.0d0
        else
            psi_cap = 0.0d0
            d_psi_cap_dP = 0.0d0
        end if

        ! Generalized suction psi_eff = max(psi_cap, psi_cryo(T)). The T
        ! dependence flows through psi_cryo, giving a nonzero dwater_dT: this
        ! is the freezing-curve slope that becomes the apparent heat capacity
        ! in the energy equation's C_TT (thermal_coefficients.F90).
        call self%gcc%calc(state, psi_cryo)
        call self%gcc%deriv_pressure(state, d_psi_cryo_dP)
        call self%gcc%deriv_temperature(state, d_psi_cryo_dT)

        call compute_effective_suction(psi_cap, psi_cryo, psi_eff, d_psi_eff_dpsi_cap, d_psi_eff_dpsi_cryo)
        d_psi_eff_dP = d_psi_eff_dpsi_cap * d_psi_cap_dP + d_psi_eff_dpsi_cryo * d_psi_cryo_dP
        d_psi_eff_dT = d_psi_eff_dpsi_cryo * d_psi_cryo_dT

        call self%wrf%deriv(-psi_eff / (rho_std * g), d_theta_liquid_dPress)

        ! Chain rule: dTheta/dX = (dTheta/dh) * (-d_psi_eff/dX) / (rho_std g).
        dwater_dP = d_theta_liquid_dPress * (-d_psi_eff_dP) / (rho_std * g)
        dwater_dT = d_theta_liquid_dPress * (-d_psi_eff_dT) / (rho_std * g)

    end subroutine calc_water_content_derivatives

    !> Temperature derivative of the generalized-suction head, dh/dT [m/K].
    !>
    !> h = -psi_eff/(rho_std g) is the generalized (freezing = drying) head
    !> that drives the liquid Darcy flux (Hansson et al. 2004, Eq. 1). Because
    !> the primary hydraulic unknown here is the capillary pore pressure rather
    !> than h itself, the temperature part of the liquid flux -K_Lh dh/dT dT/dz
    !> - the cryosuction-driven moisture migration toward the freezing front -
    !> must be supplied explicitly as the coupling flux D_HT. In the frozen
    !> zone psi_eff = psi_cryo(T), so dh/dT = -(dpsi_cryo/dT)/(rho_std g) is
    !> large; above freezing psi_cap dominates and dh/dT = 0.
    subroutine calc_cryo_head_dT(self, state, dh_dT)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: dh_dT

        real(real64) :: pressure, psi_cap, psi_cryo, psi_eff
        real(real64) :: d_psi_cryo_dT, d_psi_eff_dpsi_cap, d_psi_eff_dpsi_cryo, d_psi_eff_dT

        call state%pressure%get(pressure)
        psi_cap = max(0.0d0, -pressure)

        call self%gcc%calc(state, psi_cryo)
        call self%gcc%deriv_temperature(state, d_psi_cryo_dT)
        call compute_effective_suction(psi_cap, psi_cryo, psi_eff, d_psi_eff_dpsi_cap, d_psi_eff_dpsi_cryo)
        d_psi_eff_dT = d_psi_eff_dpsi_cryo * d_psi_cryo_dT

        dh_dT = -d_psi_eff_dT / (rho_std * g)
    end subroutine calc_cryo_head_dT

    !>
    !> @brief Generalized suction \(\max(\psi_{cap}, \psi_{cryo})\) [Pa].
    !>
    !> The generalized (freezing = drying) suction combines the capillary
    !> suction of the actual pore pressure with the cryogenic suction from the
    !> generalized Clapeyron relation. It is the single potential that drives
    !> both the unfrozen liquid content (retention) and, through its gradient,
    !> the cryosuction-driven moisture migration toward the freezing front.
    !> Stored on the state so the permeability head (calc_head_hcf) uses the
    !> same potential the content does.
    subroutine calc_effective_suction(self, state, psi_eff)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: psi_eff

        real(real64) :: pressure, psi_cap, psi_cryo

        call state%pressure%get(pressure)
        psi_cap = max(0.0d0, -pressure)
        call self%gcc%calc(state, psi_cryo)
        call compute_effective_suction(psi_cap, psi_cryo, psi_eff)
    end subroutine calc_effective_suction

    !> Project the outer ice state onto local ice-water equilibrium.
    !>
    !> The liquid content is evaluated from the actual pore pressure, while
    !> the equilibrium unfrozen content is evaluated from the Clapeyron
    !> suction. The proposed phase increment is the liquid-equivalent phase
    !> transfer used by the outer water-conserving solve:
    !> \(\Delta\theta_i=(\theta_l(p_w)-\theta_l^{eq}(T))\rho_w/\rho_i\).
    !> Bounds enforce non-negative ice and fixed pore volume. The returned
    !> increment is zero on the admissible side of either active bound.
    !>
    !> The optional \(dice\_dT\), \(dice\_dP\) are the analytic derivatives of
    !> the returned `projected_ice` with respect to the local temperature and
    !> pore pressure, holding `current_ice` fixed. They chain-rule through the
    !> same `wrf%deriv` and `gcc%deriv_temperature`/`deriv_pressure` primitives
    !> already used elsewhere for the monolithic Jacobian (e.g.
    !> `calc_water_content_derivatives`), so no new closed-form relation is
    !> introduced. On an active bound the derivative is that of the bound
    !> itself (zero at the ice-free bound; \(-d(liquid\_equilibrium)/dX\) at
    !> the full-ice bound, since the bound value \(\phi-\theta_l^{eq}(T)\)
    !> still depends on \(T,P\) through the Clapeyron suction).
    subroutine project_ice_content(self, state, projected_ice, ice_increment, equilibrium_error, active_bound, &
                                    dice_dT, dice_dP)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: projected_ice
        real(real64), intent(inout) :: ice_increment
        real(real64), intent(inout) :: equilibrium_error
        integer(int32), intent(inout), optional :: active_bound
        real(real64), intent(inout), optional :: dice_dT
        real(real64), intent(inout), optional :: dice_dP

        real(real64) :: current_ice, liquid_pressure, liquid_equilibrium
        real(real64) :: pressure, psi_cap, psi_cryo, rho_w, rho_i, porosity, upper_bound, unconstrained_ice
        integer(int32) :: bound_state
        real(real64) :: dliquid_dP, dliquid_dT, dtheta_eq_dh, dpsi_cryo_dT, dpsi_cryo_dP
        real(real64) :: dliquid_eq_dT, dliquid_eq_dP
        real(real64) :: drho_w_dT, drho_w_dP, drho_i_dT, drho_i_dP, drho_ratio_dT, drho_ratio_dP
        ! Active-set tolerance for the ice-free/full-ice complementarity
        ! bounds. It matches the outer phase-content discretization tolerance
        ! so a node is not classified simultaneously as bound-converged and
        ! as an interior point requiring pressure equality.
        real(real64), parameter :: BOUND_TOLERANCE = 1.0d-3
        logical :: ice_is_set

        current_ice = 0.0d0
        ice_is_set = .false.
        call state%ice_content%get(current_ice, ice_is_set)
        if (.not. ice_is_set) current_ice = 0.0d0
        call state%porosity%get(porosity)
        call self%calc_water_content(state, liquid_pressure)
        call self%gcc%calc(state, psi_cryo)
        call self%wrf%calc(-max(0.0d0, psi_cryo) / (rho_std * g), liquid_equilibrium)
        call self%calc_rho_water(state, rho_w)
        call self%calc_rho_ice(state, rho_i)

        if (rho_w <= tiny(1.0d0) .or. rho_i <= tiny(1.0d0)) then
            projected_ice = max(0.0d0, current_ice)
            ice_increment = 0.0d0
            equilibrium_error = 0.0d0
            if (present(active_bound)) active_bound = 0
            if (present(dice_dT)) dice_dT = 0.0d0
            if (present(dice_dP)) dice_dP = 0.0d0
            return
        end if

        unconstrained_ice = current_ice + (liquid_pressure - liquid_equilibrium) * rho_w / rho_i
        upper_bound = max(0.0d0, porosity - liquid_equilibrium)
        projected_ice = min(max(unconstrained_ice, 0.0d0), upper_bound)
        ice_increment = projected_ice - current_ice

        bound_state = 0
        if (unconstrained_ice <= 0.0d0) then
            bound_state = -1
        else if (unconstrained_ice >= upper_bound) then
            bound_state = 1
        end if
        if (present(active_bound)) active_bound = bound_state

        call state%pressure%get(pressure)
        psi_cap = max(0.0d0, -pressure)
        equilibrium_error = 0.0d0
        if (abs(ice_increment) > BOUND_TOLERANCE .or. &
            (current_ice > BOUND_TOLERANCE .and. current_ice < upper_bound - BOUND_TOLERANCE)) then
            equilibrium_error = abs(max(0.0d0, psi_cryo) - psi_cap)
        end if

        if (present(dice_dT) .or. present(dice_dP)) then
            ! d(liquid_equilibrium)/dX, X in {T,P}: liquid_equilibrium =
            ! wrf(-max(0,psi_cryo)/(rho_std g)). When psi_cryo<=0 the argument
            ! is pinned at h=0 (saturated branch), independent of T,P.
            dliquid_eq_dT = 0.0d0
            dliquid_eq_dP = 0.0d0
            if (psi_cryo > 0.0d0) then
                call self%wrf%deriv(-psi_cryo / (rho_std * g), dtheta_eq_dh)
                call self%gcc%deriv_temperature(state, dpsi_cryo_dT)
                call self%gcc%deriv_pressure(state, dpsi_cryo_dP)
                dliquid_eq_dT = -dtheta_eq_dh * dpsi_cryo_dT / (rho_std * g)
                dliquid_eq_dP = -dtheta_eq_dh * dpsi_cryo_dP / (rho_std * g)
            end if

            ! d(liquid_pressure)/dX: liquid_pressure = theta_l(p_w), reuses
            ! the same relation as calc_water_content_derivatives (dT is
            ! identically zero there since psi_cap depends only on p_w).
            call self%calc_water_content_derivatives(state, dliquid_dP, dliquid_dT)

            select case (bound_state)
            case (0)
                ! unconstrained_ice also depends on T,P through rho_w/rho_i:
                ! rho_w is pinned below the freezing point (IAPWS-97 branch
                ! in calc_rho_water) but rho_i is not (IAPWS-06 has no such
                ! floor), so d(rho_w/rho_i)/dX is nonzero and must be
                ! included by the product rule, not just the dtheta term.
                call self%calc_drho_water_dT(state, drho_w_dT)
                call self%calc_drho_water_dP(state, drho_w_dP)
                call self%calc_drho_ice_dT(state, drho_i_dT)
                call self%calc_drho_ice_dP(state, drho_i_dP)
                drho_ratio_dT = drho_w_dT / rho_i - rho_w * drho_i_dT / rho_i**2
                drho_ratio_dP = drho_w_dP / rho_i - rho_w * drho_i_dP / rho_i**2
                if (present(dice_dT)) dice_dT = (dliquid_dT - dliquid_eq_dT) * rho_w / rho_i + &
                    (liquid_pressure - liquid_equilibrium) * drho_ratio_dT
                if (present(dice_dP)) dice_dP = (dliquid_dP - dliquid_eq_dP) * rho_w / rho_i + &
                    (liquid_pressure - liquid_equilibrium) * drho_ratio_dP
            case (-1)
                if (present(dice_dT)) dice_dT = 0.0d0
                if (present(dice_dP)) dice_dP = 0.0d0
            case (1)
                if (present(dice_dT)) dice_dT = -dliquid_eq_dT
                if (present(dice_dP)) dice_dP = -dliquid_eq_dP
            end select
        end if
    end subroutine project_ice_content

    !> Water+ice-equivalent content conserved locally when a node's hydraulic
    !> conductivity has collapsed under freezing impedance (no flux in/out
    !> over the time step): \(\theta_w^n+(\rho_i/\rho_w)\theta_i^n\), read from
    !> the previous accepted step's pressure/ice history (index 2; index 1 is
    !> the current, still-being-solved step). Returns `.false.` if history is
    !> not yet available (e.g. the first step) or the state is degenerate.
    subroutine calc_conserved_target(self, state, target_total_water, available)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: target_total_water
        logical, intent(inout) :: available

        type(type_state) :: local_state
        real(real64), pointer, contiguous, dimension(:) :: pressure_history
        real(real64), pointer, contiguous, dimension(:) :: ice_content_history
        real(real64) :: liquid_content_prev, rho_w, rho_i

        available = .false.
        nullify (pressure_history, ice_content_history)
        call state%get(pressure_history=pressure_history)
        call state%ice_content_history%get(ice_content_history)
        if (.not. associated(pressure_history)) return
        if (.not. associated(ice_content_history)) return
        if (size(pressure_history) < 2 .or. size(ice_content_history) < 2) return

        call self%calc_rho_water(state, rho_w)
        call self%calc_rho_ice(state, rho_i)
        if (rho_w <= tiny(1.0d0) .or. rho_i <= tiny(1.0d0)) return

        call local_state%copy(state)
        call local_state%pressure%set(pressure_history(2))
        call self%calc_water_content(local_state, liquid_content_prev)

        target_total_water = liquid_content_prev + (rho_i / rho_w) * ice_content_history(2)
        available = .true.
    end subroutine calc_conserved_target

    !> Solve the local, flux-free phase equilibrium at fixed temperature.
    !>
    !> When hydraulic conductivity has collapsed under freezing impedance
    !> (see plan spicy-sauteeing-scroll.md, WP3 refinement), a node no longer
    !> exchanges water with its neighbors within a time step. Its converged
    !> outer-loop state is then the solution of two LOCAL equations, not a
    !> spatially coupled one: total water+ice mass is conserved (no flux) and
    !> the pore pressure satisfies the same Clapeyron liquid equilibrium the
    !> ice update is driving toward, \(\theta_l(p_w)=\theta_l^{eq}(T,p_w)\).
    !> This reduces to a SCALAR Newton iteration on \(p_w\) alone (T is fixed,
    !> supplied by the already-converged monolithic solve for this outer
    !> iteration); ice content is then read off directly from the mass
    !> constraint, not iterated. Reuses the same `wrf%deriv`,
    !> `gcc%deriv_pressure` primitives as `project_ice_content`'s `dice_dP`.
    !>
    !> `target_total_water` is the node's water+ice-equivalent content at the
    !> start of the time step (before this step's flux, assumed negligible in
    !> this regime): \(\theta_w^{n}+(\rho_i/\rho_w)\theta_i^{n}\).
    !>
    !> Returns `converged=.false.` (caller should fall back to the existing
    !> outer Picard/Anderson update) if Newton does not reach the pressure
    !> tolerance within the iteration budget, or the state is degenerate.
    subroutine solve_local_conserved_equilibrium(self, state, target_total_water, new_pressure, new_ice, converged)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(in) :: target_total_water
        real(real64), intent(inout) :: new_pressure
        real(real64), intent(inout) :: new_ice
        logical, intent(inout) :: converged

        type(type_state) :: local_state
        real(real64) :: pressure, psi_cryo, liquid_equilibrium, liquid_content
        real(real64) :: dliquid_dP, dliquid_dT_unused, dtheta_eq_dh, dpsi_cryo_dP, dliquid_eq_dP
        real(real64) :: residual, jacobian, delta_pressure, rho_w, rho_i, porosity, upper_bound
        integer(int32) :: iter
        integer(int32), parameter :: MAX_ITER = 30
        real(real64), parameter :: CONTENT_TOL = 1.0d-8
        real(real64), parameter :: MAX_STEP = 5.0d4 ! Pa; safeguards against runaway Newton steps

        converged = .false.
        call local_state%copy(state)
        call state%pressure%get(pressure)
        call self%calc_rho_water(state, rho_w)
        call self%calc_rho_ice(state, rho_i)
        call state%porosity%get(porosity)
        if (rho_w <= tiny(1.0d0) .or. rho_i <= tiny(1.0d0)) return

        do iter = 1, MAX_ITER
            call local_state%pressure%set(pressure)
            call self%calc_water_content(local_state, liquid_content)
            call self%gcc%calc(local_state, psi_cryo)
            call self%wrf%calc(-max(0.0d0, psi_cryo) / (rho_std * g), liquid_equilibrium)

            residual = liquid_content - liquid_equilibrium
            if (abs(residual) <= CONTENT_TOL) then
                converged = .true.
                exit
            end if

            call self%calc_water_content_derivatives(local_state, dliquid_dP, dliquid_dT_unused)
            dliquid_eq_dP = 0.0d0
            if (psi_cryo > 0.0d0) then
                call self%wrf%deriv(-psi_cryo / (rho_std * g), dtheta_eq_dh)
                call self%gcc%deriv_pressure(local_state, dpsi_cryo_dP)
                dliquid_eq_dP = -dtheta_eq_dh * dpsi_cryo_dP / (rho_std * g)
            end if
            jacobian = dliquid_dP - dliquid_eq_dP
            if (abs(jacobian) <= tiny(1.0d0)) return

            delta_pressure = -residual / jacobian
            delta_pressure = sign(min(abs(delta_pressure), MAX_STEP), delta_pressure)
            pressure = pressure + delta_pressure
        end do
        if (.not. converged) return

        new_pressure = pressure
        upper_bound = max(0.0d0, porosity - liquid_content)
        new_ice = min(max((target_total_water - liquid_content) * rho_w / rho_i, 0.0d0), upper_bound)
    end subroutine solve_local_conserved_equilibrium

    !> Calculate the pore-water pressure at which the gas-filled pore volume vanishes.
    !>
    !> \[ \theta_l(T,p_b)+\theta_i(T,p_b)=\phi \]
    !> Assumptions: local phase equilibrium, monotone WRF, and fixed porosity.
    !> Numerical guarantee: returns a bracketed bisection solution when a transition exists.
    !> Computational complexity: \(O(N_b)\) arithmetic and \(O(1)\) memory.
    !> Failure behavior: reports an unsaturated state when no finite transition is bracketed.
    subroutine calc_saturation_pressure(self, state, saturation_pressure, is_saturated)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: saturation_pressure
        logical, intent(inout) :: is_saturated

        integer(int32), parameter :: max_bisection_iterations = 64
        real(real64), parameter :: pressure_floor = -1.0d12
        type(type_state) :: local_state
        real(real64) :: pressure, porosity, ice_content
        real(real64) :: pressure_low, pressure_high, pressure_mid
        real(real64) :: volume_current, volume_low, volume_high, volume_mid
        real(real64) :: volume_tolerance
        integer(int32) :: i

        saturation_pressure = 0.0d0
        is_saturated = .false.
        call state%pressure%get(pressure)
        call state%porosity%get(porosity)
        call state%ice_content%get(ice_content)
        if (porosity <= 0.0d0) return

        call local_state%copy(state)
        call evaluate_unconstrained_phase_volume(local_state, pressure, volume_current)
        volume_tolerance = 64.0d0 * epsilon(1.0d0) * max(1.0d0, porosity)
        if (volume_current < porosity - volume_tolerance) return

        pressure_high = 0.0d0
        call evaluate_unconstrained_phase_volume(local_state, pressure_high, volume_high)
        if (volume_high < porosity - volume_tolerance) return

        pressure_low = -rho_std * g
        call evaluate_unconstrained_phase_volume(local_state, pressure_low, volume_low)
        do while (volume_low >= porosity .and. pressure_low > pressure_floor)
            pressure_low = max(10.0d0 * pressure_low, pressure_floor)
            call evaluate_unconstrained_phase_volume(local_state, pressure_low, volume_low)
        end do
        if (volume_low >= porosity) return

        do i = 1, max_bisection_iterations
            pressure_mid = 0.5d0 * (pressure_low + pressure_high)
            call evaluate_unconstrained_phase_volume(local_state, pressure_mid, volume_mid)
            if (volume_mid >= porosity) then
                pressure_high = pressure_mid
            else
                pressure_low = pressure_mid
            end if
        end do

        saturation_pressure = 0.5d0 * (pressure_low + pressure_high)
        is_saturated = pressure >= saturation_pressure - &
                       max(1.0d-8 * max(abs(saturation_pressure), 1.0d0), epsilon(1.0d0))

    contains
        subroutine evaluate_unconstrained_phase_volume(candidate_state, candidate_pressure, phase_volume)
            implicit none
            type(type_state), intent(inout) :: candidate_state
            real(real64), intent(in) :: candidate_pressure
            real(real64), intent(inout) :: phase_volume

            real(real64) :: theta_liquid

            call candidate_state%pressure%set(candidate_pressure)
            call self%calc_water_content(candidate_state, theta_liquid)
            phase_volume = theta_liquid + ice_content
        end subroutine evaluate_unconstrained_phase_volume
    end subroutine calc_saturation_pressure

    !>
    !> @brief Calculate derivative of ice pressure w.r.t water pressure.
    !>
    subroutine deriv_pressure_ice_water(self, state, deriv)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv

        call self%gcc%deriv_pressure_ice_water(state, deriv)

    end subroutine deriv_pressure_ice_water

end module models_phase_change_fusion
