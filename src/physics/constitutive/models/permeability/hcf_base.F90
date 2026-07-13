submodule(models_hcf) hcf_base
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real64), parameter :: gamma_0 = 71.88875d0 ! g/s^2
contains

    !> Suction head [m] controlling the retention-based relative permeability.
    !>
    !> \( h_{kr} = -p_c^*/(\rho_{std} g) \) with the generalized suction
    !> \( p_c^* = \max(\psi_{cap}, \psi_{cryo}) \) from the phase update.
    !> In unfrozen conditions this equals the pressure head, so behaviour is
    !> unchanged there; in the freezing fringe it keeps k_r consistent with
    !> the (Clapeyron-reduced) liquid content.  Evaluating k_r at the raw
    !> pressure head instead lets k_r -> 1 while the pores saturate under
    !> cryosuction pumping, which removes the self-limiting of the pumping
    !> flux and drives a physical runaway at the freezing interface.
    !> Falls back to the pressure head when the phase update has not run.
    module subroutine calc_head_hcf(self, state, head)
        implicit none
        class(abst_hcf), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: head

        real(real64) :: psi_eff, pressure
        logical :: is_set

        psi_eff = 0.0d0
        is_set = .false.
        call state%effective_suction%get(psi_eff, is_set)
        if (is_set) then
            head = -psi_eff / (rho_std * g)
        else
            call state%pressure%get(pressure)
            head = pressure / (rho_std * g)
        end if
    end subroutine calc_head_hcf

    !> Impedance factor of Hansson et al. (2004):
    !> \( Q = \theta_\mathrm{ice} / (\theta_\mathrm{T} - \theta_\mathrm{r}) \), the ice fraction of the
    !> total (minus residual) water content.  The impedance \(10^{-\Omega Q}\)
    !> with \(\Omega = 7\) was calibrated against the Mizoguchi columns using
    !> THIS definition; passing the raw volumetric ice content instead changes
    !> the meaning (and effective strength) of the calibrated factor.
    module subroutine calc_impedance_ratio_hcf(self, state, ratio)
        implicit none
        class(abst_hcf), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: ratio

        real(real64) :: Qw, Qi, denom

        call state%water_content%get(Qw)
        call state%ice_content%get(Qi)
        denom = Qw + Qi - self%config%theta_r
        if (denom > tiny(1.0d0) .and. Qi > 0.0d0) then
            ratio = min(Qi / denom, 1.0d0)
        else
            ratio = 0.0d0
        end if
    end subroutine calc_impedance_ratio_hcf

    module subroutine initialize_holder_hcfs(self, config, water, ice)
        implicit none
        class(holder_hcfs), intent(inout) :: self
        type(type_config_hcf), intent(in) :: config
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        if (allocated(self%p)) then
            deallocate (self%p)
        end if

        if (config%model == HCF_MODES%BASE) then
            allocate (type_hcf_base :: self%p)
        else if (config%model == HCF_MODES%IMPEDANCE) then
            allocate (type_hcf_impedance :: self%p)
        else if (config%model == HCF_MODES%VISCOSITY) then
            allocate (type_hcf_viscosity :: self%p)
        else if (config%model == HCF_MODES%BASE_IMPEDANCE) then
            allocate (type_hcf_base_impedance :: self%p)
        else if (config%model == HCF_MODES%BASE_VISCOSITY) then
            allocate (type_hcf_base_viscosity :: self%p)
        else if (config%model == HCF_MODES%IMPEDANCE_VISCOSITY) then
            allocate (type_hcf_impedance_viscosity :: self%p)
        else if (config%model == HCF_MODES%BASE_IMPEDANCE_VISCOSITY) then
            allocate (type_hcf_base_impedance_viscosity :: self%p)
        else
            write (*, *) 'Error: Unknown HCF model ', config%model
            stop
        end if

        call self%p%initialize(config, water, ice)

    end subroutine initialize_holder_hcfs

    module subroutine initialize_abst_hcf(self, config, water, ice)
        implicit none
        class(abst_hcf), intent(inout), target :: self
        type(type_config_hcf), intent(in) :: config
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        call self%config%copy(config)

        if (self%config%model == HCF_MODES%BASE .or. &
            self%config%model == HCF_MODES%BASE_IMPEDANCE .or. &
            self%config%model == HCF_MODES%BASE_VISCOSITY .or. &
            self%config%model == HCF_MODES%BASE_IMPEDANCE_VISCOSITY) then

            if (self%config%swcc_model == SWCC_MODELS%BC) then
                allocate (type_hcf_base_bc :: self%base)
            else if (self%config%swcc_model == SWCC_MODELS%VG) then
                allocate (type_hcf_base_vg :: self%base)
            else if (self%config%swcc_model == SWCC_MODELS%KO) then
                allocate (type_hcf_base_ko :: self%base)
            else if (self%config%swcc_model == SWCC_MODELS%MVG) then
                allocate (type_hcf_base_mvg :: self%base)
            else if (self%config%swcc_model == SWCC_MODELS%DURNER) then
                allocate (type_hcf_base_durner :: self%base)
            else if (self%config%swcc_model == SWCC_MODELS%DVGCH) then
                allocate (type_hcf_base_dvgch :: self%base)
            else
                write (*, *) 'Error: Unknown WRF model ', self%config%swcc_model
                stop
            end if

            self%base%parent => self
            call self%base%initialize()
        end if

        if (self%config%model == HCF_MODES%IMPEDANCE .or. &
            self%config%model == HCF_MODES%BASE_IMPEDANCE .or. &
            self%config%model == HCF_MODES%IMPEDANCE_VISCOSITY .or. &
            self%config%model == HCF_MODES%BASE_IMPEDANCE_VISCOSITY) then
            allocate (type_hcf_impedance_exp :: self%impedance)
            self%impedance%parent => self
        end if

        if (self%config%model == HCF_MODES%VISCOSITY .or. &
            self%config%model == HCF_MODES%BASE_VISCOSITY .or. &
            self%config%model == HCF_MODES%IMPEDANCE_VISCOSITY .or. &
            self%config%model == HCF_MODES%BASE_IMPEDANCE_VISCOSITY) then

            if (self%config%water_viscosity_model == HCF_VISCOSITY_TYPES%EXPONENTIAL) then
                allocate (type_hcf_viscosity_exp :: self%viscosity)
            else if (self%config%water_viscosity_model == HCF_VISCOSITY_TYPES%SUPERCOOLED) then
                allocate (type_hcf_viscosity_supercool :: self%viscosity)
            else
                write (*, *) 'Error: Unknown water viscosity model ', self%config%water_viscosity_model
                stop
            end if
            call self%viscosity%initialize()
            self%viscosity%parent => self
        end if

        self%water => water
        self%ice => ice

        self%vapor%parent => self
        self%vapor%water => water
        self%vapor%ice => ice

        self%initialized = .true.
    end subroutine initialize_abst_hcf

    !> Default initialization for base permeability models without cached coefficients.
    module subroutine initialize_hcf_base(self)
        implicit none
        class(abst_hcf_base), intent(inout) :: self
    end subroutine initialize_hcf_base

    module pure function is_initialized_hcf(self) result(initialized)
        implicit none
        class(abst_hcf), intent(in) :: self
        logical :: initialized

        initialized = self%initialized
    end function is_initialized_hcf

    module subroutine calc_kflh_base(self, state, kflh)
        implicit none
        class(type_hcf_base), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_base
        real(real64) :: head

        call self%calc_head(state, head)

        call self%base%calc_kr(head, kr_base)
        kflh = self%config%k_sat * kr_base

    end subroutine calc_kflh_base

    module subroutine calc_kflh_impedance(self, state, kflh)
        implicit none
        class(type_hcf_impedance), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_impedance
        real(real64) :: Q_ratio

        call self%calc_impedance_ratio(state, Q_ratio)

        call self%impedance%calc_impedance(Q_ratio, kr_impedance)
        kflh = self%config%k_sat * kr_impedance

    end subroutine calc_kflh_impedance

    module subroutine calc_kflh_viscosity(self, state, kflh)
        implicit none
        class(type_hcf_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_viscosity
        real(real64) :: temperature

        call state%temperature%get(temperature)

        call self%viscosity%calc_viscosity(temperature, kr_viscosity)
        kflh = self%config%k_sat * kr_viscosity

    end subroutine calc_kflh_viscosity

    module subroutine calc_kflh_base_impedance(self, state, kflh)
        implicit none
        class(type_hcf_base_impedance), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_base, kr_impedance
        real(real64) :: head, Q_ratio

        call self%calc_head(state, head)
        call self%calc_impedance_ratio(state, Q_ratio)

        call self%base%calc_kr(head, kr_base)
        call self%impedance%calc_impedance(Q_ratio, kr_impedance)
        kflh = self%config%k_sat * kr_base * kr_impedance

    end subroutine calc_kflh_base_impedance

    module subroutine calc_kflh_base_viscosity(self, state, kflh)
        implicit none
        class(type_hcf_base_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_base, kr_viscosity
        real(real64) :: temperature, head

        call state%temperature%get(temperature)
        call self%calc_head(state, head)

        call self%base%calc_kr(head, kr_base)
        call self%viscosity%calc_viscosity(temperature, kr_viscosity)
        kflh = self%config%k_sat * kr_base * kr_viscosity

    end subroutine calc_kflh_base_viscosity

    module subroutine calc_kflh_impedance_viscosity(self, state, kflh)
        implicit none
        class(type_hcf_impedance_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_impedance, kr_viscosity
        real(real64) :: temperature, Q_ratio

        call state%temperature%get(temperature)
        call self%calc_impedance_ratio(state, Q_ratio)

        call self%impedance%calc_impedance(Q_ratio, kr_impedance)
        call self%viscosity%calc_viscosity(temperature, kr_viscosity)
        kflh = self%config%k_sat * kr_impedance * kr_viscosity

    end subroutine calc_kflh_impedance_viscosity

    module subroutine calc_kflh_base_impedance_viscosity(self, state, kflh)
        implicit none
        class(type_hcf_base_impedance_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_base, kr_impedance, kr_viscosity
        real(real64) :: temperature, head, Q_ratio

        call state%temperature%get(temperature)
        call self%calc_impedance_ratio(state, Q_ratio)
        call self%calc_head(state, head)

        call self%base%calc_kr(head, kr_base)
        call self%impedance%calc_impedance(Q_ratio, kr_impedance)
        call self%viscosity%calc_viscosity(temperature, kr_viscosity)
        kflh = self%config%k_sat * kr_base * kr_impedance * kr_viscosity

    end subroutine calc_kflh_base_impedance_viscosity

    module subroutine calc_klT_hcf(self, state, klT)
        implicit none
        class(abst_hcf), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: klT

        real(real64) :: Klh_r, dgamma_dT
        real(real64) :: temperature
        real(real64) :: head

        call state%temperature%get(temperature)

        ! Head of the liquid potential (effective suction), consistent with
        ! the retention scaling this thermo-osmotic term derives from.
        call self%calc_head(state, head)

        if (allocated(self%base)) then
            call self%base%calc_kr(head, Klh_r)
            dgamma_dT = -0.1425d0 - 4.76d-4 * temperature
            klT = self%config%k_sat * Klh_r * head * self%config%gain_factor * (dgamma_dT / gamma_0)
        else
            klT = 0.0d0
        end if

    end subroutine calc_klT_hcf

    module subroutine calc_Kvh_hcf(self, state, Kvh)
        implicit none
        class(abst_hcf), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: Kvh

        call self%vapor%calc_Kvh(state, Kvh)

    end subroutine calc_Kvh_hcf

    module subroutine calc_KvT_hcf(self, state, KvT)
        implicit none
        class(abst_hcf), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: KvT

        call self%vapor%calc_KvT(state, KvT)

    end subroutine calc_KvT_hcf
end submodule hcf_base
