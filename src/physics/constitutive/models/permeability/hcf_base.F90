submodule(models_hcf) hcf_base
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real64), parameter :: gamma_0 = 71.88875d0 ! g/s^2
contains

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
            allocate (type_hcf_viscosity_exp :: self%viscosity)
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
        real(real64) :: pressure

        call state%pressure%get(pressure)

        call self%base%calc_kr(pressure, kr_base)
        kflh = self%config%k_sat * kr_base

    end subroutine calc_kflh_base

    module subroutine calc_kflh_impedance(self, state, kflh)
        implicit none
        class(type_hcf_impedance), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_impedance
        real(real64) :: ice_content

        call state%ice_content%get(ice_content)

        call self%impedance%calc_impedance(ice_content, kr_impedance)
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
        real(real64) :: pressure, ice_content

        call state%pressure%get(pressure)
        call state%ice_content%get(ice_content)

        call self%base%calc_kr(pressure, kr_base)
        call self%impedance%calc_impedance(ice_content, kr_impedance)
        kflh = self%config%k_sat * kr_base * kr_impedance

    end subroutine calc_kflh_base_impedance

    module subroutine calc_kflh_base_viscosity(self, state, kflh)
        implicit none
        class(type_hcf_base_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_base, kr_viscosity
        real(real64) :: temperature, pressure

        call state%temperature%get(temperature)
        call state%pressure%get(pressure)

        call self%base%calc_kr(pressure, kr_base)
        call self%viscosity%calc_viscosity(temperature, kr_viscosity)
        kflh = self%config%k_sat * kr_base * kr_viscosity

    end subroutine calc_kflh_base_viscosity

    module subroutine calc_kflh_impedance_viscosity(self, state, kflh)
        implicit none
        class(type_hcf_impedance_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_impedance, kr_viscosity
        real(real64) :: temperature, ice_content

        call state%temperature%get(temperature)
        call state%ice_content%get(ice_content)

        call self%impedance%calc_impedance(ice_content, kr_impedance)
        call self%viscosity%calc_viscosity(temperature, kr_viscosity)
        kflh = self%config%k_sat * kr_impedance * kr_viscosity

    end subroutine calc_kflh_impedance_viscosity

    module subroutine calc_kflh_base_impedance_viscosity(self, state, kflh)
        implicit none
        class(type_hcf_base_impedance_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_base, kr_impedance, kr_viscosity
        real(real64) :: temperature, pressure, ice_content

        call state%temperature%get(temperature)
        call state%ice_content%get(ice_content)
        call state%pressure%get(pressure)

        call self%base%calc_kr(pressure, kr_base)
        call self%impedance%calc_impedance(ice_content, kr_impedance)
        call self%viscosity%calc_viscosity(temperature, kr_viscosity)
        kflh = self%config%k_sat * kr_base * kr_impedance * kr_viscosity

    end subroutine calc_kflh_base_impedance_viscosity

    module subroutine calc_klT_hcf(self, state, klT)
        implicit none
        class(abst_hcf), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: klT

        real(real64) :: Klh_r, dgamma_dT
        real(real64) :: temperature, pressure

        call state%temperature%get(temperature)
        call state%pressure%get(pressure)

        if (allocated(self%base)) then
            call self%base%calc_kr(pressure, Klh_r)
            call calc_derivative_surface_tension(temperature, dgamma_dT)
            klT = self%config%k_sat * Klh_r * pressure * self%config%gain_factor * (dgamma_dT / gamma_0)
        else
            klT = 0.0d0
        end if

    end subroutine calc_klT_hcf

    subroutine calc_surface_tension(temperature, surface_tension)
        implicit none
        !> Temperature [C]
        real(real64), intent(in) :: temperature
        !> Surface tension [g/s^2]
        real(real64), intent(inout) :: surface_tension

        surface_tension = 75.6d0 - 0.1425d0 * temperature - 2.38d-4 * temperature**2

    end subroutine calc_surface_tension

    subroutine calc_derivative_surface_tension(temperature, dsurface_tension_dT)
        implicit none
        !> Temperature [C]
        real(real64), intent(in) :: temperature
        !> Derivative of surface tension with respect to temperature [g/(s^2 K)]
        real(real64), intent(inout) :: dsurface_tension_dT

        dsurface_tension_dT = -0.1425d0 - 4.76d-4 * temperature

    end subroutine calc_derivative_surface_tension

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
