submodule(physics_models_hcf) hcf_base
    implicit none
    real(real64), parameter :: gamma_0 = 71.88875d0 ! g/s^2
contains

    module subroutine initialize_holder_hcfs(self, material_id, params, water)
        implicit none
        class(holder_hcfs), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_hcf_params), intent(in) :: params
        type(type_iapws97), intent(in), target :: water

        if (allocated(self%p)) then
            deallocate (self%p)
        end if

        select case (params%model_number)
        case (HCF_BASE)
            allocate (type_hcf_base :: self%p)
        case (HCF_IMPEDANCE)
            allocate (type_hcf_impedance :: self%p)
        case (HCF_VISCOSITY)
            allocate (type_hcf_viscosity :: self%p)
        case (HCF_BASE_IMPEDANCE)
            allocate (type_hcf_base_impedance :: self%p)
        case (HCF_BASE_VISCOSITY)
            allocate (type_hcf_base_viscosity :: self%p)
        case (HCF_IMPEDANCE_VISCOSITY)
            allocate (type_hcf_impedance_viscosity :: self%p)
        case (HCF_BASE_IMPEDANCE_VISCOSITY)
            allocate (type_hcf_base_impedance_viscosity :: self%p)
        end select

        call self%p%params%copy(params)
        call self%p%params%convert(params%unit_id)
        call self%p%initialize(material_id, params, water)

    end subroutine initialize_holder_hcfs

    module subroutine reset_params_hcf(self)
        implicit none
        class(type_hcf_params), intent(inout) :: self

        self%model_number = 0
        self%unit_id = 0
        self%water_viscosity_model = 0
        self%hcf_model_number = 0
        self%k_s = 0.0d0
        self%theta_s = 0.0d0
        self%theta_r = 0.0d0
        self%alpha1 = 0.0d0
        self%n1 = 0.0d0
        self%m1 = 0.0d0
        self%h_crit = 0.0d0
        self%alpha2 = 0.0d0
        self%n2 = 0.0d0
        self%m2 = 0.0d0
        self%w1 = 0.0d0
        self%w2 = 0.0d0
        self%l = 0.0d0
        self%omega = 0.0d0
        self%gain_factor = 0.0d0

    end subroutine reset_params_hcf

    module subroutine convert_params_hcf(self, unit_id, factor)
        implicit none
        class(type_hcf_params), intent(inout) :: self
        integer(int32), intent(in) :: unit_id
        real(real64), intent(in), optional :: factor

        real(real64) :: pg_val
        real(real64) :: scale_pres
        real(real64) :: scale_inv_pres

        ! --- 比重量 (rho*g) の設定 ---
        if (present(factor)) then
            ! 指定があればそれを使う (温度変化や油などを考慮する場合)
            pg_val = factor
        else
            ! 指定がなければ標準的な水の値を使う
            pg_val = rho_std * g
        end if

        ! --- 変換係数の決定 ---
        select case (unit_id)
        case (PHYSICS_UNIT_M)
            ! m -> Pa
            scale_pres = pg_val
            scale_inv_pres = 1.0d0 / pg_val

        case (PHYSICS_UNIT_CM)
            ! cm -> m -> Pa
            scale_pres = pg_val / 100.0d0
            scale_inv_pres = 100.0d0 / pg_val

        case (PHYSICS_UNIT_PA)
            ! Pa -> Pa (係数は1.0)
            ! ※ Paの場合は pg_val が何であっても影響しないので安全
            scale_pres = 1.0d0
            scale_inv_pres = 1.0d0

        case default
            scale_pres = 1.0d0
            scale_inv_pres = 1.0d0
        end select

        ! --- モデルごとのパラメータ変換 ---
        select case (self%hcf_model_number)
        case (HCF_BC, HCF_KO)
            self%alpha1 = self%alpha1 * scale_pres
            self%h_crit = self%h_crit * scale_pres
            self%alpha2 = self%alpha2 * scale_pres

        case (HCF_VG, HCF_MVG, HCF_DURNER, HCF_DVGCH)
            self%alpha1 = self%alpha1 * scale_inv_pres
            self%alpha2 = self%alpha2 * scale_inv_pres
            self%h_crit = self%h_crit * scale_pres
        case default
            self%alpha1 = self%alpha1 * scale_inv_pres
            self%h_crit = self%h_crit * scale_pres
        end select

    end subroutine convert_params_hcf

    module subroutine copy_params_hcf(self, source)
        implicit none
        class(type_hcf_params), intent(inout) :: self
        type(type_hcf_params), intent(in) :: source

        self%model_number = source%model_number
        self%unit_id = source%unit_id
        self%water_viscosity_model = source%water_viscosity_model
        self%hcf_model_number = source%hcf_model_number
        self%k_s = source%k_s
        self%theta_s = source%theta_s
        self%theta_r = source%theta_r
        self%alpha1 = source%alpha1
        self%n1 = source%n1
        self%m1 = source%m1
        self%h_crit = source%h_crit
        self%alpha2 = source%alpha2
        self%n2 = source%n2
        self%m2 = source%m2
        self%w1 = source%w1
        self%w2 = source%w2
        self%l = source%l
        self%omega = source%omega
        self%gain_factor = source%gain_factor

    end subroutine copy_params_hcf

    module subroutine initialize_abst_hcf(self, material_id, params, water)
        implicit none
        class(abst_hcf), intent(inout), target :: self
        integer(int32), intent(in) :: material_id
        type(type_hcf_params), intent(in) :: params
        type(type_iapws97), intent(in), target :: water

        if (params%model_number == HCF_BASE .or. &
            params%model_number == HCF_BASE_IMPEDANCE .or. &
            params%model_number == HCF_BASE_VISCOSITY .or. &
            params%model_number == HCF_BASE_IMPEDANCE_VISCOSITY) then
            select case (params%hcf_model_number)
            case (HCF_BC)
                allocate (type_hcf_base_bc :: self%base)
            case (HCF_VG)
                allocate (type_hcf_base_vg :: self%base)
            case (HCF_KO)
                allocate (type_hcf_base_ko :: self%base)
            case (HCF_MVG)
                allocate (type_hcf_base_mvg :: self%base)
            case (HCF_DURNER)
                allocate (type_hcf_base_durner :: self%base)
            case (HCF_DVGCH)
                allocate (type_hcf_base_dvgch :: self%base)
            end select

            self%base%parent => self
        end if

        if (params%model_number == HCF_IMPEDANCE .or. &
            params%model_number == HCF_BASE_IMPEDANCE .or. &
            params%model_number == HCF_IMPEDANCE_VISCOSITY .or. &
            params%model_number == HCF_BASE_IMPEDANCE_VISCOSITY) then
            allocate (type_hcf_impedance_exp :: self%impedance)
            self%impedance%parent => self
        end if

        if (params%model_number == HCF_VISCOSITY .or. &
            params%model_number == HCF_BASE_VISCOSITY .or. &
            params%model_number == HCF_IMPEDANCE_VISCOSITY .or. &
            params%model_number == HCF_BASE_IMPEDANCE_VISCOSITY) then
            select case (params%water_viscosity_model)
            case (HCF_VISCOSITY_EXPONENTIAL)
                allocate (type_hcf_viscosity_exp :: self%viscosity)
            case (HCF_VISCOSITY_SUPERCOOLED)
                allocate (type_hcf_viscosity_supercool :: self%viscosity)
            end select
            call self%viscosity%initialize()
            self%viscosity%parent => self
        end if

        self%water => water

    end subroutine initialize_abst_hcf

    module pure elemental subroutine calc_kflh_base(self, state, kflh)
        implicit none
        class(type_hcf_base), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_base
        real(real64) :: pressure

        call state%pressure%get(pressure)

        call self%base%calc_kr(pressure, kr_base)
        kflh = self%params%k_s * kr_base

    end subroutine calc_kflh_base

    module pure elemental subroutine calc_kflh_impedance(self, state, kflh)
        implicit none
        class(type_hcf_impedance), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_impedance
        real(real64) :: ice_content

        call state%ice_content%get(ice_content)

        call self%impedance%calc_impedance(ice_content, kr_impedance)
        kflh = self%params%k_s * kr_impedance

    end subroutine calc_kflh_impedance

    module pure elemental subroutine calc_kflh_viscosity(self, state, kflh)
        implicit none
        class(type_hcf_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_viscosity
        real(real64) :: temperature

        call state%temperature%get(temperature)

        call self%viscosity%calc_viscosity(temperature, kr_viscosity)
        kflh = self%params%k_s * kr_viscosity

    end subroutine calc_kflh_viscosity

    module pure elemental subroutine calc_kflh_base_impedance(self, state, kflh)
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
        kflh = self%params%k_s * kr_base * kr_impedance

    end subroutine calc_kflh_base_impedance

    module pure elemental subroutine calc_kflh_base_viscosity(self, state, kflh)
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
        kflh = self%params%k_s * kr_base * kr_viscosity

    end subroutine calc_kflh_base_viscosity

    module pure elemental subroutine calc_kflh_impedance_viscosity(self, state, kflh)
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
        kflh = self%params%k_s * kr_impedance * kr_viscosity

    end subroutine calc_kflh_impedance_viscosity

    module pure elemental subroutine calc_kflh_base_impedance_viscosity(self, state, kflh)
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
        kflh = self%params%k_s * kr_base * kr_impedance * kr_viscosity

    end subroutine calc_kflh_base_impedance_viscosity

    module pure elemental subroutine calc_klT_hcf(self, state, klT)
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
            klT = self%params%k_s * Klh_r * pressure * self%params%gain_factor * (dgamma_dT / gamma_0)
        else
            klT = 0.0d0
        end if

    end subroutine calc_klT_hcf

    pure elemental subroutine calc_surface_tension(temperature, surface_tension)
        implicit none
        !> Temperature [C]
        real(real64), intent(in) :: temperature
        !> Surface tension [g/s^2]
        real(real64), intent(inout) :: surface_tension

        surface_tension = 75.6d0 - 0.1425d0 * temperature - 2.38d-4 * temperature**2

    end subroutine calc_surface_tension

    pure elemental subroutine calc_derivative_surface_tension(temperature, dsurface_tension_dT)
        implicit none
        !> Temperature [C]
        real(real64), intent(in) :: temperature
        !> Derivative of surface tension with respect to temperature [g/(s^2 K)]
        real(real64), intent(inout) :: dsurface_tension_dT

        dsurface_tension_dT = -0.1425d0 - 4.76d-4 * temperature

    end subroutine calc_derivative_surface_tension

    module pure elemental subroutine calc_Kvh_hcf(self, state, Kvh)
        implicit none
        class(abst_hcf), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: Kvh

        call self%vapor%calc_Kvh(state, Kvh)

    end subroutine calc_Kvh_hcf

    module pure elemental subroutine calc_KvT_hcf(self, state, KvT)
        implicit none
        class(abst_hcf), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: KvT

        call self%vapor%calc_KvT(state, KvT)

    end subroutine calc_KvT_hcf
end submodule hcf_base
