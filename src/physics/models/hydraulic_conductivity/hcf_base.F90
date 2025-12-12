submodule(physics_models_hcf) hcf_base
    implicit none
contains

    module subroutine initialize_holder_hcfs(self, material_id, params)
        implicit none
        class(holder_hcfs), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_params_hcf), intent(in) :: params

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
        call self%p%initialize(material_id, params)

    end subroutine initialize_holder_hcfs

    module subroutine reset_params_hcf(self)
        implicit none
        class(type_params_hcf), intent(inout) :: self

        self%model_number = 0
        self%water_viscosity_model = 0
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

    end subroutine reset_params_hcf

    module subroutine copy_params_hcf(self, source)
        implicit none
        class(type_params_hcf), intent(inout) :: self
        type(type_params_hcf), intent(in) :: source

        self%model_number = source%model_number
        self%water_viscosity_model = source%water_viscosity_model
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

    end subroutine copy_params_hcf

    module subroutine initialize_abst_hcf(self, material_id, params)
        implicit none
        class(abst_hcf), intent(inout), target :: self
        integer(int32), intent(in) :: material_id
        type(type_params_hcf), intent(in) :: params

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

        allocate (type_hcf_impedance_exp :: self%impedance)
        self%impedance%parent => self

        select case (params%water_viscosity_model)
        case (HCF_VISCOSITY_EXPONENTIAL)
            allocate (type_hcf_viscosity_exp :: self%viscosity)
        case (HCF_VISCOSITY_SUPERCOOLED)
            allocate (type_hcf_viscosity_supercool :: self%viscosity)
        end select
        call self%viscosity%initialize()
        self%viscosity%parent => self

    end subroutine initialize_abst_hcf

    module pure elemental subroutine calc_kflh_base(self, state, kflh)
        implicit none
        class(type_hcf_base), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_base

        call self%base%calc_kr(state%pressure, kr_base)
        kflh = self%params%k_s * kr_base

    end subroutine calc_kflh_base

    module pure elemental subroutine calc_kflh_impedance(self, state, kflh)
        implicit none
        class(type_hcf_impedance), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_impedance

        call self%impedance%calc_impedance(state%ice_content, kr_impedance)
        kflh = self%params%k_s * kr_impedance

    end subroutine calc_kflh_impedance

    module pure elemental subroutine calc_kflh_viscosity(self, state, kflh)
        implicit none
        class(type_hcf_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_viscosity

        call self%viscosity%calc_viscosity(state%temperature, kr_viscosity)
        kflh = self%params%k_s * kr_viscosity

    end subroutine calc_kflh_viscosity

    module pure elemental subroutine calc_kflh_base_impedance(self, state, kflh)
        implicit none
        class(type_hcf_base_impedance), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_base, kr_impedance

        call self%base%calc_kr(state%pressure, kr_base)
        call self%impedance%calc_impedance(state%ice_content, kr_impedance)
        kflh = self%params%k_s * kr_base * kr_impedance

    end subroutine calc_kflh_base_impedance

    module pure elemental subroutine calc_kflh_base_viscosity(self, state, kflh)
        implicit none
        class(type_hcf_base_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_base, kr_viscosity

        call self%base%calc_kr(state%pressure, kr_base)
        call self%viscosity%calc_viscosity(state%temperature, kr_viscosity)
        kflh = self%params%k_s * kr_base * kr_viscosity

    end subroutine calc_kflh_base_viscosity

    module pure elemental subroutine calc_kflh_impedance_viscosity(self, state, kflh)
        implicit none
        class(type_hcf_impedance_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_impedance, kr_viscosity

        call self%impedance%calc_impedance(state%ice_content, kr_impedance)
        call self%viscosity%calc_viscosity(state%temperature, kr_viscosity)
        kflh = self%params%k_s * kr_impedance * kr_viscosity

    end subroutine calc_kflh_impedance_viscosity

    module pure elemental subroutine calc_kflh_base_impedance_viscosity(self, state, kflh)
        implicit none
        class(type_hcf_base_impedance_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: kflh

        real(real64) :: kr_base, kr_impedance, kr_viscosity

        call self%base%calc_kr(state%pressure, kr_base)
        call self%impedance%calc_impedance(state%ice_content, kr_impedance)
        call self%viscosity%calc_viscosity(state%temperature, kr_viscosity)
        kflh = self%params%k_s * kr_base * kr_impedance * kr_viscosity

    end subroutine calc_kflh_base_impedance_viscosity

end submodule hcf_base
