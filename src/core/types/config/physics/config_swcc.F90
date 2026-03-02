module types_config_physics_swcc
    use, intrinsic :: iso_fortran_env
    use :: core_constants, only:type_constant_id
    use :: types_config_base, only:abst_config
    use :: types_config_physics_base, only:abst_config_physics_model
    implicit none
    private

    public :: type_config_wrf
    public :: type_config_hcf

    !> Structure to hold common parameters for all WRF models.
    type, extends(abst_config_physics_model) :: type_config_wrf
        !> Model identification number
        type(type_constant_id) :: swcc_model = type_constant_id("none", "none", -1)
        !> Residual water content, $\theta_\mathrm{r}$ [-]
        real(real64) :: theta_r = 0.0d0
        !> Saturated water content, $\theta_\mathrm{s}$ [-]
        real(real64) :: theta_s = 0.0d0
        !> Inverse of the air-entry value or scaling parameter, $\alpha_1$ [1/L]
        real(real64) :: alpha1 = 0.0d0
        !> Pore-size distribution index, $n_1$ [-]
        real(real64) :: n1 = 0.0d0
        !> Asymmetry parameter, $m_1$ [-]
        real(real64) :: m1 = 0.0d0
        !> Critical pressure head for modified models, $h_\mathrm{crit}$ [L]
        real(real64) :: h_crit = 0.0d0
        !> Scaling parameter for secondary porosity, $\alpha_2$ [1/L]
        real(real64) :: alpha2 = 0.0d0
        !> Pore-size distribution index for secondary porosity, $n_2$ [-]
        real(real64) :: n2 = 0.0d0
        !> Asymmetry parameter for secondary porosity, $m_2$ [-]
        real(real64) :: m2 = 0.0d0
        !> Weighting factor for primary porosity, $w_1$ [-]
        real(real64) :: w1 = 0.0d0
        !> Weighting factor for secondary porosity, $w_2$ [-]
        real(real64) :: w2 = 0.0d0
    contains
        procedure, pass(self), public :: copy => copy_config_wrf
        procedure, pass(self), public :: reset => reset_config_wrf
    end type type_config_wrf

    type, extends(type_config_wrf) :: type_config_hcf
        !> HCF model identification
        type(type_constant_id) :: model = type_constant_id("none", "none", -1)
        !> Water viscosity model identification
        type(type_constant_id) :: water_viscosity_model = type_constant_id("none", "none", -1)
        !> Saturated hydraulic conductivity [L/T]
        real(real64) :: k_sat
        !> Pore tortuosity / pore-connectivity parameter [-]
        real(real64) :: l
        !> Ice impedance factor [-]
        real(real64) :: omega
        !> Gain factor for numerical adjustment [-]
        real(real64) :: gain_factor
    contains
        procedure, pass(self), public :: copy => copy_config_hcf
        procedure, pass(self), public :: reset => reset_config_hcf
    end type

contains

    !> Copy values from another type_config_wrf
    subroutine copy_config_wrf(self, source)
        implicit none
        class(type_config_wrf), intent(inout) :: self
        class(abst_config), intent(in) :: source

        call copy_config_physics_model(self, source)

        select type (source)
        type is (type_config_wrf)
            ! 親型メンバーを self にコピー
            call self%set(self%swcc_model, source%swcc_model)
            call self%set(self%theta_s, source%theta_s)
            call self%set(self%theta_r, source%theta_r)
            call self%set(self%alpha1, source%alpha1)
            call self%set(self%n1, source%n1)
            call self%set(self%m1, source%m1)
            call self%set(self%h_crit, source%h_crit)
            call self%set(self%alpha2, source%alpha2)
            call self%set(self%n2, source%n2)
            call self%set(self%m2, source%m2)
            call self%set(self%w1, source%w1)
            call self%set(self%w2, source%w2)
        class default
            call self%reset()
        end select
    end subroutine copy_config_wrf

    !> Reset all values in type_config_wrf to defaults
    subroutine reset_config_wrf(self)
        implicit none
        class(type_config_wrf), intent(inout) :: self

        call reset_config_physics_model(self)

        self%swcc_model = type_constant_id("none", "none", -1)
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
    end subroutine reset_config_wrf

    !> Copy values from another type_config_hcf, calling parent copy
    subroutine copy_config_hcf(self, source)
        implicit none
        class(type_config_hcf), intent(inout) :: self
        class(abst_config), intent(in) :: source

        select type (source)
        type is (type_config_hcf)
            ! まず親型の copy を呼ぶ
            call copy_config_wrf(self, source)
            ! 追加メンバーのみコピー
            call self%set(self%model, source%model)
            call self%set(self%water_viscosity_model, source%water_viscosity_model)
            call self%set(self%k_sat, source%k_sat)
            call self%set(self%l, source%l)
            call self%set(self%omega, source%omega)
            call self%set(self%gain_factor, source%gain_factor)
        class default
            call self%reset()
        end select
    end subroutine copy_config_hcf

    !> Reset all values in type_config_hcf, calling parent reset
    subroutine reset_config_hcf(self)
        implicit none
        class(type_config_hcf), intent(inout) :: self

        ! まず親型の reset を呼ぶ
        call reset_config_wrf(self)

        ! 追加メンバーのみリセット
        self%model = type_constant_id("none", "none", -1)
        self%water_viscosity_model = type_constant_id("none", "none", -1)
        self%k_sat = 0.0d0
        self%l = 0.0d0
        self%omega = 0.0d0
        self%gain_factor = 0.0d0
    end subroutine reset_config_hcf

end module types_config_physics_swcc
