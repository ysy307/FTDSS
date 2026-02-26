module models_hcf
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core
    use :: constitutive_constants, only:TtoK => celsius_to_kelvin, Mw => molar_mass_water, &
        Rg => universal_gas_constant, g => gravity_acceleration, rho_std => reference_water_density
    use :: constitutive_base, only:abst_constitutive
    implicit none
    private

    public :: abst_hcf
    ! public :: type_config_hcf
    public :: holder_hcfs
    public :: type_hcf_base
    public :: type_hcf_impedance
    public :: type_hcf_viscosity
    public :: type_hcf_base_impedance
    public :: type_hcf_base_viscosity
    public :: type_hcf_impedance_viscosity
    public :: type_hcf_base_impedance_viscosity

    ! type :: type_config_hcf
    !     integer(int32) :: unit_id
    !     integer(int32) :: model_number
    !     integer(int32) :: hcf_model_number
    !     integer(int32) :: water_viscosity_model
    !     real(real64) :: k_s
    !     real(real64) :: theta_r
    !     real(real64) :: theta_s
    !     real(real64) :: alpha1
    !     real(real64) :: n1
    !     real(real64) :: m1
    !     real(real64) :: h_crit
    !     real(real64) :: alpha2
    !     real(real64) :: n2
    !     real(real64) :: m2
    !     real(real64) :: w1
    !     real(real64) :: w2
    !     real(real64) :: l
    !     real(real64) :: omega
    !     real(real64) :: gain_factor
    ! contains
    !     procedure, pass(self), public :: reset => reset_config_hcf
    !     procedure, pass(self), public :: copy => copy_config_hcf
    !     procedure, pass(self), public :: convert => convert_config_hcf
    ! end type type_config_hcf

    ! interface
    !     module subroutine reset_config_hcf(self)
    !         implicit none
    !         class(type_config_hcf), intent(inout) :: self

    !     end subroutine reset_config_hcf

    !     module subroutine copy_config_hcf(self, source)
    !         implicit none
    !         class(type_config_hcf), intent(inout) :: self
    !         type(type_config_hcf), intent(in) :: source

    !     end subroutine copy_config_hcf

    !     module subroutine convert_config_hcf(self, unit_id, factor)
    !         implicit none
    !         class(type_config_hcf), intent(inout) :: self
    !         integer(int32), intent(in) :: unit_id
    !         real(real64), intent(in), optional :: factor

    !     end subroutine convert_config_hcf
    ! end interface

    type :: holder_hcfs
        class(abst_hcf), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_hcfs
    end type holder_hcfs

    interface
        module subroutine initialize_holder_hcfs(self, config, water, ice)
            implicit none
            class(holder_hcfs), intent(inout) :: self
            type(type_config_hcf), intent(in) :: config
            type(type_iapws97), intent(in), target :: water
            type(type_iapws06), intent(in), target :: ice

        end subroutine initialize_holder_hcfs
    end interface

    type, extends(abst_constitutive) :: type_hcf_vapor
        private
        class(abst_hcf), pointer :: parent
    contains
        procedure, pass(self), private :: calc_diffusivity => calc_diffusivity_vapor_in_air
        procedure, pass(self), private :: calc_tortuosity_factor => calc_tortuosity_factor_vapor
        procedure, pass(self), private :: calc_enhancement_factor => calc_enhancement_factor_vapor
        procedure, pass(self), public :: calc_Kvh => calc_Kvh_vapor
        procedure, pass(self), public :: calc_KvT => calc_KvT_vapor
    end type type_hcf_vapor

    interface
        module subroutine calc_diffusivity_vapor_in_air(self, temperature, Da)
            implicit none
            class(type_hcf_vapor), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64), intent(inout) :: Da

        end subroutine calc_diffusivity_vapor_in_air

        module subroutine calc_tortuosity_factor_vapor(self, Qa, Qs, tau)
            implicit none
            class(type_hcf_vapor), intent(in) :: self
            real(real64), intent(in) :: Qa
            real(real64), intent(in) :: Qs
            real(real64), intent(inout) :: tau

        end subroutine calc_tortuosity_factor_vapor

        module subroutine calc_enhancement_factor_vapor(self, Qw, Qs, fc, eta)
            implicit none
            class(type_hcf_vapor), intent(in) :: self
            real(real64), intent(in) :: Qw
            real(real64), intent(in) :: Qs
            real(real64), intent(in) :: fc
            real(real64), intent(inout) :: eta

        end subroutine calc_enhancement_factor_vapor

        module subroutine calc_Kvh_vapor(self, state, Kvh)
            implicit none
            class(type_hcf_vapor), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: Kvh

        end subroutine calc_Kvh_vapor

        module subroutine calc_KvT_vapor(self, state, KvT)
            implicit none
            class(type_hcf_vapor), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: KvT

        end subroutine calc_KvT_vapor
    end interface

    type, extends(abst_constitutive), abstract :: abst_hcf
        private
        type(type_config_hcf) :: config
        class(abst_hcf_base), allocatable :: base
        class(abst_hcf_impedance), allocatable :: impedance
        class(abst_hcf_viscosity), allocatable :: viscosity
        type(type_hcf_vapor) :: vapor
    contains
        procedure, pass(self), public :: initialize => initialize_abst_hcf
        procedure(abst_calc_Kflh), pass(self), public, deferred :: calc_Kflh
        procedure, pass(self), public :: calc_KlT => calc_KlT_hcf
        procedure, pass(self), public :: calc_Kvh => calc_Kvh_hcf
        procedure, pass(self), public :: calc_KvT => calc_KvT_hcf
        procedure, pass(self), public :: is_initialized => is_initialized_hcf
    end type abst_hcf

    interface
        module subroutine initialize_abst_hcf(self, config, water, ice)
            implicit none
            class(abst_hcf), intent(inout), target :: self
            type(type_config_hcf), intent(in) :: config
            type(type_iapws97), intent(in), target :: water
            type(type_iapws06), intent(in), target :: ice

        end subroutine initialize_abst_hcf

        module subroutine calc_KlT_hcf(self, state, KlT)
            implicit none
            class(abst_hcf), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: KlT

        end subroutine calc_KlT_hcf

        module subroutine calc_Kvh_hcf(self, state, Kvh)
            implicit none
            class(abst_hcf), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: Kvh

        end subroutine calc_Kvh_hcf

        module subroutine calc_KvT_hcf(self, state, KvT)
            implicit none
            class(abst_hcf), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: KvT

        end subroutine calc_KvT_hcf

        module pure function is_initialized_hcf(self) result(initialized)
            implicit none
            class(abst_hcf), intent(in) :: self
            logical :: initialized

        end function is_initialized_hcf
    end interface

    abstract interface
        subroutine abst_calc_Kflh(self, state, Kflh)
            import :: abst_hcf, type_state, real64
            implicit none
            class(abst_hcf), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: Kflh

        end subroutine abst_calc_Kflh
    end interface

    type, extends(abst_hcf) :: type_hcf_base
    contains
        procedure :: calc_Kflh => calc_Kflh_base
    end type type_hcf_base

    type, extends(abst_hcf) :: type_hcf_impedance
    contains
        procedure :: calc_Kflh => calc_Kflh_impedance
    end type type_hcf_impedance

    type, extends(abst_hcf) :: type_hcf_viscosity
    contains
        procedure :: calc_Kflh => calc_Kflh_viscosity
    end type type_hcf_viscosity

    type, extends(abst_hcf) :: type_hcf_base_impedance
    contains
        procedure :: calc_Kflh => calc_Kflh_base_impedance
    end type type_hcf_base_impedance

    type, extends(abst_hcf) :: type_hcf_base_viscosity
    contains
        procedure :: calc_Kflh => calc_Kflh_base_viscosity
    end type type_hcf_base_viscosity

    type, extends(abst_hcf) :: type_hcf_impedance_viscosity
    contains
        procedure :: calc_Kflh => calc_Kflh_impedance_viscosity
    end type type_hcf_impedance_viscosity

    type, extends(abst_hcf) :: type_hcf_base_impedance_viscosity
    contains
        procedure :: calc_Kflh => calc_Kflh_base_impedance_viscosity
    end type type_hcf_base_impedance_viscosity

    interface
        module subroutine calc_Kflh_base(self, state, Kflh)
            implicit none
            class(type_hcf_base), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: Kflh

        end subroutine calc_Kflh_base

        module subroutine calc_Kflh_impedance(self, state, Kflh)
            implicit none
            class(type_hcf_impedance), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: Kflh

        end subroutine calc_Kflh_impedance

        module subroutine calc_Kflh_viscosity(self, state, Kflh)
            implicit none
            class(type_hcf_viscosity), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: Kflh

        end subroutine calc_Kflh_viscosity

        module subroutine calc_Kflh_base_impedance(self, state, Kflh)
            implicit none
            class(type_hcf_base_impedance), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: Kflh

        end subroutine calc_Kflh_base_impedance

        module subroutine calc_Kflh_base_viscosity(self, state, Kflh)
            implicit none
            class(type_hcf_base_viscosity), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: Kflh

        end subroutine calc_Kflh_base_viscosity

        module subroutine calc_Kflh_impedance_viscosity(self, state, Kflh)
            implicit none
            class(type_hcf_impedance_viscosity), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: Kflh

        end subroutine calc_Kflh_impedance_viscosity

        module subroutine calc_Kflh_base_impedance_viscosity(self, state, Kflh)
            implicit none
            class(type_hcf_base_impedance_viscosity), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: Kflh

        end subroutine calc_Kflh_base_impedance_viscosity

    end interface

    type, abstract :: abst_hcf_base
        private
        class(abst_hcf), pointer :: parent
    contains
        procedure(abst_calc_base_kr), pass(self), public, deferred :: calc_kr
    end type abst_hcf_base

    abstract interface
        subroutine abst_calc_base_kr(self, h, kr)
            import :: abst_hcf_base, real64
            implicit none
            class(abst_hcf_base), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: kr

        end subroutine abst_calc_base_kr
    end interface

    type, extends(abst_hcf_base) :: type_hcf_base_bc
    contains
        procedure :: calc_kr => calc_kr_base_bc
    end type type_hcf_base_bc

    type, extends(abst_hcf_base) :: type_hcf_base_vg
    contains
        procedure :: calc_kr => calc_kr_base_vg
    end type type_hcf_base_vg

    type, extends(abst_hcf_base) :: type_hcf_base_ko
    contains
        procedure :: calc_kr => calc_kr_base_ko
    end type type_hcf_base_ko

    type, extends(abst_hcf_base) :: type_hcf_base_mvg
    contains
        procedure :: calc_kr => calc_kr_base_mvg
    end type type_hcf_base_mvg

    type, extends(abst_hcf_base) :: type_hcf_base_durner
    contains
        procedure :: calc_kr => calc_kr_base_durner
    end type type_hcf_base_durner

    type, extends(abst_hcf_base) :: type_hcf_base_dvgch
    contains
        procedure :: calc_kr => calc_kr_base_dvgch
    end type type_hcf_base_dvgch

    interface
        module subroutine calc_kr_base_bc(self, h, kr)
            implicit none
            class(type_hcf_base_bc), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: kr

        end subroutine calc_kr_base_bc

        module subroutine calc_kr_base_vg(self, h, kr)
            implicit none
            class(type_hcf_base_vg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: kr

        end subroutine calc_kr_base_vg

        module subroutine calc_kr_base_ko(self, h, kr)
            implicit none
            class(type_hcf_base_ko), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: kr

        end subroutine calc_kr_base_ko

        module subroutine calc_kr_base_mvg(self, h, kr)
            implicit none
            class(type_hcf_base_mvg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: kr

        end subroutine calc_kr_base_mvg

        module subroutine calc_kr_base_durner(self, h, kr)
            implicit none
            class(type_hcf_base_durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: kr

        end subroutine calc_kr_base_durner

        module subroutine calc_kr_base_dvgch(self, h, kr)
            implicit none
            class(type_hcf_base_dvgch), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: kr

        end subroutine calc_kr_base_dvgch
    end interface

    type, abstract :: abst_hcf_impedance
        private
        class(abst_hcf), pointer :: parent
    contains
        procedure(abst_calc_impedance_kr), pass(self), public, deferred :: calc_impedance
    end type abst_hcf_impedance

    type, extends(abst_hcf_impedance) :: type_hcf_impedance_exp
    contains
        procedure :: calc_impedance => calc_impedance_exp
    end type type_hcf_impedance_exp

    abstract interface
        subroutine abst_calc_impedance_kr(self, Qice, kr)
            import :: abst_hcf_impedance, real64
            implicit none
            class(abst_hcf_impedance), intent(in) :: self
            real(real64), intent(in) :: Qice
            real(real64), intent(inout) :: kr

        end subroutine abst_calc_impedance_kr
    end interface

    interface
        module subroutine calc_impedance_exp(self, Qice, kr)
            implicit none
            class(type_hcf_impedance_exp), intent(in) :: self
            real(real64), intent(in) :: Qice
            real(real64), intent(inout) :: kr

        end subroutine calc_impedance_exp
    end interface

    type, abstract :: abst_hcf_viscosity
        private
        real(real64) :: mu_zero
        class(abst_hcf), pointer :: parent
    contains
        procedure, pass(self), public :: initialize => initialize_abst_hcf_viscosity
        procedure(abst_calc_mu), pass(self), public, deferred :: calc_mu
        procedure, pass(self), public :: calc_viscosity => calc_kr_abst_hcf_viscosity
    end type abst_hcf_viscosity

    interface
        module subroutine initialize_abst_hcf_viscosity(self, temperature_critical)
            implicit none
            class(abst_hcf_viscosity), intent(inout) :: self
            real(real64), intent(in), optional :: temperature_critical

        end subroutine initialize_abst_hcf_viscosity
    end interface

    abstract interface
        subroutine abst_calc_mu(self, temperature, mu)
            import :: abst_hcf_viscosity, real64
            implicit none
            class(abst_hcf_viscosity), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64), intent(inout) :: mu

        end subroutine abst_calc_mu
    end interface

    interface
        module subroutine calc_kr_abst_hcf_viscosity(self, temperature, kr)
            import :: abst_hcf_viscosity, real64
            implicit none
            class(abst_hcf_viscosity), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64), intent(inout) :: kr

        end subroutine calc_kr_abst_hcf_viscosity
    end interface

    type, extends(abst_hcf_viscosity) :: type_hcf_viscosity_exp
    contains
        procedure, pass(self) :: calc_mu => calc_mu_exponential
    end type type_hcf_viscosity_exp

    type, extends(abst_hcf_viscosity) :: type_hcf_viscosity_supercool
    contains
        procedure, pass(self) :: calc_mu => calc_mu_exponential_supercooled
    end type type_hcf_viscosity_supercool

    interface

        module subroutine calc_mu_exponential(self, temperature, mu)
            implicit none
            class(type_hcf_viscosity_exp), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64), intent(inout) :: mu

        end subroutine calc_mu_exponential

        module subroutine calc_mu_exponential_supercooled(self, temperature, mu)
            implicit none
            class(type_hcf_viscosity_supercool), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64), intent(inout) :: mu

        end subroutine calc_mu_exponential_supercooled

        module subroutine calc_viscosity_exp(self, temperature, kr)
            implicit none
            class(type_hcf_viscosity_exp), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64), intent(inout) :: kr

        end subroutine calc_viscosity_exp

        module subroutine calc_viscosity_supercool(self, temperature, kr)
            implicit none
            class(type_hcf_viscosity_supercool), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64), intent(inout) :: kr

        end subroutine calc_viscosity_supercool

    end interface

end module models_hcf
