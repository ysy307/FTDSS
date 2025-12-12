module physics_models_hcf
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
    implicit none
    private

    public :: abst_hcf
    public :: type_params_hcf
    public :: holder_hcfs
    public :: type_hcf_base
    public :: type_hcf_impedance
    public :: type_hcf_viscosity
    public :: type_hcf_base_impedance
    public :: type_hcf_base_viscosity
    public :: type_hcf_impedance_viscosity
    public :: type_hcf_base_impedance_viscosity

    type :: type_params_hcf
        integer(int32) :: model_number
        integer(int32) :: hcf_model_number
        integer(int32) :: water_viscosity_model
        real(real64) :: k_s
        real(real64) :: theta_r
        real(real64) :: theta_s
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: m1
        real(real64) :: h_crit
        real(real64) :: alpha2
        real(real64) :: n2
        real(real64) :: m2
        real(real64) :: w1
        real(real64) :: w2
        real(real64) :: l
        real(real64) :: omega
    contains
        procedure, pass(self), public :: reset => reset_params_hcf
        procedure, pass(self), public :: copy => copy_params_hcf
    end type type_params_hcf

    interface
        module subroutine reset_params_hcf(self)
            implicit none
            class(type_params_hcf), intent(inout) :: self

        end subroutine reset_params_hcf

        module subroutine copy_params_hcf(self, source)
            implicit none
            class(type_params_hcf), intent(inout) :: self
            type(type_params_hcf), intent(in) :: source

        end subroutine copy_params_hcf
    end interface

    type :: holder_hcfs
        class(abst_hcf), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_hcfs
    end type holder_hcfs

    interface
        module subroutine initialize_holder_hcfs(self, material_id, params)
            implicit none
            class(holder_hcfs), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_params_hcf), intent(in) :: params

        end subroutine initialize_holder_hcfs
    end interface

    type, abstract :: abst_hcf
        private
        type(type_params_hcf) :: params
        class(abst_hcf_base), allocatable :: base
        class(abst_hcf_impedance), allocatable :: impedance
        class(abst_hcf_viscosity), allocatable :: viscosity
    contains
        procedure, pass(self) :: initialize => initialize_abst_hcf
        procedure(abst_calc_kflh), pass(self), public, deferred :: calc_kflh
    end type abst_hcf

    interface
        module subroutine initialize_abst_hcf(self, material_id, params)
            implicit none
            class(abst_hcf), intent(inout), target :: self
            integer(int32), intent(in) :: material_id
            type(type_params_hcf), intent(in) :: params

        end subroutine initialize_abst_hcf
    end interface

    abstract interface
        pure elemental subroutine abst_calc_kflh(self, state, kflh)
            import :: abst_hcf, type_state, real64
            implicit none
            class(abst_hcf), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: kflh

        end subroutine abst_calc_kflh
    end interface

    type, extends(abst_hcf) :: type_hcf_base
    contains
        procedure :: calc_kflh => calc_kflh_base
    end type type_hcf_base

    type, extends(abst_hcf) :: type_hcf_impedance
    contains
        procedure :: calc_kflh => calc_kflh_impedance
    end type type_hcf_impedance

    type, extends(abst_hcf) :: type_hcf_viscosity
    contains
        procedure :: calc_kflh => calc_kflh_viscosity
    end type type_hcf_viscosity

    type, extends(abst_hcf) :: type_hcf_base_impedance
    contains
        procedure :: calc_kflh => calc_kflh_base_impedance
    end type type_hcf_base_impedance

    type, extends(abst_hcf) :: type_hcf_base_viscosity
    contains
        procedure :: calc_kflh => calc_kflh_base_viscosity
    end type type_hcf_base_viscosity

    type, extends(abst_hcf) :: type_hcf_impedance_viscosity
    contains
        procedure :: calc_kflh => calc_kflh_impedance_viscosity
    end type type_hcf_impedance_viscosity

    type, extends(abst_hcf) :: type_hcf_base_impedance_viscosity
    contains
        procedure :: calc_kflh => calc_kflh_base_impedance_viscosity
    end type type_hcf_base_impedance_viscosity

    interface
        module pure elemental subroutine calc_kflh_base(self, state, kflh)
            implicit none
            class(type_hcf_base), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: kflh

        end subroutine calc_kflh_base

        module pure elemental subroutine calc_kflh_impedance(self, state, kflh)
            implicit none
            class(type_hcf_impedance), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: kflh

        end subroutine calc_kflh_impedance

        module pure elemental subroutine calc_kflh_viscosity(self, state, kflh)
            implicit none
            class(type_hcf_viscosity), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: kflh

        end subroutine calc_kflh_viscosity

        module pure elemental subroutine calc_kflh_base_impedance(self, state, kflh)
            implicit none
            class(type_hcf_base_impedance), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: kflh

        end subroutine calc_kflh_base_impedance

        module pure elemental subroutine calc_kflh_base_viscosity(self, state, kflh)
            implicit none
            class(type_hcf_base_viscosity), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: kflh

        end subroutine calc_kflh_base_viscosity

        module pure elemental subroutine calc_kflh_impedance_viscosity(self, state, kflh)
            implicit none
            class(type_hcf_impedance_viscosity), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: kflh

        end subroutine calc_kflh_impedance_viscosity

        module pure elemental subroutine calc_kflh_base_impedance_viscosity(self, state, kflh)
            implicit none
            class(type_hcf_base_impedance_viscosity), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: kflh

        end subroutine calc_kflh_base_impedance_viscosity

    end interface

    type, abstract :: abst_hcf_base
        private
        class(abst_hcf), pointer :: parent => null()
    contains
        procedure(abst_calc_base_kr), pass(self), public, deferred :: calc_kr
    end type abst_hcf_base

    abstract interface
        pure elemental subroutine abst_calc_base_kr(self, h, kr)
            import :: abst_hcf_base, type_params_hcf, real64
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
        module pure elemental subroutine calc_kr_base_bc(self, h, kr)
            implicit none
            class(type_hcf_base_bc), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: kr

        end subroutine calc_kr_base_bc

        module pure elemental subroutine calc_kr_base_vg(self, h, kr)
            implicit none
            class(type_hcf_base_vg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: kr

        end subroutine calc_kr_base_vg

        module pure elemental subroutine calc_kr_base_ko(self, h, kr)
            implicit none
            class(type_hcf_base_ko), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: kr

        end subroutine calc_kr_base_ko

        module pure elemental subroutine calc_kr_base_mvg(self, h, kr)
            implicit none
            class(type_hcf_base_mvg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: kr

        end subroutine calc_kr_base_mvg

        module pure elemental subroutine calc_kr_base_durner(self, h, kr)
            implicit none
            class(type_hcf_base_durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: kr

        end subroutine calc_kr_base_durner

        module pure elemental subroutine calc_kr_base_dvgch(self, h, kr)
            implicit none
            class(type_hcf_base_dvgch), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: kr

        end subroutine calc_kr_base_dvgch
    end interface

    type, abstract :: abst_hcf_impedance
        private
        class(abst_hcf), pointer :: parent => null()
    contains
        procedure(abst_calc_impedance_kr), pass(self), public, deferred :: calc_impedance
    end type abst_hcf_impedance

    type, extends(abst_hcf_impedance) :: type_hcf_impedance_exp
    contains
        procedure :: calc_impedance => calc_impedance_exp
    end type type_hcf_impedance_exp

    abstract interface
        pure elemental subroutine abst_calc_impedance_kr(self, Qice, kr)
            import :: abst_hcf_impedance, real64
            implicit none
            class(abst_hcf_impedance), intent(in) :: self
            real(real64), intent(in) :: Qice
            real(real64), intent(inout) :: kr

        end subroutine abst_calc_impedance_kr
    end interface

    interface
        module pure elemental subroutine calc_impedance_exp(self, Qice, kr)
            implicit none
            class(type_hcf_impedance_exp), intent(in) :: self
            real(real64), intent(in) :: Qice
            real(real64), intent(inout) :: kr

        end subroutine calc_impedance_exp
    end interface

    type, abstract :: abst_hcf_viscosity
        private
        real(real64) :: mu_zero
        class(abst_hcf), pointer :: parent => null()
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
        pure elemental subroutine abst_calc_mu(self, temperature, mu)
            import :: abst_hcf_viscosity, real64
            implicit none
            class(abst_hcf_viscosity), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64), intent(inout) :: mu

        end subroutine abst_calc_mu
    end interface

    interface
        module pure elemental subroutine calc_kr_abst_hcf_viscosity(self, temperature, kr)
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

        module pure elemental subroutine calc_mu_exponential(self, temperature, mu)
            implicit none
            class(type_hcf_viscosity_exp), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64), intent(inout) :: mu

        end subroutine calc_mu_exponential

        module pure elemental subroutine calc_mu_exponential_supercooled(self, temperature, mu)
            implicit none
            class(type_hcf_viscosity_supercool), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64), intent(inout) :: mu

        end subroutine calc_mu_exponential_supercooled

        module pure elemental subroutine calc_viscosity_exp(self, temperature, kr)
            implicit none
            class(type_hcf_viscosity_exp), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64), intent(inout) :: kr

        end subroutine calc_viscosity_exp

        module pure elemental subroutine calc_viscosity_supercool(self, temperature, kr)
            implicit none
            class(type_hcf_viscosity_supercool), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64), intent(inout) :: kr

        end subroutine calc_viscosity_supercool

    end interface

contains

    ! subroutine construct_hcf_base(input, material_id,property)
    !     implicit none
    !     type(type_input), intent(in) :: input
    !     integer(int32), intent(in) :: material_id
    !     class(abst_hcf_base), allocatable :: property

    !     associate (hcf => input%basic%materials(material_id)%hydraulic%hcf)
    !         select case (hcf%model_number)
    !         case (1)
    !             property = construct_type_hcf_base_bc(alpha1=hcf%alpha1, &
    !                                                   n1=hcf%n1, &
    !                                                   l=hcf%l)
    !         case (2)
    !             property = construct_type_hcf_base_vg(alpha1=hcf%alpha1, &
    !                                                   n1=hcf%n1, &
    !                                                   l=hcf%l)
    !         case (3)
    !             property = construct_type_hcf_base_ko(alpha1=hcf%alpha1, &
    !                                                   n1=hcf%n1, &
    !                                                   l=hcf%l)
    !         case (4)
    !             property = construct_type_hcf_base_mvg(theta_s=hcf%theta_s, &
    !                                                    theta_r=hcf%theta_r, &
    !                                                    alpha1=hcf%alpha1, &
    !                                                    n1=hcf%n1, &
    !                                                    l=hcf%l, &
    !                                                    h_crit=hcf%h_crit)
    !         case (5)
    !             property = construct_type_hcf_base_durner(alpha1=hcf%alpha1, &
    !                                                       n1=hcf%n1, &
    !                                                       w1=hcf%w1, &
    !                                                       alpha2=hcf%alpha2, &
    !                                                       n2=hcf%n2, &
    !                                                       l=hcf%l)
    !         case (6)
    !             property = construct_type_hcf_base_dvgch(alpha1=hcf%alpha1, &
    !                                                      n1=hcf%n1, &
    !                                                      w1=hcf%w1, &
    !                                                      n2=hcf%n2, &
    !                                                      l=hcf%l)
    !         end select
    !     end associate

    ! end subroutine construct_hcf_base

    ! subroutine construct_hcf_impedance(input, material_id,property)
    !     implicit none
    !     type(type_input), intent(in) :: input
    !     integer(int32), intent(in) :: material_id
    !     class(abst_hcf_impedance), allocatable :: property

    !     property = construct_type_hcf_impedance(omega=input%basic%materials(material_id)%hydraulic%impedance_factor)

    ! end subroutine construct_hcf_impedance

    ! subroutine construct_hcf_viscosity(input, material_id,property)
    !     implicit none
    !     type(type_input), intent(in) :: input
    !     integer(int32), intent(in) :: material_id
    !     class(abst_hcf_viscosity), allocatable :: property

    !     property = construct_type_hcf_viscosity(input%basic%materials(material_id)%hydraulic%water_viscosity_model)

    ! end subroutine construct_hcf_viscosity

    ! subroutine create_type_hcf_base(input, material_id,property)
    !     implicit none
    !     type(type_input), intent(in) :: input
    !     integer(int32), intent(in) :: material_id
    !     class(abst_hcf), allocatable :: property

    !     if (allocated(property)) deallocate (property)
    !     allocate (type_hcf_base :: property)

    !     property%k_s = input%basic%materials(material_id)%hydraulic%hydraulic_conductivity
    !     property%base = construct_hcf_base(input, material_id)

    ! end subroutine create_type_hcf_base

    ! subroutine create_type_hcf_impedance(input, material_id,property)
    !     implicit none
    !     type(type_input), intent(in) :: input
    !     integer(int32), intent(in) :: material_id
    !     class(abst_hcf), allocatable :: property

    !     if (allocated(property)) deallocate (property)
    !     allocate (type_hcf_impedance :: property)

    !     property%k_s = input%basic%materials(material_id)%hydraulic%hydraulic_conductivity
    !     property%impedance = construct_hcf_impedance(input, material_id)

    ! end subroutine create_type_hcf_impedance

    ! subroutine create_type_hcf_viscosity(input, material_id,property)
    !     implicit none
    !     type(type_input), intent(in) :: input
    !     integer(int32), intent(in) :: material_id
    !     class(abst_hcf), allocatable :: property

    !     if (allocated(property)) deallocate (property)
    !     allocate (type_hcf_viscosity :: property)

    !     property%k_s = input%basic%materials(material_id)%hydraulic%hydraulic_conductivity
    !     property%viscosity = construct_hcf_viscosity(input, material_id)

    ! end subroutine create_type_hcf_viscosity

    ! subroutine create_type_hcf_base_impedance(input, material_id,property)
    !     implicit none
    !     type(type_input), intent(in) :: input
    !     integer(int32), intent(in) :: material_id
    !     class(abst_hcf), allocatable :: property

    !     if (allocated(property)) deallocate (property)
    !     allocate (type_hcf_base_impedance :: property)

    !     property%k_s = input%basic%materials(material_id)%hydraulic%hydraulic_conductivity
    !     property%base = construct_hcf_base(input, material_id)
    !     property%impedance = construct_hcf_impedance(input, material_id)

    ! end subroutine create_type_hcf_base_impedance

    ! subroutine create_type_hcf_base_viscosity(input, material_id,property)
    !     implicit none
    !     type(type_input), intent(in) :: input
    !     integer(int32), intent(in) :: material_id
    !     class(abst_hcf), allocatable :: property

    !     if (allocated(property)) deallocate (property)
    !     allocate (type_hcf_base_viscosity :: property)

    !     property%k_s = input%basic%materials(material_id)%hydraulic%hydraulic_conductivity
    !     property%base = construct_hcf_base(input, material_id)
    !     property%viscosity = construct_hcf_viscosity(input, material_id)

    ! end subroutine create_type_hcf_base_viscosity

    ! subroutine create_type_hcf_impedance_viscosity(input, material_id,property)
    !     implicit none
    !     type(type_input), intent(in) :: input
    !     integer(int32), intent(in) :: material_id
    !     class(abst_hcf), allocatable :: property

    !     if (allocated(property)) deallocate (property)
    !     allocate (type_hcf_impedance_viscosity :: property)

    !     property%k_s = input%basic%materials(material_id)%hydraulic%hydraulic_conductivity
    !     property%impedance = construct_hcf_impedance(input, material_id)
    !     property%viscosity = construct_hcf_viscosity(input, material_id)

    ! end subroutine create_type_hcf_impedance_viscosity

    ! subroutine create_type_hcf_base_impedance_viscosity(input, material_id,property)
    !     implicit none
    !     type(type_input), intent(in) :: input
    !     integer(int32), intent(in) :: material_id
    !     class(abst_hcf), allocatable :: property

    !     if (allocated(property)) deallocate (property)
    !     allocate (type_hcf_base_impedance_viscosity :: property)

    !     property%k_s = input%basic%materials(material_id)%hydraulic%hydraulic_conductivity
    !     property%base = construct_hcf_base(input, material_id)
    !     property%impedance = construct_hcf_impedance(input, material_id)
    !     property%viscosity = construct_hcf_viscosity(input, material_id)

    ! end subroutine create_type_hcf_base_impedance_viscosity
end module physics_models_hcf
