module physics_models_hcf
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_state
    use :: module_input, only:type_input
    implicit none
    private

    public :: abst_hcf
    public :: holder_hcfs
    public :: type_hcf_base
    public :: type_hcf_impedance
    public :: type_hcf_viscosity
    public :: type_hcf_base_impedance
    public :: type_hcf_base_viscosity
    public :: type_hcf_impedance_viscosity
    public :: type_hcf_base_impedance_viscosity

    type :: holder_hcfs
        class(abst_hcf), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_hcfs
    end type holder_hcfs

    type, abstract :: abst_hcf
        private
        real(real64) :: k_s
        class(abst_hcf_base), allocatable :: base
        class(abst_hcf_impedance), allocatable :: impedance
        class(abst_hcf_viscosity), allocatable :: viscosity
    contains
        procedure(abst_calc_kflh), pass(self), public, deferred :: calc_kflh
    end type abst_hcf

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

    abstract interface
        pure elemental function abst_calc_kflh(self, state) result(kflh)
            import :: abst_hcf, type_state, real64
            implicit none
            class(abst_hcf), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: kflh

        end function abst_calc_kflh
    end interface

    interface
        module subroutine initialize_holder_hcfs(self, input, material_id)
            implicit none
            class(holder_hcfs), intent(inout) :: self
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: material_id
        end subroutine initialize_holder_hcfs
    end interface

    interface
        module pure elemental function calc_kflh_base(self, state) result(kflh)
            implicit none
            class(type_hcf_base), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: kflh

        end function calc_kflh_base

        module pure elemental function calc_kflh_impedance(self, state) result(kflh)
            implicit none
            class(type_hcf_impedance), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: kflh

        end function calc_kflh_impedance

        module pure elemental function calc_kflh_viscosity(self, state) result(kflh)
            implicit none
            class(type_hcf_viscosity), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: kflh

        end function calc_kflh_viscosity

        module pure elemental function calc_kflh_base_impedance(self, state) result(kflh)
            implicit none
            class(type_hcf_base_impedance), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: kflh

        end function calc_kflh_base_impedance

        module pure elemental function calc_kflh_base_viscosity(self, state) result(kflh)
            implicit none
            class(type_hcf_base_viscosity), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: kflh

        end function calc_kflh_base_viscosity

        module pure elemental function calc_kflh_impedance_viscosity(self, state) result(kflh)
            implicit none
            class(type_hcf_impedance_viscosity), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: kflh

        end function calc_kflh_impedance_viscosity

        module pure elemental function calc_kflh_base_impedance_viscosity(self, state) result(kflh)
            implicit none
            class(type_hcf_base_impedance_viscosity), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: kflh

        end function calc_kflh_base_impedance_viscosity

    end interface

    type, abstract :: abst_hcf_base
        private
        real(real64) :: theta_r
        real(real64) :: theta_s
        real(real64) :: alpha1
        real(real64) :: alpha2
        real(real64) :: n1
        real(real64) :: n2
        real(real64) :: m1
        real(real64) :: m2
        real(real64) :: w1
        real(real64) :: w2
        real(real64) :: h_crit
        real(real64) :: l
    contains
        procedure(abst_calc_base_kr), pass(self), public, deferred :: calc_kr
    end type abst_hcf_base

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

    abstract interface
        pure elemental function abst_calc_base_kr(self, h) result(kr)
            import :: abst_hcf_base, real64
            implicit none
            class(abst_hcf_base), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function abst_calc_base_kr
    end interface

    interface
        module function construct_type_hcf_base_bc(alpha1, n1, l) result(structure)
            implicit none
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            class(abst_hcf_base), allocatable :: structure

        end function construct_type_hcf_base_bc

        module pure elemental function calc_kr_base_bc(self, h) result(kr)
            implicit none
            class(type_hcf_base_bc), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function calc_kr_base_bc

        module function construct_type_hcf_base_vg(alpha1, n1, l) result(structure)
            implicit none
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            class(abst_hcf_base), allocatable :: structure

        end function construct_type_hcf_base_vg

        module pure elemental function calc_kr_base_vg(self, h) result(kr)
            implicit none
            class(type_hcf_base_vg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function calc_kr_base_vg

        module function construct_type_hcf_base_ko(alpha1, n1, l) result(structure)
            implicit none
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            class(abst_hcf_base), allocatable :: structure

        end function construct_type_hcf_base_ko

        module pure elemental function calc_kr_base_ko(self, h) result(kr)
            implicit none
            class(type_hcf_base_ko), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function calc_kr_base_ko

        module function construct_type_hcf_base_mvg(theta_s, theta_r, alpha1, n1, l, h_crit) result(structure)
            implicit none
            real(real64), intent(in) :: theta_s
            real(real64), intent(in) :: theta_r
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            real(real64), intent(in) :: h_crit
            class(abst_hcf_base), allocatable :: structure

        end function construct_type_hcf_base_mvg

        module pure elemental function calc_kr_base_mvg(self, h) result(kr)
            implicit none
            class(type_hcf_base_mvg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function calc_kr_base_mvg

        module function construct_type_hcf_base_durner(alpha1, n1, w1, alpha2, n2, l) result(structure)
            implicit none
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: w1
            real(real64), intent(in) :: alpha2
            real(real64), intent(in) :: n2
            real(real64), intent(in) :: l
            class(abst_hcf_base), allocatable :: structure

        end function construct_type_hcf_base_durner

        module pure elemental function calc_kr_base_durner(self, h) result(kr)
            implicit none
            class(type_hcf_base_durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function calc_kr_base_durner

        module function construct_type_hcf_base_dvgch(alpha1, n1, w1, n2, l) result(structure)
            implicit none
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: w1
            real(real64), intent(in) :: n2
            real(real64), intent(in) :: l
            class(abst_hcf_base), allocatable :: structure

        end function construct_type_hcf_base_dvgch

        module pure elemental function calc_kr_base_dvgch(self, h) result(kr)
            implicit none
            class(type_hcf_base_dvgch), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function calc_kr_base_dvgch
    end interface

    type, abstract :: abst_hcf_impedance
        private
        real(real64) :: omega !! Impedance factor
    contains
        procedure(abst_calc_impedance_kr), pass(self), public, deferred :: calc_impedance
    end type abst_hcf_impedance

    type, extends(abst_hcf_impedance) :: type_hcf_impedance_exp
    contains
        procedure :: calc_impedance => calc_impedance_exp
    end type type_hcf_impedance_exp

    abstract interface
        pure elemental function abst_calc_impedance_kr(self, q_ice) result(kr)
            import :: abst_hcf_impedance, real64
            implicit none
            class(abst_hcf_impedance), intent(in) :: self
            real(real64), intent(in) :: q_ice
            real(real64) :: kr

        end function abst_calc_impedance_kr
    end interface

    interface
        module function construct_type_hcf_impedance(omega) result(structure)
            implicit none
            real(real64), intent(in) :: omega
            class(abst_hcf_impedance), allocatable :: structure

        end function construct_type_hcf_impedance

        module pure elemental function calc_impedance_exp(self, q_ice) result(kr)
            implicit none
            class(type_hcf_impedance_exp), intent(in) :: self
            real(real64), intent(in) :: q_ice
            real(real64) :: kr

        end function calc_impedance_exp
    end interface

    type, abstract :: abst_hcf_viscosity
        private
        real(real64) :: mu_zero
    contains
        procedure(abst_calc_viscosity_kr), pass(self), public, deferred :: calc_viscosity
    end type abst_hcf_viscosity

    type, extends(abst_hcf_viscosity) :: type_hcf_viscosity_exp
    contains
        procedure :: calc_viscosity => calc_viscosity_exp
    end type type_hcf_viscosity_exp

    type, extends(abst_hcf_viscosity) :: type_hcf_viscosity_supercool
    contains
        procedure :: calc_viscosity => calc_viscosity_supercool
    end type type_hcf_viscosity_supercool

    abstract interface
        pure elemental function abst_calc_viscosity_kr(self, temperature) result(kr)
            import :: abst_hcf_viscosity, real64
            implicit none
            class(abst_hcf_viscosity), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64) :: kr

        end function abst_calc_viscosity_kr
    end interface

    interface
        module function construct_type_hcf_viscosity(water_viscosity_model) result(structure)
            implicit none
            integer(int32), intent(in) :: water_viscosity_model
            class(abst_hcf_viscosity), allocatable :: structure

        end function construct_type_hcf_viscosity

        module pure elemental function calc_viscosity_exp(self, temperature) result(kr)
            implicit none
            class(type_hcf_viscosity_exp), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64) :: kr

        end function calc_viscosity_exp

        module pure elemental function calc_viscosity_supercool(self, temperature) result(kr)
            implicit none
            class(type_hcf_viscosity_supercool), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64) :: kr

        end function calc_viscosity_supercool

    end interface

contains

    function construct_hcf_base(input, material_id) result(property)
        implicit none
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id
        class(abst_hcf_base), allocatable :: property

        select case (input%basic%materials(material_id)%hydraulic%hcf%model_number)
        case (1)
            property = construct_type_hcf_base_bc(alpha1=input%basic%materials(material_id)%hydraulic%hcf%alpha1, &
                                                  n1=input%basic%materials(material_id)%hydraulic%hcf%n1, &
                                                  l=input%basic%materials(material_id)%hydraulic%hcf%l)
        case (2)
            property = construct_type_hcf_base_vg(alpha1=input%basic%materials(material_id)%hydraulic%hcf%alpha1, &
                                                  n1=input%basic%materials(material_id)%hydraulic%hcf%n1, &
                                                  l=input%basic%materials(material_id)%hydraulic%hcf%l)
        case (3)
            property = construct_type_hcf_base_ko(alpha1=input%basic%materials(material_id)%hydraulic%hcf%alpha1, &
                                                  n1=input%basic%materials(material_id)%hydraulic%hcf%n1, &
                                                  l=input%basic%materials(material_id)%hydraulic%hcf%l)
        case (4)
            property = construct_type_hcf_base_mvg(theta_s=input%basic%materials(material_id)%hydraulic%hcf%theta_s, &
                                                   theta_r=input%basic%materials(material_id)%hydraulic%hcf%theta_r, &
                                                   alpha1=input%basic%materials(material_id)%hydraulic%hcf%alpha1, &
                                                   n1=input%basic%materials(material_id)%hydraulic%hcf%n1, &
                                                   l=input%basic%materials(material_id)%hydraulic%hcf%l, &
                                                   h_crit=input%basic%materials(material_id)%hydraulic%hcf%h_crit)
        case (5)
            property = construct_type_hcf_base_durner(alpha1=input%basic%materials(material_id)%hydraulic%hcf%alpha1, &
                                                      n1=input%basic%materials(material_id)%hydraulic%hcf%n1, &
                                                      w1=input%basic%materials(material_id)%hydraulic%hcf%w1, &
                                                      alpha2=input%basic%materials(material_id)%hydraulic%hcf%alpha2, &
                                                      n2=input%basic%materials(material_id)%hydraulic%hcf%n2, &
                                                      l=input%basic%materials(material_id)%hydraulic%hcf%l)
        case (6)
            property = construct_type_hcf_base_dvgch(alpha1=input%basic%materials(material_id)%hydraulic%hcf%alpha1, &
                                                     n1=input%basic%materials(material_id)%hydraulic%hcf%n1, &
                                                     w1=input%basic%materials(material_id)%hydraulic%hcf%w1, &
                                                     n2=input%basic%materials(material_id)%hydraulic%hcf%n2, &
                                                     l=input%basic%materials(material_id)%hydraulic%hcf%l)
        end select

    end function construct_hcf_base

    function construct_hcf_impedance(input, material_id) result(property)
        implicit none
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id
        class(abst_hcf_impedance), allocatable :: property

        property = construct_type_hcf_impedance(omega=input%basic%materials(material_id)%hydraulic%impedance_factor)

    end function construct_hcf_impedance

    function construct_hcf_viscosity(input, material_id) result(property)
        implicit none
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id
        class(abst_hcf_viscosity), allocatable :: property

        property = construct_type_hcf_viscosity(input%basic%materials(material_id)%hydraulic%water_viscosity_model)

    end function construct_hcf_viscosity

    function create_type_hcf_base(input, material_id) result(property)
        implicit none
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id
        class(abst_hcf), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_hcf_base :: property)

        property%k_s = input%basic%materials(material_id)%hydraulic%hydraulic_conductivity
        property%base = construct_hcf_base(input, material_id)

    end function create_type_hcf_base

    function create_type_hcf_impedance(input, material_id) result(property)
        implicit none
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id
        class(abst_hcf), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_hcf_impedance :: property)

        property%k_s = input%basic%materials(material_id)%hydraulic%hydraulic_conductivity
        property%impedance = construct_hcf_impedance(input, material_id)

    end function create_type_hcf_impedance

    function create_type_hcf_viscosity(input, material_id) result(property)
        implicit none
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id
        class(abst_hcf), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_hcf_viscosity :: property)

        property%k_s = input%basic%materials(material_id)%hydraulic%hydraulic_conductivity
        property%viscosity = construct_hcf_viscosity(input, material_id)

    end function create_type_hcf_viscosity

    function create_type_hcf_base_impedance(input, material_id) result(property)
        implicit none
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id
        class(abst_hcf), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_hcf_base_impedance :: property)

        property%k_s = input%basic%materials(material_id)%hydraulic%hydraulic_conductivity
        property%base = construct_hcf_base(input, material_id)
        property%impedance = construct_hcf_impedance(input, material_id)

    end function create_type_hcf_base_impedance

    function create_type_hcf_base_viscosity(input, material_id) result(property)
        implicit none
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id
        class(abst_hcf), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_hcf_base_viscosity :: property)

        property%k_s = input%basic%materials(material_id)%hydraulic%hydraulic_conductivity
        property%base = construct_hcf_base(input, material_id)
        property%viscosity = construct_hcf_viscosity(input, material_id)

    end function create_type_hcf_base_viscosity

    function create_type_hcf_impedance_viscosity(input, material_id) result(property)
        implicit none
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id
        class(abst_hcf), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_hcf_impedance_viscosity :: property)

        property%k_s = input%basic%materials(material_id)%hydraulic%hydraulic_conductivity
        property%impedance = construct_hcf_impedance(input, material_id)
        property%viscosity = construct_hcf_viscosity(input, material_id)

    end function create_type_hcf_impedance_viscosity

    function create_type_hcf_base_impedance_viscosity(input, material_id) result(property)
        implicit none
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id
        class(abst_hcf), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_hcf_base_impedance_viscosity :: property)

        property%k_s = input%basic%materials(material_id)%hydraulic%hydraulic_conductivity
        property%base = construct_hcf_base(input, material_id)
        property%impedance = construct_hcf_impedance(input, material_id)
        property%viscosity = construct_hcf_viscosity(input, material_id)

    end function create_type_hcf_base_impedance_viscosity
end module physics_models_hcf
