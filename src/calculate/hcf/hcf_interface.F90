module calculate_hcf
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:allocate_array, type_state
#ifdef _OPENMP
    use omp_lib
#endif
    implicit none
    private

    public :: abst_hcf

    type :: holder_hcs
        class(abst_hcf), allocatable :: p
    contains
        ! procedure, pass(self) :: initialize => initialize_holder_hcs
    end type holder_hcs

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
        function abst_calc_kflh(self, state) result(kflh)
            import :: abst_hcf, type_state, real64
            implicit none
            class(abst_hcf), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: kflh

        end function abst_calc_kflh
    end interface

    interface
        module function calc_kflh_base(self, state) result(kflh)
            implicit none
            class(type_hcf_base), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: kflh

        end function calc_kflh_base

        module function calc_kflh_impedance(self, state) result(kflh)
            implicit none
            class(type_hcf_impedance), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: kflh

        end function calc_kflh_impedance

        module function calc_kflh_viscosity(self, state) result(kflh)
            implicit none
            class(type_hcf_viscosity), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: kflh

        end function calc_kflh_viscosity

        module function calc_kflh_base_impedance(self, state) result(kflh)
            implicit none
            class(type_hcf_base_impedance), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: kflh

        end function calc_kflh_base_impedance

        module function calc_kflh_base_viscosity(self, state) result(kflh)
            implicit none
            class(type_hcf_base_viscosity), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: kflh

        end function calc_kflh_base_viscosity

        module function calc_kflh_impedance_viscosity(self, state) result(kflh)
            implicit none
            class(type_hcf_impedance_viscosity), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: kflh

        end function calc_kflh_impedance_viscosity

        module function calc_kflh_base_impedance_viscosity(self, state) result(kflh)
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
        function abst_calc_base_kr(self, h) result(kr)
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

        module function calc_kr_base_bc(self, h) result(kr)
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

        module function calc_kr_base_vg(self, h) result(kr)
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

        module function calc_kr_base_ko(self, h) result(kr)
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

        module function calc_kr_base_mvg(self, h) result(kr)
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

        module function calc_kr_base_durner(self, h) result(kr)
            implicit none
            class(type_hcf_base_durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function calc_kr_base_durner

        module function calc_kr_base_dvgch(self, h) result(kr)
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
        function abst_calc_impedance_kr(self, q_ice) result(kr)
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

        module function calc_impedance_exp(self, q_ice) result(kr)
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
        function abst_calc_viscosity_kr(self, temperature) result(kr)
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

        module function calc_viscosity_exp(self, temperature) result(kr)
            implicit none
            class(type_hcf_viscosity_exp), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64) :: kr

        end function calc_viscosity_exp

        module function calc_viscosity_supercool(self, temperature) result(kr)
            implicit none
            class(type_hcf_viscosity_supercool), intent(in) :: self
            real(real64), intent(in) :: temperature
            real(real64) :: kr

        end function calc_viscosity_supercool

    end interface

contains

    ! function Construct_Type_HCF(useHCFType, Ks, thetaS, thetaR, alpha1, n1, w1, alpha2, n2, l, hcrit, omega, useViscosity, nsize) result(structure_HCF)
    !     implicit none
    !     integer(int32), intent(in) :: useHCFType
    !     real(real64), intent(in) :: Ks
    !     real(real64), intent(in), optional :: thetaS
    !     real(real64), intent(in), optional :: thetaR
    !     real(real64), intent(in), optional :: alpha1
    !     real(real64), intent(in), optional :: n1
    !     real(real64), intent(in), optional :: w1
    !     real(real64), intent(in), optional :: alpha2
    !     real(real64), intent(in), optional :: n2
    !     real(real64), intent(in), optional :: l
    !     real(real64), intent(in), optional :: hcrit
    !     real(real64), intent(in), optional :: omega
    !     integer(int32), intent(in), optional :: useViscosity
    !     integer(int32), intent(in) :: nsize
    !     class(abst_hcf), allocatable :: structure_HCF

    !     select case (useHCFType)
    !     case (11)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(l) &
    !             ) stop "Missing parameters for HCF type 11"
    !         structure_HCF = Type_HCF_Base_BC(Ks=Ks, &
    !                                          alpha1=alpha1, &
    !                                          n1=n1, &
    !                                          l=l, &
    !                                          nsize=nsize)
    !     case (12)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(l) &
    !             ) stop "Missing parameters for HCF type 12"
    !         structure_HCF = Type_HCF_Base_VG(Ks=Ks, &
    !                                          alpha1=alpha1, &
    !                                          n1=n1, &
    !                                          l=l, &
    !                                          nsize=nsize)
    !     case (13)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(l) &
    !             ) stop "Missing parameters for HCF type 13"
    !         structure_HCF = Type_HCF_Base_KO(Ks=Ks, &
    !                                          alpha1=alpha1, &
    !                                          n1=n1, &
    !                                          l=l, &
    !                                          nsize=nsize)
    !     case (14)
    !         if (.not. present(thetaS) .or. &
    !             .not. present(thetaR) .or. &
    !             .not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(l) .or. &
    !             .not. present(hcrit) &
    !             ) stop "Missing parameters for HCF type 14"
    !         structure_HCF = Type_HCF_Base_MVG(Ks=Ks, &
    !                                           thetaS=thetaS, &
    !                                           thetaR=thetaR, &
    !                                           alpha1=alpha1, &
    !                                           n1=n1, &
    !                                           l=l, &
    !                                           hcrit=hcrit, &
    !                                           nsize=nsize)
    !     case (15)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(w1) .or. &
    !             .not. present(alpha2) .or. &
    !             .not. present(n2) .or. &
    !             .not. present(l) &
    !             ) stop "Missing parameters for HCF type 15"
    !         structure_HCF = Type_HCF_Base_Durner(Ks=Ks, &
    !                                              alpha1=alpha1, &
    !                                              n1=n1, &
    !                                              w1=w1, &
    !                                              alpha2=alpha2, &
    !                                              n2=n2, &
    !                                              l=l, &
    !                                              nsize=nsize)
    !     case (16)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(w1) .or. &
    !             .not. present(n2) .or. &
    !             .not. present(l) &
    !             ) stop "Missing parameters for HCF type 16"
    !         structure_HCF = Type_HCF_Base_DVGCH(Ks=Ks, &
    !                                             alpha1=alpha1, &
    !                                             n1=n1, &
    !                                             w1=w1, &
    !                                             n2=n2, &
    !                                             l=l, &
    !                                             nsize=nsize)
    !     case (21)
    !         if (.not. present(omega)) stop "Missing omega for HCF type 21"
    !         structure_HCF = Type_HCF_Impedance_exp(Ks=Ks, &
    !                                            omega=omega, &
    !                                            nsize=nsize)
    !     case (31)
    !         if (.not. present(useViscosity)) stop "Missing useViscosity for HCF type 31"
    !         structure_HCF = Type_HCF_Viscosity(Ks=Ks, &
    !                                            useViscosity=useViscosity, &
    !                                            nsize=nsize)
    !     case (41)
    !         if (.not. present(omega) .or. &
    !             .not. present(useViscosity) &
    !             ) stop "Missing parameters for HCF type 41"
    !         structure_HCF = Type_HCF_Impedance_exp_Viscosity(Ks=Ks, &
    !                                                      omega=omega, &
    !                                                      useViscosity=useViscosity, &
    !                                                      nsize=nsize)
    !     case (51)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(l) .or. &
    !             .not. present(omega) &
    !             ) stop "Missing parameters for HCF type 51"
    !         structure_HCF = Type_HCF_Base_Impedance_BC(Ks=Ks, &
    !                                                    alpha1=alpha1, &
    !                                                    n1=n1, &
    !                                                    l=l, &
    !                                                    omega=omega, &
    !                                                    nsize=nsize)
    !     case (52)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(l) .or. &
    !             .not. present(omega) &
    !             ) stop "Missing parameters for HCF type 52"
    !         structure_HCF = Type_HCF_Base_Impedance_VG(Ks=Ks, &
    !                                                    alpha1=alpha1, &
    !                                                    n1=n1, &
    !                                                    l=l, &
    !                                                    omega=omega, &
    !                                                    nsize=nsize)
    !     case (53)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(l) .or. &
    !             .not. present(omega) &
    !             ) stop "Missing parameters for HCF type 53"
    !         structure_HCF = Type_HCF_Base_Impedance_KO(Ks=Ks, &
    !                                                    alpha1=alpha1, &
    !                                                    n1=n1, &
    !                                                    l=l, &
    !                                                    omega=omega, &
    !                                                    nsize=nsize)
    !     case (54)
    !         if (.not. present(thetaS) .or. &
    !             .not. present(thetaR) .or. &
    !             .not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(l) .or. &
    !             .not. present(hcrit) .or. &
    !             .not. present(omega) &
    !             ) stop "Missing parameters for HCF type 54"
    !         structure_HCF = Type_HCF_Base_Impedance_MVG(Ks=Ks, &
    !                                                     thetaS=thetaS, &
    !                                                     thetaR=thetaR, &
    !                                                     alpha1=alpha1, &
    !                                                     n1=n1, &
    !                                                     l=l, &
    !                                                     hcrit=hcrit, &
    !                                                     omega=omega, &
    !                                                     nsize=nsize)
    !     case (55)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(w1) .or. &
    !             .not. present(alpha2) .or. &
    !             .not. present(n2) .or. &
    !             .not. present(l) .or. &
    !             .not. present(omega) &
    !             ) stop "Missing parameters for HCF type 55"
    !         structure_HCF = Type_HCF_Base_Impedance_Durner(Ks=Ks, &
    !                                                        alpha1=alpha1, &
    !                                                        n1=n1, &
    !                                                        w1=w1, &
    !                                                        alpha2=alpha2, &
    !                                                        n2=n2, &
    !                                                        l=l, &
    !                                                        omega=omega, &
    !                                                        nsize=nsize)
    !     case (56)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(w1) .or. &
    !             .not. present(n2) .or. &
    !             .not. present(l) .or. &
    !             .not. present(omega) &
    !             ) stop "Missing parameters for HCF type 56"
    !         structure_HCF = Type_HCF_Base_Impedance_DVGCH(Ks=Ks, &
    !                                                       alpha1=alpha1, &
    !                                                       n1=n1, &
    !                                                       w1=w1, &
    !                                                       n2=n2, &
    !                                                       l=l, &
    !                                                       omega=omega, &
    !                                                       nsize=nsize)
    !     case (61)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(l) .or. &
    !             .not. present(useViscosity) &
    !             ) stop "Missing parameters for HCF type 61"
    !         structure_HCF = Type_HCF_Base_Viscosity_BC(Ks=Ks, &
    !                                                    alpha1=alpha1, &
    !                                                    n1=n1, &
    !                                                    l=l, &
    !                                                    useViscosity=useViscosity, &
    !                                                    nsize=nsize)
    !     case (62)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(l) .or. &
    !             .not. present(useViscosity) &
    !             ) stop "Missing parameters for HCF type 62"
    !         structure_HCF = Type_HCF_Base_Viscosity_VG(Ks=Ks, &
    !                                                    alpha1=alpha1, &
    !                                                    n1=n1, &
    !                                                    l=l, &
    !                                                    useViscosity=useViscosity, &
    !                                                    nsize=nsize)
    !     case (63)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(l) .or. &
    !             .not. present(useViscosity) &
    !             ) stop "Missing parameters for HCF type 63"
    !         structure_HCF = Type_HCF_Base_Viscosity_KO(Ks=Ks, &
    !                                                    alpha1=alpha1, &
    !                                                    n1=n1, &
    !                                                    l=l, &
    !                                                    useViscosity=useViscosity, &
    !                                                    nsize=nsize)
    !     case (64)
    !         if (.not. present(thetaS) .or. &
    !             .not. present(thetaR) .or. &
    !             .not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(l) .or. &
    !             .not. present(hcrit) .or. &
    !             .not. present(useViscosity) &
    !             ) stop "Missing parameters for HCF type 64"
    !         structure_HCF = Type_HCF_Base_Viscosity_MVG(Ks=Ks, &
    !                                                     thetaS=thetaS, &
    !                                                     thetaR=thetaR, &
    !                                                     alpha1=alpha1, &
    !                                                     n1=n1, &
    !                                                     l=l, &
    !                                                     hcrit=hcrit, &
    !                                                     useViscosity=useViscosity, &
    !                                                     nsize=nsize)
    !     case (65)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(w1) .or. &
    !             .not. present(alpha2) .or. &
    !             .not. present(n2) .or. &
    !             .not. present(l) .or. &
    !             .not. present(useViscosity) &
    !             ) stop "Missing parameters for HCF type 65"
    !         structure_HCF = Type_HCF_Base_Viscosity_Durner(Ks=Ks, &
    !                                                        alpha1=alpha1, &
    !                                                        n1=n1, &
    !                                                        w1=w1, &
    !                                                        alpha2=alpha2, &
    !                                                        n2=n2, &
    !                                                        l=l, &
    !                                                        useViscosity=useViscosity, &
    !                                                        nsize=nsize)
    !     case (66)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(w1) .or. &
    !             .not. present(n2) .or. &
    !             .not. present(l) .or. &
    !             .not. present(useViscosity) &
    !             ) stop "Missing parameters for HCF type 66"
    !         structure_HCF = Type_HCF_Base_Viscosity_DVGCH(Ks=Ks, &
    !                                                       alpha1=alpha1, &
    !                                                       n1=n1, &
    !                                                       w1=w1, &
    !                                                       n2=n2, &
    !                                                       l=l, &
    !                                                       useViscosity=useViscosity, &
    !                                                       nsize=nsize)
    !     case (71)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(l) .or. &
    !             .not. present(omega) .or. &
    !             .not. present(useViscosity) &
    !             ) stop "Missing parameters for HCF type 71"
    !         structure_HCF = Type_HCF_Base_Impedance_Viscosity_BC(Ks=Ks, &
    !                                                              alpha1=alpha1, &
    !                                                              n1=n1, &
    !                                                              l=l, &
    !                                                              omega=omega, &
    !                                                              useViscosity=useViscosity, &
    !                                                              nsize=nsize)
    !     case (72)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(l) .or. &
    !             .not. present(omega) .or. &
    !             .not. present(useViscosity) &
    !             ) stop "Missing parameters for HCF type 72"
    !         structure_HCF = Type_HCF_Base_Impedance_Viscosity_VG(Ks=Ks, &
    !                                                              alpha1=alpha1, &
    !                                                              n1=n1, &
    !                                                              l=l, &
    !                                                              omega=omega, &
    !                                                              useViscosity=useViscosity, &
    !                                                              nsize=nsize)
    !     case (73)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(l) .or. &
    !             .not. present(omega) .or. &
    !             .not. present(useViscosity) &
    !             ) stop "Missing parameters for HCF type 73"
    !         structure_HCF = Type_HCF_Base_Impedance_Viscosity_KO(Ks=Ks, &
    !                                                              alpha1=alpha1, &
    !                                                              n1=n1, &
    !                                                              l=l, &
    !                                                              omega=omega, &
    !                                                              useViscosity=useViscosity, &
    !                                                              nsize=nsize)
    !     case (74)
    !         if (.not. present(thetaS) .or. &
    !             .not. present(thetaR) .or. &
    !             .not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(l) .or. &
    !             .not. present(hcrit) .or. &
    !             .not. present(omega) .or. &
    !             .not. present(useViscosity) &
    !             ) stop "Missing parameters for HCF type 74"
    !         structure_HCF = Type_HCF_Base_Impedance_Viscosity_MVG(Ks=Ks, &
    !                                                               thetaS=thetaS, &
    !                                                               thetaR=thetaR, &
    !                                                               alpha1=alpha1, &
    !                                                               n1=n1, &
    !                                                               l=l, &
    !                                                               hcrit=hcrit, &
    !                                                               omega=omega, &
    !                                                               useViscosity=useViscosity, &
    !                                                               nsize=nsize)
    !     case (75)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(w1) .or. &
    !             .not. present(alpha2) .or. &
    !             .not. present(n2) .or. &
    !             .not. present(l) .or. &
    !             .not. present(omega) .or. &
    !             .not. present(useViscosity) &
    !             ) stop "Missing parameters for HCF type 75"
    !         structure_HCF = Type_HCF_Base_Impedance_Viscosity_Durner(Ks=Ks, &
    !                                                                  alpha1=alpha1, &
    !                                                                  n1=n1, &
    !                                                                  w1=w1, &
    !                                                                  alpha2=alpha2, &
    !                                                                  n2=n2, &
    !                                                                  l=l, &
    !                                                                  omega=omega, &
    !                                                                  useViscosity=useViscosity, &
    !                                                                  nsize=nsize)

    !     case (76)
    !         if (.not. present(alpha1) .or. &
    !             .not. present(n1) .or. &
    !             .not. present(w1) .or. &
    !             .not. present(n2) .or. &
    !             .not. present(l) .or. &
    !             .not. present(omega) .or. &
    !             .not. present(useViscosity) &
    !             ) stop "Missing parameters for HCF type 76"
    !         structure_HCF = Type_HCF_Base_Impedance_Viscosity_DVGCH(Ks=Ks, &
    !                                                                 alpha1=alpha1, &
    !                                                                 n1=n1, &
    !                                                                 w1=w1, &
    !                                                                 n2=n2, &
    !                                                                 l=l, &
    !                                                                 omega=omega, &
    !                                                                 useViscosity=useViscosity, &
    !                                                                 nsize=nsize)
    !     end select

    ! end function Construct_Type_HCF

    ! function Construct_Type_HCF_minimal(useHCFType) result(structure_HCF)
    !     implicit none
    !     integer(int32), intent(in) :: useHCFType
    !     class(abst_hcf), allocatable :: structure_HCF

    !     select case (useHCFType)
    !     case (11)
    !         structure_HCF = Type_HCF_Base_BC()
    !     case (12)
    !         structure_HCF = Type_HCF_Base_VG()
    !     case (13)
    !         structure_HCF = Type_HCF_Base_KO()
    !     case (14)
    !         structure_HCF = Type_HCF_Base_MVG()
    !     case (15)
    !         structure_HCF = Type_HCF_Base_Durner()
    !     case (16)
    !         structure_HCF = Type_HCF_Base_DVGCH()
    !     case (21)
    !         structure_HCF = Type_HCF_Impedance_exp()
    !     case (31)
    !         structure_HCF = Type_HCF_Viscosity()
    !     case (41)
    !         structure_HCF = Type_HCF_Impedance_exp_Viscosity()
    !     case (51)
    !         structure_HCF = Type_HCF_Base_Impedance_BC()
    !     case (52)
    !         structure_HCF = Type_HCF_Base_Impedance_VG()
    !     case (53)
    !         structure_HCF = Type_HCF_Base_Impedance_KO()
    !     case (54)
    !         structure_HCF = Type_HCF_Base_Impedance_MVG()
    !     case (55)
    !         structure_HCF = Type_HCF_Base_Impedance_Durner()
    !     case (56)
    !         structure_HCF = Type_HCF_Base_Impedance_DVGCH()
    !     case (61)
    !         structure_HCF = Type_HCF_Base_Viscosity_BC()
    !     case (62)
    !         structure_HCF = Type_HCF_Base_Viscosity_VG()
    !     case (63)
    !         structure_HCF = Type_HCF_Base_Viscosity_KO()
    !     case (64)
    !         structure_HCF = Type_HCF_Base_Viscosity_MVG()
    !     case (65)
    !         structure_HCF = Type_HCF_Base_Viscosity_Durner()
    !     case (66)
    !         structure_HCF = Type_HCF_Base_Viscosity_DVGCH()
    !     case (71)
    !         structure_HCF = Type_HCF_Base_Impedance_Viscosity_BC()
    !     case (72)
    !         structure_HCF = Type_HCF_Base_Impedance_Viscosity_VG()
    !     case (73)
    !         structure_HCF = Type_HCF_Base_Impedance_Viscosity_KO()
    !     case (74)
    !         structure_HCF = Type_HCF_Base_Impedance_Viscosity_MVG()
    !     case (75)
    !         structure_HCF = Type_HCF_Base_Impedance_Viscosity_Durner()
    !     case (76)
    !         structure_HCF = Type_HCF_Base_Impedance_Viscosity_DVGCH()
    !     end select

    ! end function Construct_Type_HCF_minimal

end module calculate_hcf
