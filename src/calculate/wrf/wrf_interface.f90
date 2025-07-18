module calculate_wrf
    use, intrinsic :: iso_fortran_env
    use :: module_input, only:type_input, type_materials_wrf
    implicit none
    private
    real(real64), parameter :: pi = 4 * atan(1.0d0)

    !-------------------------------------
    ! Pulic Types and Interfaces
    !-------------------------------------
    public :: holder_wrfs
    public :: abst_wrf
    public :: type_wrf_bc
    public :: type_wrf_vg
    public :: type_wrf_ko
    public :: type_wrf_mvg
    public :: type_wrf_durner
    public :: type_wrf_dvgch
    !-------------------------------------

    type :: holder_wrfs
        class(abst_wrf), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_wrfs
    end type holder_wrfs

    type, abstract :: abst_wrf
        real(real64) :: theta_r
        real(real64) :: theta_s
    contains
        procedure(abst_calc_wrf), deferred :: calc
        procedure(abst_calc_wrf_derivative), deferred :: deriv
    end type abst_wrf

    type, extends(abst_wrf) :: type_wrf_bc
        real(real64) :: alpha1
        real(real64) :: n1
    contains
        procedure :: calc => calculate_wrf_bc
        procedure :: deriv => calculate_wrf_bc_derivative
    end type type_wrf_bc

    type, extends(abst_wrf) :: type_wrf_vg
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: m1
    contains
        procedure :: calc => calculate_wrf_vg
        procedure :: deriv => calculate_wrf_vg_derivative
    end type type_wrf_vg

    type, extends(abst_wrf) :: type_wrf_ko
        real(real64) :: alpha1
        real(real64) :: n1
    contains
        procedure :: calc => calculate_wrf_ko
        procedure :: deriv => calculate_wrf_ko_derivative
    end type type_wrf_ko

    type, extends(abst_wrf) :: type_wrf_mvg
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: m1
        real(real64) :: h_crit
    contains
        procedure :: calc => calculate_wrf_mvg
        procedure :: deriv => calculate_wrf_mvg_derivative
    end type type_wrf_mvg

    type, extends(abst_wrf) :: type_wrf_durner
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: m1
        real(real64) :: alpha2
        real(real64) :: n2
        real(real64) :: m2
        real(real64) :: w1
        real(real64) :: w2
    contains
        procedure :: calc => calculate_wrf_durner
        procedure :: deriv => calculate_wrf_durner_derivative
    end type type_wrf_durner

    type, extends(abst_wrf) :: type_wrf_dvgch
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: n2
        real(real64) :: m1
        real(real64) :: m2
        real(real64) :: w1
        real(real64) :: w2
    contains
        procedure :: calc => calculate_wrf_dvgch
        procedure :: deriv => calculate_wrf_dvgch_derivative
    end type type_wrf_dvgch

    abstract interface
        function abst_calc_wrf(self, h) result(theta_w)
            import :: abst_wrf, real64
            implicit none
            class(abst_wrf), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: theta_w
        end function abst_calc_wrf

        function abst_calc_wrf_derivative(self, h) result(dqw_dh)
            import :: abst_wrf, real64
            implicit none
            class(abst_wrf), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: dqw_dh
        end function abst_calc_wrf_derivative
    end interface

    interface
        module subroutine initialize_holder_wrfs(self, input, i_material)
            implicit none
            class(holder_wrfs), intent(inout) :: self
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: i_material

        end subroutine initialize_holder_wrfs
    end interface

    interface
        module function construct_type_wrf_bc(input) result(property)
            implicit none
            type(type_materials_wrf), intent(in) :: input
            class(abst_wrf), allocatable :: property

        end function construct_type_wrf_bc

        module function calculate_wrf_bc(self, h) result(theta_w)
            implicit none
            class(type_wrf_bc), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: theta_w

        end function calculate_wrf_bc

        module function calculate_wrf_bc_derivative(self, h) result(dqw_dh)
            implicit none
            class(type_wrf_bc), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: dqw_dh

        end function calculate_wrf_bc_derivative
    end interface

    interface
        module function construct_type_wrf_vg(input) result(property)
            implicit none
            type(type_materials_wrf), intent(in) :: input
            class(abst_wrf), allocatable :: property

        end function construct_type_wrf_vg

        module function calculate_wrf_vg(self, h) result(theta_w)
            implicit none
            class(type_wrf_vg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: theta_w

        end function calculate_wrf_vg

        module function calculate_wrf_vg_derivative(self, h) result(dqw_dh)
            implicit none
            class(type_wrf_vg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: dqw_dh

        end function calculate_wrf_vg_derivative
    end interface

    interface
        module function construct_type_wrf_ko(input) result(property)
            implicit none
            type(type_materials_wrf), intent(in) :: input
            class(abst_wrf), allocatable :: property

        end function construct_type_wrf_ko

        module function calculate_wrf_ko(self, h) result(theta_w)
            implicit none
            class(type_wrf_ko), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: theta_w

        end function calculate_wrf_ko

        module function calculate_wrf_ko_derivative(self, h) result(dqw_dh)
            implicit none
            class(type_wrf_ko), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: dqw_dh

        end function calculate_wrf_ko_derivative

    end interface

    interface
        module function construct_type_wrf_mvg(input) result(property)
            implicit none
            type(type_materials_wrf), intent(in) :: input
            class(abst_wrf), allocatable :: property

        end function construct_type_wrf_mvg

        module function calculate_wrf_mvg(self, h) result(theta_w)
            implicit none
            class(type_wrf_mvg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: theta_w

        end function calculate_wrf_mvg

        module function calculate_wrf_mvg_derivative(self, h) result(dqw_dh)
            implicit none
            class(type_wrf_mvg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: dqw_dh

        end function calculate_wrf_mvg_derivative
    end interface

    interface
        module function construct_type_wrf_durner(input) result(property)
            implicit none
            type(type_materials_wrf), intent(in) :: input
            class(abst_wrf), allocatable :: property

        end function construct_type_wrf_durner

        module function calculate_wrf_durner(self, h) result(theta_w)
            implicit none
            class(type_wrf_durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: theta_w

        end function calculate_wrf_durner

        module function calculate_wrf_durner_derivative(self, h) result(dqw_dh)
            implicit none
            class(type_wrf_durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: dqw_dh

        end function calculate_wrf_durner_derivative
    end interface

    interface
        module function construct_type_wrf_dvgch(input) result(property)
            implicit none
            type(type_materials_wrf), intent(in) :: input
            class(abst_wrf), allocatable :: property

        end function construct_type_wrf_dvgch

        module function calculate_wrf_dvgch(self, h) result(theta_w)
            implicit none
            class(type_wrf_dvgch), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: theta_w

        end function calculate_wrf_dvgch

        module function calculate_wrf_dvgch_derivative(self, h) result(dqw_dh)
            implicit none
            class(type_wrf_dvgch), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: dqw_dh

        end function calculate_wrf_dvgch_derivative
    end interface

    interface type_wrf_bc
        module procedure :: construct_type_wrf_bc
    end interface

    interface type_wrf_vg
        module procedure :: construct_type_wrf_vg
    end interface

    interface type_wrf_ko
        module procedure :: construct_type_wrf_ko
    end interface

    interface type_wrf_mvg
        module procedure :: construct_type_wrf_mvg
    end interface

    interface type_wrf_durner
        module procedure :: construct_type_wrf_durner
    end interface

    interface type_wrf_dvgch
        module procedure :: construct_type_wrf_dvgch
    end interface

end module calculate_wrf
