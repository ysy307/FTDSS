module physics_models_wrf
    use, intrinsic :: iso_fortran_env
    use :: module_core, only:WRF_BC, WRF_VG, WRF_KO, WRF_MVG, WRF_DURNER, WRF_DVGCH, &
        PHYSICS_UNIT_M, PHYSICS_UNIT_CM, PHYSICS_UNIT_PA
    use :: physics_constants, only:pi => circle_ratio, g => gravity_acceleration, rho_std => reference_water_density
    implicit none
    private

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
    public :: type_params_wrf
    !-------------------------------------

    type :: type_params_wrf
        integer(int32) :: unit_id
        integer(int32) :: model_number
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
    contains
        procedure, pass(self), public :: reset => reset_params_wrf
        procedure, pass(self), public :: copy => copy_params_wrf
        procedure, pass(self), public :: convert => convert_params_wrf
    end type type_params_wrf

    interface
        module subroutine reset_params_wrf(self)
            implicit none
            class(type_params_wrf), intent(inout) :: self

        end subroutine reset_params_wrf

        module subroutine copy_params_wrf(self, source)
            implicit none
            class(type_params_wrf), intent(inout) :: self
            type(type_params_wrf), intent(in) :: source

        end subroutine copy_params_wrf

        module subroutine convert_params_wrf(self, unit_id, factor)
            implicit none
            class(type_params_wrf), intent(inout) :: self
            integer(int32), intent(in) :: unit_id
            real(real64), intent(in), optional :: factor
        end subroutine convert_params_wrf

    end interface

    type :: holder_wrfs
        class(abst_wrf), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_wrfs
    end type holder_wrfs

    interface
        module subroutine initialize_holder_wrfs(self, material_id, params)
            implicit none
            class(holder_wrfs), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_params_wrf), intent(in) :: params

        end subroutine initialize_holder_wrfs
    end interface

    type, abstract :: abst_wrf
        type(type_params_wrf) :: params
    contains
        procedure, pass(self), public :: initialize => initialize_abst_wrf
        procedure(abst_calc_wrf), pass(self), public, deferred :: calc
        procedure(abst_calc_wrf_derivative), pass(self), public, deferred :: deriv
    end type abst_wrf

    abstract interface
        pure elemental subroutine abst_calc_wrf(self, h, Qw)
            import :: abst_wrf, real64
            implicit none
            class(abst_wrf), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: Qw
        end subroutine abst_calc_wrf

        pure elemental subroutine abst_calc_wrf_derivative(self, h, dQw_dh)
            import :: abst_wrf, real64
            implicit none
            class(abst_wrf), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: dQw_dh
        end subroutine abst_calc_wrf_derivative
    end interface

    interface
        module subroutine initialize_abst_wrf(self, params)
            implicit none
            class(abst_wrf), intent(inout) :: self
            type(type_params_wrf), intent(in) :: params

        end subroutine initialize_abst_wrf
    end interface

    type, extends(abst_wrf) :: type_wrf_bc
    contains
        procedure, pass(self) :: calc => calculate_wrf_bc
        procedure, pass(self) :: deriv => calculate_wrf_bc_derivative
    end type type_wrf_bc

    type, extends(abst_wrf) :: type_wrf_vg
    contains
        procedure, pass(self) :: calc => calculate_wrf_vg
        procedure, pass(self) :: deriv => calculate_wrf_vg_derivative
    end type type_wrf_vg

    type, extends(abst_wrf) :: type_wrf_ko
    contains
        procedure, pass(self) :: calc => calculate_wrf_ko
        procedure, pass(self) :: deriv => calculate_wrf_ko_derivative
    end type type_wrf_ko

    type, extends(abst_wrf) :: type_wrf_mvg
    contains
        procedure, pass(self) :: calc => calculate_wrf_mvg
        procedure, pass(self) :: deriv => calculate_wrf_mvg_derivative
    end type type_wrf_mvg

    type, extends(abst_wrf) :: type_wrf_durner
    contains
        procedure, pass(self) :: calc => calculate_wrf_durner
        procedure, pass(self) :: deriv => calculate_wrf_durner_derivative
    end type type_wrf_durner

    type, extends(abst_wrf) :: type_wrf_dvgch
    contains
        procedure, pass(self) :: calc => calculate_wrf_dvgch
        procedure, pass(self) :: deriv => calculate_wrf_dvgch_derivative
    end type type_wrf_dvgch

    interface
        module pure elemental subroutine calculate_wrf_bc(self, h, Qw)
            implicit none
            class(type_wrf_bc), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: Qw

        end subroutine calculate_wrf_bc

        module pure elemental subroutine calculate_wrf_bc_derivative(self, h, dQw_dh)
            implicit none
            class(type_wrf_bc), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: dQw_dh

        end subroutine calculate_wrf_bc_derivative
    end interface

    interface
        module pure elemental subroutine calculate_wrf_vg(self, h, Qw)
            implicit none
            class(type_wrf_vg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: Qw

        end subroutine calculate_wrf_vg

        module pure elemental subroutine calculate_wrf_vg_derivative(self, h, dQw_dh)
            implicit none
            class(type_wrf_vg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: dQw_dh

        end subroutine calculate_wrf_vg_derivative
    end interface

    interface
        module pure elemental subroutine calculate_wrf_ko(self, h, Qw)
            implicit none
            class(type_wrf_ko), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: Qw

        end subroutine calculate_wrf_ko

        module pure elemental subroutine calculate_wrf_ko_derivative(self, h, dQw_dh)
            implicit none
            class(type_wrf_ko), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: dQw_dh

        end subroutine calculate_wrf_ko_derivative

    end interface

    interface
        module pure elemental subroutine calculate_wrf_mvg(self, h, Qw)
            implicit none
            class(type_wrf_mvg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: Qw

        end subroutine calculate_wrf_mvg

        module pure elemental subroutine calculate_wrf_mvg_derivative(self, h, dQw_dh)
            implicit none
            class(type_wrf_mvg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: dQw_dh

        end subroutine calculate_wrf_mvg_derivative
    end interface

    interface
        module pure elemental subroutine calculate_wrf_durner(self, h, Qw)
            implicit none
            class(type_wrf_durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: Qw

        end subroutine calculate_wrf_durner

        module pure elemental subroutine calculate_wrf_durner_derivative(self, h, dQw_dh)
            implicit none
            class(type_wrf_durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: dQw_dh

        end subroutine calculate_wrf_durner_derivative
    end interface

    interface
        module pure elemental subroutine calculate_wrf_dvgch(self, h, Qw)
            implicit none
            class(type_wrf_dvgch), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: Qw

        end subroutine calculate_wrf_dvgch

        module pure elemental subroutine calculate_wrf_dvgch_derivative(self, h, dQw_dh)
            implicit none
            class(type_wrf_dvgch), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: dQw_dh

        end subroutine calculate_wrf_dvgch_derivative
    end interface

end module physics_models_wrf
