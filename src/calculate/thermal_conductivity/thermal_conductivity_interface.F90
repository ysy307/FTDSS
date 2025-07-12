module calculate_thermal_conductivity
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_gauss_point_state
    use :: Inout_Input
    implicit none
    private

    ! --- 公開する型定義 ---
    public :: holder_thcs
    public :: abst_thc
    public :: type_thc_3phase

    type :: holder_thcs
        class(abst_thc), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_thcs
    end type holder_thcs

    type, abstract :: abst_thc
        integer(int32) :: region_id
        real(real64) :: material1 !! like a soil or a rock, a concrete
        real(real64) :: material2 !! like a water
        real(real64) :: material3 !! like a ice
        real(real64) :: material4 !! like a gas
    contains
        procedure(abst_calc_thc_gauss_point), pass(self), deferred :: calc_gauss_point !&
    end type abst_thc

    !--------------------------------------------------------------------------------
    type, extends(abst_thc) :: type_thc_3phase
    contains
        procedure, pass(self) :: calc_gauss_point => calc_thc_gauss_point_3phase !&
    end type type_thc_3phase

    abstract interface
        function abst_calc_thc_gauss_point(self, state) result(lambda)
            import :: abst_thc, type_gauss_point_state, real64
            implicit none
            class(abst_thc), intent(in) :: self
            type(type_gauss_point_state), intent(in) :: state
            real(real64) :: lambda

        end function abst_calc_thc_gauss_point
    end interface

    interface
        module subroutine initialize_holder_thcs(self, iRegion, Input)
            implicit none
            class(holder_thcs), intent(inout) :: self
            integer(int32), intent(in) :: iRegion
            type(type_Input), intent(in) :: Input

        end subroutine initialize_holder_thcs
    end interface

    interface
        module function construct_thc_3(iRegion, Input) result(property)
            implicit none
            class(abst_thc), allocatable :: property
            integer(int32), intent(in) :: iRegion
            type(type_Input), intent(in) :: Input

        end function construct_thc_3

        module function calc_thc_gauss_point_3phase(self, state) result(lambda)
            implicit none
            class(type_thc_3phase), intent(in) :: self
            type(type_gauss_point_state), intent(in) :: state
            real(real64) :: lambda

        end function calc_thc_gauss_point_3phase

    end interface

    interface
        module function calc_thc_3(lambda_soil, phi_soil, &
                                   lambda_water, phi_water, &
                                   lambda_ice, phi_ice) result(lambda)
            implicit none
            real(real64), intent(in) :: lambda_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: lambda_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: lambda_ice
            real(real64), intent(in) :: phi_ice
            real(real64) :: lambda

        end function calc_thc_3
    end interface

    interface type_thc_3phase
        module procedure construct_thc_3
    end interface
end module calculate_thermal_conductivity
