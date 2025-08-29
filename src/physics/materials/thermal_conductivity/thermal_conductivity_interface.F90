module physics_material_thermal_conductivity
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_state
    use :: module_input, only:type_input
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
        integer(int32) :: material_id
        real(real64) :: material1 !! like a soil or a rock, a concrete
        real(real64) :: material2 !! like a water
        real(real64) :: material3 !! like a ice
        real(real64) :: material4 !! like a gas
        real(real64), allocatable :: dispersity(:)
    contains
        procedure(abst_calc_thc_gauss_point), pass(self), deferred :: calc !&
    end type abst_thc

    !--------------------------------------------------------------------------------
    type, extends(abst_thc) :: type_thc_3phase
    contains
        procedure, pass(self) :: calc => calc_thc_gauss_point_3phase !&
    end type type_thc_3phase

    abstract interface
        pure elemental function abst_calc_thc_gauss_point(self, state) result(lambda)
            import :: abst_thc, type_state, real64
            implicit none
            class(abst_thc), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: lambda

        end function abst_calc_thc_gauss_point
    end interface

    interface
        module subroutine initialize_holder_thcs(self, input, material_id)
            implicit none
            class(holder_thcs), intent(inout) :: self
            type(type_Input), intent(in) :: Input
            integer(int32), intent(in) :: material_id

        end subroutine initialize_holder_thcs
    end interface

    interface
        module function construct_thc_3(input, material_id) result(property)
            implicit none
            class(abst_thc), allocatable :: property
            type(type_Input), intent(in) :: Input
            integer(int32), intent(in) :: material_id

        end function construct_thc_3

        module pure elemental function calc_thc_gauss_point_3phase(self, state) result(lambda)
            implicit none
            class(type_thc_3phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: lambda

        end function calc_thc_gauss_point_3phase

    end interface

    interface
        module pure elemental function calc_thc_3(lambda_soil, phi_soil, &
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
end module physics_material_thermal_conductivity
