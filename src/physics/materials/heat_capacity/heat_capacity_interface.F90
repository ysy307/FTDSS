module physics_material_heat_capacity
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state, type_physics_info
    use :: physics_constants, only:TtoK => celsius_to_kelvin
    implicit none
    private

    ! --- 公開する型定義 ---
    public :: holder_vhcs
    public :: abst_vhc
    public :: type_vhc_3phase
    public :: type_vhc_3phase_apparent

    ! --- ポリモーフィックなコンテナ ---
    type :: holder_vhcs
        class(abst_vhc), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_vhcs
    end type holder_vhcs

    type, abstract :: abst_vhc
        integer(int32) :: material_id
        real(real64) :: material1 !! soil, rock, concrete
        real(real64) :: material2 !! water
        real(real64) :: material3 !! ice
        real(real64) :: material4 !! gas
    contains
        procedure(abst_calc_vhc_gauss_point), pass(self), deferred :: calc
    end type abst_vhc

    ! --- 3相モデルの具象クラス ---
    type, extends(abst_vhc) :: type_vhc_3phase
    contains
        procedure :: calc => calc_vhc_gauss_point_3phase

    end type type_vhc_3phase
    type, extends(abst_vhc) :: type_vhc_3phase_apparent
    contains
        procedure :: calc => calc_vhc_gauss_point_3phase_apparent
    end type type_vhc_3phase_apparent

    ! --- 手続きのインターフェース宣言 ---
    abstract interface
        pure elemental function abst_calc_vhc_gauss_point(self, state) result(VHC)
            import :: abst_vhc, type_state, abst_den, real64
            implicit none
            class(abst_vhc), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: VHC
        end function abst_calc_vhc_gauss_point

    end interface

    interface
        module subroutine initialize_holder_vhcs(self, input, material_id)
            implicit none
            class(holder_vhcs), intent(inout) :: self
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: material_id

        end subroutine initialize_holder_vhcs

        module function construct_type_vhc_3phase(input, material_id) result(property)
            implicit none
            class(abst_vhc), allocatable :: property
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: material_id

        end function construct_type_vhc_3phase

        module pure elemental function calc_vhc_gauss_point_3phase(self, state) result(VHC)
            implicit none
            class(type_vhc_3phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: VHC

        end function calc_vhc_gauss_point_3phase

        module function construct_type_vhc_3phase_apparent(input, material_id) result(property)
            implicit none
            class(abst_vhc), allocatable :: property
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: material_id

        end function construct_type_vhc_3phase_apparent

        module pure elemental function calc_vhc_gauss_point_3phase_apparent(self, state) result(VHC)
            implicit none
            class(type_vhc_3phase_apparent), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: VHC

        end function calc_vhc_gauss_point_3phase_apparent
    end interface

    interface

        module pure elemental function calc_vhc_3(VHC_soil, phi_soil, &
                                                  VHC_water, phi_water, &
                                                  VHC_ice, phi_ice) result(VHC)
            implicit none
            real(real64), intent(in) :: VHC_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: VHC_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: VHC_ice
            real(real64), intent(in) :: phi_ice
            real(real64) :: VHC
        end function calc_vhc_3

        module pure elemental function calc_vhc_3a(VHC_soil, phi_soil, VHC_water, phi_water, &
                                                   VHC_ice, phi_ice, Lf, density_water, dQw_dT) result(VHC)
            implicit none
            real(real64), intent(in) :: VHC_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: VHC_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: VHC_ice
            real(real64), intent(in) :: phi_ice
            real(real64), intent(in) :: Lf
            real(real64), intent(in) :: density_water
            real(real64), intent(in) :: dQw_dT
            real(real64) :: VHC

        end function calc_vhc_3a
    end interface

    interface type_vhc_3phase
        module procedure construct_type_vhc_3phase
    end interface

    interface type_vhc_3phase_apparent
        module procedure construct_type_vhc_3phase_apparent
    end interface

end module physics_material_heat_capacity
