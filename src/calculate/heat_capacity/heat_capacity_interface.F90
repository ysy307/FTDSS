module calculate_volumetric_heat_capacity
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_state
    use :: Inout_Input, only:Type_Input
    use :: calculate_density, only:holder_dens, abst_den
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
        ! procedure(abst_calc_vhc_gauss_point_holder), pass(self), deferred :: calc_gauss_point_holder
        procedure(abst_calc_vhc_gauss_point), pass(self), deferred :: calc
    end type abst_vhc

    ! --- 3相モデルの具象クラス ---
    type, extends(abst_vhc) :: type_vhc_3phase
    contains
        ! procedure :: calc_gauss_point_holder => calc_vhc_gauss_point_3phase_holder
        procedure :: calc => calc_vhc_gauss_point_3phase

    end type type_vhc_3phase
    type, extends(abst_vhc) :: type_vhc_3phase_apparent
    contains
        ! procedure :: calc_gauss_point_holder => calc_vhc_gauss_point_3phase_apparent_holder
        procedure :: calc => calc_vhc_gauss_point_3phase_apparent
    end type type_vhc_3phase_apparent

    ! --- 手続きのインターフェース宣言 ---
    abstract interface
        ! function abst_calc_vhc_gauss_point_holder(self, state, DEN, LatentHeat, dQi_dT) result(VHC)
        !     import :: abst_vhc, type_state, holder_dens, real64
        !     implicit none
        !     class(abst_vhc), intent(in) :: self
        !     type(type_state), intent(in) :: state
        !     type(holder_dens), intent(in), optional :: DEN
        !     real(real64), intent(in), optional :: LatentHeat
        !     real(real64), intent(in), optional :: dQi_dT
        !     real(real64) :: VHC
        ! end function abst_calc_vhc_gauss_point_holder

        function abst_calc_vhc_gauss_point(self, state, DEN, LatentHeat, dQi_dT) result(VHC)
            import :: abst_vhc, type_state, abst_den, real64
            implicit none
            class(abst_vhc), intent(in) :: self
            type(type_state), intent(in) :: state
            class(abst_den), pointer, intent(in), optional :: DEN
            real(real64), intent(in), optional :: LatentHeat
            real(real64), intent(in), optional :: dQi_dT
            real(real64) :: VHC
        end function abst_calc_vhc_gauss_point

    end interface

    interface
        module subroutine initialize_holder_vhcs(self, input, i_material)
            implicit none
            class(holder_vhcs), intent(inout) :: self
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: i_material
        end subroutine initialize_holder_vhcs

        module function construct_type_vhc_3phase(input, i_material) result(property)
            implicit none
            class(abst_vhc), allocatable :: property
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: i_material
        end function construct_type_vhc_3phase

        module pure function calc_vhc_gauss_point_3phase(self, state, DEN, LatentHeat, dQi_dT) result(VHC)
            implicit none
            class(type_vhc_3phase), intent(in) :: self
            type(type_state), intent(in) :: state
            class(abst_den), pointer, intent(in), optional :: DEN
            real(real64), intent(in), optional :: LatentHeat
            real(real64), intent(in), optional :: dQi_dT
            real(real64) :: VHC
        end function calc_vhc_gauss_point_3phase

        module function construct_type_vhc_3phase_apparent(input, i_material) result(property)
            implicit none
            class(abst_vhc), allocatable :: property
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: i_material
        end function construct_type_vhc_3phase_apparent

        module pure function calc_vhc_gauss_point_3phase_apparent(self, state, DEN, LatentHeat, dQi_dT) result(VHC)
            implicit none
            class(type_vhc_3phase_apparent), intent(in) :: self
            type(type_state), intent(in) :: state
            class(abst_den), pointer, intent(in), optional :: DEN
            real(real64), intent(in), optional :: LatentHeat
            real(real64), intent(in), optional :: dQi_dT
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

end module calculate_volumetric_heat_capacity
