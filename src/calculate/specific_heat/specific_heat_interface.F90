module calculate_specific_heat
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_gauss_point_state
    use :: Inout_Input, only:Type_Input
    implicit none
    private

    ! --- 公開する型定義 ---
    public :: holder_sphs
    public :: abst_sph
    public :: type_sph_3phase

    ! --- ポリモーフィックなコンテナ ---
    type :: holder_sphs
        class(abst_sph), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_sphs
    end type holder_sphs

    ! --- 密度の抽象基底クラス (インターフェースの契約) ---
    type, abstract :: abst_sph
        integer(int32) :: region_id
        real(real64) :: material1 !! soil, rock, concrete
        real(real64) :: material2 !! water
        real(real64) :: material3 !! ice
        real(real64) :: material4 !! gas
    contains
        procedure(abst_calc_sph_gauss_point), pass(self), deferred :: calc_gauss_point
    end type abst_sph

    ! --- 3相モデルの具象クラス ---
    type, extends(abst_sph) :: type_sph_3phase
    contains
        procedure :: calc_gauss_point => calc_sph_gauss_point_3phase
    end type type_sph_3phase

    ! --- 手続きのインターフェース宣言 ---
    abstract interface
        function abst_calc_sph_gauss_point(self, state) result(SpecificHeat)
            import :: abst_sph, type_gauss_point_state, real64
            implicit none
            class(abst_sph), intent(in) :: self
            type(type_gauss_point_state), intent(in) :: state
            real(real64) :: SpecificHeat
        end function abst_calc_sph_gauss_point
    end interface

    ! このモジュールで実装される手続きのインターフェース
    interface
        module subroutine initialize_holder_sphs(self, iRegion, Input)
            implicit none
            class(holder_sphs), intent(inout) :: self
            integer(int32), intent(in) :: iRegion
            type(Type_Input), intent(in) :: Input
        end subroutine initialize_holder_sphs

        module function construct_sph_3phase(iRegion, Input) result(property)
            implicit none
            class(abst_sph), allocatable :: property
            integer(int32), intent(in) :: iRegion
            type(Type_Input), intent(in) :: Input
        end function construct_sph_3phase

        module function calc_sph_gauss_point_3phase(self, state) result(SpecificHeat)
            implicit none
            class(type_sph_3phase), intent(in) :: self
            type(type_gauss_point_state), intent(in) :: state
            real(real64) :: SpecificHeat
        end function calc_sph_gauss_point_3phase
    end interface

    interface

        module function Calc_SPH_3(SpecificHeat_soil, phi_soil, &
                                   SpecificHeat_water, phi_water, &
                                   SpecificHeat_ice, phi_ice) result(SpecificHeat)
            implicit none
            real(real64), intent(in) :: SpecificHeat_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: SpecificHeat_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: SpecificHeat_ice
            real(real64), intent(in) :: phi_ice
            real(real64) :: SpecificHeat
        end function Calc_SPH_3
    end interface

    interface type_sph_3phase
        module procedure construct_sph_3phase
    end interface

end module calculate_specific_heat
