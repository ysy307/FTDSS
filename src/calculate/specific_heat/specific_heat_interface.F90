module calculate_specific_heat
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_gauss_point_state
    use :: module_input, only:type_input
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
        integer(int32) :: material_id
        real(real64) :: material1 !! soil, rock, concrete
        real(real64) :: material2 !! water
        real(real64) :: material3 !! ice
        real(real64) :: material4 !! gas
    contains
        procedure(abst_calc_sph_gauss_point), pass(self), deferred :: calc
    end type abst_sph

    ! --- 3相モデルの具象クラス ---
    type, extends(abst_sph) :: type_sph_3phase
    contains
        procedure :: calc => calc_sph_gauss_point_3phase
    end type type_sph_3phase

    ! --- 手続きのインターフェース宣言 ---
    abstract interface
        function abst_calc_sph_gauss_point(self, state) result(specific_heat)
            import :: abst_sph, type_gauss_point_state, real64
            implicit none
            class(abst_sph), intent(in) :: self
            type(type_gauss_point_state), intent(in) :: state
            real(real64) :: specific_heat
        end function abst_calc_sph_gauss_point
    end interface

    ! このモジュールで実装される手続きのインターフェース
    interface
        module subroutine initialize_holder_sphs(self, input, i_material)
            implicit none
            class(holder_sphs), intent(inout) :: self
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: i_material
        end subroutine initialize_holder_sphs

        module function construct_sph_3phase(input, i_material) result(property)
            implicit none
            class(abst_sph), allocatable :: property
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: i_material
        end function construct_sph_3phase

        module function calc_sph_gauss_point_3phase(self, state) result(specific_heat)
            implicit none
            class(type_sph_3phase), intent(in) :: self
            type(type_gauss_point_state), intent(in) :: state
            real(real64) :: specific_heat
        end function calc_sph_gauss_point_3phase
    end interface

    interface

        module function Calc_SPH_3(specific_heat_soil, phi_soil, &
                                   specific_heat_water, phi_water, &
                                   specific_heat_ice, phi_ice) result(specific_heat)
            implicit none
            real(real64), intent(in) :: specific_heat_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: specific_heat_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: specific_heat_ice
            real(real64), intent(in) :: phi_ice
            real(real64) :: specific_heat
        end function Calc_SPH_3
    end interface

    interface type_sph_3phase
        module procedure construct_sph_3phase
    end interface

end module calculate_specific_heat
