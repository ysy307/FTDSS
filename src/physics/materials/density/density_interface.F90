module physics_material_density
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, TtoK => celsius_to_kelvin
    implicit none
    private

    ! --- 公開する型定義 ---
    public :: holder_dens
    public :: abst_den
    public :: type_den_3phase

    ! --- ポリモーフィックなコンテナ ---
    type :: holder_dens
        class(abst_den), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_dens
    end type holder_dens

    interface
        module subroutine initialize_holder_dens(self, material_id, phase_info)
            implicit none
            class(holder_dens), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_physics_phase), intent(in) :: phase_info

        end subroutine initialize_holder_dens
    end interface

    ! --- 密度の抽象基底クラス (インターフェースの契約) ---
    type, abstract :: abst_den
        integer(int32) :: material_id
        real(real64) :: material1 !! soil, rock, concrete
        real(real64) :: material2 !! water
        real(real64) :: material3 !! ice
        real(real64) :: material4 !! gas
    contains
        procedure(initialize_abst_den), pass(self), public, deferred :: initialize
        procedure(abst_calc_den_gauss_point), pass(self), public, deferred :: calc
    end type abst_den

    abstract interface
        subroutine initialize_abst_den(self, material_id, phase_info)
            import :: abst_den, type_physics_phase, int32
            implicit none
            class(abst_den), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_physics_phase), intent(in) :: phase_info

        end subroutine initialize_abst_den

        pure elemental function abst_calc_den_gauss_point(self, state) result(density)
            import :: abst_den, type_state, real64
            implicit none
            class(abst_den), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: density
        end function abst_calc_den_gauss_point
    end interface

    type, extends(abst_den) :: type_den_3phase
    contains
        procedure, pass(self) :: initialize => initialize_type_den_3phase
        procedure, pass(self) :: calc => calc_den_gauss_point_3phase
    end type type_den_3phase

    interface
        module subroutine initialize_type_den_3phase(self, material_id, phase_info)
            implicit none
            class(type_den_3phase), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_physics_phase), intent(in) :: phase_info

        end subroutine initialize_type_den_3phase

        module pure elemental function calc_den_gauss_point_3phase(self, state) result(density)
            implicit none
            class(type_den_3phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: density

        end function calc_den_gauss_point_3phase
    end interface

    ! ------------------------------------------------------------------------------
    ! 密度計算のための関数インターフェース
    ! ------------------------------------------------------------------------------
    interface
        module pure elemental function calc_den_3(density_soil, phi_soil, &
                                                  density_water, phi_water, &
                                                  density_ice, phi_ice) result(density)
            implicit none
            real(real64), intent(in) :: density_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: density_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: density_ice
            real(real64), intent(in) :: phi_ice
            real(real64) :: density
        end function calc_den_3

        module pure elemental function calc_den_saturated_vapor(temperature) result(density_vapor)
            implicit none
            real(real64), intent(in) :: temperature
            real(real64) :: density_vapor
        end function calc_den_saturated_vapor
    end interface

end module physics_material_density
