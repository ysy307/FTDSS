module calculate_density
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_state
    use :: module_input, only:type_input
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

    ! --- 密度の抽象基底クラス (インターフェースの契約) ---
    type, abstract :: abst_den
        integer(int32) :: material_id
        real(real64) :: material1 !! soil, rock, concrete
        real(real64) :: material2 !! water
        real(real64) :: material3 !! ice
        real(real64) :: material4 !! gas
    contains
        procedure(abst_calc_den_gauss_point), pass(self), deferred :: calc
    end type abst_den

    type, extends(abst_den) :: type_den_3phase
    contains
        procedure :: calc => calc_den_gauss_point_3phase
    end type type_den_3phase

    ! ----------------------------------------------------------------
    ! 抽象基底クラスのインターフェース
    ! ----------------------------------------------------------------
    abstract interface
        pure elemental function abst_calc_den_gauss_point(self, state) result(density)
            import :: abst_den, type_state, real64
            implicit none
            class(abst_den), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: density

        end function abst_calc_den_gauss_point
    end interface

    interface
        module subroutine initialize_holder_dens(self, input, material_id)
            implicit none
            class(holder_dens), intent(inout) :: self
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: material_id

        end subroutine initialize_holder_dens

        module function construct_den_3phase(input, material_id) result(property)
            implicit none
            class(abst_den), allocatable :: property
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: material_id

        end function construct_den_3phase

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
    end interface

    interface type_den_3phase
        module procedure construct_den_3phase
    end interface

end module calculate_density
