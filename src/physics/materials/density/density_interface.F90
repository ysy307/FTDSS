module physics_materials_density
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state, type_physics_info
    use :: physics_constants, only:TtoK => celsius_to_kelvin
    use :: physics_materials_base, only:abst_material
    implicit none
    private

    public :: holder_dens
    public :: abst_den
    public :: type_den_1phase
    public :: type_den_2phase
    public :: type_den_3phase
    public :: type_den_4phase

    type :: holder_dens
        class(abst_den), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_dens
    end type holder_dens

    interface
        module subroutine initialize_holder_dens(self, material_id, physics_info, water, ice)
            implicit none
            class(holder_dens), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_physics_info), intent(in) :: physics_info
            type(type_iapws97), intent(in), target :: water
            type(type_iapws06), intent(in), target :: ice

        end subroutine initialize_holder_dens
    end interface

    type, extends(abst_material), abstract :: abst_den
    contains
        procedure(abst_calc_den_gp), pass(self), public, deferred :: calc
    end type abst_den

    abstract interface
        subroutine abst_calc_den_gp(self, state, density)
            import :: abst_den, type_state, type_iapws97, type_iapws06, real64
            implicit none
            class(abst_den), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: density
        end subroutine abst_calc_den_gp
    end interface

    type, extends(abst_den) :: type_den_1phase
    contains
        procedure, pass(self) :: calc => calc_den_gp_1phase
    end type type_den_1phase

    interface
        module subroutine calc_den_gp_1phase(self, state, density)
            implicit none
            class(type_den_1phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: density
        end subroutine calc_den_gp_1phase
    end interface

    type, extends(abst_den) :: type_den_2phase
    contains
        procedure, pass(self) :: calc => calc_den_gp_2phase
    end type type_den_2phase

    interface
        module subroutine calc_den_gp_2phase(self, state, density)
            implicit none
            class(type_den_2phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: density
        end subroutine calc_den_gp_2phase
    end interface

    type, extends(abst_den) :: type_den_3phase
    contains
        procedure, pass(self) :: calc => calc_den_gp_3phase
    end type type_den_3phase

    interface
        module subroutine calc_den_gp_3phase(self, state, density)
            implicit none
            class(type_den_3phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: density
        end subroutine calc_den_gp_3phase
    end interface

    type, extends(abst_den) :: type_den_4phase
    contains
        procedure, pass(self) :: calc => calc_den_gp_4phase
    end type type_den_4phase

    interface
        module subroutine calc_den_gp_4phase(self, state, density)
            implicit none
            class(type_den_4phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: density
        end subroutine calc_den_gp_4phase
    end interface

    ! ------------------------------------------------------------------------------
    ! 密度計算のための関数インターフェース
    ! ------------------------------------------------------------------------------
    interface
        module subroutine calc_den_2(density_soil, phi_soil, &
                                     density_water, phi_water, density)
            implicit none
            real(real64), intent(in) :: density_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: density_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(inout) :: density

        end subroutine calc_den_2

        module subroutine calc_den_3(density_soil, phi_soil, &
                                     density_water, phi_water, &
                                     density_ice, phi_ice, density)
            implicit none
            real(real64), intent(in) :: density_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: density_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: density_ice
            real(real64), intent(in) :: phi_ice
            real(real64), intent(inout) :: density

        end subroutine calc_den_3

        module subroutine calc_den_4(density_soil, phi_soil, &
                                     density_water, phi_water, &
                                     density_ice, phi_ice, &
                                     density_vapor, phi_vapor, density)
            implicit none
            real(real64), intent(in) :: density_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: density_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: density_ice
            real(real64), intent(in) :: phi_ice
            real(real64), intent(in) :: density_vapor
            real(real64), intent(in) :: phi_vapor
            real(real64), intent(inout) :: density

        end subroutine calc_den_4
    end interface

end module physics_materials_density
