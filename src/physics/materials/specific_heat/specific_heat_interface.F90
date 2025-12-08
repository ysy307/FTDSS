module physics_material_specific_heat
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state, type_physics_info
    use :: physics_constants, only:TtoK => celsius_to_kelvin
    implicit none
    private

    public :: holder_sphs
    public :: abst_sph
    public :: type_sph_2phase
    public :: type_sph_3phase
    public :: type_sph_4phase

    ! --- ポリモーフィックなコンテナ ---
    type :: holder_sphs
        class(abst_sph), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_sphs
    end type holder_sphs

    interface
        module subroutine initialize_holder_sphs(self, material_id, physics_info, water, ice)
            implicit none
            class(holder_sphs), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_physics_info), intent(in) :: physics_info
            type(type_iapws97), intent(in), target :: water
            type(type_iapws06), intent(in), target :: ice

        end subroutine initialize_holder_sphs
    end interface

    type, abstract :: abst_sph
        integer(int32) :: material_id = -1
        real(real64) :: material1 = 0.0d0 !! soil, rock, concrete
        real(real64) :: material2 = 0.0d0 !! water
        real(real64) :: material3 = 0.0d0 !! ice
        real(real64) :: material4 = 0.0d0 !! gas
        type(type_iapws97), pointer :: water => null()
        type(type_iapws06), pointer :: ice => null()
    contains
        procedure(abst_initialize_sph), pass(self), public, deferred :: initialize
        procedure(abst_calc_sph_gp), pass(self), deferred :: calc
    end type abst_sph

    abstract interface
        subroutine abst_initialize_sph(self, material_id, physics_info, water, ice)
            import :: abst_sph, type_physics_info, int32, type_iapws97, type_iapws06
            implicit none
            class(abst_sph), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_physics_info), intent(in) :: physics_info
            type(type_iapws97), intent(in), target :: water
            type(type_iapws06), intent(in), target :: ice

        end subroutine abst_initialize_sph

        pure elemental subroutine abst_calc_sph_gp(self, state, specific_heat)
            import :: abst_sph, type_state, real64
            implicit none
            class(abst_sph), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: specific_heat
        end subroutine abst_calc_sph_gp
    end interface

    type, extends(abst_sph) :: type_sph_2phase
    contains
        procedure :: initialize => initialize_sph_2phase
        procedure :: calc => calc_sph_gp_2phase
    end type type_sph_2phase

    interface
        module subroutine initialize_sph_2phase(self, material_id, physics_info, water, ice)
            implicit none
            class(type_sph_2phase), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_physics_info), intent(in) :: physics_info
            type(type_iapws97), intent(in), target :: water
            type(type_iapws06), intent(in), target :: ice

        end subroutine initialize_sph_2phase

        module pure elemental subroutine calc_sph_gp_2phase(self, state, specific_heat)
            implicit none
            class(type_sph_2phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: specific_heat
        end subroutine calc_sph_gp_2phase
    end interface

    type, extends(abst_sph) :: type_sph_3phase
    contains
        procedure :: initialize => initialize_sph_3phase
        procedure :: calc => calc_sph_gp_3phase
    end type type_sph_3phase

    interface
        module subroutine initialize_sph_3phase(self, material_id, physics_info, water, ice)
            implicit none
            class(type_sph_3phase), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_physics_info), intent(in) :: physics_info
            type(type_iapws97), intent(in), target :: water
            type(type_iapws06), intent(in), target :: ice

        end subroutine initialize_sph_3phase

        module pure elemental subroutine calc_sph_gp_3phase(self, state, specific_heat)
            implicit none
            class(type_sph_3phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: specific_heat
        end subroutine calc_sph_gp_3phase
    end interface

    type, extends(abst_sph) :: type_sph_4phase
    contains
        procedure :: initialize => initialize_sph_4phase
        procedure :: calc => calc_sph_gp_4phase
    end type type_sph_4phase

    interface
        module subroutine initialize_sph_4phase(self, material_id, physics_info, water, ice)
            implicit none
            class(type_sph_4phase), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_physics_info), intent(in) :: physics_info
            type(type_iapws97), intent(in), target :: water
            type(type_iapws06), intent(in), target :: ice

        end subroutine initialize_sph_4phase

        module pure elemental subroutine calc_sph_gp_4phase(self, state, specific_heat)
            implicit none
            class(type_sph_4phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: specific_heat
        end subroutine calc_sph_gp_4phase
    end interface

    interface

        module pure elemental subroutine calc_sph_2(specific_heat_soil, phi_soil, &
                                                    specific_heat_water, phi_water, specific_heat)
            implicit none
            real(real64), intent(in) :: specific_heat_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: specific_heat_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(inout) :: specific_heat
        end subroutine calc_sph_2

        module pure elemental subroutine calc_sph_3(specific_heat_soil, phi_soil, &
                                                    specific_heat_water, phi_water, &
                                                    specific_heat_ice, phi_ice, specific_heat)
            implicit none
            real(real64), intent(in) :: specific_heat_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: specific_heat_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: specific_heat_ice
            real(real64), intent(in) :: phi_ice
            real(real64), intent(inout) :: specific_heat
        end subroutine calc_sph_3

        module pure elemental subroutine calc_sph_4(specific_heat_soil, phi_soil, &
                                                    specific_heat_water, phi_water, &
                                                    specific_heat_ice, phi_ice, &
                                                    specific_heat_vapor, phi_vapor, specific_heat)
            implicit none
            real(real64), intent(in) :: specific_heat_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: specific_heat_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: specific_heat_ice
            real(real64), intent(in) :: phi_ice
            real(real64), intent(in) :: specific_heat_vapor
            real(real64), intent(in) :: phi_vapor
            real(real64), intent(inout) :: specific_heat
        end subroutine calc_sph_4
    end interface

end module physics_material_specific_heat
