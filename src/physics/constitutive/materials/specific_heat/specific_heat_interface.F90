module materials_specific_heat
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state, type_config_constitutive
    use :: constitutive_constants, only:TtoK => celsius_to_kelvin
    use :: materials_base, only:abst_material
    implicit none
    private

    public :: holder_sphs
    public :: abst_sph
    public :: type_sph_1phase
    public :: type_sph_2phase
    public :: type_sph_3phase
    public :: type_sph_4phase

    ! --- Polymorphic container ---
    type :: holder_sphs
        class(abst_sph), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_sphs
    end type holder_sphs

    interface
        module subroutine initialize_holder_sphs(self, material_id, constitutive_info, water, ice)
            implicit none
            class(holder_sphs), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_config_constitutive), intent(in) :: constitutive_info
            type(type_iapws97), intent(in), target :: water
            type(type_iapws06), intent(in), target :: ice

        end subroutine initialize_holder_sphs
    end interface

    type, extends(abst_material), abstract :: abst_sph
    contains
        procedure(abst_calc_sph_gp), pass(self), deferred :: calc
    end type abst_sph

    abstract interface
        subroutine abst_calc_sph_gp(self, state, specific_heat)
            import :: abst_sph, type_state, real64
            implicit none
            class(abst_sph), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: specific_heat
        end subroutine abst_calc_sph_gp
    end interface

    type, extends(abst_sph) :: type_sph_1phase
    contains
        procedure :: calc => calc_sph_gp_1phase
    end type type_sph_1phase

    interface
        module subroutine calc_sph_gp_1phase(self, state, specific_heat)
            implicit none
            class(type_sph_1phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: specific_heat
        end subroutine calc_sph_gp_1phase
    end interface

    type, extends(abst_sph) :: type_sph_2phase
    contains
        procedure :: calc => calc_sph_gp_2phase
    end type type_sph_2phase

    interface
        module subroutine calc_sph_gp_2phase(self, state, specific_heat)
            implicit none
            class(type_sph_2phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: specific_heat
        end subroutine calc_sph_gp_2phase
    end interface

    type, extends(abst_sph) :: type_sph_3phase
    contains
        procedure :: calc => calc_sph_gp_3phase
    end type type_sph_3phase

    interface
        module subroutine calc_sph_gp_3phase(self, state, specific_heat)
            implicit none
            class(type_sph_3phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: specific_heat
        end subroutine calc_sph_gp_3phase
    end interface

    type, extends(abst_sph) :: type_sph_4phase
    contains
        procedure :: calc => calc_sph_gp_4phase
    end type type_sph_4phase

    interface
        module subroutine calc_sph_gp_4phase(self, state, specific_heat)
            implicit none
            class(type_sph_4phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: specific_heat
        end subroutine calc_sph_gp_4phase
    end interface

    interface

        module subroutine calc_sph_2(specific_heat_soil, phi_soil, &
                                     specific_heat_water, phi_water, specific_heat)
            implicit none
            real(real64), intent(in) :: specific_heat_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: specific_heat_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(inout) :: specific_heat
        end subroutine calc_sph_2

        module subroutine calc_sph_3(specific_heat_soil, phi_soil, &
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

        module subroutine calc_sph_4(specific_heat_soil, phi_soil, &
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

end module materials_specific_heat
