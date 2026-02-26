module constitutive_materials_heat_capacity
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state, type_constitutive_info
    use :: constitutive_constants, only:TtoK => celsius_to_kelvin
    use :: constitutive_materials_base, only:abst_material
    implicit none
    private

    public :: holder_vhcs
    public :: abst_vhc
    public :: type_vhc_1phase
    public :: type_vhc_2phase
    public :: type_vhc_3phase
    public :: type_vhc_4phase

    ! --- ポリモーフィックなコンテナ ---
    type :: holder_vhcs
        class(abst_vhc), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_vhcs
    end type holder_vhcs

    interface
        module subroutine initialize_holder_vhcs(self, material_id, constitutive_info, water, ice)
            implicit none
            class(holder_vhcs), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_constitutive_info), intent(in) :: constitutive_info
            type(type_iapws97), intent(in), target :: water
            type(type_iapws06), intent(in), target :: ice

        end subroutine initialize_holder_vhcs
    end interface

    type, extends(abst_material), abstract :: abst_vhc
    contains
        procedure(abst_calc_vhc_gp), pass(self), public, deferred :: calc
    end type abst_vhc

    abstract interface
        subroutine abst_calc_vhc_gp(self, state, vhc)
            import :: abst_vhc, type_state, real64
            implicit none
            class(abst_vhc), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: vhc
        end subroutine abst_calc_vhc_gp
    end interface

    type, extends(abst_vhc) :: type_vhc_1phase
    contains
        procedure :: calc => calc_vhc_gp_1phase
    end type type_vhc_1phase

    interface
        module subroutine calc_vhc_gp_1phase(self, state, vhc)
            implicit none
            class(type_vhc_1phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: vhc
        end subroutine calc_vhc_gp_1phase
    end interface

    type, extends(abst_vhc) :: type_vhc_2phase
    contains
        procedure :: calc => calc_vhc_gp_2phase
    end type type_vhc_2phase

    interface
        module subroutine calc_vhc_gp_2phase(self, state, vhc)
            implicit none
            class(type_vhc_2phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: vhc
        end subroutine calc_vhc_gp_2phase
    end interface

    type, extends(abst_vhc) :: type_vhc_3phase
    contains
        procedure :: calc => calc_vhc_gp_3phase
    end type type_vhc_3phase

    interface
        module subroutine calc_vhc_gp_3phase(self, state, vhc)
            implicit none
            class(type_vhc_3phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: vhc
        end subroutine calc_vhc_gp_3phase
    end interface

    type, extends(abst_vhc) :: type_vhc_4phase
    contains
        procedure :: calc => calc_vhc_gp_4phase
    end type type_vhc_4phase

    interface
        module subroutine calc_vhc_gp_4phase(self, state, vhc)
            implicit none
            class(type_vhc_4phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: vhc
        end subroutine calc_vhc_gp_4phase
    end interface

    interface
        module subroutine calc_vhc_2(vhc_soil, phi_soil, &
                                     vhc_water, phi_water, vhc)
            implicit none
            real(real64), intent(in) :: vhc_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: vhc_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(inout) :: vhc

        end subroutine calc_vhc_2

        module subroutine calc_vhc_3a(vhc_soil, phi_soil, vhc_water, phi_water, &
                                      vhc_ice, phi_ice, density_water, latent_heat_fusion, dQw_dT, vhc)
            implicit none
            real(real64), intent(in) :: vhc_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: vhc_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: vhc_ice
            real(real64), intent(in) :: phi_ice
            real(real64), intent(in) :: density_water
            real(real64), intent(in) :: latent_heat_fusion
            real(real64), intent(in) :: dQw_dT
            real(real64), intent(inout) :: vhc

        end subroutine calc_vhc_3a

        module subroutine calc_vhc_4a(vhc_soil, phi_soil, vhc_water, phi_water, &
                                      vhc_ice, phi_ice, phi_vapor, vhc_vapor, density_water, latent_heat_fusion, &
                                      dQw_dT, latent_heat_vaporization, dQv_dT, vhc)
            implicit none
            real(real64), intent(in) :: vhc_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: vhc_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: vhc_ice
            real(real64), intent(in) :: phi_ice
            real(real64), intent(in) :: phi_vapor
            real(real64), intent(in) :: vhc_vapor
            real(real64), intent(in) :: density_water
            real(real64), intent(in) :: latent_heat_fusion
            real(real64), intent(in) :: dQw_dT
            real(real64), intent(in) :: latent_heat_vaporization
            real(real64), intent(in) :: dQv_dT
            real(real64), intent(inout) :: vhc

        end subroutine calc_vhc_4a
    end interface

end module constitutive_materials_heat_capacity
