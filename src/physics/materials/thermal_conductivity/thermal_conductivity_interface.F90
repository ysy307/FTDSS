module physics_material_thermal_conductivity
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state, type_physics_info, allocate_array
    use :: physics_constants, only:TtoK => celsius_to_kelvin
    implicit none
    private

    public :: holder_thcs
    public :: abst_thc
    public :: type_thc_3phase

    type :: holder_thcs
        class(abst_thc), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_thcs
    end type holder_thcs

    interface
        module subroutine initialize_holder_thcs(self, material_id, physics_info, water, ice)
            implicit none
            class(holder_thcs), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_physics_info), intent(in) :: physics_info
            type(type_iapws97), intent(in), target :: water
            type(type_iapws06), intent(in), target :: ice

        end subroutine initialize_holder_thcs
    end interface

    type :: type_thc_dispersity
        real(real64) :: lambda_xx = 0.0d0
        real(real64) :: lambda_yy = 0.0d0
        real(real64) :: lambda_zz = 0.0d0
        real(real64) :: lambda_xy = 0.0d0
        real(real64) :: lambda_yz = 0.0d0
        real(real64) :: lambda_zx = 0.0d0
    contains
        procedure, pass(self), public :: reset => reset_thc_dispersity
    end type type_thc_dispersity

    interface
        module pure elemental subroutine reset_thc_dispersity(self)
            implicit none
            class(type_thc_dispersity), intent(inout) :: self
        end subroutine reset_thc_dispersity
    end interface

    type, abstract :: abst_thc
        integer(int32) :: material_id = -1
        real(real64) :: material1 = 0.0d0 !! like a soil or a rock, a concrete
        real(real64) :: material2 = 0.0d0 !! like a water
        real(real64) :: material3 = 0.0d0 !! like a ice
        real(real64) :: material4 = 0.0d0 !! like a gas
        logical :: use_dispersity = .false.
        type(type_iapws97), pointer :: water => null()
        type(type_iapws06), pointer :: ice => null()
    contains
        procedure(abst_initialize_thc), pass(self), public, deferred :: initialize
        procedure(abst_calc_thc_gp), pass(self), public, deferred :: calc_lambda_0
        procedure(abst_calc_thc_dispersity_gp), pass(self), public, deferred :: calc_lambda_dispersity
        generic, public :: calc => calc_lambda_0, calc_lambda_dispersity
    end type abst_thc

    abstract interface
        subroutine abst_initialize_thc(self, material_id, physics_info, water, ice)
            import :: abst_thc, type_physics_info, int32, type_iapws97, type_iapws06
            implicit none
            class(abst_thc), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_physics_info), intent(in) :: physics_info
            type(type_iapws97), intent(in), target :: water
            type(type_iapws06), intent(in), target :: ice

        end subroutine abst_initialize_thc

        pure elemental subroutine abst_calc_thc_gp(self, state, lambda)
            import :: abst_thc, type_state, real64
            implicit none
            class(abst_thc), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: lambda

        end subroutine abst_calc_thc_gp

        pure elemental subroutine abst_calc_thc_dispersity_gp(self, state, lambda)
            import :: abst_thc, type_state, type_thc_dispersity
            implicit none
            class(abst_thc), intent(in) :: self
            type(type_state), intent(in) :: state
            type(type_thc_dispersity), intent(inout) :: lambda

        end subroutine abst_calc_thc_dispersity_gp
    end interface

    !--------------------------------------------------------------------------------
    type, extends(abst_thc) :: type_thc_3phase
        real(real64), allocatable :: dispersity(:)
    contains
        procedure, pass(self) :: initialize => initialize_type_thc_3phase
        procedure, pass(self) :: calc_lambda_0 => calc_thc_gp_3phase
        procedure, pass(self) :: calc_lambda_dispersity => calc_thc_dispersity_gp_3phase
    end type type_thc_3phase

    interface
        module subroutine initialize_type_thc_3phase(self, material_id, physics_info, water, ice)
            implicit none
            class(type_thc_3phase), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_physics_info), intent(in) :: physics_info
            type(type_iapws97), intent(in), target :: water
            type(type_iapws06), intent(in), target :: ice

        end subroutine initialize_type_thc_3phase

        module pure elemental subroutine calc_thc_gp_3phase(self, state, lambda)
            implicit none
            class(type_thc_3phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: lambda

        end subroutine calc_thc_gp_3phase

        module pure elemental subroutine calc_thc_dispersity_gp_3phase(self, state, lambda)
            implicit none
            class(type_thc_3phase), intent(in) :: self
            type(type_state), intent(in) :: state
            type(type_thc_dispersity), intent(inout) :: lambda

        end subroutine calc_thc_dispersity_gp_3phase
    end interface

    type, extends(abst_thc) :: type_thc_4phase
        real(real64), allocatable :: dispersity(:)
        real(real64), allocatable :: params(:)
    contains
        procedure, pass(self) :: initialize => initialize_type_thc_4phase
        procedure, pass(self) :: calc_lambda_0 => calc_thc_gp_4phase
        procedure, pass(self) :: calc_lambda_dispersity => calc_thc_dispersity_gp_4phase
    end type type_thc_4phase

    interface
        module subroutine initialize_type_thc_4phase(self, material_id, physics_info, water, ice)
            implicit none
            class(type_thc_4phase), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_physics_info), intent(in) :: physics_info
            type(type_iapws97), intent(in), target :: water
            type(type_iapws06), intent(in), target :: ice

        end subroutine initialize_type_thc_4phase

        module pure elemental subroutine calc_thc_gp_4phase(self, state, lambda)
            implicit none
            class(type_thc_4phase), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: lambda

        end subroutine calc_thc_gp_4phase

        module pure elemental subroutine calc_thc_dispersity_gp_4phase(self, state, lambda)
            implicit none
            class(type_thc_4phase), intent(in) :: self
            type(type_state), intent(in) :: state
            type(type_thc_dispersity), intent(inout) :: lambda

        end subroutine calc_thc_dispersity_gp_4phase
    end interface

    interface
        module pure elemental subroutine calc_thc_2(lambda_soil, phi_soil, &
                                                    lambda_water, phi_water, lambda)
            implicit none
            real(real64), intent(in) :: lambda_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: lambda_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(inout) :: lambda

        end subroutine calc_thc_2

        module pure elemental subroutine calc_thc_3(lambda_soil, phi_soil, &
                                                    lambda_water, phi_water, &
                                                    lambda_ice, phi_ice, lambda)
            implicit none
            real(real64), intent(in) :: lambda_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: lambda_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: lambda_ice
            real(real64), intent(in) :: phi_ice
            real(real64), intent(inout) :: lambda

        end subroutine calc_thc_3

        module pure elemental subroutine calc_thc_4(lambda_soil, phi_soil, &
                                                    lambda_water, phi_water, &
                                                    lambda_ice, phi_ice, &
                                                    lambda_vapor, phi_vapor, lambda)
            implicit none
            real(real64), intent(in) :: lambda_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: lambda_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: lambda_ice
            real(real64), intent(in) :: phi_ice
            real(real64), intent(in) :: lambda_vapor
            real(real64), intent(in) :: phi_vapor
            real(real64), intent(inout) :: lambda

        end subroutine calc_thc_4

        module pure elemental subroutine calc_thc_4_vadoze(A, B, C, D, F1, F2, phi_water, phi_ice, phi_vapor, lambda)
            implicit none
            real(real64), intent(in) :: A, B, C, D
            real(real64), intent(in) :: F1, F2
            real(real64), intent(in) :: phi_water, phi_ice, phi_vapor
            real(real64), intent(inout) :: lambda

        end subroutine calc_thc_4_vadoze

        module pure elemental subroutine calc_thc_dispersity(lambda_0, lambda_T, lambda_L, &
                                                             htc_water, q_x, q_y, q_z, lambda)
            implicit none
            real(real64), intent(in) :: lambda_0
            real(real64), intent(in) :: lambda_T
            real(real64), intent(in) :: lambda_L
            real(real64), intent(in) :: htc_water
            real(real64), intent(in) :: q_x
            real(real64), intent(in) :: q_y
            real(real64), intent(in) :: q_z
            type(type_thc_dispersity), intent(inout) :: lambda
        end subroutine calc_thc_dispersity
    end interface

end module physics_material_thermal_conductivity
