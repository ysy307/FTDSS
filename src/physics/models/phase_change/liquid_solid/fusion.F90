module physics_models_phase_change_liquid_solid_fusion
    use, intrinsic :: iso_fortran_env
    use :: iapws, only:type_iapws97, type_iapws06
    use :: physics_types, only:abst_physics
    use :: physics_models_wrf, only:abst_wrf, type_wrf_params
    use :: physics_models_phase_change_liquid_solid_gcc, only:abst_gcc
    implicit none
    private

    public :: type_fusion

    !>
    !> @brief Model for fusion (melting/freezing) physics.
    !>
    type, extends(abst_physics) :: type_fusion
        private
        class(abst_wrf), pointer :: wrf => null()
        class(abst_gcc), pointer :: gcc => null()
    contains
        procedure, pass(self), public :: initialize => initialize_type_fusion
        ! procedure, pass(self), public :: calc_latent_heat_fusion
    end type type_fusion

contains
    !>
    !> @brief Initialize fusion model.
    !>
    subroutine initialize_type_fusion(self, wrf, gcc, water, ice)
        implicit none
        !> Fusion model object
        class(type_fusion), intent(inout) :: self
        class(abst_wrf), intent(in), target :: wrf
        class(abst_gcc), intent(in), target :: gcc
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        self%wrf => wrf
        self%gcc => gcc
        self%water => water
        self%ice => ice

    end subroutine initialize_type_fusion

end module physics_models_phase_change_liquid_solid_fusion
