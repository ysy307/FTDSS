module physics_models_phase_change_liquid_solid_fusion
    use, intrinsic :: iso_fortran_env
    use :: physics_models_wrf, only:abst_wrf, type_wrf_params
    use :: physics_models_phase_change_liquid_solid_gcc, only:abst_gcc
    implicit none
    private

    public :: type_fusion

    !>
    !> @brief Model for fusion (melting/freezing) physics.
    !>
    type :: type_fusion
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
    subroutine initialize_type_fusion(self, wrf, gcc)
        implicit none
        !> Fusion model object
        class(type_fusion), intent(inout) :: self
        class(abst_wrf), target, intent(in) :: wrf
        class(abst_gcc), target, intent(in) :: gcc

        self%wrf => wrf
        self%gcc => gcc
        ! Initialization code can be added here if needed in the future.

    end subroutine initialize_type_fusion

end module physics_models_phase_change_liquid_solid_fusion
