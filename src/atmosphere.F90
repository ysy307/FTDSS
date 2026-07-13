!> Atmosphere module: facade for ETKF data assimilation components.
module governing_atmosphere
    use :: physics_governing_atmosphere_ensemble, only: &
        type_atmos_state, &
        type_ensemble_manager
    use :: physics_governing_atmosphere_observation, only: &
        type_obs_record, &
        type_observation_manager
    use :: physics_governing_atmosphere_etkf, only: &
        type_etkf_solver
    use :: physics_governing_atmosphere_controller, only: &
        type_da_config, &
        type_assimilation_controller
    implicit none
    public
end module governing_atmosphere
