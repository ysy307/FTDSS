module types_config_conditions
    use :: types_config_conditions_boundary, only: &
        type_config_bc
    use :: types_config_conditions_initial, only: &
        type_config_ic
    implicit none
    private

    public :: type_config_bc
    public :: type_config_ic

end module types_config_conditions
