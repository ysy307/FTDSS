module core_types_config_physics
    use :: core_types_config_physics_base, only: &
        abst_config_physics_model, &
        abst_config_physics_material
    use :: core_types_config_physics_swcc, only: &
        type_config_wrf, &
        type_config_hcf
    use :: core_types_config_physics_gcc, only: &
        type_config_gcc
    use :: core_types_config_physics_constitutive, only: &
        type_config_constitutive

    implicit none
    private

    public :: abst_config_physics_model
    public :: abst_config_physics_material
    public :: type_config_wrf
    public :: type_config_hcf
    public :: type_config_gcc
    public :: type_config_constitutive

end module core_types_config_physics
