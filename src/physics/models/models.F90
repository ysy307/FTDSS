module module_physics_models
    use :: physics_models_wrf
    use :: physics_models_hcf
    use :: physics_models_phase_change_liquid_solid_gcc
    use :: physics_models_phase_change_liquid_solid_fusion
    use :: physics_models_phase_change_liquid_vapor_vaporization
    implicit none
    private

    public :: holder_wrfs
    public :: abst_wrf
    public :: type_wrf_bc
    public :: type_wrf_vg
    public :: type_wrf_ko
    public :: type_wrf_mvg
    public :: type_wrf_durner
    public :: type_wrf_dvgch
    public :: type_wrf_params

    public :: abst_hcf
    public :: type_hcf_params
    public :: holder_hcfs
    public :: type_hcf_base
    public :: type_hcf_impedance
    public :: type_hcf_viscosity
    public :: type_hcf_base_impedance
    public :: type_hcf_base_viscosity
    public :: type_hcf_impedance_viscosity
    public :: type_hcf_base_impedance_viscosity

    public :: type_fusion
    public :: holder_gccs
    public :: abst_gcc
    public :: type_gcc_non_segregation
    public :: type_gcc_segregation

    public :: type_evaporation

end module module_physics_models
