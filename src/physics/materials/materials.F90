module module_materials
    use :: physics_materials_density
    use :: physics_materials_specific_heat
    use :: physics_materials_thermal_conductivity
    use :: physics_materials_heat_capacity
    implicit none
    private

    public :: holder_dens
    public :: abst_den
    public :: type_den_1phase
    public :: type_den_2phase
    public :: type_den_3phase
    public :: type_den_4phase

    public :: holder_sphs
    public :: abst_sph
    public :: type_sph_1phase
    public :: type_sph_2phase
    public :: type_sph_3phase
    public :: type_sph_4phase

    public :: holder_vhcs
    public :: abst_vhc
    public :: type_vhc_1phase
    public :: type_vhc_2phase
    public :: type_vhc_3phase
    public :: type_vhc_4phase

    public :: holder_thcs
    public :: abst_thc
    public :: type_thc_1phase
    public :: type_thc_2phase
    public :: type_thc_3phase
    public :: type_thc_4phase

end module module_materials
