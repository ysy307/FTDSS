module module_calculate
    use :: calculate_blas, only:norm_2, inner_product
    use :: calculate_gcc, only:holder_gccs, abst_gcc, type_gcc_non_segregation_m, type_gcc_non_segregation_pa, type_gcc_segregation_m, type_gcc_segregation_pa
    use :: calculate_wrf, only:holder_wrfs, abst_wrf, type_wrf_bc, type_wrf_vg, type_wrf_ko, type_wrf_mvg, type_wrf_durner, type_wrf_dvgch
    use :: calculate_density, only:holder_dens, abst_den, type_den_3phase
    use :: calculate_thermal_conductivity, only:holder_thcs, abst_thc, type_thc_3phase
    use :: calculate_specific_heat, only:holder_sphs, abst_sph, type_sph_3phase
    use :: calculate_volumetric_heat_capacity, only:holder_vhcs, abst_vhc, type_vhc_3phase, type_vhc_3phase_apparent
    implicit none
    private

    !-------------------------------------------------------------------------------------------------------------------------------
    ! BLAS calculation module
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: norm_2
    public :: inner_product

    !-------------------------------------------------------------------------------------------------------------------------------
    !  GCC calculation module
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: holder_gccs
    public :: abst_gcc
    public :: type_gcc_non_segregation_m
    public :: type_gcc_non_segregation_pa
    public :: type_gcc_segregation_m
    public :: type_gcc_segregation_pa

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Water retention function module
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: holder_wrfs
    public :: abst_wrf
    public :: type_wrf_bc
    public :: type_wrf_vg
    public :: type_wrf_ko
    public :: type_wrf_mvg
    public :: type_wrf_durner
    public :: type_wrf_dvgch

    !-------------------------------------------------------------------------------------------------------------------------------
    !  Density calculation module
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: holder_dens
    public :: abst_den
    public :: type_den_3phase

    !-------------------------------------------------------------------------------------------------------------------------------
    !  Thermal conductivity calculation module
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: holder_thcs
    public :: abst_thc
    public :: type_thc_3phase

    !-------------------------------------------------------------------------------------------------------------------------------
    !  Specific heat calculation module
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: holder_sphs
    public :: abst_sph
    public :: type_sph_3phase

    !-------------------------------------------------------------------------------------------------------------------------------
    !  Volumetric heat capacity calculation module
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: holder_vhcs
    public :: abst_vhc
    public :: type_vhc_3phase
    public :: type_vhc_3phase_apparent

end module module_calculate
