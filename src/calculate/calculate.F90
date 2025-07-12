module module_calculate
    use :: calculate_gcc, only:holder_gcc, abst_gcc, type_gcc_non_segregation_m, type_gcc_non_segregation_pa, type_gcc_segregation_m, type_gcc_segregation_pa
    use :: calculate_density, only:holder_den, abst_den, type_den_3phase
    implicit none
    private

    !-------------------------------------------------------------------------------------------------------------------------------
    !  GCC calculation module
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: holder_gcc
    public :: abst_gcc
    public :: type_gcc_non_segregation_m
    public :: type_gcc_non_segregation_pa
    public :: type_gcc_segregation_m
    public :: type_gcc_segregation_pa

    !-------------------------------------------------------------------------------------------------------------------------------
    !  Dinsity calculation module
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: holder_den
    public :: abst_den
    public :: type_den_3phase

end module module_calculate
