add_library(CALC_HCF STATIC
    hcf_interface.F90
    hcf_base.F90
    hcf_bc.F90
    hcf_vg.F90
    hcf_ko.F90
    hcf_mvg.F90
    hcf_durner.F90
    hcf_dvgch.F90
    hcf_impedance.F90
    hcf_viscosity.F90
)
target_include_directories(CALC_HCF PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(CALC_HCF PUBLIC
    CORE
    INPUT
)
enable_build_flags(CALC_HCF)