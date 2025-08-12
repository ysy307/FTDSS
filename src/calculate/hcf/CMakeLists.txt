add_library(Calc_HCF OBJECT
    hcf.F90
    hcf_bc.F90
    hcf_vg.F90
    hcf_ko.F90
    hcf_mvg.F90
    hcf_durner.F90
    hcf_dvgch.F90
    hcf_impedance.F90
    hcf_viscosity.F90
)
target_link_libraries(Calc_HCF PRIVATE
    Core
    Input
)
enable_build_flags(Calc_HCF)