add_library(Calc_HCF OBJECT
    HCF.F90
    HCF_BC.F90
    HCF_VG.F90
    HCF_KO.F90
    HCF_MVG.F90
    HCF_Durner.F90
    HCF_DVGCH.F90
    HCF_Impedance.F90
    HCF_Viscosity.F90
)
target_link_libraries(Calc_HCF PRIVATE
    Core
    Input
)
enable_build_flags(Calc_HCF)