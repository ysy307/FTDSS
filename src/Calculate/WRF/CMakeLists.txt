add_library(Calc_WRF
    WRF.F90
    WRF_BC.F90
    WRF_VG.F90
    WRF_KO.F90
    WRF_MVG.F90
    WRF_Durner.F90
    WRF_DVGCH.F90
)
target_link_libraries(Calc_WRF PRIVATE
    Input
)
enable_build_flags(Calc_WRF)