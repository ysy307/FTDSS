add_library(Calc_WRF OBJECT
    wrf_interface.F90
    wrf_bc.F90
    wrf_vg.F90
    wrf_ko.F90
    wrf_mvg.F90
    wrf_durner.F90
    wrf_dvgcgh.F90
    wrf_base.F90
)
target_link_libraries(Calc_WRF PRIVATE
    Input
)
enable_build_flags(Calc_WRF)