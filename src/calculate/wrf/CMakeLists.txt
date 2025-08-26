add_library(CALC_WRF STATIC
    wrf_interface.F90
    wrf_bc.F90
    wrf_vg.F90
    wrf_ko.F90
    wrf_mvg.F90
    wrf_durner.F90
    wrf_dvgch.F90
    wrf_base.F90
)
target_include_directories(CALC_WRF PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(CALC_WRF PUBLIC
    INPUT
)
enable_build_flags(CALC_WRF)