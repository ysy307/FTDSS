add_library(Calc_GCC OBJECT
    gcc_interface.F90
    gcc_non_segregation_m.F90
    gcc_non_segregation_pa.F90
    gcc_segregation_m.F90
    gcc_segregation_pa.F90
    gcc_base.F90
)
target_link_libraries(Calc_GCC PRIVATE
    Input
)
enable_build_flags(Calc_GCC)