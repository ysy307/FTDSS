add_library(Core OBJECT
    c_utils/signal_flag.c
    c_utils/system_info.c
    c_utils/memory_stats.c
    c_utils/signal_flag.F90
    c_utils/system_info.F90
    c_utils/memory_stats.F90
    c_utils/c_utils.F90
    fortran_utils/system_info_wrapper.F90
    fortran_utils/memory_stats_wrapper.F90  
    fortran_utils/signal_flag_wrapper.F90
    fortran_utils/fortran_utils.F90
    error.F90
    check_nan.f90
    Allocate.F90
    BaseTypes.F90
    Unique.F90
    VTK_Constants.F90
    VTK.F90
)
enable_build_flags(Core)
enable_thirdparty (Core)