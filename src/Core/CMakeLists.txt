add_library(Core OBJECT
    Error.F90
    Allocate.F90
    BaseTypes.F90
    Unique.F90
    C_Util.F90
    VTK.F90
)
enable_build_flags(Core)
enable_thirdparty (Core)