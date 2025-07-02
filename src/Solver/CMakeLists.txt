add_library(Solver OBJECT
    Solve.f90
    Solve_BiCGSTAB.F90
    Solve_LU.F90
    Solve_Preconditioner.F90
)
target_link_libraries(Solver PRIVATE
    Core
    Input
    Matrix
    Calc_BLAS
)
enable_build_flags(Solver)