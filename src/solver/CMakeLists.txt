add_library(Solver OBJECT
    solve_interface.f90
    direct/solve_lu.F90
    iterative/solve_bicgstab.F90
    iterative/prconditioner/solve_preconditioner.F90
    solver.F90
)
target_link_libraries(Solver PRIVATE
    Core
    Input
    Matrix
    Calc_BLAS
)
enable_build_flags(Solver)