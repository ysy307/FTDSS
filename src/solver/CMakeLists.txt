add_library(SOLVER STATIC
    solve_interface.F90
    direct/solve_lu.F90
    iterative/solve_bicgstab.F90
    iterative/prconditioner/solve_preconditioner.F90
    solver_factory.F90
    solver.F90
)
target_include_directories(SOLVER PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(SOLVER PRIVATE
    CORE
    INPUT
    CALC_BLAS
)
enable_build_flags(SOLVER)