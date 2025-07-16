module module_solver
    use :: solver_solve, only:abst_solver, &
        type_solver_sparse_crs_bicgstab, &
        type_solver_sparse_crs_lu, &
        type_solver_dense_lu
    use :: solver_solver_factory, only:create_solver
    implicit none

end module module_solver
