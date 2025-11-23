module module_solver
    use :: solver_solve
    use :: solver_preconditioner
    implicit none
    private

    public :: create_solver
    public :: abst_solver
    public :: type_solver_bicgstab
    public :: type_solver_gmres

    public :: type_solver_settings
    public :: type_preconditioner_settings

end module module_solver
