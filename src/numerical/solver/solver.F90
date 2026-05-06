module module_solver
    use :: numerical_solver_interface
    use :: numerical_solver_preconditioner
    implicit none
    private

    public :: create_solver
    public :: abst_solver
    public :: type_solver_bicgstab
    public :: type_solver_gmres
    public :: type_solver_pardiso

    public :: type_solver_settings
    public :: type_preconditioner_settings

end module module_solver
