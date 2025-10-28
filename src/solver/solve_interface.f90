module solver_solve
    use, intrinsic :: iso_fortran_env, only: int32, real64
!$  use omp_lib
    use :: module_input, only:type_input
    use :: module_core
    use :: module_linalg, only:norm_2, dot
    use :: module_field, only:type_jacobian_matrix
    implicit none
    private

    public :: abst_solver
    public :: type_solver_bicgstab

    public :: create_solver

    type, abstract :: abst_solver
        private
        class(abst_matrix), pointer :: A => null()
    contains
        procedure(abst_solve), pass(self), deferred :: solve
        procedure(abst_check), pass(self), deferred :: check
    end type abst_solver

    abstract interface
        subroutine abst_solve(self, b, x, status)
            import :: abst_solver, abst_matrix, int32, real64
            implicit none
            class(abst_solver), intent(inout) :: self
            real(real64), intent(inout) :: b(:)
            real(real64), intent(inout) :: x(:)
            integer(int32), intent(inout) :: status
        end subroutine abst_solve

        subroutine abst_check(self, status, time)
            import :: abst_solver, int32, real64
            implicit none
            class(abst_solver), intent(inout) :: self
            integer(int32), intent(in) :: status
            real(real64), intent(in) :: time
        end subroutine abst_check
    end interface

    type, extends(abst_solver) :: type_solver_bicgstab
        integer(int32) :: size
        real(real64), allocatable :: m(:)
        real(real64), allocatable :: p(:)
        real(real64), allocatable :: phat(:)
        real(real64), allocatable :: s(:)
        real(real64), allocatable :: shat(:)
        real(real64), allocatable :: r(:)
        real(real64), allocatable :: r0(:)
        real(real64), allocatable :: t(:)
        real(real64), allocatable :: v(:)
        real(real64), allocatable :: x(:)

        real(real64) :: tolerance
        integer(int32) :: max_iterations

        integer(int32) :: preconditioner
        ! 0: No preconditioner (No implemented)
        ! 1: Jacobi preconditioner
        ! 2: ILU preconditioner (No implemented)
    contains
        procedure :: solve => solve_bicgstab
        procedure :: check => check_bicgstab
        procedure, private, pass(self) :: create_preconditioner => create_preconditioner_bicgstab
        procedure, private, pass(self) :: apply_preconditioner => apply_preconditioner_bicgstab
        final :: destruct_type_solver_bicgstab
    end type type_solver_bicgstab

    interface
        module function construct_type_solver_bicgstab(A, tolerance, max_iterations, preconditioner) result(structure)
            implicit none
            type(type_jacobian_matrix), intent(in), target :: A
            real(real64), intent(in) :: tolerance
            integer(int32), intent(in) :: max_iterations
            integer(int32), intent(in) :: preconditioner
            class(abst_solver), allocatable :: structure

        end function construct_type_solver_bicgstab

        module subroutine solve_bicgstab(self, b, x, status)
            implicit none
            class(type_solver_bicgstab), intent(inout) :: self
            real(real64), intent(inout) :: b(:)
            real(real64), intent(inout) :: x(:)
            integer(int32), intent(inout) :: status

        end subroutine solve_bicgstab

        module subroutine check_bicgstab(self, status, time)
            implicit none
            class(type_solver_bicgstab), intent(inout) :: self
            integer(int32), intent(in) :: status
            real(real64), intent(in) :: time

        end subroutine check_bicgstab
    end interface

    interface

        module subroutine create_preconditioner_jacobi(N, A, M)
            implicit none
            integer(int32), intent(in) :: N
            class(abst_matrix), intent(in) :: A
            real(real64), intent(inout) :: M(:)

        end subroutine create_preconditioner_jacobi

        module subroutine apply_preconditioner_jacobi(N, M, r, z)
            implicit none
            integer(int32), intent(in) :: N
            real(real64), intent(in) :: M(:)
            real(real64), intent(in) :: r(:)
            real(real64), intent(inout) :: z(:)

        end subroutine apply_preconditioner_jacobi

        module subroutine create_preconditioner_bicgstab(self, A)
            implicit none
            class(type_solver_bicgstab), intent(inout) :: self
            class(abst_matrix), intent(in) :: A

        end subroutine create_preconditioner_bicgstab

        module subroutine apply_preconditioner_bicgstab(self, b, x)
            implicit none
            class(type_solver_bicgstab), intent(inout) :: self
            real(real64), intent(inout) :: b(:)
            real(real64), intent(inout) :: x(:)
        end subroutine apply_preconditioner_bicgstab

        module subroutine destruct_type_solver_bicgstab(self)
            implicit none
            type(type_solver_bicgstab), intent(inout) :: self

        end subroutine destruct_type_solver_bicgstab

    end interface

    interface
        module function create_solver(input, target_solver, target_matrix, num_node) result(solver)
            implicit none
            type(type_input), intent(in) :: input
            character(*), intent(in) :: target_solver
            type(type_jacobian_matrix), intent(in), target :: target_matrix
            integer(int32), intent(in) :: num_node
            class(abst_solver), allocatable :: solver
        end function create_solver
    end interface

end module solver_solve
