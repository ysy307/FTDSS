#ifdef _PETSC
#include <petsc/finclude/petscksp.h>
#endif

module numerical_solver_interface
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit
    use, intrinsic :: iso_c_binding, only: c_int
!$  use omp_lib
    use :: stdlib_strings, only:strip
    use :: module_core
    use :: module_linalg
#ifdef _PETSC
    use :: petscksp, only: tKSP, tMat, tVec, tVecScatter
#endif
    implicit none
    private

    public :: abst_solver
#ifdef _PETSC
    public :: type_solver_petsc
#endif

    public :: type_solver_settings
    public :: create_solver

    type :: type_solver_settings
        private
        integer(int32) :: id
        real(real64) :: tolerance
        real(real64) :: relative_tolerance = 1.0d-6
        integer(int32) :: max_iterations
        integer(int32) :: num_nodes
        !> Raw PETSc option string from the project file.
        character(len=512) :: petsc_options = ''
    contains
        procedure :: set => set_solver_settings
    end type type_solver_settings

    type, abstract :: abst_solver
        private
        ! Solver basic info
        !> Name of the solver.
        character(:), allocatable :: name
        !> Status of the solver after execution.
        integer(int32) :: status = SOLVER_STATUS%SUCCESS%ID

        integer(int32) :: num_nodes = -1
        real(real64) :: tolerance = 0.0d0
        real(real64) :: relative_tolerance = 1.0d-6
        integer(int32) :: max_iterations = 0
        character(len=512) :: petsc_options = ''

        !> Global index of every local degree of freedom, in the order the
        !> local right-hand side uses. It is the dof layout the DM built, and
        !> it is what places a local entry in the distributed vector.
        integer(int32), allocatable :: global_dof(:)

        type(type_vector_dp) :: residual_history
        integer(int32) :: current_iteration = 0

    contains
        procedure(abst_solver_initialize), pass(self), public, deferred :: initialize !&
        procedure(abst_solver_solve),      pass(self), public, deferred :: solve !&
        procedure,                         pass(self), public           :: set_dof_map => set_dof_map_solver !&
        procedure,                         pass(self), public           :: check => check_solver !&
        procedure,                         pass(self), public           :: is_success => is_success_solver !&
        procedure,                         pass(self), public           :: display_rhistory => display_residual_history_solver !&
        procedure(abst_solver_destroy),    pass(self), public, deferred :: destroy !&
    end type abst_solver

    abstract interface
        subroutine abst_solver_initialize(self, solver_settings)
            import :: abst_solver, type_solver_settings
            implicit none
            class(abst_solver), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
        end subroutine abst_solver_initialize

        subroutine abst_solver_solve(self, A, b, x)
            import :: abst_solver, tMat, type_vector_dp, int32
            implicit none
            class(abst_solver), intent(inout) :: self
            !> The assembled PETSc operator. It is solved in place, never copied.
            Mat, intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x

        end subroutine abst_solver_solve

        subroutine abst_solver_destroy(self)
            import :: abst_solver
            implicit none
            class(abst_solver), intent(inout) :: self

        end subroutine abst_solver_destroy

    end interface

#ifdef _PETSC
    !> KSP-backed solver. The JSON supplies the tolerances and a default
    !> KSP/PC pair; everything else comes from the PETSc option database, so
    !> -ksp_type / -pc_type / -ksp_monitor work without new JSON keys.
    !>
    !> The operator arrives already assembled and already distributed, so this
    !> solver only has to supply the right-hand side and read the solution back.
    !> The vectors are created from the matrix, which is what guarantees they
    !> share its layout.
    type, extends(abst_solver) :: type_solver_petsc
        KSP :: ksp
        Vec :: b_petsc
        Vec :: x_petsc
        VecScatter :: gather
        Vec :: x_local
        logical :: objects_ready = .false.
        logical :: ksp_ready = .false.
        !> Cached layout signature; a mismatch forces the vectors to be rebuilt.
        integer(int32) :: cached_local_size = -1
    contains
        procedure :: initialize => initialize_type_solver_petsc
        procedure :: solve => solve_type_solver_petsc
        procedure :: destroy => destroy_type_solver_petsc
    end type type_solver_petsc
#endif

#ifdef _PETSC
    interface
        module subroutine initialize_type_solver_petsc(self, solver_settings)
            implicit none
            class(type_solver_petsc), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
        end subroutine initialize_type_solver_petsc

        module subroutine solve_type_solver_petsc(self, A, b, x)
            implicit none
            class(type_solver_petsc), intent(inout) :: self
            Mat, intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_petsc

        module subroutine destroy_type_solver_petsc(self)
            implicit none
            class(type_solver_petsc), intent(inout) :: self

        end subroutine destroy_type_solver_petsc

    end interface
#endif

contains
    subroutine set_solver_settings(self, num_nodes, tolerance, max_iterations, &
                                   relative_tolerance, petsc_options)
        implicit none
        class(type_solver_settings), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        real(real64), intent(in) :: tolerance
        integer(int32), intent(in) :: max_iterations
        real(real64), intent(in), optional :: relative_tolerance
        character(len=*), intent(in), optional :: petsc_options

        self%num_nodes = num_nodes
        self%tolerance = tolerance
        self%relative_tolerance = 1.0d-6
        if (present(relative_tolerance)) self%relative_tolerance = relative_tolerance
        self%max_iterations = max_iterations
        self%petsc_options = ''
        if (present(petsc_options)) self%petsc_options = petsc_options

    end subroutine set_solver_settings

    !> Hand the solver the node numbering of a distributed mesh. The back-end
    !> uses it to assemble one global system instead of one system per rank.
    !> Install the dof numbering the DM produced. Without it the solver has no
    !> way to place a local entry into the distributed vectors.
    subroutine set_dof_map_solver(self, global_dof)
        implicit none
        class(abst_solver), intent(inout) :: self
        integer(int32), intent(in) :: global_dof(:)

        if (allocated(self%global_dof)) deallocate (self%global_dof)
        allocate (self%global_dof, source=global_dof)
    end subroutine set_dof_map_solver

    subroutine check_solver(self, time, unit_display)
        implicit none
        class(abst_solver), intent(inout) :: self
        real(real64), intent(in), optional :: time
        integer(int32), intent(in), optional :: unit_display
        integer(int32) :: unit

        if (self%status == SOLVER_STATUS%SUCCESS%ID) return

        if (present(unit_display)) then
            unit = unit_display
        else
            unit = output_unit
        end if

        if (present(time)) then
            select case (self%status)
            case (SOLVER_STATUS%ILL_OPTIONS%ID)
                write (unit, '(a,es13.4,a)') strip(self%name), ": ", time, " Day: Solver encountered ILL_OPTIONS."
            case (SOLVER_STATUS%BREAKDOWN%ID)
                write (unit, '(a,es13.4,a)') strip(self%name), ": ", time, " Day: Solver encountered BREAKDOWN."
            case (SOLVER_STATUS%OUT_OF_MEMORY%ID)
                write (unit, '(a,es13.4,a)') strip(self%name), ": ", time, " Day: Solver encountered OUT_OF_MEMORY."
            case (SOLVER_STATUS%MAXITER%ID)
                write (unit, '(a,es13.4,a)') strip(self%name), ": ", time, " Day: Solver encountered MAXITER."
            case (SOLVER_STATUS%NOT_IMPLEMENTED%ID)
                write (unit, '(a,es13.4,a)') strip(self%name), ": ", time, " Day: Solver encountered NOT_IMPLEMENTED."
            end select
        else
            select case (self%status)
            case (SOLVER_STATUS%ILL_OPTIONS%ID)
                write (unit, '(2a)') strip(self%name), ": Solver encountered ILL_OPTIONS."
            case (SOLVER_STATUS%BREAKDOWN%ID)
                write (unit, '(2a)') strip(self%name), ": Solver encountered BREAKDOWN."
            case (SOLVER_STATUS%OUT_OF_MEMORY%ID)
                write (unit, '(2a)') strip(self%name), ": Solver encountered OUT_OF_MEMORY."
            case (SOLVER_STATUS%MAXITER%ID)
                write (unit, '(2a)') strip(self%name), ": Solver encountered MAXITER."
            case (SOLVER_STATUS%NOT_IMPLEMENTED%ID)
                write (unit, '(2a)') strip(self%name), ": Solver encountered NOT_IMPLEMENTED."
            end select
        end if

    end subroutine check_solver

    pure function is_success_solver(self) result(ret)
        implicit none
        class(abst_solver), intent(in) :: self
        logical :: ret
        ret = (self%status == SOLVER_STATUS%SUCCESS%ID)
    end function is_success_solver

    subroutine display_residual_history_solver(self, unit_display)
        implicit none
        class(abst_solver), intent(inout) :: self
        integer(int32), intent(in), optional :: unit_display

        integer(int32) :: unit
        integer(int32) :: i
        real(real64), pointer :: residual_history_ptr(:)

        if (present(unit_display)) then
            unit = unit_display
        else
            unit = output_unit
        end if

        residual_history_ptr => self%residual_history%get_data()

        write (unit, '(a)') "Residual history:"
        do i = 1, self%current_iteration
            write (unit, '(i6,2x,es13.6)') i, residual_history_ptr(i)
        end do

    end subroutine display_residual_history_solver

    !> Build the linear solver. PETSc is the only back-end; the algorithm is
    !> named in the project file and applied through the PETSc option database.
    subroutine create_solver(solver, solver_settings, ierr)
        implicit none
        class(abst_solver), allocatable, intent(inout) :: solver
        type(type_solver_settings), intent(in) :: solver_settings
        integer(int32), intent(inout) :: ierr

        if (allocated(solver)) deallocate (solver)

#ifdef _PETSC
        allocate (type_solver_petsc :: solver)
        call solver%initialize(solver_settings)
        ierr = solver%status
#else
        ierr = SOLVER_STATUS%NOT_IMPLEMENTED%ID
#endif
    end subroutine create_solver

end module numerical_solver_interface
