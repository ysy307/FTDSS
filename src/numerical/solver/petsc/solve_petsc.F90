#include <petsc/finclude/petscksp.h>

submodule(numerical_solver_interface) impl_solve_type_solver_petsc
    use :: petscksp
    implicit none

    !> Live PETSc solver instances, so PetscInitialize/PetscFinalize are called
    !> exactly once around them.
    integer(int32) :: num_live_petsc_solvers = 0

    !> Warn once per run rather than once per solve.
    logical :: warned_rank_local = .false.
    logical :: warned_nonlinear_serial = .false.

contains

    module subroutine initialize_type_solver_petsc(self, solver_settings)
        implicit none
        class(type_solver_petsc), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings

        PetscErrorCode :: ierr
        PetscBool :: already_initialized

        self%name = "PETSc"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%relative_tolerance = solver_settings%relative_tolerance
        self%max_iterations = solver_settings%max_iterations
        self%petsc_options = solver_settings%petsc_options
        self%status = SOLVER_STATUS%SUCCESS%ID

        call self%residual_history%initialize(1)
        self%current_iteration = 0

        ierr = 0
        call PetscInitialized(already_initialized, ierr)
        if (ierr /= 0) then
            self%status = SOLVER_STATUS%ILL_OPTIONS%ID
            return
        end if

        if (already_initialized .eqv. PETSC_FALSE) then
            call PetscInitialize(ierr)
            if (ierr /= 0) then
                self%status = SOLVER_STATUS%ILL_OPTIONS%ID
                return
            end if
        end if





        num_live_petsc_solvers = num_live_petsc_solvers + 1

    end subroutine initialize_type_solver_petsc

    !> The KSP must share the matrix's communicator, and that is only known
    !> once the first system arrives, so it is built here rather than in
    !> initialize.
    subroutine ensure_ksp(self, ierr)
        implicit none
        class(type_solver_petsc), intent(inout) :: self
        PetscErrorCode, intent(inout) :: ierr

        PC :: pc

        if (self%ksp_ready) return

        call KSPCreate(PETSC_COMM_WORLD, self%ksp, ierr)
        if (ierr /= 0) return

        ! Defaults only; ksp_type / pc_type / petsc_options in the project file
        ! choose the real algorithm through KSPSetFromOptions below.
        call KSPSetType(self%ksp, KSPGMRES, ierr)

        ! `relative_tolerance` is the field the caller varies per solver (1e-6
        ! monolithic, 5e-2 for the staggered thermal block), so it is rtol.
        ! `tolerance` is the absolute floor. A minimum of one Krylov iteration
        ! stops KSP returning du = 0 when ||b|| is already below that floor,
        ! which would stall the nonlinear iteration.
        if (ierr == 0) then
            call KSPSetTolerances(self%ksp, &
                                  real(self%relative_tolerance, PETSC_REAL_KIND), &
                                  real(self%tolerance, PETSC_REAL_KIND), &
                                  PETSC_CURRENT_REAL, &
                                  int(max(self%max_iterations, 1), kind(1_PETSC_INT_KIND)), &
                                  ierr)
        end if
        if (ierr == 0) call KSPSetMinimumIterations(self%ksp, 1_PETSC_INT_KIND, ierr)

        if (ierr == 0) call KSPGetPC(self%ksp, pc, ierr)
        if (ierr == 0) call PCSetType(pc, PCILU, ierr)

        ! The coupled T/p system has a column-scale disparity of many orders of
        ! magnitude. KSP's own diagonal scaling handles it on the assembled
        ! operator and unscales the solution, so the caller never sees scaled
        ! variables. A project file can turn it off again.
        if (ierr == 0) call PetscOptionsInsertString(PETSC_NULL_OPTIONS, &
                                                     "-ksp_diagonal_scale -ksp_diagonal_scale_fix", ierr)

        ! Everything above is a default. The project file's petsc_options is
        ! inserted first, then the command line overrides it, and
        ! KSPSetFromOptions applies both.
        if (ierr == 0 .and. len_trim(self%petsc_options) > 0) then
            call PetscOptionsInsertString(PETSC_NULL_OPTIONS, trim(self%petsc_options), ierr)
        end if
        if (ierr == 0) call KSPSetFromOptions(self%ksp, ierr)

        if (ierr == 0) self%ksp_ready = .true.
    end subroutine ensure_ksp

    !> Solve the assembled system.
    !>
    !> The operator is already a distributed PETSc Mat: the element loop added
    !> straight into it and MatAssembly summed the ranks' contributions on the
    !> shared rows. Nothing is copied here. The only work left is to move the
    !> local right-hand side into a Vec with the matrix's own layout, and to
    !> bring the solution back to every local dof, halo entries included.
    module subroutine solve_type_solver_petsc(self, A, b, x)
        implicit none
        class(type_solver_petsc), intent(inout) :: self
        Mat, intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

        real(real64), pointer :: b_src(:) => null()
        real(real64), pointer :: x_dst(:) => null()
        PetscScalar, pointer :: vec_data(:) => null()

        PetscErrorCode :: ierr
        PetscInt :: num_local_rows
        PetscInt, allocatable :: global_row(:)
        KSPConvergedReason :: reason
        PetscInt :: iterations
        PetscReal :: residual_norm
        real(real64), pointer :: history(:) => null()
        integer(int32) :: i

        self%status = SOLVER_STATUS%SUCCESS%ID
        ierr = 0

        b_src => b%get_data()
        x_dst => x%get_data()
        if (.not. associated(b_src) .or. .not. associated(x_dst)) then
            self%status = SOLVER_STATUS%ILL_OPTIONS%ID
            return
        end if

        num_local_rows = int(size(b_src), PETSC_INT_KIND)
        if (.not. allocated(self%global_dof)) then
            self%status = SOLVER_STATUS%ILL_OPTIONS%ID
            return
        end if
        if (size(self%global_dof) /= int(num_local_rows, int32)) then
            self%status = SOLVER_STATUS%ILL_OPTIONS%ID
            return
        end if

        allocate (global_row(num_local_rows))
        do i = 1, int(num_local_rows, int32)
            global_row(i) = int(self%global_dof(i), PETSC_INT_KIND)
        end do

        if (self%objects_ready .and. self%cached_local_size /= int(num_local_rows, int32)) then
            call destroy_petsc_objects(self)
        end if

        if (.not. self%objects_ready) then
            ! Created from the matrix, so the layout is the matrix's by
            ! construction rather than by agreement.
            call MatCreateVecs(A, self%x_petsc, self%b_petsc, ierr)
            if (ierr == 0) call VecCreateSeq(PETSC_COMM_SELF, num_local_rows, self%x_local, ierr)
            if (ierr == 0) then
                block
                    IS :: from_is
                    call ISCreateGeneral(PETSC_COMM_SELF, num_local_rows, global_row, &
                                         PETSC_COPY_VALUES, from_is, ierr)
                    if (ierr == 0) call VecScatterCreate(self%x_petsc, from_is, self%x_local, &
                                                         PETSC_NULL_IS, self%gather, ierr)
                    if (ierr == 0) call ISDestroy(from_is, ierr)
                end block
            end if
            if (ierr /= 0) then
                self%status = SOLVER_STATUS%OUT_OF_MEMORY%ID
                deallocate (global_row)
                return
            end if
            self%cached_local_size = int(num_local_rows, int32)
            self%objects_ready = .true.
        end if

        ! A shared dof is held by several ranks, each with its own partial
        ! right-hand side, so the entries are added, exactly as the matrix rows
        ! were.
        call VecZeroEntries(self%b_petsc, ierr)
        if (ierr == 0) call VecSetValues(self%b_petsc, num_local_rows, global_row, b_src, ADD_VALUES, ierr)
        if (ierr == 0) call VecAssemblyBegin(self%b_petsc, ierr)
        if (ierr == 0) call VecAssemblyEnd(self%b_petsc, ierr)
        if (ierr /= 0) then
            self%status = SOLVER_STATUS%ILL_OPTIONS%ID
            deallocate (global_row)
            return
        end if

        call ensure_ksp(self, ierr)
        if (ierr == 0) call KSPSetOperators(self%ksp, A, A, ierr)
        if (ierr == 0) call KSPSolve(self%ksp, self%b_petsc, self%x_petsc, ierr)
        if (ierr /= 0) then
            ! KSPSolve keeps a read lock on the right-hand side that it cannot
            ! release when it errors out, so the vectors are dropped and rebuilt
            ! rather than reused. The caller retries the step with a smaller dt.
            self%status = SOLVER_STATUS%BREAKDOWN%ID
            call destroy_petsc_objects(self)
            deallocate (global_row)
            return
        end if

        call KSPGetConvergedReason(self%ksp, reason, ierr)
        call KSPGetIterationNumber(self%ksp, iterations, ierr)
        call KSPGetResidualNorm(self%ksp, residual_norm, ierr)

        self%current_iteration = 1
        history => self%residual_history%get_data()
        if (associated(history)) history(1) = real(residual_norm, real64)

        self%status = map_converged_reason(int(reason%v, int32))
        if (self%status /= SOLVER_STATUS%SUCCESS%ID) then
            write (*, '(A,I0,A,I0,A,ES13.5)') &
                '   [PETSc-KSP] reason=', int(reason%v, int32), &
                ' its=', int(iterations, int32), &
                ' ||r||=', real(residual_norm, real64)
            deallocate (global_row)
            return
        end if

        ! Back to every local dof, the halo entries included, so the next
        ! assembly sees the same increment on both sides of a partition.
        call VecScatterBegin(self%gather, self%x_petsc, self%x_local, INSERT_VALUES, SCATTER_FORWARD, ierr)
        if (ierr == 0) call VecScatterEnd(self%gather, self%x_petsc, self%x_local, &
                                          INSERT_VALUES, SCATTER_FORWARD, ierr)
        if (ierr == 0) then
            call VecGetArrayRead(self%x_local, vec_data, ierr)
            x_dst = vec_data
            call VecRestoreArrayRead(self%x_local, vec_data, ierr)
        else
            self%status = SOLVER_STATUS%ILL_OPTIONS%ID
        end if

        deallocate (global_row)
    end subroutine solve_type_solver_petsc

    module subroutine destroy_type_solver_petsc(self)
        implicit none
        class(type_solver_petsc), intent(inout) :: self

        PetscErrorCode :: ierr

        call destroy_petsc_objects(self)

        ierr = 0
        if (self%ksp_ready) then
            call KSPDestroy(self%ksp, ierr)
            self%ksp_ready = .false.
        end if

        call self%residual_history%destroy()
        self%current_iteration = 0

        num_live_petsc_solvers = num_live_petsc_solvers - 1
        if (num_live_petsc_solvers <= 0) then
            num_live_petsc_solvers = 0
            call PetscFinalize(ierr)
        end if
    end subroutine destroy_type_solver_petsc

    !> Release the vectors and the scatter, keeping the KSP. The matrix belongs
    !> to the system layer and is never owned here.
    subroutine destroy_petsc_objects(self)
        implicit none
        class(type_solver_petsc), intent(inout) :: self

        PetscErrorCode :: ierr

        if (.not. self%objects_ready) return

        ierr = 0
        call VecScatterDestroy(self%gather, ierr)
        call VecDestroy(self%x_local, ierr)
        call VecDestroy(self%b_petsc, ierr)
        call VecDestroy(self%x_petsc, ierr)

        self%objects_ready = .false.
        self%cached_local_size = -1
    end subroutine destroy_petsc_objects

    !> `reason` is the raw enum value of a KSPConvergedReason.




    pure function map_converged_reason(reason) result(status)
        implicit none
        integer(int32), intent(in) :: reason
        integer(int32) :: status

        if (reason > 0) then
            status = SOLVER_STATUS%SUCCESS%ID
        else if (reason == int(KSP_DIVERGED_ITS%v, int32)) then
            status = SOLVER_STATUS%MAXITER%ID
        else if (reason == int(KSP_DIVERGED_BREAKDOWN%v, int32) .or. &
                 reason == int(KSP_DIVERGED_BREAKDOWN_BICG%v, int32) .or. &
                 reason == int(KSP_DIVERGED_NANORINF%v, int32)) then
            status = SOLVER_STATUS%BREAKDOWN%ID
        else if (reason == int(KSP_DIVERGED_PC_FAILED%v, int32) .or. &
                 reason == int(KSP_DIVERGED_INDEFINITE_PC%v, int32)) then
            status = SOLVER_STATUS%DECOMPOSITION_FAILURE%ID
        else
            status = SOLVER_STATUS%ILL_OPTIONS%ID
        end if
    end function map_converged_reason

end submodule impl_solve_type_solver_petsc
