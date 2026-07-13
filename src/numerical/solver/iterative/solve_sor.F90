!> SOR (Successive Over-Relaxation) iterative solver.
!> Uses SSOR preconditioner for the relaxation sweep.
!> x_{k+1} = x_k + M^{-1} (b - A x_k)
submodule(numerical_solver_interface) impl_solve_type_solver_sor
    implicit none
contains

    module subroutine initialize_type_solver_sor(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_sor), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: ierr

        self%ID = solver_settings%ID
        self%name = "SOR"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations

        call self%r%initialize(self%num_nodes)
        call self%z%initialize(self%num_nodes)
        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        self%status = SOLVER_STATUS%SUCCESS%ID
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_sor

    module subroutine solve_type_solver_sor(self, A, b, x)
        implicit none
        class(type_solver_sor), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: resid, resid0
        integer(int32) :: iter, ierr

        call self%residual_history%zero()
        call self%pc%setup(A)

        do iter = 1, self%max_iterations
            self%current_iteration = iter

            ! r = b - Ax
            call matvec(A, x, self%r, ierr)
            call vector_axpyz(-1.0d0, self%r, b, self%r)

            resid = vector_norm2(self%r)
            if (iter == 1) resid0 = resid
            call self%residual_history%set(MATRIX_OPS%INS, iter, resid)

            if (resid < self%tolerance .or. &
                (resid0 > self%tolerance .and. resid < self%relative_tolerance * resid0)) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                return
            end if

            ! z = M^{-1} r (SSOR preconditioner provides relaxation)
            call self%pc%apply(self%r, self%z)

            ! x = x + z
            call vector_axpy(1.0d0, self%z, x)

            if (was_interrupted()) stop
        end do

        self%status = SOLVER_STATUS%MAXITER%ID

    end subroutine solve_type_solver_sor

    module subroutine destroy_type_solver_sor(self)
        implicit none
        class(type_solver_sor), intent(inout) :: self

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        call self%r%destroy()
        call self%z%destroy()
        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_sor

end submodule impl_solve_type_solver_sor
