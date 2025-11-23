submodule(solver_solve) solve_type_solver_gmres
    implicit none
contains
    module subroutine initialize_type_solver_gmres(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_gmres), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        self%id = solver_settings%id
        self%name = "GMRES"

        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations
        self%m_restart = solver_settings%m_restart

        ! Initialize vectors and matrices specific to GMRES here

        call create_preconditioner(self%pc, preconditioner_settings, self%status)

        self%status = SOLVER_STATUS_SUCCESS

    end subroutine initialize_type_solver_gmres

    module subroutine solve_type_solver_gmres(self, A, b, x)
        implicit none
        class(type_solver_gmres), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x
    end subroutine solve_type_solver_gmres

    module subroutine destroy_type_solver_gmres(self)
        implicit none
        class(type_solver_gmres), intent(inout) :: self

        ! Destroy vectors and matrices specific to GMRES here
        self%id = -1
        if (allocated(self%name)) deallocate (self%name)
        self%num_nodes = -1
        self%tolerance = 0.0d0
        self%max_iterations = 0
        self%m_restart = 0

        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if

        self%status = SOLVER_STATUS_SUCCESS
    end subroutine destroy_type_solver_gmres
end submodule solve_type_solver_gmres
