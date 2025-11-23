submodule(solver_preconditioner) solver_preconditioner_none

contains
    module subroutine initialize_preconditioner_none(self, info)
        implicit none
        class(type_preconditioner_none), intent(inout) :: self
        type(type_preconditioner_settings), intent(in) :: info

        self%name = "None"
        self%id = SOLVER_PRECONDITION_NONE
        self%status = SOLVER_STATUS_SUCCESS
    end subroutine initialize_preconditioner_none

    module subroutine setup_preconditioner_none(self, A)
        implicit none
        class(type_preconditioner_none), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        self%status = SOLVER_STATUS_SUCCESS
    end subroutine setup_preconditioner_none

    module subroutine apply_preconditioner_none(self, r, z)
        implicit none
        class(type_preconditioner_none), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        real(real64), dimension(:), pointer :: r_data, z_data

        r_data => r%get_data()
        z_data => z%get_data()
        ! z = I * r
        z_data = r_data

        self%status = SOLVER_STATUS_SUCCESS
    end subroutine apply_preconditioner_none

    module subroutine destroy_preconditioner_none(self)
        implicit none
        class(type_preconditioner_none), intent(inout) :: self

        self%status = SOLVER_STATUS_SUCCESS
    end subroutine destroy_preconditioner_none

end submodule solver_preconditioner_none
