submodule(numerical_solver_preconditioner) solver_preconditioner_none
    implicit none
contains
    module subroutine initialize_preconditioner_none(self, info)
        implicit none
        class(type_preconditioner_none), intent(inout) :: self
        type(type_preconditioner_settings), intent(in) :: info

        self%ID = PRECONDITIONER_TYPES%NONE%ID
        self%name = "None"
        self%status = SOLVER_STATUS%SUCCESS%ID
    end subroutine initialize_preconditioner_none

    module subroutine setup_preconditioner_none(self, A)
        implicit none
        class(type_preconditioner_none), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        self%status = SOLVER_STATUS%SUCCESS%ID
    end subroutine setup_preconditioner_none

    module subroutine apply_preconditioner_none(self, r, z)
        implicit none
        class(type_preconditioner_none), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        ! z = I * r
        call z%copy(r)

        self%status = SOLVER_STATUS%SUCCESS%ID
    end subroutine apply_preconditioner_none

    module subroutine destroy_preconditioner_none(self)
        implicit none
        class(type_preconditioner_none), intent(inout) :: self

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        self%status = SOLVER_STATUS%SUCCESS%ID
    end subroutine destroy_preconditioner_none

end submodule solver_preconditioner_none
