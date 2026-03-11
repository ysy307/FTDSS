submodule(physics_governing_thermal) thermal_matrix
    implicit none
contains

    module subroutine assemble_local_thermal(self, control, workspace, K_TT, K_TH, F_T)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_control), intent(in) :: control
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: K_TT
        type(type_matrix_dense), intent(inout), optional :: K_TH
        type(type_vector_dp), intent(inout), optional :: F_T

        if (control%is_compute_newton()) then
            call self%assemble_local_newton(control, workspace, K_TT, K_TH, F_T)
        else if (control%is_compute_picard()) then
            call self%assemble_local_picard(control, workspace, K_TT, K_TH, F_T)
        end if

    end subroutine assemble_local_thermal

    module subroutine assemble_local_newton_thermal(self, control, workspace, K_TT, K_TH, F_T)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_control), intent(in) :: control
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: K_TT
        type(type_matrix_dense), intent(inout), optional :: K_TH
        type(type_vector_dp), intent(inout), optional :: F_T

        integer(int32) :: i
        integer(int32) :: ierr
        real(real64) :: bdf0
        real(real64), pointer :: K_TT_val(:, :)
        real(real64), pointer :: F_T_val(:)
        logical :: has_advection

        bdf0 = workspace%bdf_coeffs(1)
        nullify (K_TT_val)
        nullify (F_T_val)

        if (present(K_TT)) K_TT_val => K_TT%get_val()
        if (present(F_T)) F_T_val => F_T%get_data()

        workspace%work_C(:) = 0.0d0
        workspace%work_D(:, :, :) = 0.0d0
        workspace%work_d_dt(:) = 0.0d0
        workspace%work_V(:, :) = 0.0d0

        has_advection = .false.

        do i = 1, workspace%num_fe_gauss
            call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), &
                                        workspace%work_C(i), scheme_opt=SCHEME_TANGENT)

            call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%work_D(:, :, i))

            call self%compute_transient_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%bdf_coeffs(1:workspace%bdf_order + 1), &
                                             workspace%work_d_dt(i))

            call self%compute_advective_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%work_V(:, i))
            if (any(abs(workspace%work_V(:, i)) > 1.0d-30)) has_advection = .true.
        end do

        ! Mass term -> K_TT
        call workspace%compute_K1(workspace%work_C, workspace%work_matrix)
        if (associated(K_TT_val)) then
            K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) = &
                K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) + &
                bdf0 * workspace%work_matrix(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes)
        end if

        ! Transient residual -> F_T
        if (associated(F_T_val)) then
            workspace%work_vec(:) = 0.0d0
            call workspace%compute_R1(workspace%work_d_dt, workspace%work_vec)
            F_T_val(1:workspace%num_fe_nodes) = F_T_val(1:workspace%num_fe_nodes) - &
                                                workspace%work_vec(1:workspace%num_fe_nodes)
        end if

        ! Diffusion term -> K_TT + F_T
        call workspace%compute_K2(workspace%work_D, workspace%work_matrix)
        if (associated(K_TT_val)) then
            K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) = &
                K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) + &
                workspace%work_matrix(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes)
        end if

        if (associated(F_T_val)) then
            workspace%work_vec(:) = 0.0d0
            call matvec(workspace%work_matrix, workspace%T_node, workspace%work_vec, ierr)
            F_T_val(1:workspace%num_fe_nodes) = F_T_val(1:workspace%num_fe_nodes) - &
                                                workspace%work_vec(1:workspace%num_fe_nodes)
        end if

        ! Advection term -> K_TT + F_T
        if (has_advection) then
            call workspace%compute_K3(workspace%work_V, workspace%work_matrix)
            if (associated(K_TT_val)) then
                K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) = &
                    K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) + &
                    workspace%work_matrix(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes)
            end if

            if (associated(F_T_val)) then
                workspace%work_vec(:) = 0.0d0
                call matvec(workspace%work_matrix, workspace%T_node, workspace%work_vec, ierr)
                F_T_val(1:workspace%num_fe_nodes) = F_T_val(1:workspace%num_fe_nodes) - &
                                                    workspace%work_vec(1:workspace%num_fe_nodes)
            end if
        end if

    end subroutine assemble_local_newton_thermal

    module subroutine assemble_local_picard_thermal(self, control, workspace, K_TT, K_TH, F_T)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_control), intent(in) :: control
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: K_TT
        type(type_matrix_dense), intent(inout), optional :: K_TH
        type(type_vector_dp), intent(inout), optional :: F_T

        integer(int32) :: i
        integer(int32) :: ierr
        real(real64) :: val_T, bdf0
        real(real64), pointer :: K_TT_val(:, :)
        real(real64), pointer :: F_T_val(:)

        real(real64), allocatable :: local_vec_transient(:)
        real(real64), allocatable :: local_vec_diff_flux(:)
        real(real64), allocatable :: local_vec_adv_flux(:)
        integer(int32) :: n_nodes
        logical :: has_advection

        n_nodes = workspace%num_fe_nodes
        allocate (local_vec_transient(n_nodes))
        allocate (local_vec_diff_flux(n_nodes))
        allocate (local_vec_adv_flux(n_nodes))

        workspace%work_C(:) = 0.0d0
        workspace%work_D(:, :, :) = 0.0d0
        workspace%work_d_dt(:) = 0.0d0
        workspace%work_V(:, :) = 0.0d0

        local_vec_transient(:) = 0.0d0
        local_vec_diff_flux(:) = 0.0d0
        local_vec_adv_flux(:) = 0.0d0

        has_advection = .false.

        bdf0 = workspace%bdf_coeffs(1)
        nullify (K_TT_val)
        nullify (F_T_val)

        if (present(K_TT)) K_TT_val => K_TT%get_val()
        if (present(F_T)) F_T_val => F_T%get_data()

        do i = 1, workspace%num_fe_gauss
            call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), workspace%work_C(i))

            call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), workspace%work_D(:, :, i))

            call self%compute_transient_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%bdf_coeffs(1:workspace%bdf_order + 1), &
                                             workspace%work_d_dt(i))

            call self%compute_advective_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%work_V(:, i))
            if (any(abs(workspace%work_V(:, i)) > 1.0d-30)) has_advection = .true.
        end do

        ! Mass matrix (LHS)
        call workspace%compute_K1(workspace%work_C, workspace%work_matrix)
        if (associated(K_TT_val)) then
            K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) = &
                K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) + &
                bdf0 * workspace%work_matrix(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes)
        end if

        ! Diffusion matrix (LHS) and flux
        call workspace%compute_K2(workspace%work_D, workspace%work_matrix)
        if (associated(K_TT_val)) then
            K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) = &
                K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) + &
                workspace%work_matrix(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes)
        end if
        call matvec(workspace%work_matrix, workspace%T_node, local_vec_diff_flux, ierr)

        ! Advection matrix (LHS) and flux
        if (has_advection) then
            call workspace%compute_K3(workspace%work_V, workspace%work_matrix)
            if (associated(K_TT_val)) then
                K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) = &
                    K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) + &
                    workspace%work_matrix(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes)
            end if
            call matvec(workspace%work_matrix, workspace%T_node, local_vec_adv_flux, ierr)
        end if

        ! Residual vector
        if (associated(F_T_val)) then
            call workspace%compute_R1(workspace%work_d_dt, local_vec_transient)

            do i = 1, workspace%num_fe_nodes
                val_T = -local_vec_transient(i) - local_vec_diff_flux(i) - local_vec_adv_flux(i)
                F_T_val(i) = F_T_val(i) + val_T
            end do
        end if

        if (allocated(local_vec_transient)) deallocate (local_vec_transient)
        if (allocated(local_vec_diff_flux)) deallocate (local_vec_diff_flux)
        if (allocated(local_vec_adv_flux)) deallocate (local_vec_adv_flux)

    end subroutine assemble_local_picard_thermal

end submodule thermal_matrix
