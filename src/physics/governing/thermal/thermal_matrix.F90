submodule(physics_governing_thermal) thermal_matrix
    implicit none
contains

    !> @brief Assemble local (element) matrices and residual vectors
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

    ! ==========================================================================
    ! Newton-Raphson Assembly (Tangent Stiffness & Enthalpy Residual)
    ! ==========================================================================
    !> @brief Compute tangent stiffness matrix and residual vector using Newton-Raphson method
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

        bdf0 = workspace%bdf_coeffs(1)
        nullify (K_TT_val)
        nullify (F_T_val)

        if (present(K_TT)) K_TT_val => K_TT%get_val()
        if (present(F_T)) F_T_val => F_T%get_data()

        ! 1. Gauss point loop (state and coefficient evaluation)
        workspace%work_C(:) = 0.0d0
        workspace%work_D(:, :, :) = 0.0d0
        workspace%work_d_dt(:) = 0.0d0

        do i = 1, workspace%num_fe_gauss
            ! (A) Mass Term: tangent heat capacity C_tan
            call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), &
                                        workspace%work_C(i), scheme_opt=SCHEME_TANGENT)

            ! (B) Diffusion Term: thermal conductivity D (evaluated at current temperature)
            call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%work_D(:, :, i))

            ! (C) Transient Residual: enthalpy time derivative dH/dt
            call self%compute_transient_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%bdf_coeffs(1:workspace%bdf_order + 1), &
                                             workspace%work_d_dt(i))
        end do

        ! 2. Mass term contribution
        call workspace%compute_K1(workspace%work_C, workspace%work_matrix)

        if (associated(K_TT_val)) then
            ! Jacobian += bdf0 * MassMatrix
            K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) = &
                K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) + &
                bdf0 * workspace%work_matrix(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes)
        end if

        ! 3. Transient residual contribution
        if (associated(F_T_val)) then
            workspace%work_vec(:) = 0.0d0
            ! compute_R1: integral of N^T * dH/dt dV
            call workspace%compute_R1(workspace%work_d_dt, workspace%work_vec)

            F_T_val(1:workspace%num_fe_nodes) = F_T_val(1:workspace%num_fe_nodes) - &
                                                workspace%work_vec(1:workspace%num_fe_nodes)
        end if

        ! 4. Diffusion term contribution
        call workspace%compute_K2(workspace%work_D, workspace%work_matrix)

        if (associated(K_TT_val)) then
            ! Jacobian += StiffnessMatrix
            K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) = &
                K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) + &
                workspace%work_matrix(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes)
        end if

        ! 5. Internal force residual contribution
        if (associated(F_T_val)) then
            workspace%work_vec(:) = 0.0d0
            ! F_int = K * T_node (work_matrix currently holds the K2 result)
            call matvec(workspace%work_matrix, workspace%T_node, workspace%work_vec, ierr)

            F_T_val(1:workspace%num_fe_nodes) = F_T_val(1:workspace%num_fe_nodes) - &
                                                workspace%work_vec(1:workspace%num_fe_nodes)
        end if

    end subroutine assemble_local_newton_thermal

    ! ==========================================================================
    ! Picard Assembly (Secant Stiffness & Linearized Residual)
    ! ==========================================================================
    !> @brief Compute linearized matrix and RHS vector using modified Picard method
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

        ! Allocatable work vectors to avoid automatic arrays with member-dependent size
        real(real64), allocatable :: local_vec_transient(:)
        real(real64), allocatable :: local_vec_diff_flux(:)
        integer(int32) :: n_nodes

        n_nodes = workspace%num_fe_nodes
        allocate (local_vec_transient(n_nodes))
        allocate (local_vec_diff_flux(n_nodes))

        ! Initialize work arrays
        workspace%work_C(:) = 0.0d0
        workspace%work_D(:, :, :) = 0.0d0
        workspace%work_d_dt(:) = 0.0d0

        local_vec_transient(:) = 0.0d0
        local_vec_diff_flux(:) = 0.0d0

        bdf0 = workspace%bdf_coeffs(1)
        nullify (K_TT_val)
        nullify (F_T_val)

        if (present(K_TT)) K_TT_val => K_TT%get_val()
        if (present(F_T)) F_T_val => F_T%get_data()

        ! ----------------------------------------------------------------------
        ! 1. Gauss point loop
        ! ----------------------------------------------------------------------
        do i = 1, workspace%num_fe_gauss
            ! (A) Instantaneous heat capacity C (for Picard method)
            call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), workspace%work_C(i))

            ! (B) Thermal conductivity D
            call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), workspace%work_D(:, :, i))

            ! (C) [Important] Enthalpy time rate dH/dt for residual (exact computation, same as Newton)
            !     Uses compute_transient_term, not compute_history_term
            call self%compute_transient_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%bdf_coeffs(1:workspace%bdf_order + 1), &
                                             workspace%work_d_dt(i))
        end do

        ! ----------------------------------------------------------------------
        ! 2. Mass matrix (LHS) construction
        !    Uses C_app (or C_vol) for Picard matrix stability
        ! ----------------------------------------------------------------------
        call workspace%compute_K1(workspace%work_C, workspace%work_matrix)
        if (associated(K_TT_val)) then
            K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) = &
                K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) + &
                bdf0 * workspace%work_matrix(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes)
        end if

        ! ----------------------------------------------------------------------
        ! 3. Diffusion matrix (LHS) and diffusion flux (partial RHS) construction
        ! ----------------------------------------------------------------------
        call workspace%compute_K2(workspace%work_D, workspace%work_matrix)

        ! Add to K_TT matrix
        if (associated(K_TT_val)) then
            K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) = &
                K_TT_val(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes) + &
                workspace%work_matrix(1:workspace%num_fe_nodes, 1:workspace%num_fe_nodes)
        end if

        ! Current diffusion flux is computed as K*T
        ! (consistent since D(T)*grad(T) matches the matrix-vector product)
        call matvec(workspace%work_matrix, workspace%T_node, local_vec_diff_flux, ierr)

        ! ----------------------------------------------------------------------
        ! 4. Residual vector construction
        !    R = F_ext - ( dH/dt + K*T )
        ! ----------------------------------------------------------------------
        if (associated(F_T_val)) then
            ! Integrate enthalpy term (dH/dt) into local_vec_transient
            call workspace%compute_R1(workspace%work_d_dt, local_vec_transient)

            do i = 1, workspace%num_fe_nodes
                ! Residual = -(transient term + diffusion term)

                val_T = -local_vec_transient(i)
                val_T = val_T - local_vec_diff_flux(i)

                F_T_val(i) = F_T_val(i) + val_T
            end do
        end if

        if (allocated(local_vec_transient)) deallocate (local_vec_transient)
        if (allocated(local_vec_diff_flux)) deallocate (local_vec_diff_flux)

    end subroutine assemble_local_picard_thermal

end submodule thermal_matrix
