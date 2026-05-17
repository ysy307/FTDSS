submodule(physics_governing_thermal) thermal_matrix
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
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

        call self%assemble_local_picard(control, workspace, K_TT, K_TH, F_T)

    end subroutine assemble_local_thermal

    !> @brief Assemble Picard local components (backward Euler, no BDF history)
    module subroutine assemble_local_picard_thermal(self, control, workspace, K_TT, K_TH, F_T)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_control), intent(in) :: control
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: K_TT
        type(type_matrix_dense), intent(inout), optional :: K_TH
        type(type_vector_dp), intent(inout), optional :: F_T

        integer(int32) :: i, j
        integer(int32) :: ierr
        real(real64) :: val_T, bdf0, C_TH_gp
        real(real64), pointer :: K_TT_val(:, :)
        real(real64), pointer :: K_TH_val(:, :)
        real(real64), pointer :: F_T_val(:)

        real(real64), allocatable :: local_vec_diff_flux(:)
        real(real64), allocatable :: local_vec_adv_flux(:)
        real(real64), allocatable :: work_C_TH(:)
        real(real64), allocatable :: work_Q_seg(:)
        real(real64), allocatable :: C_TH_nodes(:)
        real(real64) :: S_seg, Lf, rho_w, grad_T_mag
        type(type_coordinate_dp), pointer :: grad_T_ptr
        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: gp_coords_th
        integer(int32) :: n_nodes
        logical :: has_advection

        n_nodes = workspace%num_fe_nodes
        allocate (local_vec_diff_flux(n_nodes))
        allocate (local_vec_adv_flux(n_nodes))

        workspace%work_C(:) = 0.0d0
        workspace%work_D(:, :, :) = 0.0d0
        workspace%work_d_dt(:) = 0.0d0
        workspace%work_V(:, :) = 0.0d0

        local_vec_diff_flux(:) = 0.0d0
        local_vec_adv_flux(:) = 0.0d0

        has_advection = .false.

        bdf0 = workspace%bdf_coeffs(1)

        nullify (K_TT_val)
        nullify (K_TH_val)
        nullify (F_T_val)
        nullify (grad_T_ptr)
        nullify (gp_coords_th)

        if (present(K_TT)) K_TT_val => K_TT%get_val()
        if (present(K_TH)) K_TH_val => K_TH%get_val()
        if (present(F_T)) F_T_val => F_T%get_data()

        if (present(K_TH)) then
            allocate (work_C_TH(workspace%num_fe_gauss))
            allocate (C_TH_nodes(n_nodes))
            work_C_TH(:) = 0.0d0
            ! Lerp C_TH from node states to capture non-zero coupling at freezing-front elements
            call workspace%fe%get_gauss(gp_coords_th)
            do i = 1, n_nodes
                call self%compute_coupling_mass_term(workspace%material_id, workspace%state(i), C_TH_nodes(i))
            end do
        end if

        allocate (work_Q_seg(workspace%num_fe_gauss))
        work_Q_seg(:) = 0.0d0

        ! Gauss Loop
        do i = 1, workspace%num_fe_gauss
            call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), workspace%work_C(i))

            call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), workspace%work_D(:, :, i))

            call self%compute_advective_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%work_V(:, i))
            if (any(abs(workspace%work_V(:, i)) > 1.0d-30)) has_advection = .true.

            call self%compute_transient_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%bdf_coeffs, workspace%work_d_dt(i))

            if (present(K_TH)) then
                call workspace%fe%lerp(gp_coords_th(i), C_TH_nodes(1:n_nodes), C_TH_gp)
                work_C_TH(i) = C_TH_gp
            end if

            ! Segregation latent heat source term
            nullify (grad_T_ptr)
            call workspace%state_gp(i)%grad_T%get(grad_T_ptr)
            if (associated(grad_T_ptr)) then
                grad_T_mag = sqrt(grad_T_ptr%x**2 + grad_T_ptr%y**2 + grad_T_ptr%z**2)
                if (grad_T_mag > 0.0d0) then
                    S_seg = 0.0d0
                    call self%physics%calc_segregation_sink( &
                        workspace%material_id, workspace%state_gp(i), grad_T_mag, S_seg)
                    if (S_seg > 0.0d0) then
                        Lf = 0.0d0
                        rho_w = 0.0d0
                        call self%physics%calc_latent_heat_fusion( &
                            workspace%material_id, workspace%state_gp(i), Lf)
                        call self%calc_density_water(workspace%state_gp(i), rho_w)
                        work_Q_seg(i) = Lf * rho_w * S_seg
                    end if
                end if
            end if
        end do

        ! Mass matrix (LHS, factor bdf0)
        call workspace%compute_K1(workspace%work_C, workspace%work_matrix)
        if (associated(K_TT_val)) then
            K_TT_val(1:n_nodes, 1:n_nodes) = &
                K_TT_val(1:n_nodes, 1:n_nodes) + &
                bdf0 * workspace%work_matrix(1:n_nodes, 1:n_nodes)
        end if

        ! Diffusion matrix (LHS, factor 1.0) + diffusion flux for F_T
        call workspace%compute_K2(workspace%work_D, workspace%work_matrix)
        if (associated(K_TT_val)) then
            K_TT_val(1:n_nodes, 1:n_nodes) = &
                K_TT_val(1:n_nodes, 1:n_nodes) + &
                workspace%work_matrix(1:n_nodes, 1:n_nodes)
        end if
        call matvec(workspace%work_matrix, workspace%T_node, local_vec_diff_flux, ierr)

        ! Advection matrix (LHS, factor 1.0) + advection flux for F_T
        if (has_advection) then
            call workspace%compute_K3(workspace%work_V, workspace%work_matrix)
            if (associated(K_TT_val)) then
                K_TT_val(1:n_nodes, 1:n_nodes) = &
                    K_TT_val(1:n_nodes, 1:n_nodes) + &
                    workspace%work_matrix(1:n_nodes, 1:n_nodes)
            end if
            call matvec(workspace%work_matrix, workspace%T_node, local_vec_adv_flux, ierr)
        end if

        ! Coupling K_TH (K1, factor bdf0, lerped C_TH)
        if (associated(K_TH_val)) then
            call workspace%compute_K1(work_C_TH, workspace%work_matrix)
            K_TH_val(1:n_nodes, 1:n_nodes) = &
                K_TH_val(1:n_nodes, 1:n_nodes) + &
                bdf0 * workspace%work_matrix(1:n_nodes, 1:n_nodes)
        end if

        ! Residual vector
        if (associated(F_T_val)) then
            workspace%work_vec(:) = 0.0d0
            call workspace%compute_R1(work_Q_seg, workspace%work_vec)

            do i = 1, n_nodes
                val_T = -local_vec_diff_flux(i) - local_vec_adv_flux(i) + workspace%work_vec(i)
                F_T_val(i) = F_T_val(i) + val_T
            end do

            workspace%work_vec(:) = 0.0d0
            call workspace%compute_R1(workspace%work_d_dt, workspace%work_vec)
            do i = 1, n_nodes
                F_T_val(i) = F_T_val(i) - workspace%work_vec(i)
            end do
        end if

        if (allocated(local_vec_diff_flux)) deallocate (local_vec_diff_flux)
        if (allocated(local_vec_adv_flux)) deallocate (local_vec_adv_flux)
        if (allocated(work_C_TH)) deallocate (work_C_TH)
        if (allocated(work_Q_seg)) deallocate (work_Q_seg)
        if (allocated(C_TH_nodes)) deallocate (C_TH_nodes)

    end subroutine assemble_local_picard_thermal

end submodule thermal_matrix
