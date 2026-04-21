!> Monolithic Newton-Raphson element assembly for coupled P-T system.
!>
!> Implements a 2x2 block Jacobian (per node pair) for the simultaneous
!> solution of the moisture conservation and energy conservation equations
!> in unsaturated frozen soil.
!>
!> Mathematical formulation:
!> - Mixed-form: capacity terms use serialized-path secant method
!> - SUPG stabilization applied to the thermal advection term only
!> - Lumped mass matrix for capacity blocks (diagonal only)
!> - Column scaling (P_ref) + row equilibration applied before return
module physics_governing_monolithic
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_constitutive, only:g => gravity_acceleration
    use :: module_core
    use :: physics_governing_base, only:type_assemble_workspace
    use :: physics_governing_thermal, only:type_thermal, SCHEME_TANGENT
    use :: physics_governing_hydraulic, only:type_hydraulic
    implicit none
    private

    public :: assemble_element_monolithic
    public :: check_convergence_monolithic

contains

    !> Assemble element Jacobian blocks and residual vectors for monolithic P-T Newton-Raphson.
    !>
    !> Mathematical definition:
    !> - Mass: serialized-path secant with nodal evaluation of Theta and H
    !> - Stiffness: standard Galerkin for P, SUPG for T advection
    !>
    !> Assumptions:
    !> - workspace%lerp() has been called; state_gp and P_node/T_node are current
    !> - BDF history index 2 holds values at t_n
    !>
    !> Numerical guarantee:
    !> - Min cutoffs on D_HH diagonals and D_TT diagonals prevent zero conductivity
    !> - Serialized secant path guarantees conservation-consistent capacity terms
    !>
    !> Computational complexity:
    !> - O(n_gauss * n_nodes^2) per element
    !>
    !> Failure behavior:
    !> - Falls back to analytical derivatives when |delta_P| or |delta_T| < eps_tol
    subroutine assemble_element_monolithic(hydraulic, thermal, material_id, bdf_coeffs, &
                                           workspace, J_PP, J_PT, J_TP, J_TT, R_P, R_T)
        implicit none
        class(type_hydraulic), intent(in) :: hydraulic
        class(type_thermal), intent(in) :: thermal
        integer(int32), intent(in) :: material_id
        real(real64), intent(in) :: bdf_coeffs(:)
        type(type_assemble_workspace), intent(inout) :: workspace
        real(real64), intent(inout) :: J_PP(:, :)
        real(real64), intent(inout) :: J_PT(:, :)
        real(real64), intent(inout) :: J_TP(:, :)
        real(real64), intent(inout) :: J_TT(:, :)
        real(real64), intent(inout) :: R_P(:)
        real(real64), intent(inout) :: R_T(:)

        integer(int32) :: n_nodes, n_gauss, n_dim, gp, i, j, d
        real(real64) :: bdf0, time_step, wJ, detJ, measure, h_e

        real(real64), parameter :: K_min = 1.0d-20
        real(real64), parameter :: lambda_min = 1.0d-10
        real(real64), parameter :: eps_tol = 1.0d-12
        real(real64), parameter :: P_ref = 1.0d5
        real(real64), parameter :: S_s = 1.0d-8
        real(real64), parameter :: q_e_thr = 1.0d-30

        type(type_coordinate_dp), pointer, contiguous :: gauss_pts(:)
        real(real64), pointer, contiguous :: weights(:)
        real(real64), pointer, contiguous :: ptr_ph(:), ptr_th(:), ptr_phi_h(:)

        ! Nodal scalar arrays (size n_nodes)
        real(real64), allocatable :: Pk(:), Tk(:), Pn(:), Tn(:)
        real(real64), allocatable :: Theta_k(:), Theta_n(:), Theta_nTk(:)
        real(real64), allocatable :: H_k(:), H_n(:), H_nTk(:)
        real(real64), allocatable :: C_eq_n(:), C_T_n(:), H_P_n(:), H_T_n(:)

        ! GP-level tensors
        real(real64), allocatable :: D_HH(:, :), D_HT(:, :), D_TT(:, :)
        real(real64), allocatable :: q_e(:), grad_P(:), grad_T(:)

        ! Row scaling vectors
        real(real64), allocatable :: row_max_P(:), row_max_T(:)

        real(real64) :: K_lP_raw, K_vP_raw, K_lT_raw, K_vT_raw
        real(real64) :: rho_w_gp, c_liq, c_vap
        real(real64) :: Cl_heat, Cv_heat, coeff_advP, coeff_advT
        real(real64) :: C_eq_gp, C_T_gp, H_P_gp, H_T_gp
        real(real64) :: dTheta_gp, dH_gp
        real(real64) :: q_e_norm, lambda_sc, Pe_elem, tau, Wi
        real(real64) :: delta_P, delta_T, Qw, Qi, Qv, rho_w_loc, rho_i_loc
        real(real64) :: dQw_dP, dQi_dP, dQv_dP, dQw_dT, dQi_dT, dQv_dT
        real(real64) :: phi_i, phi0_i

        type(type_state) :: st_k, st_nTk, st_n

        nullify (gauss_pts, weights, ptr_ph, ptr_th, ptr_phi_h)

        n_nodes = workspace%num_fe_nodes
        n_gauss = workspace%num_fe_gauss
        n_dim = workspace%num_fe_dimension
        bdf0 = bdf_coeffs(1)
        time_step = 1.0d0 / bdf0

        allocate (Pk(n_nodes), Tk(n_nodes), Pn(n_nodes), Tn(n_nodes))
        allocate (Theta_k(n_nodes), Theta_n(n_nodes), Theta_nTk(n_nodes))
        allocate (H_k(n_nodes), H_n(n_nodes), H_nTk(n_nodes))
        allocate (C_eq_n(n_nodes), C_T_n(n_nodes), H_P_n(n_nodes), H_T_n(n_nodes))
        allocate (D_HH(n_dim, n_dim), D_HT(n_dim, n_dim), D_TT(n_dim, n_dim))
        allocate (q_e(n_dim), grad_P(n_dim), grad_T(n_dim))
        allocate (row_max_P(n_nodes), row_max_T(n_nodes))

        J_PP = 0.0d0; J_PT = 0.0d0; J_TP = 0.0d0; J_TT = 0.0d0
        R_P = 0.0d0; R_T = 0.0d0

        ! ---- Extract current iterate and previous time-step nodal values ----
        do i = 1, n_nodes
            Pk(i) = workspace%P_node(i)
            Tk(i) = workspace%T_node(i)
            call workspace%state(i)%pressure_history%get(ptr_ph)
            Pn(i) = ptr_ph(2)
            nullify (ptr_ph)
            call workspace%state(i)%temperature_history%get(ptr_th)
            Tn(i) = ptr_th(2)
            nullify (ptr_th)
        end do

        ! ---- Nodal serialized-path secant evaluation ----
        do i = 1, n_nodes
            call workspace%state(i)%porosity%get(phi_i)
            call workspace%state(i)%porosity_history%get(ptr_phi_h)
            if (associated(ptr_phi_h) .and. size(ptr_phi_h) >= 2) then
                phi0_i = ptr_phi_h(2)
            else
                phi0_i = phi_i
            end if
            nullify (ptr_phi_h)

            ! State (Pk, Tk)
            call st_k%reset()
            call st_k%temperature%set(Tk(i))
            call st_k%pressure%set(Pk(i))
            call st_k%porosity%set(phi_i)
            call hydraulic%update_water_phases(material_id, st_k)
            call st_k%water_content%get(Qw)
            call st_k%ice_content%get(Qi)
            call st_k%vapor_content%get(Qv)
            call thermal%calc_density_water(st_k, rho_w_loc)
            call thermal%calc_density_ice(st_k, rho_i_loc)
            Theta_k(i) = Qw + (rho_i_loc / rho_w_loc) * Qi + Qv
            call thermal%calc_enthalpy_density(material_id, st_k, H_k(i))

            ! Intermediate state (Pn, Tk) - serialized path for P direction
            call st_nTk%reset()
            call st_nTk%temperature%set(Tk(i))
            call st_nTk%pressure%set(Pn(i))
            call st_nTk%porosity%set(phi_i)
            call hydraulic%update_water_phases(material_id, st_nTk)
            call st_nTk%water_content%get(Qw)
            call st_nTk%ice_content%get(Qi)
            call st_nTk%vapor_content%get(Qv)
            call thermal%calc_density_water(st_nTk, rho_w_loc)
            call thermal%calc_density_ice(st_nTk, rho_i_loc)
            Theta_nTk(i) = Qw + (rho_i_loc / rho_w_loc) * Qi + Qv
            call thermal%calc_enthalpy_density(material_id, st_nTk, H_nTk(i))

            ! Previous-step state (Pn, Tn)
            call st_n%reset()
            call st_n%temperature%set(Tn(i))
            call st_n%pressure%set(Pn(i))
            call st_n%porosity%set(phi_i)
            call hydraulic%update_water_phases(material_id, st_n)
            call st_n%water_content%get(Qw)
            call st_n%ice_content%get(Qi)
            call st_n%vapor_content%get(Qv)
            call thermal%calc_density_water(st_n, rho_w_loc)
            call thermal%calc_density_ice(st_n, rho_i_loc)
            Theta_n(i) = Qw + (rho_i_loc / rho_w_loc) * Qi + Qv
            call thermal%calc_enthalpy_density(material_id, st_n, H_n(i))

            ! Secant coefficients
            delta_P = Pk(i) - Pn(i)
            delta_T = Tk(i) - Tn(i)

            if (abs(delta_P) > eps_tol) then
                C_eq_n(i) = (Theta_k(i) - Theta_nTk(i)) / delta_P + S_s * Theta_k(i) / max(phi0_i, 1.0d-12)
                H_P_n(i) = (H_k(i) - H_nTk(i)) / delta_P
            else
                ! Analytical fallback from state_k derivatives
                call thermal%calc_density_water(st_k, rho_w_loc)
                call thermal%calc_density_ice(st_k, rho_i_loc)
                call st_k%dQw_dP%get(dQw_dP)
                call st_k%dQi_dP%get(dQi_dP)
                call st_k%dQv_dP%get(dQv_dP)
                C_eq_n(i) = dQw_dP + (rho_i_loc / rho_w_loc) * dQi_dP + dQv_dP + &
                            S_s * Theta_k(i) / max(phi0_i, 1.0d-12)
                call thermal%compute_coupling_mass_term(material_id, st_k, H_P_n(i))
            end if

            if (abs(delta_T) > eps_tol) then
                C_T_n(i) = (Theta_nTk(i) - Theta_n(i)) / delta_T
                H_T_n(i) = (H_nTk(i) - H_n(i)) / delta_T
            else
                ! Analytical fallback (tangent scheme)
                call thermal%calc_density_water(st_k, rho_w_loc)
                call thermal%calc_density_ice(st_k, rho_i_loc)
                call st_k%dQw_dT%get(dQw_dT)
                call st_k%dQi_dT%get(dQi_dT)
                call st_k%dQv_dT%get(dQv_dT)
                C_T_n(i) = dQw_dT + (rho_i_loc / rho_w_loc) * dQi_dT + dQv_dT
                call thermal%compute_mass_term(material_id, st_k, H_T_n(i), scheme_opt=SCHEME_TANGENT)
            end if
        end do

        ! ---- Element size for SUPG ----
        measure = 0.0d0
        call workspace%fe%calc_measure(workspace%coordinates, measure)
        h_e = measure**(1.0d0 / real(n_dim, real64))

        ! ---- Gauss integration loop ----
        call workspace%fe%get_gauss(gauss_pts)
        call workspace%fe%get_weight(weights)

        do gp = 1, n_gauss
            workspace%work_psi(:) = 0.0d0
            workspace%work_dpsi_dx(:, :) = 0.0d0
            detJ = 0.0d0
            call workspace%fe%calc_shape_function(gauss_pts(gp), workspace%coordinates, &
                                                  psi=workspace%work_psi, &
                                                  dpsi_dx=workspace%work_dpsi_dx, &
                                                  determinant_jacobian=detJ)
            wJ = weights(gp) * abs(detJ)

            ! Interpolate nodal secant coefficients to GP
            C_eq_gp = dot_product(workspace%work_psi(1:n_nodes), C_eq_n)
            C_T_gp = dot_product(workspace%work_psi(1:n_nodes), C_T_n)
            H_P_gp = dot_product(workspace%work_psi(1:n_nodes), H_P_n)
            H_T_gp = dot_product(workspace%work_psi(1:n_nodes), H_T_n)
            dTheta_gp = dot_product(workspace%work_psi(1:n_nodes), Theta_k - Theta_n)
            dH_gp = dot_product(workspace%work_psi(1:n_nodes), H_k - H_n)

            ! Current gradients at GP
            grad_P = matmul(workspace%work_dpsi_dx, workspace%P_node(1:n_nodes))
            grad_T = matmul(workspace%work_dpsi_dx, workspace%T_node(1:n_nodes))

            ! GP-level material properties
            D_HH = 0.0d0; D_HT = 0.0d0; D_TT = 0.0d0
            call hydraulic%compute_diffusion_term(material_id, workspace%state_gp(gp), D_HH)
            call hydraulic%compute_coupling_diffusion_term(material_id, workspace%state_gp(gp), D_HT)
            call thermal%compute_diffusion_term(material_id, workspace%state_gp(gp), D_TT)

            call hydraulic%calc_K_wP(material_id, workspace%state_gp(gp), K_lP_raw)
            call hydraulic%calc_K_vP(material_id, workspace%state_gp(gp), K_vP_raw)
            call hydraulic%calc_K_wT(material_id, workspace%state_gp(gp), K_lT_raw)
            call hydraulic%calc_K_vT(material_id, workspace%state_gp(gp), K_vT_raw)

            call thermal%calc_density_water(workspace%state_gp(gp), rho_w_gp)
            call thermal%calc_specific_heat_water(workspace%state_gp(gp), c_liq)
            call thermal%calc_specific_heat_vapor(workspace%state_gp(gp), c_vap)
            Cl_heat = rho_w_gp * c_liq
            Cv_heat = rho_w_gp * c_vap

            ! Apply physical lower bounds
            do d = 1, n_dim
                D_HH(d, d) = max(D_HH(d, d), K_min)
                D_TT(d, d) = max(D_TT(d, d), lambda_min)
            end do

            ! SUPG advective heat flux vector
            ! K_lP_raw = K_flh [m/s], divide by g to match D_HH units [s]
            coeff_advP = Cl_heat * K_lP_raw / g + Cv_heat * K_vP_raw / g
            coeff_advT = Cl_heat * K_lT_raw + Cv_heat * K_vT_raw
            q_e = -coeff_advP * grad_P - coeff_advT * grad_T

            q_e_norm = sqrt(dot_product(q_e, q_e))
            if (q_e_norm > q_e_thr) then
                lambda_sc = max(D_TT(1, 1), lambda_min)
                Pe_elem = q_e_norm * h_e / (2.0d0 * lambda_sc)
                tau = (h_e / (2.0d0 * q_e_norm)) * max(0.0d0, 1.0d0 - 1.0d0 / Pe_elem)
            else
                tau = 0.0d0
            end if

            ! Node loop: residuals and Jacobian
            do i = 1, n_nodes
                Wi = workspace%work_psi(i) + tau * dot_product(q_e, workspace%work_dpsi_dx(:, i))

                ! Residuals
                R_P(i) = R_P(i) + wJ * ( &
                         workspace%work_psi(i) * dTheta_gp / time_step + &
                         dot_product(workspace%work_dpsi_dx(:, i), &
                                     matmul(D_HH, grad_P) + matmul(D_HT, grad_T)))

                ! Segregation sink contribution to pressure residual.
                ! Pass the current time step so the clamped S_seg matches
                ! the Qi_seg forward-Euler update (total-water conservation).
                block
                    real(real64) :: S_seg_mono
                    S_seg_mono = 0.0d0
                    call hydraulic%calc_segregation_sink(material_id, workspace%state_gp(gp), time_step, S_seg_mono)
                    R_P(i) = R_P(i) + wJ * workspace%work_psi(i) * S_seg_mono
                end block

                R_T(i) = R_T(i) + wJ * ( &
                         workspace%work_psi(i) * dH_gp / time_step + &
                         dot_product(workspace%work_dpsi_dx(:, i), matmul(D_TT, grad_T)) + &
                         Wi * dot_product(q_e, grad_T))

                ! Lumped mass (diagonal only)
                J_PP(i, i) = J_PP(i, i) + wJ * workspace%work_psi(i) * C_eq_gp * bdf0
                J_PT(i, i) = J_PT(i, i) + wJ * workspace%work_psi(i) * C_T_gp * bdf0
                J_TP(i, i) = J_TP(i, i) + wJ * workspace%work_psi(i) * H_P_gp * bdf0
                J_TT(i, i) = J_TT(i, i) + wJ * workspace%work_psi(i) * H_T_gp * bdf0

                ! Stiffness (all node pairs)
                do j = 1, n_nodes
                    J_PP(i, j) = J_PP(i, j) + wJ * &
                                 dot_product(workspace%work_dpsi_dx(:, i), &
                                             matmul(D_HH, workspace%work_dpsi_dx(:, j)))

                    J_PT(i, j) = J_PT(i, j) + wJ * &
                                 dot_product(workspace%work_dpsi_dx(:, i), &
                                             matmul(D_HT, workspace%work_dpsi_dx(:, j)))

                    J_TP(i, j) = J_TP(i, j) - wJ * Wi * coeff_advP * &
                                 dot_product(workspace%work_dpsi_dx(:, j), grad_T)

                    J_TT(i, j) = J_TT(i, j) + wJ * ( &
                                 dot_product(workspace%work_dpsi_dx(:, i), &
                                             matmul(D_TT, workspace%work_dpsi_dx(:, j))) + &
                                 Wi * (dot_product(q_e, workspace%work_dpsi_dx(:, j)) &
                                       - coeff_advT * dot_product(workspace%work_dpsi_dx(:, j), grad_T)))
                end do
            end do
        end do

        ! ---- Column scaling: P columns / P_ref, T columns unchanged (T_ref=1) ----
        J_PP = J_PP / P_ref
        J_TP = J_TP / P_ref

        ! ---- Row equilibration ----
        do i = 1, n_nodes
            row_max_P(i) = max(maxval(abs(J_PP(i, :))), maxval(abs(J_PT(i, :))), tiny(1.0d0))
            J_PP(i, :) = J_PP(i, :) / row_max_P(i)
            J_PT(i, :) = J_PT(i, :) / row_max_P(i)
            R_P(i) = R_P(i) / row_max_P(i)
        end do
        do i = 1, n_nodes
            row_max_T(i) = max(maxval(abs(J_TP(i, :))), maxval(abs(J_TT(i, :))), tiny(1.0d0))
            J_TP(i, :) = J_TP(i, :) / row_max_T(i)
            J_TT(i, :) = J_TT(i, :) / row_max_T(i)
            R_T(i) = R_T(i) / row_max_T(i)
        end do

        nullify (gauss_pts, weights)
        deallocate (Pk, Tk, Pn, Tn)
        deallocate (Theta_k, Theta_n, Theta_nTk, H_k, H_n, H_nTk)
        deallocate (C_eq_n, C_T_n, H_P_n, H_T_n)
        deallocate (D_HH, D_HT, D_TT, q_e, grad_P, grad_T)
        deallocate (row_max_P, row_max_T)

    end subroutine assemble_element_monolithic

    !> Mixed convergence check for monolithic P-T solve.
    !>
    !> Mathematical definition:
    !> - Hydraulic converged: max|theta_residual| < eps_theta
    !> - Thermal converged: max|dT| < eps_abs_T
    !> - dP is monitor-only and not used in convergence condition
    subroutine check_convergence_monolithic(theta_residual, dP_monitor, dT, eps_theta, eps_abs_T, converged)
        implicit none
        real(real64), intent(in) :: theta_residual(:)
        real(real64), intent(in) :: dP_monitor(:)
        real(real64), intent(in) :: dT(:)
        real(real64), intent(in) :: eps_theta
        real(real64), intent(in) :: eps_abs_T
        logical, intent(inout) :: converged
        real(real64) :: dP_max

        dP_max = 0.0d0
        if (size(dP_monitor) > 0) dP_max = maxval(abs(dP_monitor))

        converged = (maxval(abs(theta_residual)) < eps_theta) .and. (maxval(abs(dT)) < eps_abs_T)

    end subroutine check_convergence_monolithic

end module physics_governing_monolithic
