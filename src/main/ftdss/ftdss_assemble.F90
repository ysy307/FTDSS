submodule(main_ftdss) ftdss_assemble
    implicit none

contains

    !> Perform the global assembly for the FTDSS solver.
    module subroutine assemble_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        type(type_matrix_dense) :: local_J_TT, local_J_TH, local_J_HH, local_J_HT
        type(type_vector_dp) :: local_R_T, local_R_H

        integer(int32) :: num_elements
        integer(int32), pointer, contiguous, dimension(:) :: p_connectivity
        integer(int32) :: i
        integer(int32) :: thermal_dof, hydraulic_dof

        call self%controls%profiler%start("Assemble")

        call self%J%zero()
        call self%R%zero()

        num_elements = self%domain%get_num_elements()

        do i = 1, num_elements
            call self%assemble_local(i, local_J_TT, local_J_TH, local_J_HH, local_J_HT, &
                                     local_R_T, local_R_H)

            call self%domain%get_element_connectivity(i, p_connectivity)
            call self%domain%get_target_dof(PHYSICS_TYPE_THERMAL, thermal_dof)
            call self%domain%get_target_dof(PHYSICS_TYPE_HYDRAULIC, hydraulic_dof)

            call self%J%add(thermal_dof, thermal_dof, p_connectivity, local_J_TT)
            ! call self%J%add(thermal_dof, hydraulic_dof, p_connectivity, local_J_TH)
            ! call self%J%add(hydraulic_dof, hydraulic_dof, p_connectivity, local_J_HH)
            ! call self%J%add(hydraulic_dof, thermal_dof, p_connectivity, local_J_HT)

            call self%R%add(thermal_dof, p_connectivity, local_R_T)
            ! call self%R%add(hydraulic_dof, p_connectivity, local_R_H)
        end do

        call local_J_TT%destroy()
        call local_J_TH%destroy()
        call local_J_HH%destroy()
        call local_J_HT%destroy()
        call local_R_T%destroy()
        call local_R_H%destroy()

        call self%controls%profiler%stop("Assemble")

    end subroutine assemble_ftdss

    !> Compute local matrices and residual vectors for a specific element.
    module subroutine assemble_local_ftdss(self, element_id, local_J_TT, local_J_TH, &
                                           local_J_HH, local_J_HT, local_R_T, local_R_H)
        implicit none
        class(type_ftdss), intent(inout) :: self
        integer(int32), intent(in) :: element_id
        type(type_matrix_dense), intent(inout), optional :: local_J_TT, local_J_TH, local_J_HH, local_J_HT
        type(type_vector_dp), intent(inout), optional :: local_R_T, local_R_H

        class(abst_fe), pointer :: fe
        integer(int32) :: num_nodes, num_gauss, material_id, dim
        integer(int32), pointer, contiguous, dimension(:) :: connectivity
        real(real64), pointer, contiguous, dimension(:) :: weights
        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: gauss_points
        real(real64), allocatable :: coordinates(:, :)

        !!------
        type(type_assemble_workspace) :: workspace
        integer(int32) :: i
        !!------

        ! Physical Coefficients
        real(real64), allocatable :: C_TT(:), C_TH(:), C_HH(:), C_HT(:)
        real(real64), allocatable :: M_TT(:, :, :), M_TH(:, :, :), M_HH(:, :, :), M_HT(:, :, :)
        real(real64), allocatable :: V_TT(:, :), V_TH(:, :), V_HH(:, :), V_HT(:, :)
        real(real64), allocatable :: R_T_C(:), R_T_D(:, :), R_H_C(:), R_H_D(:, :)

        ! Workspace
        real(real64), allocatable :: local_J(:, :)
        real(real64), allocatable :: local_R(:)

        ! BDF Scaling
        real(real64), pointer, dimension(:) :: time_coef => null()
        real(real64) :: bdf_coeff

        ! --- Get Element Info ---
        call self%domain%get_material_id(element_id, material_id)
        call self%domain%get_element(element_id, fe)
        call self%domain%get_element_connectivity(element_id, connectivity)
        call self%domain%get_element_coordinate(element_id, coordinates)

        !!------
        call workspace%initialize(fe, material_id, element_id, self%controls)
        do i = 1, size(connectivity)
            call self%set_state(connectivity(i), element_id, workspace%state(i))
        end do
        call workspace%lerp()
        do i = 1, workspace%num_fe_gauss
            call self%thermal%update_water_phases(material_id, workspace%state_gp(i))
        end do
        !!------

        dim = self%domain%get_computation_dimension()
        call fe%get_num_nodes(num_nodes)

        ! --- Initialize Matrices/Vectors ---
        if (present(local_J_TT)) call local_J_TT%initialize(num_nodes)
        if (present(local_J_TH)) call local_J_TH%initialize(num_nodes)
        if (present(local_J_HH)) call local_J_HH%initialize(num_nodes)
        if (present(local_J_HT)) call local_J_HT%initialize(num_nodes)
        if (present(local_R_T)) call local_R_T%initialize(num_nodes)
        if (present(local_R_H)) call local_R_H%initialize(num_nodes)

        if (present(local_J_TT)) call local_J_TT%zero()
        if (present(local_J_TH)) call local_J_TH%zero()
        if (present(local_J_HH)) call local_J_HH%zero()
        if (present(local_J_HT)) call local_J_HT%zero()
        if (present(local_R_T)) call local_R_T%zero()
        if (present(local_R_H)) call local_R_H%zero()

        ! --- Allocate Workspace ---
        allocate (local_J(num_nodes, num_nodes))
        allocate (local_R(num_nodes))

        call allocate_coefficient_arrays(dim, num_nodes, &
                                         C_TT, C_TH, C_HH, C_HT, &
                                         M_TT, M_TH, M_HH, M_HT, &
                                         V_TT, V_TH, V_HH, V_HT, &
                                         R_T_C, R_T_D, R_H_C, R_H_D)

        ! --- Compute Coefficients ---
        call compute_nodal_coefficients(self, element_id, material_id, num_nodes, connectivity, &
                                        C_TT, C_TH, C_HH, C_HT, &
                                        M_TT, M_TH, M_HH, M_HT, &
                                        V_TT, V_TH, V_HH, V_HT, &
                                        R_T_C, R_T_D, R_H_C, R_H_D)

        ! --- Get BDF Coefficient ---
        call self%controls%time%get_bdf_coeffs(time_coef)
        if (associated(time_coef)) then
            bdf_coeff = time_coef(1)
        else
            bdf_coeff = 0.0d0
        end if

        ! --- Apply Scaling ---
        if (abs(bdf_coeff) > epsilon(0.0d0)) then
            C_TT = C_TT * bdf_coeff
            C_TH = C_TH * bdf_coeff
            C_HH = C_HH * bdf_coeff
            C_HT = C_HT * bdf_coeff
        end if

        ! [符号設定]
        ! R_T_C (Mass): 正(+)のまま使用。
        ! R_T_D (Flux): 負の流束(-q)なので、-1倍して正の剛性(+K)にする。
        R_T_D = R_T_D * (-1.0d0)
        R_H_D = R_H_D * (-1.0d0)

        ! --- Assembly ---
        call assemble_matrices(fe, coordinates, dim, local_J, local_R, &
                               C_TT, C_TH, C_HH, C_HT, &
                               M_TT, M_TH, M_HH, M_HT, &
                               V_TT, V_TH, V_HH, V_HT, &
                               R_T_C, R_T_D, R_H_C, R_H_D, &
                               local_J_TT, local_J_TH, local_J_HH, local_J_HT, &
                               local_R_T, local_R_H)

    end subroutine assemble_local_ftdss

    ! ==========================================================================
    ! Subroutines for Allocation & Computation (No changes)
    ! ==========================================================================
    subroutine allocate_coefficient_arrays(dim, num_nodes, &
                                           C_TT, C_TH, C_HH, C_HT, &
                                           M_TT, M_TH, M_HH, M_HT, &
                                           V_TT, V_TH, V_HH, V_HT, &
                                           R_T_C, R_T_D, R_H_C, R_H_D)
        implicit none
        integer(int32), intent(in) :: dim, num_nodes
        real(real64), allocatable, intent(inout) :: C_TT(:), C_TH(:), C_HH(:), C_HT(:)
        real(real64), allocatable, intent(inout) :: M_TT(:, :, :), M_TH(:, :, :), M_HH(:, :, :), M_HT(:, :, :)
        real(real64), allocatable, intent(inout) :: V_TT(:, :), V_TH(:, :), V_HH(:, :), V_HT(:, :)
        real(real64), allocatable, intent(inout) :: R_T_C(:), R_T_D(:, :), R_H_C(:), R_H_D(:, :)

        call allocate_array(C_TT, num_nodes)
        call allocate_array(C_TH, num_nodes)
        call allocate_array(C_HH, num_nodes)
        call allocate_array(C_HT, num_nodes)

        call allocate_array(M_TT, dim, dim, num_nodes)
        call allocate_array(M_TH, dim, dim, num_nodes)
        call allocate_array(M_HH, dim, dim, num_nodes)
        call allocate_array(M_HT, dim, dim, num_nodes)

        call allocate_array(V_TT, dim, num_nodes)
        call allocate_array(V_TH, dim, num_nodes)
        call allocate_array(V_HH, dim, num_nodes)
        call allocate_array(V_HT, dim, num_nodes)

        call allocate_array(R_T_C, num_nodes)
        call allocate_array(R_T_D, dim, num_nodes)
        call allocate_array(R_H_C, num_nodes)
        call allocate_array(R_H_D, dim, num_nodes)
    end subroutine allocate_coefficient_arrays

    subroutine compute_nodal_coefficients(self, element_id, material_id, num_nodes, connectivity, &
                                          C_TT, C_TH, C_HH, C_HT, &
                                          M_TT, M_TH, M_HH, M_HT, &
                                          V_TT, V_TH, V_HH, V_HT, &
                                          R_T_C, R_T_D, R_H_C, R_H_D)
        implicit none
        class(type_ftdss), intent(inout) :: self
        integer(int32), intent(in) :: element_id, material_id, num_nodes
        integer(int32), intent(in) :: connectivity(:)
        real(real64), intent(inout) :: C_TT(:), C_TH(:), C_HH(:), C_HT(:)
        real(real64), intent(inout) :: M_TT(:, :, :), M_TH(:, :, :), M_HH(:, :, :), M_HT(:, :, :)
        real(real64), intent(inout) :: V_TT(:, :), V_TH(:, :), V_HH(:, :), V_HT(:, :)
        real(real64), intent(inout) :: R_T_C(:), R_T_D(:, :), R_H_C(:), R_H_D(:, :)
        integer(int32) :: i
        type(type_state) :: state
        real(real64), pointer, contiguous, dimension(:) :: bdf_coeffs

        call self%controls%time%get_bdf_coeffs(bdf_coeffs)

        do i = 1, num_nodes
            call self%set_state(connectivity(i), element_id, state)
            call self%thermal%compute_C_T(material_id, self%controls, state, C_TT(i), C_TH(i))
            call self%thermal%compute_D_T(material_id, self%controls, state, M_TT(:, :, i), M_TH(:, :, i))
            call self%thermal%compute_V_T(material_id, self%controls, state, V_TT(:, i), V_TH(:, i))
            call self%thermal%compute_R_T(material_id, self%controls, state, R_T_C(i), R_T_D(:, i))
            call self%hydraulic%compute_C_H(material_id, state, C_HH(i), C_HT(i))
            call self%hydraulic%compute_D_H(material_id, state, M_HH(:, :, i), M_HT(:, :, i))
            call self%hydraulic%compute_V_H(material_id, state, V_HH(:, i), V_HT(:, i))
            call self%hydraulic%compute_R_H(material_id, state, bdf_coeffs, R_H_C(i), R_H_D(:, i))
        end do
    end subroutine compute_nodal_coefficients

    ! ==========================================================================
    ! Assemble Matrices
    ! ==========================================================================
    subroutine assemble_matrices(fe, coordinates, dim, local_J, local_R, &
                                 C_TT, C_TH, C_HH, C_HT, &
                                 M_TT, M_TH, M_HH, M_HT, &
                                 V_TT, V_TH, V_HH, V_HT, &
                                 R_T_C, R_T_D, R_H_C, R_H_D, &
                                 local_J_TT, local_J_TH, local_J_HH, local_J_HT, &
                                 local_R_T, local_R_H)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coordinates(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(inout) :: local_J(:, :)
        real(real64), intent(inout) :: local_R(:)
        real(real64), intent(in) :: C_TT(:), C_TH(:), C_HH(:), C_HT(:)
        real(real64), intent(in) :: M_TT(:, :, :), M_TH(:, :, :), M_HH(:, :, :), M_HT(:, :, :)
        real(real64), intent(in) :: V_TT(:, :), V_TH(:, :), V_HH(:, :), V_HT(:, :)
        real(real64), intent(in) :: R_T_C(:), R_T_D(:, :), R_H_C(:), R_H_D(:, :)
        type(type_matrix_dense), intent(inout), optional :: local_J_TT, local_J_TH, local_J_HH, local_J_HT
        type(type_vector_dp), intent(inout), optional :: local_R_T, local_R_H

        ! --- C Terms (LHS Jacobian) -> Lumped Mass ---
        if (present(local_J_TT)) call add_term_scalar_lumped(fe, coordinates, dim, C_TT, local_J, local_J_TT)
        if (present(local_J_TH)) call add_term_scalar_lumped(fe, coordinates, dim, C_TH, local_J, local_J_TH)
        if (present(local_J_HH)) call add_term_scalar_lumped(fe, coordinates, dim, C_HH, local_J, local_J_HH)
        if (present(local_J_HT)) call add_term_scalar_lumped(fe, coordinates, dim, C_HT, local_J, local_J_HT)

        ! --- M Terms (LHS Jacobian) -> Consistent ---
        if (present(local_J_TT)) call add_term_tensor(fe, coordinates, dim, M_TT, local_J, local_J_TT)
        if (present(local_J_TH)) call add_term_tensor(fe, coordinates, dim, M_TH, local_J, local_J_TH)
        if (present(local_J_HH)) call add_term_tensor(fe, coordinates, dim, M_HH, local_J, local_J_HH)
        if (present(local_J_HT)) call add_term_tensor(fe, coordinates, dim, M_HT, local_J, local_J_HT)

        ! --- V Terms (LHS Jacobian) -> Consistent ---
        if (present(local_J_TT)) call add_term_vector(fe, coordinates, dim, V_TT, local_J, local_J_TT)
        if (present(local_J_TH)) call add_term_vector(fe, coordinates, dim, V_TH, local_J, local_J_TH)
        if (present(local_J_HH)) call add_term_vector(fe, coordinates, dim, V_HH, local_J, local_J_HH)
        if (present(local_J_HT)) call add_term_vector(fe, coordinates, dim, V_HT, local_J, local_J_HT)

        ! --- Residuals ---
        if (present(local_R_T)) then
            ! [重要修正] ここで "True Lumping" を使用する
            ! 以前は coeff を重みとして積分していたため Consistent と同等になっていた
            call add_residual_scalar_lumped(fe, coordinates, dim, R_T_C, local_R, local_R_T)
            call add_residual_vector(fe, coordinates, dim, R_T_D, local_R, local_R_T)
        end if
        if (present(local_R_H)) then
            call add_residual_scalar_lumped(fe, coordinates, dim, R_H_C, local_R, local_R_H)
            call add_residual_vector(fe, coordinates, dim, R_H_D, local_R, local_R_H)
        end if
    end subroutine assemble_matrices

    ! ==========================================================================
    ! Helper Subroutines
    ! ==========================================================================

    !> [修正済] Adds a scalar residual term with True Mass Lumping.
    !> 1. Compute Geometric Mass Matrix (with coeff=1.0)
    !> 2. Lump it to get nodal volumes
    !> 3. Multiply by nodal value (coeff)
    subroutine add_residual_scalar_lumped(fe, coords, dim, coeff, buffer, target_vec)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coords(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(in) :: coeff(:)
        real(real64), intent(inout) :: buffer(:)
        type(type_vector_dp), intent(inout) :: target_vec

        real(real64), allocatable :: mat_buffer(:, :)
        real(real64), allocatable :: ones(:)
        integer(int32) :: i, nd
        real(real64) :: row_sum

        nd = size(coeff)
        allocate (mat_buffer(nd, nd))
        allocate (ones(nd))
        ones = 1.0d0

        ! 1. Compute Geometric Mass Matrix (weight = 1.0)
        call fe%compute_K(coords, ones, mat_buffer)

        ! 2. Row-sum to get nodal volumes (Geometric Lumping)
        do i = 1, nd
            row_sum = sum(mat_buffer(i, :))
            ! 3. Multiply by nodal value (R_T_C) and add
            !    Result: Volume_i * Value_i
            call target_vec%set(OP_ADD, i, row_sum * coeff(i))
        end do

        deallocate (ones)
        deallocate (mat_buffer)
    end subroutine add_residual_scalar_lumped

    !> Adds a scalar coefficient term with Mass Lumping (Row-sum) for LHS.
    subroutine add_term_scalar_lumped(fe, coords, dim, coeff, buffer, target_mat)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coords(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(in) :: coeff(:)
        real(real64), intent(inout) :: buffer(:, :)
        type(type_matrix_dense), intent(inout) :: target_mat
        integer(int32) :: i, nd
        real(real64) :: row_sum

        nd = size(buffer, 1)

        ! 1. Compute Consistent Mass Matrix weighted by coeff
        !    (This integrates \int psi_i psi_j coeff dV)
        call fe%compute_K(coords, coeff, buffer)

        ! 2. Perform Row-Sum Lumping
        do i = 1, nd
            row_sum = sum(buffer(i, :))
            buffer(i, :) = 0.0d0
            buffer(i, i) = row_sum
        end do

        ! 3. Add Lumped Matrix to Target
        do i = 1, nd
            call target_mat%set(OP_ADD, i, i, buffer(i, i))
        end do
    end subroutine add_term_scalar_lumped

    subroutine add_term_scalar(fe, coords, dim, coeff, buffer, target_mat)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coords(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(in) :: coeff(:)
        real(real64), intent(inout) :: buffer(:, :)
        type(type_matrix_dense), intent(inout) :: target_mat
        integer(int32) :: i, j, nd
        nd = size(buffer, 1)
        call fe%compute_K(coords, coeff, buffer)
        do i = 1, nd
            do j = 1, nd
                call target_mat%set(OP_ADD, i, j, buffer(i, j))
            end do
        end do
    end subroutine add_term_scalar

    subroutine add_term_tensor(fe, coords, dim, coeff, buffer, target_mat)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coords(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(in) :: coeff(:, :, :)
        real(real64), intent(inout) :: buffer(:, :)
        type(type_matrix_dense), intent(inout) :: target_mat
        integer(int32) :: i, j, nd
        nd = size(buffer, 1)
        call fe%compute_K(coords, coeff, buffer)
        do i = 1, nd
            do j = 1, nd
                call target_mat%set(OP_ADD, i, j, buffer(i, j))
            end do
        end do
    end subroutine add_term_tensor

    subroutine add_term_vector(fe, coords, dim, coeff, buffer, target_mat)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coords(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(in) :: coeff(:, :)
        real(real64), intent(inout) :: buffer(:, :)
        type(type_matrix_dense), intent(inout) :: target_mat
        integer(int32) :: i, j, nd
        nd = size(buffer, 1)
        call fe%compute_K(coords, coeff, buffer)
        do i = 1, nd
            do j = 1, nd
                call target_mat%set(OP_ADD, i, j, buffer(i, j))
            end do
        end do
    end subroutine add_term_vector

    ! (整合積分の add_residual_scalar/vector は念のため残すか、削除しても良い)
    subroutine add_residual_scalar(fe, coords, dim, coeff, buffer, target_vec)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coords(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(in) :: coeff(:)
        real(real64), intent(inout) :: buffer(:)
        type(type_vector_dp), intent(inout) :: target_vec
        integer(int32) :: i, nd
        nd = size(buffer, 1)
        call fe%compute_R(coords, coeff, buffer)
        do i = 1, nd
            call target_vec%set(OP_ADD, i, buffer(i))
        end do
    end subroutine add_residual_scalar

    subroutine add_residual_vector(fe, coords, dim, coeff, buffer, target_vec)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coords(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(in) :: coeff(:, :)
        real(real64), intent(inout) :: buffer(:)
        type(type_vector_dp), intent(inout) :: target_vec
        integer(int32) :: i, nd
        nd = size(buffer, 1)
        call fe%compute_R(coords, coeff, buffer)
        do i = 1, nd
            call target_vec%set(OP_ADD, i, buffer(i))
        end do
    end subroutine add_residual_vector

end submodule ftdss_assemble
