submodule(app_ftdss) ftdss_boundary
    implicit none
contains

    !>
    !> Applies all boundary conditions for active physics.
    !> Order: Prescribe (Step 0) -> Natural (Step 1) -> Essential (Step 2)
    !>
    module subroutine apply_bc_ftdss(self, prescribed)
        implicit none
        class(type_ftdss), intent(inout) :: self
        logical, intent(in), optional :: prescribed

        real(real64) :: current_time
        integer(int32) :: dof_offset
        logical :: prescribe_essential

        call self%control%get_time(current_time)

        prescribe_essential = optval(prescribed, .true.)

        ! ----------------------------------------------------------------------
        ! Step 0: Prescribe Dirichlet Values (Update Field Variables directly)
        ! ----------------------------------------------------------------------
        if (prescribe_essential) then
            if (self%is_active_thermal()) then
                call self%prescribe_essential_bc_generic(PHYSICS_TYPES%THERMAL, current_time, self%temperature)
            end if

            if (self%is_active_hydraulic()) then
                call self%prescribe_essential_bc_generic(PHYSICS_TYPES%HYDRAULIC, current_time, self%pressure)
            end if
        end if

        ! ----------------------------------------------------------------------
        ! Step 1: Apply Natural BCs (Neumann, Robin, etc.)
        ! ----------------------------------------------------------------------
        if (self%is_active_thermal()) then
            call self%domain%get_start_dof_index(PHYSICS_TYPES%THERMAL, dof_offset)
            call self%apply_natural_bc_generic(PHYSICS_TYPES%THERMAL, current_time, &
                                               self%temperature, dof_offset)
        end if

        if (self%is_active_hydraulic()) then
            call self%domain%get_start_dof_index(PHYSICS_TYPES%HYDRAULIC, dof_offset)
            call self%apply_natural_bc_generic(PHYSICS_TYPES%HYDRAULIC, current_time, &
                                               self%pressure, dof_offset)
        end if

        ! ----------------------------------------------------------------------
        ! Step 2: Apply Essential BCs (Dirichlet Constraints)
        ! ----------------------------------------------------------------------
        if (self%is_active_thermal()) then
            call self%domain%get_start_dof_index(PHYSICS_TYPES%THERMAL, dof_offset)
            call self%apply_essential_bc_generic(PHYSICS_TYPES%THERMAL, current_time, &
                                                 self%temperature, dof_offset)
        end if

        if (self%is_active_hydraulic()) then
            call self%domain%get_start_dof_index(PHYSICS_TYPES%HYDRAULIC, dof_offset)
            call self%apply_essential_bc_generic(PHYSICS_TYPES%HYDRAULIC, current_time, &
                                                 self%pressure, dof_offset)
        end if

    end subroutine apply_bc_ftdss

    !>
    !> Enforces Dirichlet values directly into the solution vector.
    !>
    module subroutine prescribe_essential_bc_generic(self, physics_type, current_time, variable)
        implicit none
        class(type_ftdss), intent(inout), target :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(in) :: current_time
        type(type_variable), intent(inout) :: variable

        integer(int32) :: i_patch, num_patches
        integer(int32) :: i, glob_node_id
        integer(int32) :: entity_id, bc_idx
        real(real64) :: val_curr
        type(type_bc_result) :: bc_result
        type(type_boundary_patch), pointer :: bc_patch

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) return

        call self%domain%get_num_bc_patches(num_patches)

        do i_patch = 1, num_patches
            call self%domain%get_bc_patch(i_patch, bc_patch)

            entity_id = bc_patch%entity_id
            call self%bc(physics_type%ID)%get_bc_index(entity_id, bc_idx)
            if (bc_idx < 0) cycle

            if (allocated(bc_patch%connectivity%col_ind)) then
                do i = 1, size(bc_patch%connectivity%col_ind)
                    glob_node_id = bc_patch%connectivity%col_ind(i)

                    call variable%get_current(glob_node_id, val_curr)
                    call self%bc(physics_type%ID)%evaluate(bc_idx, current_time, val_curr, bc_result)

                    if (bc_result%is_dirichlet) then
                        call variable%set_current(glob_node_id, bc_result%prescribed_value)
                        call variable%set_previous(glob_node_id, bc_result%prescribed_value)
                    end if
                end do
            end if
        end do
    end subroutine prescribe_essential_bc_generic

!>
    !> Generic routine to integrate and assemble Natural BCs (Fluxes).
    !>
    module subroutine apply_natural_bc_generic(self, physics_type, current_time, variable, dof_offset)
        implicit none
        class(type_ftdss), intent(inout), target :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(in) :: current_time
        type(type_variable), intent(in) :: variable
        integer(int32), intent(in) :: dof_offset

        integer(int32) :: i_patch, num_patches
        integer(int32) :: i_elem, k_gp
        integer(int32) :: num_nodes_loc, n_dim
        integer(int32) :: i, j
        integer(int32) :: num_gp
        integer(int32) :: start_idx, end_idx
        integer(int32) :: entity_id, bc_idx

        real(real64) :: u_curr, q_flux, dq_du, w_vol, det_j
        real(real64), allocatable :: psi(:)
        real(real64), allocatable :: node_coords(:, :)
        real(real64), pointer, contiguous, dimension(:) :: fe_weights => null()
        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: fe_gauss_pts => null()
        type(type_coordinate_dp) :: r
        real(real64) :: val
        integer(int32), allocatable :: connectivity(:)

        class(abst_fe), pointer :: fe => null()
        type(type_bc_result) :: bc_result
        type(type_boundary_patch), pointer :: bc_patch => null()

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) return

        call self%domain%get_num_bc_patches(num_patches)

        do i_patch = 1, num_patches
            call self%domain%get_bc_patch(i_patch, bc_patch)

            entity_id = bc_patch%entity_id
            call self%bc(physics_type%ID)%get_bc_index(entity_id, bc_idx)
            if (bc_idx < 0) cycle

            call self%bc(physics_type%ID)%evaluate(bc_idx, current_time, 0.0d0, bc_result)
            if (bc_result%is_dirichlet) cycle

            if (bc_patch%num_fe > 0) then
                do i_elem = 1, bc_patch%num_fe
                    ! Get the FE object for this boundary element
                    call bc_patch%fe_manager%get_fe(i_elem, fe)

                    start_idx = bc_patch%connectivity%row_ptr(i_elem)
                    end_idx = bc_patch%connectivity%row_ptr(i_elem + 1) - 1
                    num_nodes_loc = end_idx - start_idx + 1

                    if (allocated(connectivity)) deallocate (connectivity)
                    allocate (connectivity(num_nodes_loc))
                    connectivity = bc_patch%connectivity%col_ind(start_idx:end_idx)

                    call self%domain%nodes%get_dimension(n_dim)
                    if (allocated(node_coords)) deallocate (node_coords)
                    allocate (node_coords(n_dim, num_nodes_loc))

                    call self%domain%nodes%get_coordinate(connectivity, node_coords)

                    call fe%get_num_gauss(num_gp)
                    call fe%get_weight(fe_weights)
                    call fe%get_gauss(fe_gauss_pts)

                    if (allocated(psi)) deallocate (psi)
                    allocate (psi(num_nodes_loc))

                    do k_gp = 1, num_gp
                        r = fe_gauss_pts(k_gp)
                        call fe%calc_shape_function(r, node_coords, psi=psi, determinant_jacobian=det_j)
                        w_vol = fe_weights(k_gp) * det_j

                        u_curr = 0.0d0
                        do i = 1, num_nodes_loc
                            call variable%get_current(connectivity(i), val)
                            u_curr = u_curr + psi(i) * val
                        end do

                        call self%bc(physics_type%ID)%evaluate(bc_idx, current_time, u_curr, bc_result)
                        q_flux = bc_result%flux_value
                        dq_du = bc_result%flux_derivative

                        do i = 1, num_nodes_loc
                            call self%F%add(dof_offset, connectivity(i), psi(i) * q_flux * w_vol)

                            do j = 1, num_nodes_loc
                                call self%K%add(dof_offset, dof_offset, &
                                                connectivity(i), connectivity(j), &
                                                psi(i) * dq_du * psi(j) * w_vol)
                            end do
                        end do
                    end do
                end do
            end if
        end do
    end subroutine apply_natural_bc_generic

    !>
    !> Generic routine to apply Essential BCs (Dirichlet Constraints).
    !>
    module subroutine apply_essential_bc_generic(self, physics_type, current_time, variable, dof_offset)
        implicit none
        class(type_ftdss), intent(inout), target :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(in) :: current_time
        type(type_variable), intent(in) :: variable
        integer(int32), intent(in) :: dof_offset

        integer(int32) :: i_patch, num_patches
        integer(int32) :: i, glob_node_id
        integer(int32) :: entity_id, bc_idx
        integer(int32) :: num_matched_patches, num_dirichlet_nodes
        type(type_bc_result) :: bc_result
        type(type_boundary_patch), pointer :: bc_patch

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) return

        call self%domain%get_num_bc_patches(num_patches)
        num_matched_patches = 0
        num_dirichlet_nodes = 0

        do i_patch = 1, num_patches
            call self%domain%get_bc_patch(i_patch, bc_patch)

            entity_id = bc_patch%entity_id
            call self%bc(physics_type%ID)%get_bc_index(entity_id, bc_idx)
            if (bc_idx < 0) cycle
            num_matched_patches = num_matched_patches + 1

            call self%bc(physics_type%ID)%evaluate(bc_idx, current_time, 0.0d0, bc_result)

            if (.not. bc_result%is_dirichlet) cycle

            if (allocated(bc_patch%connectivity%col_ind)) then
                do i = 1, size(bc_patch%connectivity%col_ind)
                    glob_node_id = bc_patch%connectivity%col_ind(i)

                    ! 1. Zero out the row of the Jacobian matrix
                    call self%K%zero(glob_node_id, dof_offset)
                    ! Set diagonal element to 1.0
                    call self%K%set(dof_offset, dof_offset, glob_node_id, glob_node_id, 1.0d0)

                    ! 2. Set Residual/Force vector
                    call self%F%set(dof_offset, glob_node_id, 0.0d0)
                    num_dirichlet_nodes = num_dirichlet_nodes + 1
                end do
            end if
        end do

        if (num_matched_patches == 0) then
            write (*, '(A,1X,A)') 'Error: No boundary patch matched boundary_conditions IDs for physics:', trim(physics_type%name)
            error stop 'Boundary condition ID mismatch between mesh entity IDs and Conditions.json IDs.'
        end if

        if (num_dirichlet_nodes == 0) then
            write (*, '(A,1X,A)') 'Error: No Dirichlet nodes were constrained for physics:', trim(physics_type%name)
            error stop 'No essential BC applied. The linear system may be singular.'
        end if
    end subroutine apply_essential_bc_generic

end submodule ftdss_boundary
