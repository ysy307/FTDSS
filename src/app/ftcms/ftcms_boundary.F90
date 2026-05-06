submodule(app_ftcms) ftcms_boundary
    implicit none
contains

    module subroutine prescribe_dirichlet_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self
        real(real64) :: current_time

        call self%control%get_time(current_time)

        if (self%is_active_thermal()) then
            call self%prescribe_essential_bc_generic(PHYSICS_TYPES%THERMAL, current_time, self%temperature)
        end if

        if (self%is_active_hydraulic()) then
            call self%prescribe_essential_bc_generic(PHYSICS_TYPES%HYDRAULIC, current_time, self%pressure)
        end if
    end subroutine prescribe_dirichlet_ftcms

    !>
    !> Applies all boundary conditions for active physics.
    !> Order: Prescribe (Step 0) -> Natural (Step 1) -> Essential (Step 2)
    !>
    module subroutine apply_bc_ftcms(self, prescribed)
        implicit none
        class(type_ftcms), intent(inout) :: self
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

    end subroutine apply_bc_ftcms

    !>
    !> Freezes all DOFs of a given physics in the linear system.
    !> Sets K[i,i]=1, K row=0, F[i]=0 for every node.
    !> After this, the linear solve yields du=0 for those DOFs,
    !> so reflect_variables leaves that variable unchanged.
    !> Called after apply_bc in the staggered nonlinear loop.
    module subroutine freeze_physics_dofs_ftcms(self, physics_type)
        implicit none
        class(type_ftcms), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type

        real(real64) :: diag_val
        integer(int32) :: i_node, num_nodes, dof_index, k, row_start, row_end
        integer(int32), pointer :: ptr(:) => null(), ind(:) => null()
        real(real64), pointer :: val(:, :, :) => null()
        class(abst_matrix), pointer :: matrix_ptr => null()

        ! In staggered coupling each physics owns a separate matrix; freezing
        ! the inactive one is structurally unnecessary and would touch the wrong
        ! sub-system. The active phase solves only its own sub-matrix.
        if (self%control%is_staggered()) return

        call self%domain%get_num_nodes(num_nodes)
        call self%domain%get_start_dof_index(physics_type, dof_index)

        matrix_ptr => self%K%get_matrix()
        select type (matrix_ptr)
        type is (type_matrix_bsr)
            ptr => matrix_ptr%get_ptr()
            ind => matrix_ptr%get_ind()
            val => matrix_ptr%get_val()
            if (associated(ptr) .and. associated(ind) .and. associated(val)) then
                do i_node = 1, num_nodes
                    row_start = ptr(i_node)
                    row_end = ptr(i_node + 1) - 1
                    do k = row_start, row_end
                        val(:, dof_index, k) = 0.0d0
                    end do

                    diag_val = 0.0d0
                    do k = row_start, row_end
                        if (ind(k) == i_node) then
                            diag_val = val(dof_index, dof_index, k)
                            exit
                        end if
                    end do

                    do k = row_start, row_end
                        val(dof_index, :, k) = 0.0d0
                    end do

                    if (abs(diag_val) < 1.0d-12) diag_val = 1.0d0
                    do k = row_start, row_end
                        if (ind(k) == i_node) then
                            val(dof_index, dof_index, k) = diag_val
                            exit
                        end if
                    end do

                    call self%F%set(physics_type%ID, i_node, 0.0d0)
                end do
            end if
            nullify (ptr)
            nullify (ind)
            nullify (val)
        class default
            continue
        end select
        nullify (matrix_ptr)

    end subroutine freeze_physics_dofs_ftcms

    !> Zero the increment (du) entries for the frozen physics DOFs.
    !> Prevents GMRES restart drift from contaminating frozen-phase updates.
    module subroutine zero_frozen_increment_ftcms(self, frozen_physics)
        implicit none
        class(type_ftcms), intent(inout) :: self
        type(type_constant_id), intent(in) :: frozen_physics

        integer(int32) :: i_node, num_nodes, dof_index, num_dofs_per_node
        real(real64), pointer :: du(:) => null()

        if (self%control%is_staggered()) return

        call self%domain%get_num_nodes(num_nodes)
        call self%domain%get_num_dof_per_node(num_dofs_per_node)
        call self%domain%get_start_dof_index(frozen_physics, dof_index)

        du => self%du%get_data()
        if (.not. associated(du)) return

        do i_node = 1, num_nodes
            du(num_dofs_per_node * (i_node - 1) + dof_index) = 0.0d0
        end do

        nullify (du)

    end subroutine zero_frozen_increment_ftcms

    !>
    !> Enforces Dirichlet values directly into the solution vector.
    !>
    module subroutine prescribe_essential_bc_generic(self, physics_type, current_time, variable)
        implicit none
        class(type_ftcms), intent(inout), target :: self
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
        class(type_ftcms), intent(inout), target :: self
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
                            call self%F%add(physics_type%ID, connectivity(i), psi(i) * q_flux * w_vol)

                            do j = 1, num_nodes_loc
                                call self%K%add(physics_type%ID, physics_type%ID, &
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
        class(type_ftcms), intent(inout), target :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(in) :: current_time
        type(type_variable), intent(in) :: variable
        integer(int32), intent(in) :: dof_offset

        integer(int32) :: i_patch, num_patches
        integer(int32) :: i, glob_node_id
        integer(int32) :: entity_id, bc_idx
        integer(int32) :: num_matched_patches, num_dirichlet_nodes
        integer(int32) :: bdf_order
        integer(int32) :: n_dim
        integer(int32) :: first_dirichlet_node
        real(real64) :: first_dirichlet_coord(3)
        real(real64), allocatable :: node_coord(:)
        logical, save :: printed_thermal_dirichlet_location = .false.
        logical, save :: printed_hydraulic_dirichlet_location = .false.
        type(type_bc_result) :: bc_result
        type(type_boundary_patch), pointer :: bc_patch

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) return

        call self%domain%get_num_bc_patches(num_patches)
        num_matched_patches = 0
        num_dirichlet_nodes = 0
        first_dirichlet_node = 0
        first_dirichlet_coord(:) = 0.0d0
        n_dim = 0
        call self%domain%nodes%get_dimension(n_dim)
        if (n_dim > 0) then
            allocate (node_coord(n_dim))
            node_coord(:) = 0.0d0
        end if

        do i_patch = 1, num_patches
            call self%domain%get_bc_patch(i_patch, bc_patch)

            entity_id = bc_patch%entity_id
            call self%bc(physics_type%ID)%get_bc_index(entity_id, bc_idx)
            if (bc_idx < 0) cycle
            num_matched_patches = num_matched_patches + 1

            call self%bc(physics_type%ID)%evaluate(bc_idx, current_time, 0.0d0, bc_result)

            if (.not. bc_result%is_dirichlet) then
                cycle
            end if

            if (allocated(bc_patch%connectivity%col_ind)) then
                do i = 1, size(bc_patch%connectivity%col_ind)
                    glob_node_id = bc_patch%connectivity%col_ind(i)

                    ! 1. Zero out the row of the Jacobian matrix
                    call self%K%zero(glob_node_id, physics_type%ID)
                    ! Set diagonal element to 1.0
                    call self%K%set(physics_type%ID, physics_type%ID, glob_node_id, glob_node_id, 1.0d0)

                    ! 2. Set Residual/Force vector
                    call self%F%set(physics_type%ID, glob_node_id, 0.0d0)
                    num_dirichlet_nodes = num_dirichlet_nodes + 1

                    if (first_dirichlet_node == 0) then
                        first_dirichlet_node = glob_node_id
                        if (allocated(node_coord)) then
                            call self%domain%nodes%get_coordinate(glob_node_id, node_coord)
                            first_dirichlet_coord(:) = 0.0d0
                            first_dirichlet_coord(1:min(3, n_dim)) = node_coord(1:min(3, n_dim))
                        end if
                    end if
                end do
            end if
        end do

        if (num_dirichlet_nodes > 0) then
            if (physics_type%ID == PHYSICS_TYPES%THERMAL%ID .and. .not. printed_thermal_dirichlet_location) then
                write (*, '(A,I0,A,I0,A,I0,A,3(ES13.5,A))') '   [BC] thermal Dirichlet nodes=', num_dirichlet_nodes, &
                    ', matched_patches=', num_matched_patches, ', sample_node=', first_dirichlet_node, &
                    ', sample_coord(x,y,z)=', first_dirichlet_coord(1), ',', first_dirichlet_coord(2), ',', &
                    first_dirichlet_coord(3), ''
                printed_thermal_dirichlet_location = .true.
            else if (physics_type%ID == PHYSICS_TYPES%HYDRAULIC%ID .and. .not. printed_hydraulic_dirichlet_location) then
                write (*, '(A,I0,A,I0,A,I0,A,3(ES13.5,A))') '   [BC] hydraulic Dirichlet nodes=', num_dirichlet_nodes, &
                    ', matched_patches=', num_matched_patches, ', sample_node=', first_dirichlet_node, &
                    ', sample_coord(x,y,z)=', first_dirichlet_coord(1), ',', first_dirichlet_coord(2), ',', &
                    first_dirichlet_coord(3), ''
                printed_hydraulic_dirichlet_location = .true.
            end if
        end if

        if (num_matched_patches == 0) then
            write (*, '(A,1X,A)') 'Error: No boundary patch matched boundary_conditions IDs for physics:', trim(physics_type%name)
            error stop 'Boundary condition ID mismatch between mesh entity IDs and Conditions.json IDs.'
        end if

        if (physics_type%ID == PHYSICS_TYPES%HYDRAULIC%ID) then
            self%hydraulic_has_dirichlet_bc = (num_dirichlet_nodes > 0)
        else if (physics_type%ID == PHYSICS_TYPES%THERMAL%ID) then
            self%thermal_has_dirichlet_bc = (num_dirichlet_nodes > 0)
        end if

        if (num_dirichlet_nodes == 0) then
            bdf_order = 0
            call self%control%get_bdf_coeffs(bdf_order=bdf_order)
            if (bdf_order <= 0) then
                write (*, '(A,1X,A)') 'Error: No Dirichlet BC for physics in non-transient mode:', trim(physics_type%name)
                error stop 'All-Neumann problem without transient storage is singular. Provide Dirichlet BC or transient terms.'
            end if

        end if

        if (allocated(node_coord)) deallocate (node_coord)

    end subroutine apply_essential_bc_generic

end submodule ftcms_boundary
