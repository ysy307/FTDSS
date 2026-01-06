submodule(main_ftdss) ftdss_boundary
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

        call self%controls%time%get_time(current_time)

        if (.not. present(prescribed)) then
            prescribe_essential = .true.
        else
            prescribe_essential = prescribed
        end if

        ! ----------------------------------------------------------------------
        ! Step 0: Prescribe Dirichlet Values (Update Field Variables directly)
        ! ----------------------------------------------------------------------
        if (prescribe_essential) then
            if (self%controls%is_physics_active(PHYSICS_TYPE_THERMAL)) then
                call self%prescribe_essential_bc_generic(PHYSICS_TYPE_THERMAL, current_time, self%temperature)
            end if

            if (self%controls%is_physics_active(PHYSICS_TYPE_HYDRAULIC)) then
                call self%prescribe_essential_bc_generic(PHYSICS_TYPE_HYDRAULIC, current_time, self%pressure)
            end if
        end if

        ! ----------------------------------------------------------------------
        ! Step 1: Apply Natural BCs (Neumann, Robin, etc.)
        ! ----------------------------------------------------------------------
        if (self%controls%is_physics_active(PHYSICS_TYPE_THERMAL)) then
            dof_offset = self%domain%dof_map%start_dof_index(PHYSICS_TYPE_THERMAL)
            call self%apply_natural_bc_generic(PHYSICS_TYPE_THERMAL, current_time, &
                                               self%temperature, dof_offset)
        end if

        if (self%controls%is_physics_active(PHYSICS_TYPE_HYDRAULIC)) then
            dof_offset = self%domain%dof_map%start_dof_index(PHYSICS_TYPE_HYDRAULIC)
            call self%apply_natural_bc_generic(PHYSICS_TYPE_HYDRAULIC, current_time, &
                                               self%pressure, dof_offset)
        end if

        ! ----------------------------------------------------------------------
        ! Step 2: Apply Essential BCs (Dirichlet Constraints)
        ! ----------------------------------------------------------------------
        if (self%controls%is_physics_active(PHYSICS_TYPE_THERMAL)) then
            dof_offset = self%domain%dof_map%start_dof_index(PHYSICS_TYPE_THERMAL)
            call self%apply_essential_bc_generic(PHYSICS_TYPE_THERMAL, current_time, &
                                                 self%temperature, dof_offset)
        end if

        if (self%controls%is_physics_active(PHYSICS_TYPE_HYDRAULIC)) then
            dof_offset = self%domain%dof_map%start_dof_index(PHYSICS_TYPE_HYDRAULIC)
            call self%apply_essential_bc_generic(PHYSICS_TYPE_HYDRAULIC, current_time, &
                                                 self%pressure, dof_offset)
        end if

    end subroutine apply_bc_ftdss

    !>
    !> Enforces Dirichlet values directly into the solution vector.
    !>
    module subroutine prescribe_essential_bc_generic(self, physics_type, current_time, variable)
        implicit none
        class(type_ftdss), intent(inout), target :: self
        integer(int32), intent(in) :: physics_type
        real(real64), intent(in) :: current_time
        type(type_variable), intent(inout) :: variable

        integer(int32) :: i_patch, i, glob_node_id
        real(real64) :: val_fixed
        logical :: is_active
        class(abst_bc), pointer :: bc_obj

        associate (phys_mgr => self%domain%boundaries%physics(physics_type))
            do i_patch = 1, phys_mgr%num_bcs
                bc_obj => phys_mgr%bcs(i_patch)%condition

                call bc_obj%get_dirichlet_value(current_time, val_fixed, is_active)
                if (.not. is_active) cycle

                associate (patch => phys_mgr%bcs(i_patch))
                    do i = 1, size(patch%connectivity%val)
                        glob_node_id = patch%connectivity%val(i)
                        ! 現在の変数値をBC値に上書き
                        variable%pre(glob_node_id) = val_fixed
                        variable%new(glob_node_id) = val_fixed
                    end do
                end associate
            end do
        end associate
    end subroutine prescribe_essential_bc_generic

    !>
    !> Generic routine to integrate and assemble Natural BCs (Fluxes).
    !>
    module subroutine apply_natural_bc_generic(self, physics_type, current_time, variable, dof_offset)
        implicit none
        class(type_ftdss), intent(inout), target :: self
        integer(int32), intent(in) :: physics_type
        real(real64), intent(in) :: current_time
        type(type_variable), intent(in) :: variable
        integer(int32), intent(in) :: dof_offset

        integer(int32) :: i_patch, i_elem, k_gp
        integer(int32) :: num_nodes_loc
        integer(int32) :: i, j
        integer(int32) :: num_gp

        real(real64) :: u_curr, q_flux, dq_du, w_vol, det_j

        real(real64), allocatable :: psi(:)
        real(real64), allocatable :: dpsi_dx(:, :)
        real(real64), allocatable :: node_coords(:, :)
        real(real64), pointer, contiguous, dimension(:) :: fe_weights
        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: fe_gauss_pts
        type(type_coordinate_dp) :: r

        integer(int32), pointer, contiguous, dimension(:) :: connectivity
        class(abst_bc), pointer :: bc_obj
        class(abst_fe), pointer :: fe

        associate (phys_mgr => self%domain%boundaries%physics(physics_type))
            do i_patch = 1, phys_mgr%num_bcs
                bc_obj => phys_mgr%bcs(i_patch)%condition

                select type (bc_obj)
                type is (type_bc_dirichlet)
                    cycle
                end select

                associate (patch => phys_mgr%bcs(i_patch))
                    ! パッチ内の全ての境界要素についてFEタイプは同一と仮定して代表を取得 (index=1)
                    fe => patch%fe_manager%get_fe(1)

                    do i_elem = 1, patch%num_elements

                        ! コネクティビティ取得
                        call self%domain%get_element_connectivity( &
                            patch%connectivity%val(patch%connectivity%ind(i_elem)), connectivity)

                        if (.not. associated(connectivity)) cycle
                        num_nodes_loc = size(connectivity)

                        ! 座標取得 (境界要素に対応するノード群)
                        if (allocated(node_coords)) deallocate (node_coords)
                        allocate (node_coords(self%domain%computation_dimension, num_nodes_loc))
                        node_coords = self%domain%nodes%coordinates(:, connectivity)

                        ! ガウス積分情報
                        call fe%get_num_gauss(num_gp)
                        call fe%get_weight(fe_weights)
                        call fe%get_gauss(fe_gauss_pts)

                        if (allocated(psi)) deallocate (psi)
                        if (allocated(dpsi_dx)) deallocate (dpsi_dx)
                        allocate (psi(num_nodes_loc))
                        allocate (dpsi_dx(num_nodes_loc, self%domain%computation_dimension))

                        do k_gp = 1, num_gp
                            r = fe_gauss_pts(k_gp)

                            call fe%calc_shape_data(r, node_coords, psi, dpsi_dx, det_j)
                            w_vol = fe_weights(k_gp) * det_j

                            u_curr = 0.0d0
                            do i = 1, num_nodes_loc
                                u_curr = u_curr + psi(i) * variable%pre(connectivity(i))
                            end do

                            call bc_obj%get_flux_and_derivative(current_time, u_curr, q_flux, dq_du)

                            do i = 1, num_nodes_loc
                                ! Residual: add(row_dof, global_node_index, value)
                                call self%R%add(dof_offset, connectivity(i), psi(i) * q_flux * w_vol)

                                do j = 1, num_nodes_loc
                                    ! Jacobian: add(row_dof, col_dof, row_node, col_node, value)
                                    call self%J%add(dof_offset, dof_offset, &
                                                    connectivity(i), connectivity(j), &
                                                    psi(i) * dq_du * psi(j) * w_vol)
                                end do
                            end do
                        end do
                    end do
                end associate
            end do
        end associate

    end subroutine apply_natural_bc_generic

    !>
    !> Generic routine to apply Essential BCs (Dirichlet Constraints).
    !>
    module subroutine apply_essential_bc_generic(self, physics_type, current_time, variable, dof_offset)
        implicit none
        class(type_ftdss), intent(inout), target :: self
        integer(int32), intent(in) :: physics_type
        real(real64), intent(in) :: current_time
        type(type_variable), intent(in) :: variable
        integer(int32), intent(in) :: dof_offset

        integer(int32) :: i_patch, i, glob_node_id
        real(real64) :: val_fixed, val_curr
        logical :: is_active
        class(abst_bc), pointer :: bc_obj

        associate (phys_mgr => self%domain%boundaries%physics(physics_type))
            do i_patch = 1, phys_mgr%num_bcs
                bc_obj => phys_mgr%bcs(i_patch)%condition

                call bc_obj%get_dirichlet_value(current_time, val_fixed, is_active)
                if (.not. is_active) cycle

                associate (patch => phys_mgr%bcs(i_patch))
                    do i = 1, size(patch%connectivity%val)
                        glob_node_id = patch%connectivity%val(i)

                        val_curr = variable%new(glob_node_id)

                        ! 1. Jacobianの行をゼロ化 (zero_row使用)
                        call self%J%zero(glob_node_id, dof_offset)

                        !    対角成分に1.0をセット
                        call self%J%set(dof_offset, dof_offset, glob_node_id, glob_node_id, 1.0d0)

                        ! 2. 残差ベクトルを上書き (set使用)
                        call self%R%set(dof_offset, glob_node_id, val_curr - val_fixed)
                    end do
                end associate
            end do
        end associate

    end subroutine apply_essential_bc_generic

end submodule ftdss_boundary
