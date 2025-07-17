submodule(conditions_boundary) conditions_boundary_dirichlet
    implicit none
contains

    module subroutine initialize_type_bc_thermal_dirichlet(self, input, domain, id, i_material, time_conv)
        implicit none
        class(type_bc_thermal_dirichlet), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_domain), intent(in) :: domain
        integer(int32), intent(in) :: id
        integer(int32), intent(in) :: i_material
        real(real64), intent(in) :: time_conv

        integer(int32) :: i
        integer(int32), allocatable :: tmp_indices(:)

        self%material_id = input%conditions%boundary_conditions(id)%id
        self%boundary_name = input%conditions%boundary_conditions(id)%thermal%type
        self%is_uniform = input%conditions%boundary_conditions(id)%thermal%is_uniform

        !! Time settings
        if (allocated(self%time_points)) deallocate (self%time_points)
        allocate (self%time_points, source=input%conditions%time_control%boundary_time_points)
        self%time_points = self%time_points * time_conv

        if (allocated(self%values)) deallocate (self%values)
        allocate (self%values, source=input%conditions%boundary_conditions(id)%thermal%values)

        call find_target_edges_by_group(domain, i_material, self%target_edges)
        self%num_target_edges = size(self%target_edges, 2)

        select case (input%basic%solver_settings%reordering)
        case ("cm", "rcm")
            call allocate_array(tmp_indices, 2_int32)
            do i = 1, self%num_target_edges
                call domain%reordering%to_reordered(self%target_edges(:, i), tmp_indices)
                self%target_edges(:, i) = tmp_indices(:)
            end do
            call deallocate_array(tmp_indices)
        end select

    end subroutine initialize_type_bc_thermal_dirichlet

    module subroutine apply_dense_thermal_dirichlet(self, current_time, A, b, domain, mode)
        implicit none
        class(type_bc_thermal_dirichlet), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(inout), optional :: A(:, :)
        real(real64), intent(inout) :: b(:)
        type(type_domain), intent(in) :: domain
        integer(int32), intent(in), optional :: mode

    end subroutine apply_dense_thermal_dirichlet

    module subroutine apply_crs_thermal_dirichlet(self, current_time, A, b, domain, mode)
        implicit none
        class(type_bc_thermal_dirichlet), intent(in) :: self
        real(real64), intent(in) :: current_time
        type(type_crs), intent(inout), optional :: A
        real(real64), intent(inout) :: b(:)
        type(type_domain), intent(in) :: domain
        integer(int32), intent(in), optional :: mode

        real(real64) :: value_dirichlet, timeCoe
        integer(int32) :: idx, iEdge

        if (present(A)) then
            if (.not. present(mode)) then
                call calculate_time_coefficient(current_time, self%time_points, timeCoe, idx)
                value_dirichlet = (self%values(idx) * (1.0d0 - timeCoe) + self%values(idx + 1) * timeCoe)

                do iEdge = 1, self%num_target_edges
                    call apply_crs_dirichlet_base(A=A, &
                                                  b=b, &
                                                  is_uniform=self%is_uniform, &
                                                  edge=self%target_edges(:, iEdge), &
                                                  value_dirichlet=value_dirichlet)
                end do
            else
                select case (mode)
                case (1)
                    call calculate_time_coefficient(current_time, self%time_points, timeCoe, idx)
                    value_dirichlet = (self%values(idx) * (1.0d0 - timeCoe) + self%values(idx + 1) * timeCoe)
                case (0)
                    !! Newton-Raphson step
                    value_dirichlet = 0.0d0
                case (-1)
                    !! initial condition
                    value_dirichlet = self%values(1)
                end select

                do iEdge = 1, self%num_target_edges
                    call apply_crs_dirichlet_base(A=A, &
                                                  b=b, &
                                                  is_uniform=self%is_uniform, &
                                                  edge=self%target_edges(:, iEdge), &
                                                  value_dirichlet=value_dirichlet)
                end do
            end if
        else
            if (.not. present(mode)) then
                call calculate_time_coefficient(current_time, self%time_points, timeCoe, idx)
                value_dirichlet = (self%values(idx) * (1.0d0 - timeCoe) + self%values(idx + 1) * timeCoe)

                do iEdge = 1, self%num_target_edges
                    call apply_crs_dirichlet_base(b=b, &
                                                  is_uniform=self%is_uniform, &
                                                  edge=self%target_edges(:, iEdge), &
                                                  value_dirichlet=value_dirichlet)
                end do
            else
                select case (mode)
                case (1)
                    call calculate_time_coefficient(current_time, self%time_points, timeCoe, idx)
                    value_dirichlet = (self%values(idx) * (1.0d0 - timeCoe) + self%values(idx + 1) * timeCoe)
                case (0)
                !! Newton-Raphson step
                    value_dirichlet = 0.0d0
                case (-1)
                !! initial condition
                    value_dirichlet = self%values(1)
                end select

                do iEdge = 1, self%num_target_edges
                    call apply_crs_dirichlet_base(b=b, &
                                                  is_uniform=self%is_uniform, &
                                                  edge=self%target_edges(:, iEdge), &
                                                  value_dirichlet=value_dirichlet)
                end do
            end if
        end if

    end subroutine apply_crs_thermal_dirichlet

    subroutine apply_crs_dirichlet_base(A, b, is_uniform, edge, value_dirichlet)
        implicit none
        type(type_crs), intent(inout), optional :: A
        real(real64), intent(inout) :: b(:)
        logical, intent(in) :: is_uniform
        integer(int32), intent(in) :: edge(2)
        real(real64), intent(in) :: value_dirichlet

        integer(int32) :: i, j, k, p, N
        integer(int32) :: p_idx
        integer(int32) :: ps, pe
        real(real64) :: Aij
        logical, allocatable :: is_dirichlet_node(:)
        integer(int32) :: ind, p1, p2

        if (.not. is_uniform) return

        if (present(A)) then
            N = size(b)

            p1 = Edge(1)
            p2 = Edge(2)

            if (present(A)) then
                call A%find(p1, p1, ind)
                ps = A%ptr(p1)
                pe = A%ptr(p1 + 1) - 1
                A%val(ps:pe) = 0.0d0
                A%val(ind) = 1.0d0

                call A%find(p2, p2, ind)
                ps = A%ptr(p2)
                pe = A%ptr(p2 + 1) - 1
                A%val(ps:pe) = 0.0d0
                A%val(ind) = 1.0d0
            end if

            b(p1) = value_dirichlet
            b(p2) = value_dirichlet
        else
            ! 行列 A がない場合は、b のみを変更
            b(edge(1)) = value_dirichlet
            b(edge(2)) = value_dirichlet
        end if

    end subroutine apply_crs_dirichlet_base

    subroutine apply_Dense_Dirichlet_base(A, b, is_uniform, edge, value_dirichlet)
        implicit none
        real(real64), intent(inout), optional :: A(:, :)
        real(real64), intent(inout) :: b(:)
        logical, intent(in) :: is_uniform
        integer(int32), intent(in) :: edge(2)
        real(real64), intent(in) :: value_dirichlet

        integer(int32) :: i, ind, ps, pe
        integer(int32) :: p1, p2

        ! if (is_uniform) then
        !     ! ! --- ここからデバッグ用コード ---
        !     ! print *, 'Debug: edge = ', edge(1), edge(2)
        !     ! print *, 'Debug: size(perm) = ', size(perm)
        !     ! if (present(A)) then
        !     !     print *, 'Debug: shape(A) = ', shape(A)
        !     ! end if
        !     ! print *, 'Debug: size(b) = ', size(b)
        !     ! ! --- ここまでデバッグ用コード ---
        !     p1 = edge(1)
        !     p2 = edge(2)

        !     if (present(A)) then
        !         A(p1, :) = 0.0d0
        !         A(p1, p1) = 1.0d0

        !         A(p2, :) = 0.0d0
        !         A(p2, p2) = 1.0d0
        !     end if

        !     b(p1) = value_dirichlet
        !     b(p2) = value_dirichlet
        ! end if

    end subroutine apply_Dense_Dirichlet_base

end submodule conditions_boundary_Dirichlet
