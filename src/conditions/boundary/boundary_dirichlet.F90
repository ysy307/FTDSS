submodule(conditions_boundary) conditions_boundary_dirichlet
    implicit none
contains

    module subroutine initialize_type_bc_thermal_dirichlet(self, input, domain, i_material, time_conv)
        implicit none
        class(type_bc_thermal_dirichlet), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_domain), intent(in) :: domain
        integer(int32), intent(in) :: i_material
        real(real64), intent(in) :: time_conv

        integer(int32) :: i
        integer(int32), allocatable :: tmp_indices(:)

        self%material_id = input%conditions%boundary_conditions(i_material)%id
        self%boundary_name = input%conditions%boundary_conditions(i_material)%thermal%type
        self%is_uniform = input%conditions%boundary_conditions(i_material)%thermal%is_uniform

        !! Time settings
        if (allocated(self%time_points)) deallocate (self%time_points)
        allocate (self%time_points, source=input%conditions%time_control%boundary_time_points)
        self%time_points = self%time_points * time_conv

        if (allocated(self%values)) deallocate (self%values)
        allocate (self%values, source=input%conditions%boundary_conditions(i_material)%thermal%values)

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
                call calc_time_coefficient(current_time, self%time_points, timeCoe, idx)
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
                    call calc_time_coefficient(current_time, self%time_points, timeCoe, idx)
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
                call calc_time_coefficient(current_time, self%time_points, timeCoe, idx)
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
                    call calc_time_coefficient(current_time, self%time_points, timeCoe, idx)
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

        if (.not. is_uniform) return

        if (present(A)) then
            N = size(b)

            ! ディリクレノードかどうかを判定するフラグ配列を用意
            call allocate_array(is_dirichlet_node, N)
            is_dirichlet_node = .false.
            do p_idx = 1, size(edge)
                is_dirichlet_node(edge(p_idx)) = .true.
            end do

            ! --- 対称性を保つディリクレ境界条件の適用 ---

            ! STEP 1: 全てのディリクレノード p の「列」の影響を右辺 b に移す
            !         b(i) <- b(i) - A(i, p) * value_dirichlet
            !         A(i, p) <- 0  (ただし i はディリクレノードでない)
            do p_idx = 1, size(edge)
                p = edge(p_idx)

                do i = 1, N
                    ! i がディリクレノードの行の場合、この操作は不要 (STEP 2で上書きされるため)
                    if (is_dirichlet_node(i)) cycle

                    ! 行 i を探索して A(i, p) を見つける
                    ps = A%ptr(i)
                    pe = A%ptr(i + 1) - 1
                    do k = ps, pe
                        if (A%ind(k) == p) then
                            Aij = A%val(k)
                            if (Aij /= 0.0d0) then
                                b(i) = b(i) - Aij * value_dirichlet
                                A%val(k) = 0.0d0 ! 列要素をゼロ化
                            end if
                            exit ! 行 i に p 列の要素は高々1つ
                        end if
                    end do
                end do
            end do

            ! STEP 2: 全てのディリクレノード p の「行」を処理する
            !         A(p, p) = 1, A(p, j) = 0 (j!=p), b(p) = value_dirichlet
            do p_idx = 1, size(edge)
                p = edge(p_idx)

                ps = A%ptr(p)
                pe = A%ptr(p + 1) - 1
                do k = ps, pe
                    j = A%ind(k)
                    if (j == p) then
                        A%val(k) = 1.0d0 ! 対角要素を 1 に
                    else
                        A%val(k) = 0.0d0 ! 非対角要素を 0 に
                    end if
                end do
                b(p) = value_dirichlet
            end do

            deallocate (is_dirichlet_node)

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
        !     p1 = perm(edge(1))
        !     p2 = perm(edge(2))

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
