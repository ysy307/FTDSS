submodule(main_ftdss) ftdss_compute
    implicit none
contains
    ! --------------------------------------------------------------------------
    ! 処理: 要素ごとの状態量を節点値へスムージング（体積平均）する
    ! --------------------------------------------------------------------------
    module subroutine update_variables_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        integer(int32) :: i_node, i_elem, j
        integer(int32) :: num_nodes, num_neighbors, material_id
        integer(int32), pointer, contiguous :: element_list(:) => null()

        ! 状態量計算用のワーク変数
        type(type_state) :: state

        ! 値の一時保管用
        real(real64) :: elem_qw, elem_qi, elem_qa, elem_qv
        real(real64) :: elem_vol

        ! 累積計算用 (Volume Weighted Sum)
        real(real64) :: sum_vol
        real(real64) :: sum_qw_vol, sum_qi_vol, sum_qa_vol, sum_qv_vol

        call self%controls%profiler%start("Setup")
        call self%domain%get_num_nodes(num_nodes)

        ! ----------------------------------------------------------------------
        ! 節点ループ
        ! ----------------------------------------------------------------------
        do i_node = 1, num_nodes

            ! 1. 隣接要素リストの取得
            call self%domain%element_adjacency%get_list(i_node, element_list)

            ! 初期化
            sum_vol = 0.0d0
            sum_qw_vol = 0.0d0
            sum_qi_vol = 0.0d0
            sum_qa_vol = 0.0d0
            sum_qv_vol = 0.0d0

            if (associated(element_list)) then
                num_neighbors = size(element_list)

                ! 2. 隣接要素ループ
                do j = 1, num_neighbors
                    i_elem = element_list(j)

                    ! 要素体積の取得 (これが抜けると重み付けできません)
                    call self%domain%calc_measure(i_elem, elem_vol)

                    ! 状態変数の更新と取得
                    call self%domain%get_material_id(i_elem, material_id)
                    call self%set_state(i_node, i_elem, state)

                    ! Stateから各相の体積含水率などを取得
                    call state%get(water_content=elem_qw, ice_content=elem_qi, &
                                   air_content=elem_qa, vapor_content=elem_qv)
                    ! write (*, '("Qw: ", F8.4, " Qi: ", F8.4, " Qa: ", F8.4, " Qv: ", F8.4, "measure: ", F8.4)') &
                    !     elem_qw, elem_qi, elem_qa, elem_qv, elem_vol

                    ! 重み付き加算
                    sum_vol = sum_vol + elem_vol
                    sum_qw_vol = sum_qw_vol + (elem_qw * elem_vol)
                    sum_qi_vol = sum_qi_vol + (elem_qi * elem_vol)
                    sum_qa_vol = sum_qa_vol + (elem_qa * elem_vol)
                    sum_qv_vol = sum_qv_vol + (elem_qv * elem_vol)
                end do
            end if

            ! 3. 正規化して節点へ格納
            if (abs(sum_vol) > epsilon(1.0d0)) then
                ! write (*, '(" Node ", I6, ": sum_vol=", F8.4, " Qw=", F8.4, " Qi=", F8.4, " Qa=", F8.4, " Qv=", F8.4)') &
                !     i_node, sum_vol, sum_qw_vol / sum_vol, sum_qi_vol / sum_vol, sum_qa_vol / sum_vol, sum_qv_vol / sum_vol
                call self%Qw%set_current(i_node, sum_qw_vol / sum_vol)
                call self%Qi%set_current(i_node, sum_qi_vol / sum_vol)
                call self%Qa%set_current(i_node, sum_qa_vol / sum_vol)
                call self%Qv%set_current(i_node, sum_qv_vol / sum_vol)
            else
                ! 孤立節点等の処理
                call self%Qw%set_current(i_node, 0.0d0)
                call self%Qi%set_current(i_node, 0.0d0)
                call self%Qa%set_current(i_node, 0.0d0)
                call self%Qv%set_current(i_node, 0.0d0)
            end if

        end do

        call self%controls%profiler%stop("Setup")

    end subroutine update_variables_ftdss

    module subroutine solve_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        class(abst_matrix), pointer :: K_ptr => null()
        type(type_vector_dp), pointer :: F_ptr => null()
        type(type_vector_dp), pointer :: u_ptr => null()

        call self%controls%profiler%start("Solve")

        K_ptr => self%K%get_matrix()
        F_ptr => self%F%get_vector()
        u_ptr => self%u%get_vector()

        call self%solver%solve(K_ptr, F_ptr, u_ptr)
        call self%solver%check()

        nullify (K_ptr)
        nullify (F_ptr)
        nullify (u_ptr)

        call self%controls%profiler%stop("Solve")

    end subroutine solve_ftdss

    !>
    !> 節点上の物理量勾配を計算する（L2射影 / Lumped Mass法）
    !>
    module subroutine calc_gradient_ftdss(self, values_vec, grad)
        implicit none
        class(type_ftdss), intent(inout) :: self
        real(real64), intent(in) :: values_vec(:)
        type(type_coordinate_array_dp), intent(inout) :: grad

        class(abst_fe), pointer :: fe
        integer(int32), dimension(:), pointer, contiguous :: p_conn

        ! 要素データ用配列
        real(real64), allocatable :: elem_u(:)
        real(real64), allocatable :: node_coords(:, :)
        real(real64), allocatable :: psi(:)
        real(real64), allocatable :: dpsi_dx(:, :)

        ! FE情報キャッシュ用
        real(real64), pointer, contiguous, dimension(:) :: fe_weights
        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: fe_gauss_pts

        real(real64), allocatable :: nodal_vol(:)

        real(real64) :: det_j
        real(real64) :: gauss_grad(3)
        real(real64) :: w_vol, shape_weight
        type(type_coordinate_dp) :: r

        integer(int32) :: num_elements, num_total_nodes, dim
        integer(int32) :: n_nodes_elem, n_gauss
        integer(int32) :: i, p, k, d, global_nid

        call self%domain%get_num_elements(num_elements)
        call self%domain%get_num_nodes(num_total_nodes)
        call self%domain%get_computation_dimension(dim)

        call grad%zero()

        if (allocated(nodal_vol)) deallocate (nodal_vol)
        allocate (nodal_vol(num_total_nodes))
        nodal_vol(:) = 0.0d0

        do i = 1, num_elements
            call self%domain%get_element(i, fe)
            call self%domain%get_element_connectivity(i, p_conn)

            call fe%get_num_nodes(n_nodes_elem)
            call fe%get_num_gauss(n_gauss)

            call fe%get_weight(fe_weights)
            call fe%get_gauss(fe_gauss_pts)

            ! 作業用配列の再確保 (allocatableは自動再割り当てされる場合もあるが明示的に管理)
            if (allocated(elem_u)) deallocate (elem_u)
            if (allocated(psi)) deallocate (psi)
            if (allocated(dpsi_dx)) deallocate (dpsi_dx)
            ! node_coordsは get_element_coordinate 内で handle されるためここでは deallocate しない方が安全だが、
            ! エラー回避のために明示的に ensure することも可能。
            ! ここでは元のコードの意図通り get_element_coordinate に任せる。

            allocate (elem_u(n_nodes_elem))
            allocate (psi(n_nodes_elem))
            allocate (dpsi_dx(dim, n_nodes_elem))

            elem_u(:) = values_vec(p_conn(:))

            ! 座標取得 (allocatable引数)
            call self%domain%get_element_coordinate(i, node_coords)

            do p = 1, n_gauss
                r = fe_gauss_pts(p)

                call fe%calc_shape_function(r, node_coords, psi=psi, dpsi_dx=dpsi_dx, determinant_jacobian=det_j)
                w_vol = fe_weights(p) * det_j

                gauss_grad = 0.0d0
                do d = 1, dim
                    gauss_grad(d) = vector_dot(elem_u, dpsi_dx(d, :))
                end do

                do k = 1, n_nodes_elem
                    global_nid = p_conn(k)
                    shape_weight = psi(k) * w_vol

                    nodal_vol(global_nid) = nodal_vol(global_nid) + shape_weight

                    if (allocated(grad%x)) grad%x(global_nid) = grad%x(global_nid) + shape_weight * gauss_grad(1)
                    if (dim >= 2) then
                        if (allocated(grad%y)) grad%y(global_nid) = grad%y(global_nid) + shape_weight * gauss_grad(2)
                    end if
                    if (dim >= 3) then
                        if (allocated(grad%z)) grad%z(global_nid) = grad%z(global_nid) + shape_weight * gauss_grad(3)
                    end if
                end do
            end do
        end do

        do k = 1, num_total_nodes
            if (nodal_vol(k) > epsilon(1.0d0)) then
                if (allocated(grad%x)) grad%x(k) = grad%x(k) / nodal_vol(k)
                if (allocated(grad%y)) grad%y(k) = grad%y(k) / nodal_vol(k)
                if (allocated(grad%z)) grad%z(k) = grad%z(k) / nodal_vol(k)
            else
                if (allocated(grad%x)) grad%x(k) = 0.0d0
                if (allocated(grad%y)) grad%y(k) = 0.0d0
                if (allocated(grad%z)) grad%z(k) = 0.0d0
            end if
        end do

        if (allocated(elem_u)) deallocate (elem_u)
        if (allocated(node_coords)) deallocate (node_coords)
        if (allocated(psi)) deallocate (psi)
        if (allocated(dpsi_dx)) deallocate (dpsi_dx)
        if (allocated(nodal_vol)) deallocate (nodal_vol)

    end subroutine calc_gradient_ftdss

    module subroutine calc_gradient_temperature_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        real(real64), pointer, contiguous, dimension(:) :: temperature => null()

        if (.not. self%controls%is_physics_active(PHYSICS_TYPE_THERMAL)) return

        call self%temperature%get_current(temperature)
        call self%calc_gradient(temperature, self%temperature%grad)
        nullify (temperature)

    end subroutine calc_gradient_temperature_ftdss

    module subroutine calc_gradient_pressure_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        real(real64), pointer, contiguous, dimension(:) :: pressure => null()

        if (.not. self%controls%is_physics_active(PHYSICS_TYPE_HYDRAULIC)) return

        call self%pressure%get_current(pressure)
        call self%calc_gradient(pressure, self%pressure%grad)

        nullify (pressure)

    end subroutine calc_gradient_pressure_ftdss

    module subroutine calc_water_flux_ftdss(self, material_id, state, grad_T, grad_P, water_flux)
        implicit none
        class(type_ftdss), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        type(type_coordinate_dp), intent(in) :: grad_T, grad_P
        type(type_coordinate_dp), intent(inout) :: water_flux

        integer(int32) :: computation_type

        real(real64) :: K_wT, K_wP
        real(real64) :: rho_w, gravity_term

        call self%domain%get_computation_dimension(computation_type)

        call self%hydraulic%calc_K_wT(material_id, state, K_wT)
        call self%hydraulic%calc_K_wP(material_id, state, K_wP)

        ! --- 重力項の計算 ---
        ! K_wP は K/(rho*g) なので，重力項(透水係数 K そのもの)を復元する
        ! gravity_term = K = K_wP * rho * g
        call self%thermal%calc_density_water(state, rho_w)
        gravity_term = K_wP * rho_w * g

        ! --- 流束の計算 (Darcy則: q = -K_wT*grad_T - K_wP*grad_P - K*grad_z) ---
        select case (computation_type)
        case (COMP_TYPE_2D_XY)
            water_flux%x = -K_wT * grad_T%x - K_wP * grad_P%x
            water_flux%y = -K_wT * grad_T%y - K_wP * grad_P%y
            water_flux%z = 0.0d0
        case (COMP_TYPE_2D_XZ)
            water_flux%x = -K_wT * grad_T%x - K_wP * grad_P%x
            water_flux%y = 0.0d0
            water_flux%z = -K_wT * grad_T%z - K_wP * grad_P%z - gravity_term ! Zを鉛直と仮定
        case (COMP_TYPE_3D)
            water_flux%x = -K_wT * grad_T%x - K_wP * grad_P%x
            water_flux%y = -K_wT * grad_T%y - K_wP * grad_P%y
            water_flux%z = -K_wT * grad_T%z - K_wP * grad_P%z - gravity_term ! Zを鉛直と仮定
        end select

    end subroutine calc_water_flux_ftdss

    module subroutine calc_vapor_flux_ftdss(self, material_id, state, grad_T, grad_P, water_flux)
        implicit none
        class(type_ftdss), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        type(type_coordinate_dp), intent(in) :: grad_T, grad_P
        type(type_coordinate_dp), intent(inout) :: water_flux

        integer(int32) :: computation_type

        real(real64) :: K_vT, K_vP

        call self%domain%get_computation_dimension(computation_type)

        call self%hydraulic%calc_K_vT(material_id, state, K_vT)
        call self%hydraulic%calc_K_vP(material_id, state, K_vP)

        select case (computation_type)
        case (COMP_TYPE_2D_XY)
            water_flux%x = -K_vT * grad_T%x - K_vP * grad_P%x
            water_flux%y = -K_vT * grad_T%y - K_vP * grad_P%y
            water_flux%z = 0.0d0
        case (COMP_TYPE_2D_XZ)
            water_flux%x = -K_vT * grad_T%x - K_vP * grad_P%x
            water_flux%y = 0.0d0
            water_flux%z = -K_vT * grad_T%z - K_vP * grad_P%z
        case (COMP_TYPE_3D)
            water_flux%x = -K_vT * grad_T%x - K_vP * grad_P%x
            water_flux%y = -K_vT * grad_T%y - K_vP * grad_P%y
            water_flux%z = -K_vT * grad_T%z - K_vP * grad_P%z
        end select

    end subroutine calc_vapor_flux_ftdss

    module subroutine solve_time_step_initial_setup_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        call self%controls%iteration%increment_total()
        call self%controls%iteration%reset_nonlinear()

    end subroutine solve_time_step_initial_setup_ftdss

    module subroutine solve_time_step_setup_ftdss(self, prescribe_bc)
        implicit none
        class(type_ftdss), intent(inout) :: self
        logical, intent(inout) :: prescribe_bc

        integer(int32) :: iter

        call self%controls%iteration%increment_nonlinear()
        call self%controls%iteration%get_nonlinear_iter(iter)

        if (iter == 1) then
            prescribe_bc = .true.
        else
            prescribe_bc = .false.
        end if

        call self%calc_gradient_temperature()
        call self%calc_gradient_pressure()

    end subroutine solve_time_step_setup_ftdss

    module subroutine solve_time_step_check_convergence_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout), target :: self

        integer(int32) :: iter

        real(real64), pointer, contiguous, dimension(:) :: F_values => null()
        real(real64), pointer, contiguous, dimension(:) :: u_values => null()

        F_values => self%F%get_data()
        u_values => self%u%get_data()

        call self%controls%iteration%get_nonlinear_iter(iter)

        if (iter == 1) then
            call self%controls%iteration%set_initial_norms(F_values, u_values)
        else
            call self%controls%iteration%check_convergence(F_values, u_values)
        end if

        nullify (F_values)
        nullify (u_values)

    end subroutine solve_time_step_check_convergence_ftdss

    module subroutine solve_time_step_ftdss(self, is_step_converged)
        implicit none
        class(type_ftdss), intent(inout) :: self
        logical, intent(inout) :: is_step_converged
        logical :: prescribe_bc

        ! 1. 初期化
        is_step_converged = .false.
        call self%solve_time_step_initial_setup()

        ! 2. 非線形反復ループ（Newtonループ）
        !    収束判定は check_convergence で状態を更新し、ここ(should_continue)で抜ける
        do while (self%controls%iteration%should_continue())

            ! 2.1 セットアップ (iter更新, BCフラグ設定, 勾配計算など)
            call self%solve_time_step_setup(prescribe_bc)

            ! 2.2 行列・残差のアセンブル
            call self%assemble()

            ! 2.3 境界条件の適用
            call self%apply_bc(prescribe_bc)

            ! 2.4 線形ソルバー (K * u = F)
            call self%solve()

            ! 2.5 収束判定
            call self%solve_time_step_check_convergence()

            ! 2.6 解の更新 (U <= U + delta)
            call self%reflect_variables()

        end do

        ! 3. 最終的な収束状態を取得して返す
        is_step_converged = self%controls%iteration%has_converged()

    end subroutine solve_time_step_ftdss
end submodule ftdss_compute
