submodule(main_ftdss) ftdss_compute
    implicit none
contains

    ! --------------------------------------------------------------------------
    ! 機能: 要素ごとの計算値（水分量や流束）を節点値にスムージングする
    ! 手法: 体積重み付き平均 (Lumped Mass Matrix的なアプローチ)
    ! --------------------------------------------------------------------------
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

        num_nodes = self%domain%get_num_nodes()

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
                    call self%domain%get_geometry(i_elem, elem_vol)

                    ! 状態変数の更新と取得
                    call self%domain%get_material_id(i_elem, material_id)
                    call self%set_state(i_node, i_elem, state)

                    ! Stateから各相の体積含水率などを取得
                    call state%water_content%get(elem_qw)
                    call state%ice_content%get(elem_qi)
                    call state%air_content%get(elem_qa)
                    call state%vapor_content%get(elem_qv)

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
                self%Qw%new(i_node) = sum_qw_vol / sum_vol
                self%Qi%new(i_node) = sum_qi_vol / sum_vol
                self%Qa%new(i_node) = sum_qa_vol / sum_vol
                self%Qv%new(i_node) = sum_qv_vol / sum_vol
            else
                ! 孤立節点等の処理
                self%Qw%new(i_node) = 0.0d0
                self%Qi%new(i_node) = 0.0d0
                self%Qa%new(i_node) = 0.0d0
                self%Qv%new(i_node) = 0.0d0
            end if

        end do

        call self%controls%profiler%stop("Setup")

    end subroutine update_variables_ftdss

    module subroutine solve_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        class(abst_matrix), pointer :: J_ptr => null()
        type(type_vector_dp), pointer :: R_ptr => null()
        type(type_vector_dp), pointer :: delta_prt => null()

        call self%controls%profiler%start("Solve")

        J_ptr => self%J%get_matrix()
        R_ptr => self%R%get_vector()
        delta_prt => self%delta%get_vector()

        call self%solver%solve(J_ptr, R_ptr, delta_prt)
        call self%solver%check()

        J_ptr => null()
        R_ptr => null()
        delta_prt => null()

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

        num_elements = self%domain%get_num_elements()
        num_total_nodes = self%domain%get_num_nodes()
        dim = self%domain%get_computation_dimension()

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
            allocate (dpsi_dx(n_nodes_elem, dim))

            elem_u(:) = values_vec(p_conn(:))

            ! 座標取得 (allocatable引数)
            call self%domain%get_element_coordinate(i, node_coords)

            do p = 1, n_gauss
                r = fe_gauss_pts(p)

                call fe%calc_shape_data(r, node_coords, psi, dpsi_dx, det_j)
                w_vol = fe_weights(p) * det_j

                gauss_grad = 0.0d0
                do d = 1, dim
                    gauss_grad(d) = dot_product(elem_u, dpsi_dx(:, d))
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

        call self%calc_gradient(self%temperature%new, self%temperature%grad)

    end subroutine calc_gradient_temperature_ftdss

    module subroutine calc_gradient_pressure_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        call self%calc_gradient(self%pressure%new, self%pressure%grad)

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

        computation_type = self%domain%get_computation_type()

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

        computation_type = self%domain%get_computation_type()

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

        call self%controls%iteration%reset_nonlinear()

    end subroutine solve_time_step_initial_setup_ftdss

    module subroutine solve_time_step_setup_ftdss(self, prescribe_bc)
        implicit none
        class(type_ftdss), intent(inout) :: self
        logical, intent(inout) :: prescribe_bc

        integer(int32) :: iter

        call self%controls%iteration%get_nonlinear_iter(iter)

        if (iter == 1) then
            prescribe_bc = .true.
        else
            prescribe_bc = .false.
        end if

        ! 2.1．物理量の勾配計算など，アセンブル前の準備
        ! 前の反復（またはタイムステップ）で得られた状態量から勾配等を更新する．
        call self%calc_gradient_temperature()
        call self%calc_gradient_pressure()

    end subroutine solve_time_step_setup_ftdss

    module subroutine solve_time_step_ftdss(self, is_step_converged)
        implicit none
        class(type_ftdss), intent(inout) :: self
        logical, intent(inout) :: is_step_converged

        logical :: prescribe_bc

        ! 1．初期化
        ! 反復計算の開始前に必要な変数を初期化する．
        is_step_converged = .false.
        call self%solve_time_step_initial_setup()

        ! 2．非線形反復ループ（Newtonループ）開始
        do while (self%controls%iteration%should_continue())

            call self%solve_time_step_setup(prescribe_bc)

            ! 2.2．大域行列（Jacobian）と残差ベクトル（Residual）のアセンブル
            ! 各要素で局所行列を作成し，大域行列に足し合わせる．
            call self%assemble()

            ! 2.3．境界条件の適用
            ! ディリクレ境界条件等を連立方程式に反映させる．
            call self%apply_bc(prescribe_bc)

            ! 2.4．収束判定
            ! 残差ベクトルのノルムや解の更新量をチェックする．
            ! 収束していればループを抜け，成功フラグを立てる．
            ! if (check_convergence(self%R)) then
            !     is_step_converged = .true.
            !     exit
            ! end if

            ! 2.5．線形ソルバーの実行
            ! J * delta = -R を解き，修正量 delta を求める．
            call self%solve()

            ! 2.6．解（主変数）の更新
            ! Temperature, Pressure 等を delta を用いて更新する．
            call self%reflect_variables()

        end do

        ! 3．ループ終了後の処理
        ! 収束しなかった場合の警告や，収束した場合の後処理を行う．

    end subroutine solve_time_step_ftdss
end submodule ftdss_compute
