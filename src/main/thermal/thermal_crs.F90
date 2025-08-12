submodule(Main_Thermal) Main_Thermal_3Phase
    implicit none
contains
    module function construct_type_thermal_crs(input, coordinate, domain) result(structure)
        implicit none
        class(abst_thermal), allocatable :: structure
        type(type_input), intent(inout) :: input
        type(type_dp_3d), intent(inout), pointer :: coordinate
        type(type_domain), intent(inout) :: domain

        integer(int32) :: i
        integer(int32) :: num_nodes

        integer(int32) :: ierr

        if (allocated(structure)) deallocate (structure)
        allocate (type_thermal_crs :: structure)

        num_nodes = domain%get_num_nodes()

        call structure%KT_star%initialize(domain)

        structure%KT_l = structure%KT_star
        structure%KT_old = structure%KT_star
        structure%CT_l = structure%KT_star
        structure%order = input%basic%solver_settings%bdf_order

        ! allocate (structure%CT_old(structure%order))
        ! do i = 1, structure%order
        !     structure%CT_old(i) = structure%KT_star
        ! end do

        call allocate_array(structure%FT, num_nodes)
        call allocate_array(structure%FT_old, num_nodes)
        call allocate_array(structure%PHIT, num_nodes)
        call allocate_array(structure%PHIT_old, num_nodes)

        call structure%T%initialize(num_nodes, structure%order)

        call structure%Qw%initialize(num_nodes, structure%order)
        call structure%Qice%initialize(num_nodes, structure%order)
        call structure%Si%initialize(num_nodes, structure%order)

        if (associated(structure%assemble_global)) nullify (structure%assemble_global)

        select case (input%basic%solver_settings%nonlinear_solver%method)
        case ("none")
            if (input%basic%solver_settings%parallel_settings%threads%is_parallel) then
                structure%assemble_global => assemble_thermal_matrices_1_parallel
            else
                structure%assemble_global => assemble_thermal_matrices_1
            end if
        end select

        !---------------------------------------------------------------------------------------------------------------------------
        ! 線形求解ソルバーの設定
        !---------------------------------------------------------------------------------------------------------------------------
        structure%solver = create_solver(input, "thermal", structure%KT_star, num_nodes)
        !---------------------------------------------------------------------------------------------------------------------------

    end function construct_type_thermal_crs

    module subroutine update_type_thermal_crs(self, domain, property, porosity)
        implicit none
        class(type_thermal_crs), intent(inout) :: self
        type(type_domain), intent(inout), target :: domain
        type(type_properties_manager), intent(inout) :: property
        real(real64), intent(in) :: porosity(:)

        ! --- 変数宣言 ---
        integer(int32) :: i, j, n_nodes
        integer(int32) :: element_id, group_id, num_elem_nodes
        integer(int32), pointer :: neighbor_list(:) => null()

        real(real64) :: total_weighted_qw, total_weight, weight, temp_qw, element_area
        real(real64), allocatable :: original_temperature(:), original_porosity(:), temp_Qws(:)
        type(type_gauss_point_state) :: state

        ! --- 初期化 ---
        n_nodes = domain%get_num_nodes()

        ! --- 前処理：リオーダリング対応と一時配列の確保 ---
        allocate (temp_Qws(n_nodes))
        if (domain%reordering%get_algorithm_name() == "none") then
            allocate (original_temperature, source=self%T%pre(:))
            allocate (original_porosity, source=porosity)
        else
            allocate (original_temperature, mold=self%T%pre)
            allocate (original_porosity, mold=porosity)
            call domain%reordering%to_original_value(self%T%pre(:), original_temperature)
            call domain%reordering%to_original_value(porosity, original_porosity)
        end if

        ! --- メイン計算ループ (OpenMPによる並列化) ---
        !$omp parallel do private(j, state, neighbor_list, element_id, group_id, temp_qw, element_area, num_elem_nodes, weight) &
        !$omp default(shared) schedule(static)
        do i = 1, n_nodes
            ! この節点の状態を設定
            state%temperature = original_temperature(i)
            state%porosity = original_porosity(i)

            total_weighted_qw = 0.0d0
            total_weight = 0.0d0

            ! ★ domainオブジェクトから隣接要素リストへのポインタを取得
            neighbor_list => domain%map_node_to_element%get_list(i)
            if (.not. associated(neighbor_list)) cycle

            ! 隣接する全要素でループし、重み付き和を計算
            do j = 1, size(neighbor_list)
                element_id = neighbor_list(j)

                ! a) 重みを計算
                element_area = domain%elements(element_id)%e%get_area()
                num_elem_nodes = domain%elements(element_id)%e%get_num_nodes()
                weight = element_area / dble(num_elem_nodes)

                ! b) この要素の物性でQwを計算
                group_id = domain%elements(element_id)%e%get_group()
                temp_qw = property%get_qw(state, group_id)

                ! 物理的な範囲に収める
                if (temp_qw > state%porosity) temp_qw = state%porosity
                if (temp_qw < 0.0d0) temp_qw = 0.0d0

                ! c) 重み付き和と重みの合計を更新
                total_weighted_qw = total_weighted_qw + temp_qw * weight
                total_weight = total_weight + weight
            end do

            ! 重み付き平均を計算し、一時配列に格納
            if (total_weight > 1.0d-12) then
                temp_Qws(i) = total_weighted_qw / total_weight
            else
                temp_Qws(i) = 0.0d0
            end if
        end do
        !$omp end parallel do

        ! --- 後処理：結果の格納とメモリ解放 ---
        deallocate (original_temperature, original_porosity)

        call domain%reordering%to_reordered_value(temp_Qws(:), self%Qw%pre)
        ! end if
        deallocate (temp_Qws)

        self%Qw%dif(:) = self%Qw%pre(:) - self%Qw%old(:, 1)
        self%Qice%pre(:) = porosity(:) - self%Qw%pre(:)
        self%Qice%dif(:) = self%Qice%pre(:) - self%Qice%old(:, 1)
        self%Si%pre(:) = (porosity(:) - self%Qice%pre(:)) / porosity(:)

    end subroutine update_type_thermal_crs

    module subroutine shift_type_thermal_crs(self)
        implicit none
        class(type_thermal_crs), intent(inout) :: self

        call self%T%shift()
        call self%Qw%shift()
        call self%Qice%shift()
        call self%Si%shift()

    end subroutine shift_type_thermal_crs

    module subroutine assemble_type_thermal_crs(self, domain, property, porosity, time, iteration)
        implicit none
        ! Arguments
        class(type_thermal_crs), intent(inout) :: self
        type(type_domain), intent(inout) :: domain
        type(type_properties_manager), intent(inout) :: property
        real(real64), intent(in) :: porosity(:)
        type(type_time), intent(in) :: time
        type(type_iteration), intent(in) :: iteration

        ! Local variables
        integer(int32) :: actual_order
        real(real64) :: dt_n

        real(real64), allocatable :: coefficients(:)

        ! Initialization
        self%KT_star%val(:) = 0.0d0
        self%PHIT(:) = 0.0d0
        self%KT_l%val(:) = 0.0d0
        self%CT_l%val(:) = 0.0d0

        ! --------------------------------------------------------------------------
        ! 履歴に基づいて、このステップで使用する実際のBDF次数を決定する
        ! --------------------------------------------------------------------------
        actual_order = min(self%order, iteration%get_step())
        call allocate_array(coefficients, bounds=[0, actual_order])

        ! --------------------------------------------------------------------------
        ! 剛性行列と質量行列を組み立てる
        ! --------------------------------------------------------------------------
        call self%assemble_global(self%CT_l, self%KT_l, domain, self%T%pre, porosity, property)
        ! call self%assemble_mass(self%CT_l, domain, self%T%pre, porosity, property)
        ! call self%assemble_diffusive(self%KT_l, domain, self%T%pre, porosity, property)

        ! --------------------------------------------------------------------------
        ! 決定されたBDFスキームに基づいて左辺行列(LHS)と右辺ベクトル(RHS)を構築する
        ! --------------------------------------------------------------------------
        select case (actual_order)
        case (1) ! BDF1 (Backward Euler)
            dt_n = time%dt
            call time%get_time_coefficients(actual_order, coefficients)

            self%KT_star = coefficients(0) * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coefficients(1) * (self%CT_l * self%T%old(:, 1))

        case (2) ! BDF2
            dt_n = time%dt
            call time%get_time_coefficients(actual_order, coefficients)

            self%KT_star = coefficients(0) * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coefficients(1) * (self%CT_l * self%T%old(:, 1)) + &
                        -coefficients(2) * (self%CT_l * self%T%old(:, 2))

        case (3)
            dt_n = time%dt
            call time%get_time_coefficients(actual_order, coefficients)

            self%KT_star = coefficients(0) * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coefficients(1) * (self%CT_l * self%T%old(:, 1)) + &
                        -coefficients(2) * (self%CT_l * self%T%old(:, 2)) + &
                        -coefficients(3) * (self%CT_l * self%T%old(:, 3))

        case (4)
            dt_n = time%dt
            call time%get_time_coefficients(actual_order, coefficients)

            self%KT_star = coefficients(0) * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coefficients(1) * (self%CT_l * self%T%old(:, 1)) + &
                        -coefficients(2) * (self%CT_l * self%T%old(:, 2)) + &
                        -coefficients(3) * (self%CT_l * self%T%old(:, 3)) + &
                        -coefficients(4) * (self%CT_l * self%T%old(:, 4))
        case (5)
            dt_n = time%dt
            call time%get_time_coefficients(actual_order, coefficients)

            self%KT_star = coefficients(0) * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coefficients(1) * (self%CT_l * self%T%old(:, 1)) + &
                        -coefficients(2) * (self%CT_l * self%T%old(:, 2)) + &
                        -coefficients(3) * (self%CT_l * self%T%old(:, 3)) + &
                        -coefficients(4) * (self%CT_l * self%T%old(:, 4)) + &
                        -coefficients(5) * (self%CT_l * self%T%old(:, 5))
        case (6)
            dt_n = time%dt
            call time%get_time_coefficients(actual_order, coefficients)

            self%KT_star = coefficients(0) * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coefficients(1) * (self%CT_l * self%T%old(:, 1)) + &
                        -coefficients(2) * (self%CT_l * self%T%old(:, 2)) + &
                        -coefficients(3) * (self%CT_l * self%T%old(:, 3)) + &
                        -coefficients(4) * (self%CT_l * self%T%old(:, 4)) + &
                        -coefficients(5) * (self%CT_l * self%T%old(:, 5)) + &
                        -coefficients(6) * (self%CT_l * self%T%old(:, 6))
        end select

        call deallocate_array(coefficients)

    end subroutine assemble_type_thermal_crs

    module subroutine solve_type_thermal_crs(self, time, iteration)
        implicit none
        class(type_thermal_crs), intent(inout) :: self
        type(type_time), intent(inout) :: time
        type(type_iteration), intent(inout) :: iteration

        integer(int32) :: stat

        select case (trim(iteration%get_algorithm_name()))
        case ("none")
            call self%solver%solve(self%KT_star, self%PHIT, self%T%new(:), stat)
            self%T%dif(:) = self%T%new(:) - self%T%pre(:)
        case ("newton", "modified_newton")
            call self%solver%solve(self%KT_star, self%PHIT, self%T%dif(:), stat)
        end select
        call self%solver%check(stat, time%get_time())
    end subroutine solve_type_thermal_crs

end submodule Main_Thermal_3Phase
