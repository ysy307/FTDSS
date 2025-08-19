submodule(Main_Thermal) main_thermal_3phase
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

        ! structure%KT_l = structure%KT_star
        ! structure%KT_old = structure%KT_star
        ! structure%CT_l = structure%KT_star
        structure%order = input%basic%solver_settings%bdf_order

        call allocate_array(structure%FT, num_nodes)
        ! call allocate_array(structure%FT_old, num_nodes)
        call allocate_array(structure%PHIT, num_nodes)
        ! call allocate_array(structure%PHIT_old, num_nodes)

        ! call structure%T%initialize(num_nodes, structure%order)

        print *, "Thermal CRS: num_nodes = ", num_nodes
        call structure%Qw%initialize(num_nodes, structure%order)
        call structure%Qice%initialize(num_nodes, structure%order)
        call structure%Si%initialize(num_nodes, structure%order)

        if (associated(structure%assemble_global)) nullify (structure%assemble_global)

        structure%algorithm = input%basic%solver_settings%nonlinear_solver%method
        select case (structure%algorithm)
        case ("none")
            if (input%basic%solver_settings%parallel_settings%threads%is_parallel) then
                structure%assemble_global => thermal_assemble_system_linear_1_parallel
            else
                structure%assemble_global => thermal_assemble_system_linear_1
            end if
        case ("picard")
            if (input%basic%solver_settings%parallel_settings%threads%is_parallel) then
                structure%assemble_global => thermal_assemble_system_linear_1_parallel
            else
                structure%assemble_global => thermal_assemble_system_linear_1
            end if
        end select

        !---------------------------------------------------------------------------------------------------------------------------
        ! 線形求解ソルバーの設定
        !---------------------------------------------------------------------------------------------------------------------------
        structure%solver = create_solver(input, "thermal", structure%KT_star, num_nodes)
        !---------------------------------------------------------------------------------------------------------------------------

    end function construct_type_thermal_crs

    module subroutine update_type_thermal_crs(self, domain, property, temperature, porosity)
        implicit none
        class(type_thermal_crs), intent(inout) :: self
        type(type_domain), intent(inout), target :: domain
        type(type_properties_manager), intent(inout) :: property
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)

        ! --- 変数宣言 ---
        integer(int32) :: i, j, n_nodes
        integer(int32) :: element_id, group_id, num_elem_nodes
        integer(int32), pointer :: neighbor_list(:) => null()

        real(real64) :: total_weighted_qw, total_weight, weight, temp_qw, element_area
        real(real64), allocatable :: temp_Qws(:)
        type(type_state) :: state

        ! --- 初期化 ---
        n_nodes = domain%get_num_nodes()

        ! --- メイン計算ループ (OpenMPによる並列化) ---
        !$omp parallel do private(j, state, neighbor_list, element_id, group_id, temp_qw, element_area, num_elem_nodes, weight, &
        !$omp total_weighted_qw, total_weight) &
        !$omp default(shared) schedule(static)
        do i = 1, n_nodes
            ! この節点の状態を設定
            state%temperature = temperature(i)
            state%porosity = porosity(i)

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
                self%Qw%pre(i) = total_weighted_qw / total_weight
            else
                self%Qw%pre(i) = 0.0d0
            end if
        end do
        !$omp end parallel do

        self%Qw%dif(:) = self%Qw%pre(:) - self%Qw%old(:, 1)
        self%Qice%pre(:) = porosity(:) - self%Qw%pre(:)
        self%Qice%dif(:) = self%Qice%pre(:) - self%Qice%old(:, 1)
        self%Si%pre(:) = (porosity(:) - self%Qice%pre(:)) / porosity(:)

    end subroutine update_type_thermal_crs

    module subroutine shift_type_thermal_crs(self)
        implicit none
        class(type_thermal_crs), intent(inout) :: self

        call self%Qw%shift()
        call self%Qice%shift()
        call self%Si%shift()

    end subroutine shift_type_thermal_crs

    module subroutine solve_type_thermal_crs(self, temperature, time, iteration)
        implicit none
        class(type_thermal_crs), intent(inout) :: self
        type(type_variable), intent(inout) :: temperature
        type(type_time), intent(inout) :: time
        type(type_iteration), intent(inout) :: iteration

        integer(int32) :: stat

        select case (trim(iteration%get_algorithm_name()))
        case ("none")
            call self%solver%solve(self%KT_star, self%PHIT, temperature%new(:), stat)
            temperature%dif(:) = temperature%new(:) - temperature%pre(:)
        case ("newton", "modified_newton", "picard")
            call self%solver%solve(self%KT_star, self%PHIT, temperature%dif(:), stat)
            temperature%new(:) = temperature%pre(:) + temperature%dif(:)
        end select
        call self%solver%check(stat, time%get_time())
    end subroutine solve_type_thermal_crs

    module subroutine compute_type_thermal_crs(self, domain, property, temperature, porosity, time, iteration, bc)
        implicit none
        ! Arguments
        class(type_thermal_crs), intent(inout) :: self
        type(type_domain), intent(inout) :: domain
        type(type_properties_manager), intent(inout) :: property
        type(type_variable), intent(inout) :: temperature
        type(type_variable), intent(inout) :: porosity
        type(type_time), intent(inout) :: time
        type(type_iteration), intent(inout) :: iteration
        type(type_bc), intent(inout) :: bc

        ! Local variables
        integer(int32) :: actual_order
        integer(int32) :: mode_bc

        call time%profile_start("Setup")
        select case (self%algorithm)
        case ("none")
            mode_bc = mode_value
        case default
            mode_bc = mode_nr
        end select
        print *, mode_bc
        stop
        call time%profile_stop("Setup")

        NR_LOOP_THERMAL: do while (iteration%should_continue())
            call time%profile_start("Setup")
            call iteration%increment_step()
            self%KT_star%val(:) = 0.0d0
            self%PHIT(:) = 0.0d0
            call time%profile_stop("Setup")

            call time%profile_start("Assemble")
            actual_order = min(self%order, iteration%get_step())
            call self%assemble_global(self%KT_star, self%PHIT, domain, temperature, porosity, property, time, actual_order)
            call time%profile_stop("Assemble")

            call time%profile_start("Setup")
            call bc%apply_crs(boundary_target='thermal', &
                              current_time=time%get_time(), &
                              A=self%KT_star, &
                              b=self%PHIT, &
                              Domain=Domain, &
                              mode=mode_bc)
            if (iteration%get_step() == 1) call iteration%set_initial_norms(res_vec=self%PHIT)
            call time%profile_stop("Setup")

            call time%profile_start("Solve")
            call self%solve(temperature, time, iteration)
            call time%profile_stop("Solve")

            call time%profile_start("Setup")
            if (iteration%get_step() == 1) call iteration%set_initial_norms(upd_vec=temperature%dif(:))
            call iteration%check_convergence(self%PHIT, temperature%dif(:))
            temperature%pre(:) = temperature%new(:)
            call time%profile_stop("Setup")
        end do NR_LOOP_THERMAL

    end subroutine compute_type_thermal_crs

end submodule main_thermal_3phase
