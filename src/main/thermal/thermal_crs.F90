submodule(main_thermal) main_thermal_crs
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
        integer(int32), allocatable :: row_ptr(:), col_ind(:)

        integer(int32) :: ierr

        if (allocated(structure)) deallocate (structure)
        allocate (type_thermal_crs :: structure)

        num_nodes = domain%get_num_nodes()
        call domain%node_adjacency%get_csr(row_ptr, col_ind)

        print *, 1
        print *, row_ptr(:)
        print *, col_ind(:)
        call structure%KT_star%initialize(num_nodes, row_ptr, col_ind)
        print *, 2
        structure%order = input%basic%solver_settings%bdf_order

        call allocate_array(structure%FT, num_nodes)
        call allocate_array(structure%PHIT, num_nodes)

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

    module subroutine update_type_thermal_crs(self, domain, property, temperature, porosity, controls)
        implicit none
        class(type_thermal_crs), intent(inout) :: self
        type(type_domain), intent(inout), target :: domain
        type(type_properties_manager), intent(inout) :: property
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(type_controls), intent(in) :: controls

        ! --- 変数宣言 ---
        integer(int32) :: i, j, num_nodes
        integer(int32) :: element_id, material_id, num_elem_nodes
        integer(int32), pointer :: neighbor_list(:) => null()
        real(real64) :: total_weight, weight, temp_qw, element_area
        type(type_state) :: state
        integer(int32) :: actual_order, iO
        real(real64) :: Qw_hist_i
        real(real64), allocatable :: coefficients(:)
        real(real64), allocatable :: ratio_densities(:)
        real(real64) :: total_weighted_qw, total_weighted_density_ratio, density_ratio
        type(type_phase_property) :: density

        ! --- 初期化 ---
        num_nodes = domain%get_num_nodes()
        call allocate_array(ratio_densities, num_nodes)

        ! ★ BDFの次数と係数を取得
        actual_order = min(self%order, controls%iteration%get_iter())
        call allocate_array(coefficients, bounds=[0, actual_order])
        call controls%time%get_time_coefficients(actual_order, coefficients)

        ! --- メイン計算ループ (OpenMPによる並列化) ---
        !$omp parallel do private(j, state, neighbor_list, element_id, material_id, temp_qw, element_area, &
        !$omp                     num_elem_nodes, weight, total_weighted_qw, total_weight, &
        !$omp                     total_weighted_density_ratio, density_ratio, density)
        do i = 1, num_nodes
            state%temperature = temperature(i)
            state%porosity = porosity(i)
            total_weighted_qw = 0.0d0
            total_weighted_density_ratio = 0.0d0
            total_weight = 0.0d0

            neighbor_list => domain%map_node_to_element%get_list(i)
            if (.not. associated(neighbor_list)) cycle

            do j = 1, size(neighbor_list)
                element_id = neighbor_list(j)
                material_id = domain%elements(element_id)%e%get_group()
                if (.not. controls%is_target(calc_thermal, material_id)) cycle

                element_area = domain%elements(element_id)%e%get_geometry()
                num_elem_nodes = domain%elements(element_id)%e%get_num_nodes()
                density = property%get_phase_dens(material_id)
                weight = element_area / dble(num_elem_nodes)

                density_ratio = density%water / density%ice
                temp_qw = property%calc_qw(material_id, state)
                total_weighted_qw = total_weighted_qw + temp_qw * weight
                total_weighted_density_ratio = total_weighted_density_ratio + density_ratio * weight
                total_weight = total_weight + weight
            end do

            if (total_weight > 1.0d-12) then
                self%Qw%pre(i) = total_weighted_qw / total_weight
                ratio_densities(i) = total_weighted_density_ratio / total_weight
            else
                self%Qw%pre(i) = 0.0d0
                ratio_densities(i) = 0.0d0
            end if
        end do
        !$omp end parallel do

        !----------------------------------------------------------------------------------
        ! ★ 質量保存則の適用とBDF履歴項を使った計算
        !----------------------------------------------------------------------------------
        self%Qice%pre(:) = ratio_densities(:) * (porosity(:) - self%Qw%pre(:))
        where (self%Qice%pre(:) < 0.0d0) self%Qice%pre(:) = 0.0d0

        do i = 1, num_nodes
            Qw_hist_i = 0.0d0
            do iO = 1, actual_order
                Qw_hist_i = Qw_hist_i + coefficients(iO) * self%Qw%old(i, iO)
            end do
            self%Qw%dif(i) = coefficients(0) * self%Qw%pre(i) + Qw_hist_i
            self%Qice%dif(i) = -ratio_densities(i) * self%Qw%dif(i)
        end do

        where (porosity(:) > 1.0d-12)
            self%Si%pre(:) = self%Qice%pre(:) / porosity(:)
        elsewhere
            self%Si%pre(:) = 0.0d0
        end where

        deallocate (coefficients)
        deallocate (ratio_densities)

    end subroutine update_type_thermal_crs

    module subroutine shift_type_thermal_crs(self)
        implicit none
        class(type_thermal_crs), intent(inout) :: self

        call self%Qw%shift()
        call self%Qice%shift()
        call self%Si%shift()

    end subroutine shift_type_thermal_crs

    module subroutine solve_type_thermal_crs(self, temperature, controls)
        implicit none
        class(type_thermal_crs), intent(inout) :: self
        type(type_variable), intent(inout) :: temperature
        type(type_controls), intent(in) :: controls

        integer(int32) :: stat

        select case (trim(controls%iteration%get_algorithm_name()))
        case ("none")
            call self%solver%solve(self%KT_star, self%PHIT, temperature%new(:), stat)
            temperature%dif(:) = temperature%new(:) - temperature%pre(:)
        case ("newton", "modified_newton", "picard")
            call self%solver%solve(self%KT_star, self%PHIT, temperature%dif(:), stat)
            temperature%new(:) = temperature%pre(:) + temperature%dif(:)
        end select
        call self%solver%check(stat, controls%time%get_time())
    end subroutine solve_type_thermal_crs

    module subroutine compute_type_thermal_crs(self, domain, property, temperature, porosity, controls, bc)
        implicit none
        ! Arguments
        class(type_thermal_crs), intent(inout) :: self
        type(type_domain), intent(inout) :: domain
        type(type_properties_manager), intent(in) :: property
        type(type_variable), intent(inout) :: temperature
        type(type_variable), intent(inout) :: porosity
        type(type_controls), intent(inout) :: controls
        type(type_bc), intent(inout) :: bc

        ! Local variables
        integer(int32) :: actual_order
        integer(int32) :: mode_bc

        call controls%time%profile_start("Setup")
        select case (self%algorithm)
        case ("none")
            mode_bc = mode_value
        case default
            mode_bc = mode_nr
        end select

        call controls%time%profile_stop("Setup")

        NR_LOOP_THERMAL: do while (controls%iteration%should_continue())
            call controls%time%profile_start("Setup")
            call controls%iteration%increment_step()
            call controls%time%profile_stop("Setup")

            call controls%time%profile_start("Assemble")
            actual_order = min(self%order, controls%iteration%get_step())
            call self%assemble_global(self%KT_star, self%PHIT, domain, temperature, porosity, property, controls, actual_order)
            call controls%time%profile_stop("Assemble")

            call controls%time%profile_start("Setup")
            call bc%apply_crs(boundary_target='thermal', &
                              current_time=controls%time%get_time(), &
                              A=self%KT_star, &
                              b=self%PHIT, &
                              Domain=Domain, &
                              mode=mode_bc)
            if (controls%iteration%get_step() == 1) call controls%iteration%set_initial_norms(res_vec=self%PHIT)
            call controls%time%profile_stop("Setup")

            call controls%time%profile_start("Solve")
            call self%solve(temperature, controls)
            call controls%time%profile_stop("Solve")

            call controls%time%profile_start("Setup")
            if (controls%iteration%get_step() == 1) call controls%iteration%set_initial_norms(upd_vec=temperature%dif(:))
            call controls%iteration%check_convergence(self%PHIT, temperature%dif(:))
            temperature%pre(:) = temperature%new(:)
            call controls%time%profile_stop("Setup")
        end do NR_LOOP_THERMAL

    end subroutine compute_type_thermal_crs

end submodule main_thermal_crs
