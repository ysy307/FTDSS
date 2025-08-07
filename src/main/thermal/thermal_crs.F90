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

        call allocate_array(structure%Qw, num_nodes)
        call allocate_array(structure%D_Qice, num_nodes)
        call allocate_array(structure%Qice, num_nodes)
        call allocate_array(structure%Si, num_nodes)

        if (associated(structure%assemble_mass)) nullify (structure%assemble_mass)
        if (associated(structure%assemble_diffusive)) nullify (structure%assemble_diffusive)

        select case (input%basic%solver_settings%nonlinear_solver%method)
        case ("none")
            if (input%basic%solver_settings%parallel_settings%threads%is_parallel) then
                structure%assemble_mass => Assemble_Mass_Heat_1_Parallel
                structure%assemble_diffusive => Assemble_Diffusion_Heat_1_Parallel
            else
                structure%assemble_mass => Assemble_Mass_Heat_1
                structure%assemble_diffusive => Assemble_Diffusion_Heat_1
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
        type(type_domain), intent(inout) :: domain
        type(type_properties_manager), intent(inout) :: property
        real(real64), intent(in) :: porosity(:)

        integer(int32) :: i
        integer(int32) :: group_id

        real(real64), allocatable :: original_temperature(:)
        real(real64), allocatable :: original_porosity(:)
        type(type_gauss_point_state) :: state

        if (domain%reordering%get_algorithm_name() == "none") then
            allocate (original_temperature, source=self%T%pre(:))
            allocate (original_porosity, source=porosity)

        else
            allocate (original_temperature, mold=self%T%pre(:))
            allocate (original_porosity, mold=porosity)
            call domain%reordering%to_original_value(self%T%pre(:), original_temperature)
            call domain%reordering%to_original_value(porosity, original_porosity)
        end if

        do i = 1, size(original_porosity)
            state%temperature = original_temperature(i)
            state%porosity = original_porosity(i)
            group_id = domain%elements(i)%e%get_group()
            state%water_content = property%get_qw(state, group_id)
            if (state%water_content > state%porosity) state%water_content = state%porosity
            if (state%water_content < 0.0d0) state%water_content = 0.0d0

        end do

    end subroutine update_type_thermal_crs

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
        call self%assemble_mass(self%CT_l, domain, self%T%pre, porosity, property)
        call self%assemble_diffusive(self%KT_l, domain, self%T%pre, porosity, property)

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
