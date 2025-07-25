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

        call structure%KT_star_0%initialize(domain)

        structure%KT_l = structure%KT_star_0
        structure%KT_old = structure%KT_star_0
        structure%CT_l = structure%KT_star_0
        structure%order = input%basic%solver_settings%bdf_order

        allocate (structure%CT_old(structure%order))
        do i = 1, structure%order
            structure%CT_old(i) = structure%KT_star_0
        end do

        call allocate_array(structure%FT, num_nodes)
        call allocate_array(structure%FT_old, num_nodes)
        call allocate_array(structure%PHIT, num_nodes)
        call allocate_array(structure%PHIT_old, num_nodes)

        call structure%T%initialize(num_nodes, structure%order)

        if (associated(structure%assemble_mass)) nullify (structure%assemble_mass)
        if (associated(structure%assemble_diffusive)) nullify (structure%assemble_diffusive)

        if (input%basic%solver_settings%parallel_settings%threads%is_parallel) then
            structure%assemble_mass => Assemble_Mass_Heat_1_Parallel
            structure%assemble_diffusive => Assemble_Diffusion_Heat_1_Parallel
        else
            structure%assemble_mass => Assemble_Mass_Heat_1
            structure%assemble_diffusive => Assemble_Diffusion_Heat_1
        end if

        !---------------------------------------------------------------------------------------------------------------------------
        ! 線形求解ソルバーの設定
        !---------------------------------------------------------------------------------------------------------------------------
        structure%solver = create_solver(input, "thermal", structure%KT_star_0, num_nodes)
        !---------------------------------------------------------------------------------------------------------------------------

    end function construct_type_thermal_crs

    ! module subroutine type_thermal_crs_Update(self, NodeBelonging, arr_phi)
    !     implicit none
    !     class(type_thermal_crs), intent(inout) :: self
    !     type(Belonging), intent(inout), optional :: NodeBelonging(:)
    !     real(real64), intent(inout) :: arr_phi(:)

    !     call self%Ice(1)%f%Update_Ice(NodeBelonging=NodeBelonging, &
    !                                   arr_T=self%T%pre(:), &
    !                                   arr_phi=arr_phi(:), &
    !                                   Density=self%DEN, &
    !                                   arr_Cp=self%HTC%value(:, 1), &
    !                                   arr_Qw=self%Qw%pre(:), &
    !                                   arr_Qice=self%Qice%pre(:), &
    !                                   arr_Si=self%Si)

    !     call self%THC%Update(NodeBelonging, 1.0d0 - arr_phi(:), self%Qw%pre, self%Qice%pre)
    !     call self%SPH%Update(NodeBelonging, 1.0d0 - arr_phi(:), self%Qw%pre, self%Qice%pre)
    !     call self%DEN%Update(NodeBelonging, 1.0d0 - arr_phi(:), self%Qw%pre, self%Qice%pre)
    !     call self%HTC%Update(NodeBelonging=NodeBelonging, &
    !                          arr_phi1=1.0d0 - arr_phi(:), &
    !                          arr_phi2=self%Qw%pre, &
    !                          arr_phi3=self%Qice%pre, &
    !                          Ice=self%Ice(1)%f, &
    !                          Temperature=self%T%pre(:), &
    !                          Density=self%DEN)
    ! end subroutine type_thermal_crs_Update

    module subroutine assemble_type_thermal_crs(self, domain, property, porosity, time, iteration)
        implicit none
        ! Arguments
        class(type_thermal_crs), intent(inout) :: self
        type(type_domain), intent(inout) :: domain
        type(type_proereties_manager), intent(inout) :: property
        real(real64), intent(in) :: porosity(:)
        type(type_time), intent(in) :: time
        type(type_iteration), intent(in) :: iteration

        ! Local variables
        integer(int32) :: actual_order
        real(real64) :: dt_n

        real(real64), allocatable :: coefficients(:)

        ! Initialization
        self%KT_star_0%val(:) = 0.0d0
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

            self%KT_star_0 = coefficients(0) * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coefficients(1) * (self%CT_l * self%T%old(:, 1))

        case (2) ! BDF2
            dt_n = time%dt
            call time%get_time_coefficients(actual_order, coefficients)

            self%KT_star_0 = coefficients(0) * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coefficients(1) * (self%CT_l * self%T%old(:, 1)) + &
                        -coefficients(2) * (self%CT_l * self%T%old(:, 2))

        case (3)
            dt_n = time%dt
            call time%get_time_coefficients(actual_order, coefficients)

            self%KT_star_0 = coefficients(0) * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coefficients(1) * (self%CT_l * self%T%old(:, 1)) + &
                        -coefficients(2) * (self%CT_l * self%T%old(:, 2)) + &
                        -coefficients(3) * (self%CT_l * self%T%old(:, 3))

        case (4)
            dt_n = time%dt
            call time%get_time_coefficients(actual_order, coefficients)

            self%KT_star_0 = coefficients(0) * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coefficients(1) * (self%CT_l * self%T%old(:, 1)) + &
                        -coefficients(2) * (self%CT_l * self%T%old(:, 2)) + &
                        -coefficients(3) * (self%CT_l * self%T%old(:, 3)) + &
                        -coefficients(4) * (self%CT_l * self%T%old(:, 4))
        case (5)
            dt_n = time%dt
            call time%get_time_coefficients(actual_order, coefficients)

            self%KT_star_0 = coefficients(0) * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coefficients(1) * (self%CT_l * self%T%old(:, 1)) + &
                        -coefficients(2) * (self%CT_l * self%T%old(:, 2)) + &
                        -coefficients(3) * (self%CT_l * self%T%old(:, 3)) + &
                        -coefficients(4) * (self%CT_l * self%T%old(:, 4)) + &
                        -coefficients(5) * (self%CT_l * self%T%old(:, 5))
        case (6)
            dt_n = time%dt
            call time%get_time_coefficients(actual_order, coefficients)

            self%KT_star_0 = coefficients(0) * self%CT_l + dt_n * self%KT_l
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
            call self%solver%solve(self%KT_star_0, self%PHIT, self%T%new(:), stat)
            self%T%dif(:) = self%T%new(:) - self%T%pre(:)
        case ("newton", "modified_newton")
            call self%solver%solve(self%KT_star_0, self%PHIT, self%T%dif(:), stat)
        end select
        call self%solver%check(stat, time%get_time())
    end subroutine solve_type_thermal_crs

end submodule Main_Thermal_3Phase
