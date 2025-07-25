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
        real(real64) :: dt_n, dt_nm1, dt_nm2, dt_nm3, dt_nm4, dt_nm5
        real(real64) :: rho1, rho2, rho3, rho4, rho5
        real(real64) :: coef_c0, coef_c1, coef_c2, coef_c3, coef_c4, coef_c5, coef_c6
        real(real64) :: coef_k

        ! Initialization
        self%KT_star_0%val(:) = 0.0d0
        self%PHIT(:) = 0.0d0
        self%KT_l%val(:) = 0.0d0
        self%CT_l%val(:) = 0.0d0

        ! --------------------------------------------------------------------------
        ! 履歴に基づいて、このステップで使用する実際のBDF次数を決定する
        ! --------------------------------------------------------------------------
        actual_order = min(self%order, iteration%step)

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

            coef_c0 = 1.0d0
            coef_c1 = -1.0d0

            self%KT_star_0 = coef_c0 * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coef_c1 * (self%CT_l * self%T%old(:, 1))

        case (2) ! BDF2
            dt_n = time%dt
            dt_nm1 = time%dt_old(1)
            rho1 = dt_n / dt_nm1

            coef_c0 = (2.0d0 * rho1 + 1.0d0) / (rho1 + 1.0d0)
            coef_c1 = -(rho1 + 1.0d0)
            coef_c2 = rho1**2.0d0 / (rho1 + 1.0d0)

            self%KT_star_0 = coef_c0 * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coef_c1 * (self%CT_l * self%T%old(:, 1)) + &
                        -coef_c2 * (self%CT_l * self%T%old(:, 2))

        case (3)
            dt_n = time%dt
            dt_nm1 = time%dt_old(1)
            dt_nm2 = time%dt_old(2)

            rho1 = dt_n / dt_nm1
            rho2 = dt_nm1 / dt_nm2

            coef_c0 = (3.0d0 * rho1**2.0d0 * rho2 + 4.0d0 * rho1 * rho2 + 2.0d0 * rho1 + rho2 + 1.0d0) &
                      / ((rho1 + 1.0d0) * (rho1 * rho2 + rho2 + 1.0d0))
            coef_c1 = -(rho1 + 1.0d0) * (rho1 * rho2 + rho2 + 1.0d0) / (rho2 + 1.0d0)
            coef_c2 = rho1**2.0d0 * (rho1 * rho2 + rho2 + 1.0d0) / (rho2 + 1.0d0)
            coef_c3 = -rho1**2.0d0 * rho2**3.0d0 * (rho1 + 1.0d0) / ((rho2 + 1.0d0) * (rho1 * rho2 + rho2 + 1.0d0))

            self%KT_star_0 = coef_c0 * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coef_c1 * (self%CT_l * self%T%old(:, 1)) + &
                        -coef_c2 * (self%CT_l * self%T%old(:, 2)) + &
                        -coef_c3 * (self%CT_l * self%T%old(:, 3))

        case (4)
            dt_n = time%dt
            dt_nm1 = time%dt_old(1)
            dt_nm2 = time%dt_old(2)
            dt_nm3 = time%dt_old(3)

            rho1 = dt_n / dt_nm1
            rho2 = dt_nm1 / dt_nm2
            rho3 = dt_nm2 / dt_nm3
            coef_c0 = (4.0d0 * rho1**3.0d0 * rho2**2.0d0 * rho3 + 9.0d0 * rho1**2.0d0 * rho2**2.0d0 * rho3 + &
                       6.0d0 * rho1**2.0d0 * rho2 * rho3 + 3.0d0 * rho1**2.0d0 * rho2 + &
                       6.0d0 * rho1 * rho2**2.0d0 * rho3 + 8.0d0 * rho1 * rho2 * rho3 + &
                       4.0d0 * rho1 * rho2 + 2.0d0 * rho1 * rho3 + 2.0d0 * rho1 + &
                       rho2**2.0d0 * rho3 + 2.0d0 * rho2 * rho3 + rho2 + rho3 + 1.0d0) &
                      / ((rho1 + 1.0d0) * (rho1 * rho2 + rho2 + 1.0d0) * (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0))
            coef_c1 = -(rho1 + 1.0d0) * (rho1 * rho2 + rho2 + 1.0d0) * (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0) &
                      / ((rho2 + 1.0d0) * (rho2 * rho3 + rho3 + 1.0d0))
            coef_c2 = rho1**2.0d0 * (rho1 * rho2 + rho2 + 1.0d0) * (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0) &
                      / ((rho1 + 1.0d0) * (rho3 + 1.0d0))
            coef_c3 = -rho1**2.0d0 * rho2**3.0d0 * (rho1 + 1.0d0) * (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0) &
                      / ((rho2 + 1.0d0) * (rho1 * rho2 + rho2 + 1.0d0))
            coef_c4 = rho1**2.0d0 * rho2**3.0d0 * rho3**4.0d0 * (rho1 + 1.0d0) * (rho1 * rho2 + rho2 + 1.0d0) &
                      / ((rho3 + 1.0d0) * (rho2 * rho3 + rho3 + 1.0d0) * (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0))
            self%KT_star_0 = coef_c0 * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coef_c1 * (self%CT_l * self%T%old(:, 1)) + &
                        -coef_c2 * (self%CT_l * self%T%old(:, 2)) + &
                        -coef_c3 * (self%CT_l * self%T%old(:, 3)) + &
                        -coef_c4 * (self%CT_l * self%T%old(:, 4))
        case (5)
            dt_n = time%dt
            dt_nm1 = time%dt_old(1)
            dt_nm2 = time%dt_old(2)
            dt_nm3 = time%dt_old(3)
            dt_nm4 = time%dt_old(4)

            rho1 = dt_n / dt_nm1
            rho2 = dt_nm1 / dt_nm2
            rho3 = dt_nm2 / dt_nm3
            rho4 = dt_nm3 / dt_nm4
            coef_c0 = (5.0d0 * rho1**4.0d0 * rho2**3.0d0 * rho3**2.0d0 * rho4 + &
                       16.0d0 * rho1**3.0d0 * rho2**3.0d0 * rho3**2.0d0 * rho4 + &
                       12.0d0 * rho1**3.0d0 * rho2**2.0d0 * rho3**2.0d0 * rho4 + &
                       8.0d0 * rho1**3.0d0 * rho2**2.0d0 * rho3 * rho4 + &
                       4.0d0 * rho1**3.0d0 * rho2**2.0d0 * rho3 + &
                       18.0d0 * rho1**2.0d0 * rho2**3.0d0 * rho3**2.0d0 * rho4 + &
                       27.0d0 * rho1**2.0d0 * rho2**2.0d0 * rho3**2.0d0 * rho4 + &
                       18.0d0 * rho1**2.0d0 * rho2**2.0d0 * rho3 * rho4 + &
                       9.0d0 * rho1**2.0d0 * rho2**2.0d0 * rho3 + &
                       9.0d0 * rho1**2.0d0 * rho2 * rho3**2.0d0 * rho4 + &
                       12.0d0 * rho1**2.0d0 * rho2 * rho3 * rho4 + &
                       6.0d0 * rho1**2.0d0 * rho2 * rho3 + &
                       3.0d0 * rho1**2.00 * rho2 * rho4 + &
                       3.0d0 * rho1**2.0d0 * rho2 + &
                       8.0d0 * rho1 * rho2**3.0d0 * rho3**2.0d0 * rho4 + &
                       18.0d0 * rho1 * rho2**2.0d0 * rho3**2.0d0 * rho4 + &
                       12.0d0 * rho1 * rho2**2.0d0 * rho3 * rho4 + &
                       6.0d0 * rho1 * rho2**2.0d0 * rho3 + &
                       12.0d0 * rho1 * rho2 * rho3**2.0d0 * rho4 + &
                       16.0d0 * rho1 * rho2 * rho3 * rho4 + &
                       8.0d0 * rho1 * rho2 * rho3 + &
                       4.0d0 * rho1 * rho2 * rho4 + &
                       4.0d0 * rho1 * rho2 + &
                       2.0d0 * rho1 * rho3**2.0d0 * rho4 + &
                       4.0d0 * rho1 * rho3 * rho4 + &
                       2.0d0 * rho1 * rho3 + &
                       2.0d0 * rho1 * rho4 + &
                       2.0d0 * rho1 + &
                       rho2**3.0d0 * rho3**2.0d0 * rho4 + &
                       3.0d0 * rho2**2.0d0 * rho3**2.0d0 * rho4 + &
                       2.0d0 * rho2**2.0d0 * rho3 * rho4 + &
                       rho2**2.0d0 * rho3 + &
                       3.0d0 * rho2 * rho3**2.0d0 * rho4 + &
                       4.0d0 * rho2 * rho3 * rho4 + &
                       2.0d0 * rho2 * rho3 + &
                       rho2 * rho4 + &
                       rho2 + &
                       rho3**2.0d0 * rho4 + &
                       2.0d0 * rho3 * rho4 + &
                       rho3 + &
                       rho4 + 1.0d0) &
                      / ((rho1 + 1.0d0) * (rho1 * rho2 + rho2 + 1.0d0) * &
                         (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0) * &
                         (rho1 * rho2 * rho3 * rho4 + rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0))
            coef_c1 = -(rho1 + 1.0d0) * (rho1 * rho2 + rho2 + 1.0d0) * &
                      (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0) * &
                      (rho1 * rho2 * rho3 * rho4 + rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0) &
                      / ((rho2 + 1.0d0) * (rho2 * rho3 + rho3 + 1.0d0) * &
                         (rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0))
            coef_c2 = rho1**2.0d0 * (rho1 * rho2 + rho2 + 1.0d0) * &
                      (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0) * &
                      (rho1 * rho2 * rho3 * rho4 + rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0) &
                      / ((rho1 + 1.0d0) * (rho3 + 1.0d0) * (rho3 * rho4 + rho4 + 1.0d0))
            coef_c3 = -rho1**2.0d0 * rho2**3.0d0 * (rho1 + 1.0d0) * &
                      (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0) * &
                      (rho1 * rho2 * rho3 * rho4 + rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0) &
                      / ((rho2 + 1.0d0) * (rho4 + 1.0d0) * (rho1 * rho2 + rho2 + 1.0d0))
            coef_c4 = rho1**2.0d0 * rho2**3.0d0 * rho3**4.0d0 * (rho1 + 1.0d0) * &
                      (rho1 * rho2 + rho2 + 1.0d0) * &
                      (rho1 * rho2 * rho3 * rho4 + rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0) &
                      / ((rho3 + 1.0d0) * (rho2 * rho3 + rho3 + 1.0d0) * &
                         (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0))
            coef_c5 = -rho1**2.0d0 * rho2**3.0d0 * rho3**4.0d0 * rho4**5.0d0 * (rho1 + 1.0d0) * &
                      (rho1 * rho2 + rho2 + 1.0d0) * (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0) &
                      / ((rho4 + 1.0d0) * (rho3 * rho4 + rho4 + 1.0d0) * &
                         (rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0) * &
                         (rho1 * rho2 * rho3 * rho4 + rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0))

            self%KT_star_0 = coef_c0 * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coef_c1 * (self%CT_l * self%T%old(:, 1)) + &
                        -coef_c2 * (self%CT_l * self%T%old(:, 2)) + &
                        -coef_c3 * (self%CT_l * self%T%old(:, 3)) + &
                        -coef_c4 * (self%CT_l * self%T%old(:, 4)) + &
                        -coef_c5 * (self%CT_l * self%T%old(:, 5))
        case (6)
            dt_n = time%dt
            dt_nm1 = time%dt_old(1)
            dt_nm2 = time%dt_old(2)
            dt_nm3 = time%dt_old(3)
            dt_nm4 = time%dt_old(4)
            dt_nm5 = time%dt_old(5)

            rho1 = dt_n / dt_nm1
            rho2 = dt_nm1 / dt_nm2
            rho3 = dt_nm2 / dt_nm3
            rho4 = dt_nm3 / dt_nm4
            rho5 = dt_nm4 / dt_nm5

            coef_c0 = (6.0d0 * rho1**5.0d0 * rho2**4.0d0 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       25.0d0 * rho1**4.0d0 * rho2**4.0d0 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       20.0d0 * rho1**4.0d0 * rho2**3.0d0 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       15.0d0 * rho1**4.0d0 * rho2**3.0d0 * rho3**2.0d0 * rho4**2.0d0 * rho5 + &
                       10.0d0 * rho1**4.0d0 * rho2**3.0d0 * rho3**2.0d0 * rho4 * rho5 + &
                       5.0d0 * rho1**4.0d0 * rho2**3.0d0 * rho3**2.0d0 * rho4 + &
                       40.0d0 * rho1**3.0d0 * rho2**4.0d0 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       64.0d0 * rho1**3.0d0 * rho2**3.0d0 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       48.0d0 * rho1**3.0d0 * rho2**3.0d0 * rho3**2.00 * rho4**2.00 * rho5 + &
                       32.0d0 * rho1**3.0d0 * rho2**3.0d0 * rho3**2.0d0 * rho4 * rho5 + &
                       16.0d0 * rho1**3.0d0 * rho2**3.0d0 * rho3**2.0d0 * rho4 + &
                       24.0d0 * rho1**3.0d0 * rho2**2.0d0 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       36.0d0 * rho1**3.0d0 * rho2**2.0d0 * rho3**2.0d0 * rho4**2.0d0 * rho5 + &
                       24.0d0 * rho1**3.0d0 * rho2**2.0d0 * rho3**2.0d0 * rho4 * rho5 + &
                       12.0d0 * rho1**3.0d0 * rho2**2.0d0 * rho3**2.0d0 * rho4 + &
                       12.0d0 * rho1**3.0d0 * rho2**2.0d0 * rho3 * rho4**2.0d0 * rho5 + &
                       16.0d0 * rho1**3.0d0 * rho2**2.0d0 * rho3 * rho4 * rho5 + &
                       8.0d0 * rho1**3.0d0 * rho2**2.0d0 * rho3 * rho4 + &
                       4.0d0 * rho1**3.0d0 * rho2**2.0d0 * rho3 * rho5 + &
                       4.0d0 * rho1**3.0d0 * rho2**2.0d0 * rho3 + &
                       30.0d0 * rho1**2.0d0 * rho2**4.0d0 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       72.0d0 * rho1**2.0d0 * rho2**3.0d0 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       54.0d0 * rho1**2.0d0 * rho2**3.0d0 * rho3**2.0d0 * rho4**2.0d0 * rho5 + &
                       36.0d0 * rho1**2.0d0 * rho2**3.0d0 * rho3**2.0d0 * rho4 * rho5 + &
                       18.0d0 * rho1**2.0d0 * rho2**3.0d0 * rho3**2.0d0 * rho4 + &
                       54.0d0 * rho1**2.0d0 * rho2**2.0d0 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       81.0d0 * rho1**2.0d0 * rho2**2.0d0 * rho3**2.0d0 * rho4**2.0d0 * rho5 + &
                       54.0d0 * rho1**2.0d0 * rho2**2.0d0 * rho3**2.0d0 * rho4 * rho5 + &
                       27.0d0 * rho1**2.0d0 * rho2**2.0d0 * rho3**2.0d0 * rho4 + &
                       27.0d0 * rho1**2.0d0 * rho2**2.0d0 * rho3 * rho4**2.0d0 * rho5 + &
                       36.0d0 * rho1**2.0d0 * rho2**2.0d0 * rho3 * rho4 * rho5 + &
                       18.0d0 * rho1**2.0d0 * rho2**2.0d0 * rho3 * rho4 + &
                       9.0d0 * rho1**2.0d0 * rho2**2.0d0 * rho3 * rho5 + &
                       9.0d0 * rho1**2.0d0 * rho2**2.0d0 * rho3 + &
                       12.0d0 * rho1**2.0d0 * rho2 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       27.0d0 * rho1**2.0d0 * rho2 * rho3**2.0d0 * rho4**2.0d0 * rho5 + &
                       18.0d0 * rho1**2.0d0 * rho2 * rho3**2.0d0 * rho4 * rho5 + &
                       9.0d0 * rho1**2.0d0 * rho2 * rho3**2.0d0 * rho4 + &
                       18.0d0 * rho1**2.0d0 * rho2 * rho3 * rho4**2.0d0 * rho5 + &
                       24.0d0 * rho1**2.0d0 * rho2 * rho3 * rho4 * rho5 + &
                       12.0d0 * rho1**2.0d0 * rho2 * rho3 * rho4 + &
                       6.0d0 * rho1**2.0d0 * rho2 * rho3 * rho5 + &
                       6.0d0 * rho1**2.0d0 * rho2 * rho3 + &
                       3.0d0 * rho1**2.0d0 * rho2 * rho4**2.0d0 * rho5 + &
                       6.0d0 * rho1**2.0d0 * rho2 * rho4 * rho5 + &
                       3.0d0 * rho1**2.0d0 * rho2 * rho4 + &
                       3.0d0 * rho1**2.0d0 * rho2 * rho5 + &
                       3.0d0 * rho1**2.0d0 * rho2 + &
                       10.0d0 * rho1 * rho2**4.0d0 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       32.0d0 * rho1 * rho2**3.0d0 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       24.0d0 * rho1 * rho2**3.0d0 * rho3**2.0d0 * rho4**2.0d0 * rho5 + &
                       16.0d0 * rho1 * rho2**3.0d0 * rho3**2.0d0 * rho4 * rho5 + &
                       8.0d0 * rho1 * rho2**3.0d0 * rho3**2.0d0 * rho4 + &
                       36.0d0 * rho1 * rho2**2.0d0 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       54.0d0 * rho1 * rho2**2.0d0 * rho3**2.0d0 * rho4**2.0d0 * rho5 + &
                       36.0d0 * rho1 * rho2**2.0d0 * rho3**2.0d0 * rho4 * rho5 + &
                       18.0d0 * rho1 * rho2**2.0d0 * rho3**2.0d0 * rho4 + &
                       18.0d0 * rho1 * rho2**2.0d0 * rho3 * rho4**2.0d0 * rho5 + &
                       24.0d0 * rho1 * rho2**2.0d0 * rho3 * rho4 * rho5 + &
                       12.0d0 * rho1 * rho2**2.0d0 * rho3 * rho4 + &
                       6.0d0 * rho1 * rho2**2.0d0 * rho3 * rho5 + &
                       6.0d0 * rho1 * rho2**2.0d0 * rho3 + &
                       16.0d0 * rho1 * rho2 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       36.0d0 * rho1 * rho2 * rho3**2.0d0 * rho4**2.0d0 * rho5 + &
                       24.0d0 * rho1 * rho2 * rho3**2.d0 * rho4 * rho5 + &
                       12.0d0 * rho1 * rho2 * rho3**2.0d0 * rho4 + &
                       24.0d0 * rho1 * rho2 * rho3 * rho4**2.0d0 * rho5 + &
                       32.0d0 * rho1 * rho2 * rho3 * rho4 * rho5 + &
                       16.0d0 * rho1 * rho2 * rho3 * rho4 + &
                       8.0d0 * rho1 * rho2 * rho3 * rho5 + &
                       8.0d0 * rho1 * rho2 * rho3 + &
                       4.0d0 * rho1 * rho2 * rho4**2.0d0 * rho5 + &
                       8.0d0 * rho1 * rho2 * rho4 * rho5 + &
                       4.0d0 * rho1 * rho2 * rho4 + &
                       4.0d0 * rho1 * rho2 * rho5 + &
                       4.0d0 * rho1 * rho2 + &
                       2.0d0 * rho1 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       6.0d0 * rho1 * rho3**2.0d0 * rho4**2.0d0 * rho5 + &
                       4.0d0 * rho1 * rho3**2.0d0 * rho4 * rho5 + &
                       2.0d0 * rho1 * rho3**2.0d0 * rho4 + &
                       6.0d0 * rho1 * rho3 * rho4**2.0d0 * rho5 + &
                       8.0d0 * rho1 * rho3 * rho4 * rho5 + &
                       4.0d0 * rho1 * rho3 * rho4 + &
                       2.0d0 * rho1 * rho3 * rho5 + &
                       2.0d0 * rho1 * rho3 + &
                       2.0d0 * rho1 * rho4**2.0d0 * rho5 + &
                       4.0d0 * rho1 * rho4 * rho5 + &
                       2.0d0 * rho1 * rho4 + &
                       2.0d0 * rho1 * rho5 + &
                       2.0d0 * rho1 + &
                       rho2**4.0d0 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       4.0d0 * rho2**3.0d0 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       3.0d0 * rho2**3.0d0 * rho3**2.0d0 * rho4**2.0d0 * rho5 + &
                       2.0d0 * rho2**3.0d0 * rho3**2.0d0 * rho4 * rho5 + &
                       rho2**3.0d0 * rho3**2.0d0 * rho4 + &
                       6.0d0 * rho2**2.0d0 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       9.0d0 * rho2**2.0d0 * rho3**2.0d0 * rho4**2.0d0 * rho5 + &
                       6.0d0 * rho2**2.0d0 * rho3**2.0d0 * rho4 * rho5 + &
                       3.0d0 * rho2**2.0d0 * rho3**2.0d0 * rho4 + &
                       3.0d0 * rho2**2.0d0 * rho3 * rho4**2.0d0 * rho5 + &
                       4.0d0 * rho2**2.0d0 * rho3 * rho4 * rho5 + &
                       2.0d0 * rho2**2.0d0 * rho3 * rho4 + &
                       rho2**2.0d0 * rho3 * rho5 + &
                       rho2**2.0d0 * rho3 + &
                       4.0d0 * rho2 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       9.0d0 * rho2 * rho3**2.0d0 * rho4**2.0d0 * rho5 + &
                       6.0d0 * rho2 * rho3**2.0d0 * rho4 * rho5 + &
                       3.0d0 * rho2 * rho3**2.0d0 * rho4 + &
                       6.0d0 * rho2 * rho3 * rho4**2.0d0 * rho5 + &
                       8.0d0 * rho2 * rho3 * rho4 * rho5 + &
                       4.0d0 * rho2 * rho3 * rho4 + &
                       2.0d0 * rho2 * rho3 * rho5 + &
                       2.0d0 * rho2 * rho3 + &
                       rho2 * rho4**2.0d0 * rho5 + &
                       2.0d0 * rho2 * rho4 * rho5 + &
                       rho2 * rho4 + &
                       rho2 * rho5 + &
                       rho2 + &
                       rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       3.0d0 * rho3**3.0d0 * rho4**2.0d0 * rho5 + &
                       2.0d0 * rho3**3.0d0 * rho4 * rho5 + &
                       rho3**3.0d0 * rho4 + &
                       3.0d0 * rho3 * rho4**2.0d0 * rho5 + &
                       4.0d0 * rho3 * rho4 * rho5 + &
                       2.0d0 * rho3 * rho4 + &
                       rho3 * rho5 + &
                       rho3 + &
                       rho4**2.0d0 * rho5 + &
                       2.0d0 * rho4 * rho5 + &
                       rho4 + &
                       rho5 + &
                       1.0d0) / &
                      ((rho1 + 1.0d0) * (rho1 * rho2 + rho3 + 1.0d0) * &
                       (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0) * &
                       (rho1 * rho2 * rho3 * rho4 + rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0) * &
                       (rho1 * rho2 * rho3 * rho4 * rho5 + rho2 * rho3 * rho4 * rho5 + &
                        rho3 * rho4 * rho5 + rho4 * rho5 + rho5 + 1.0d0))

            coef_c1 = -(rho1 + 1.0d0) * (rho1 * rho2 + rho2 + 1.0d0) * &
                      (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0) * &
                      (rho1 * rho2 * rho3 * rho4 + rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0) * &
                      (rho1 * rho2 * rho3 * rho4 * rho5 + rho2 * rho3 * rho4 * rho5 + &
                       rho3 * rho4 * rho5 + rho4 * rho5 + rho5 + 1.0d0) / &
                      ((rho2 + 1.0d0) * (rho2 * rho3 + rho3 + 1.0d0) * &
                       (rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0) * &
                       (rho2 * rho3 * rho4 * rho5 + rho3 * rho4 * rho5 + rho4 * rho5 + rho5 + 1.0d0))
            coef_c2 = rho1**2.0d0 * (rho1 * rho2 + rho2 + 1.0d0) * &
                      (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0) * &
                      (rho1 * rho2 * rho3 * rho4 + rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0) * &
                      (rho1 * rho2 * rho3 * rho4 * rho5 + rho2 * rho3 * rho4 * rho5 + &
                       rho3 * rho4 * rho5 + rho4 * rho5 + rho5 + 1.0d0) / &
                      ((rho1 + 1.0d0) * (rho3 + 1.0d0) * &
                       (rho3 * rho4 + rho4 + 1.0d0) * &
                       (rho3 * rho4 * rho5 + rho4 * rho5 + rho5 + 1.0d0))
            coef_c3 = -rho1**2.0d0 * rho2**3.0d0 * (rho1 + 1.0d0) * &
                      (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0) * &
                      (rho1 * rho2 * rho3 * rho4 + rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0) * &
                      (rho1 * rho2 * rho3 * rho4 * rho5 + rho2 * rho3 * rho4 * rho5 + &
                       rho3 * rho4 * rho5 + rho4 * rho5 + rho5 + 1.0d0) / &
                      ((rho2 + 1.0d0) * (rho4 + 1.0d0) * &
                       (rho1 * rho2 + rho2 + 1.0d0) * &
                       (rho4 * rho5 + rho5 + 1.0d0))
            coef_c4 = rho1**2.0d0 * rho2**3.0d0 * rho3**4.0d0 * (rho1 + 1.0d0) * &
                      (rho1 * rho2 + rho2 + 1.0d0) * &
                      (rho1 * rho2 * rho3 * rho4 + rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0) * &
                      (rho1 * rho2 * rho3 * rho4 * rho5 + rho2 * rho3 * rho4 * rho5 + &
                       rho3 * rho4 * rho5 + rho4 * rho5 + rho5 + 1.0d0) / &
                      ((rho3 + 1.0d0) * (rho5 + 1.0d0) * &
                       (rho2 * rho3 + rho3 + 1.0d0) * &
                       (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0))
            coef_c5 = -rho1**2.0d0 * rho2**3.0d0 * rho3**4.0d0 * rho4**5.0d0 * (rho1 + 1.0d0) * &
                      (rho1 * rho2 + rho2 + 1.0d0) * &
                      (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0) * &
                      (rho1 * rho2 * rho3 * rho4 * rho5 + rho2 * rho3 * rho4 * rho5 + &
                       rho3 * rho4 * rho5 + rho4 * rho5 + rho5 + 1.0d0) / &
                      ((rho4 + 1.0d0) * (rho3 * rho4 + rho4 + 1.0d0) * &
                       (rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0) * &
                       (rho1 * rho2 * rho3 * rho4 + rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0))
            coef_c6 = rho1**2.0d0 * rho2**3.0d0 * rho3**4.0d0 * rho4**5.0d0 * rho5**6.0d0 * (rho1 + 1.0d0) * &
                      (rho1 * rho2 + rho2 + 1.0d0) * &
                      (rho1 * rho2 * rho3 + rho2 * rho3 + rho3 + 1.0d0) * &
                      (rho1 * rho2 * rho3 * rho4 + rho2 * rho3 * rho4 + rho3 * rho4 + rho4 + 1.0d0) / &
                      ((rho5 + 1.0d0) * (rho4 * rho5 + rho5 + 1.0d0) * &
                       (rho3 * rho4 * rho5 + rho4 * rho5 + rho5 + 1.0d0) * &
                       (rho2 * rho3 * rho4 * rho5 + rho3 * rho4 * rho5 + &
                        rho4 * rho5 + rho5 + 1.0d0) * &
                       (rho1 * rho2 * rho3 * rho4 * rho5 + rho2 * rho3 * rho4 * rho5 + &
                        rho3 * rho4 * rho5 + rho4 * rho5 + rho5 + 1.0d0))

            self%KT_star_0 = coef_c0 * self%CT_l + dt_n * self%KT_l
            self%PHIT = -coef_c1 * (self%CT_l * self%T%old(:, 1)) + &
                        -coef_c2 * (self%CT_l * self%T%old(:, 2)) + &
                        -coef_c3 * (self%CT_l * self%T%old(:, 3)) + &
                        -coef_c4 * (self%CT_l * self%T%old(:, 4)) + &
                        -coef_c5 * (self%CT_l * self%T%old(:, 5)) + &
                        -coef_c6 * (self%CT_l * self%T%old(:, 6))
        end select

    end subroutine assemble_type_thermal_crs

end submodule Main_Thermal_3Phase
