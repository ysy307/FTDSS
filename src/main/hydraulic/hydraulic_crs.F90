submodule(main_hydraulic) main_hydraulic_crs
    implicit none
contains
    module function construct_type_hydraulic_crs(input, coordinate, domain) result(structure)
        implicit none
        class(abst_hydraulic), allocatable :: structure
        type(type_input), intent(inout) :: input
        type(type_coordinate_array_dp), intent(inout), pointer :: coordinate
        type(type_domain), intent(inout) :: domain

        integer(int32) :: i
        integer(int32) :: num_nodes
        integer(int32), allocatable :: row_ptr(:), col_ind(:)

        integer(int32) :: ierr

        if (allocated(structure)) deallocate (structure)
        allocate (type_hydraulic_crs :: structure)

        num_nodes = domain%get_num_nodes()
        call domain%node_adjacency%get_csr(row_ptr, col_ind)

        call structure%KH_star%initialize(num_nodes, row_ptr, col_ind)
        structure%order = input%basic%solver_settings%bdf_order

        call allocate_array(structure%FH, num_nodes)
        call allocate_array(structure%PHIH, num_nodes)

        if (associated(structure%assemble_global)) nullify (structure%assemble_global)

        structure%algorithm = input%basic%solver_settings%nonlinear_solver%method
        select case (structure%algorithm)
        case ("none")
            if (input%basic%solver_settings%parallel_settings%threads%is_parallel) then
                structure%assemble_global => hydraulic_assemble_system_linear_1_parallel
            else
                structure%assemble_global => hydraulic_assemble_system_linear_1
            end if
        case ("picard")
            if (input%basic%solver_settings%parallel_settings%threads%is_parallel) then
                structure%assemble_global => hydraulic_assemble_system_linear_1_parallel
            else
                structure%assemble_global => hydraulic_assemble_system_linear_1
            end if
        end select

        !---------------------------------------------------------------------------------------------------------------------------
        ! 線形求解ソルバーの設定
        !---------------------------------------------------------------------------------------------------------------------------
        structure%solver = create_solver(input, "hydraulic", structure%KH_star, num_nodes)
        !---------------------------------------------------------------------------------------------------------------------------

    end function construct_type_hydraulic_crs

    module subroutine update_type_hydraulic_crs(self, domain, property, pressure, porosity)
        implicit none
        class(type_hydraulic_crs), intent(inout) :: self
        type(type_domain), intent(inout), target :: domain
        type(type_properties_manager), intent(inout) :: property
        real(real64), intent(in) :: pressure(:)
        real(real64), intent(in) :: porosity(:)

    end subroutine update_type_hydraulic_crs

    module subroutine shift_type_hydraulic_crs(self)
        implicit none
        class(type_hydraulic_crs), intent(inout) :: self

    end subroutine shift_type_hydraulic_crs

    module subroutine solve_type_hydraulic_crs(self, pressure, controls)
        implicit none
        class(type_hydraulic_crs), intent(inout) :: self
        type(type_variable), intent(inout) :: pressure
        type(type_controls), intent(inout) :: controls

        integer(int32) :: stat

        select case (trim(controls%iteration%get_algorithm_name()))
        case ("none")
            call self%solver%solve(self%KH_star, self%PHIH, pressure%new(:), stat)
            pressure%dif(:) = pressure%new(:) - pressure%pre(:)
        case ("newton", "modified_newton", "picard")
            call self%solver%solve(self%KH_star, self%PHIH, pressure%dif(:), stat)
            pressure%new(:) = pressure%pre(:) + pressure%dif(:)
        end select
        call self%solver%check(stat, controls%time%get_time())

    end subroutine solve_type_hydraulic_crs

    module subroutine compute_type_hydraulic_crs(self, domain, property, pressure, temperature, porosity, ice, controls, bc)
        implicit none
        class(type_hydraulic_crs), intent(inout) :: self
        type(type_domain), intent(inout) :: domain
        type(type_properties_manager), intent(in) :: property
        type(type_variable), intent(inout) :: pressure
        type(type_variable), intent(inout) :: temperature
        type(type_variable), intent(inout) :: porosity
        type(type_variable), intent(inout) :: ice
        type(type_controls), intent(inout) :: controls
        type(type_bc), intent(inout) :: bc

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
            ! J, R, domain, pressure, temperature, porosity, ice, &
            !                                                properties, controls, actual_order
            call self%assemble_global(self%KH_star, self%PHIH, domain, pressure, temperature, ice, porosity, &
                                      property, controls, actual_order)
            call controls%time%profile_stop("Assemble")

            call controls%time%profile_start("Setup")
            call bc%apply_crs(boundary_target='hydraulic', &
                              current_time=controls%time%get_time(), &
                              A=self%KH_star, &
                              b=self%PHIH, &
                              Domain=Domain, &
                              mode=mode_bc)
            if (controls%iteration%get_step() == 1) call controls%iteration%set_initial_norms(res_vec=self%PHIH)
            call controls%time%profile_stop("Setup")

            call controls%time%profile_start("Solve")
            call self%solve(pressure, controls)
            call controls%time%profile_stop("Solve")

            call controls%time%profile_start("Setup")
            if (controls%iteration%get_step() == 1) call controls%iteration%set_initial_norms(upd_vec=pressure%dif(:))
            call controls%iteration%check_convergence(self%PHIH, pressure%dif(:))
            pressure%pre(:) = pressure%new(:)
            call controls%time%profile_stop("Setup")
        end do NR_LOOP_THERMAL

    end subroutine compute_type_hydraulic_crs

end submodule main_hydraulic_crs
