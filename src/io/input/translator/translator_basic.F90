submodule(inout_input_translator) input_translator_basic
    implicit none
contains

    module subroutine execute_basic_swcc(self, input, material_id, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id
        class(type_config_wrf), intent(inout) :: config

        if (material_id < 1 .or. material_id > input%basic%num_materials) then
            error stop "Input Error: material_id is out of range."
        end if

        select type (config)
        type is (type_config_hcf)

            call setup_wrf_basic(input, material_id, config)

            associate (hcf => input%basic%materials(material_id)%hydraulic_conductivity_model, &
                       swcc => input%basic%materials(material_id)%water_characteristic_curve)

                config%model = HCF_MODES%to_object(hcf%model_number)
                config%water_viscosity_model = HCF_VISCOSITY_TYPES%to_object(hcf%water_viscosity_model)

                config%k_sat = hcf%saturated_conductivity
                config%l = swcc%l
                config%omega = hcf%impedance_factor
                config%gain_factor = hcf%gain_factor

            end associate
        type is (type_config_wrf)

            call setup_wrf_basic(input, material_id, config)

        class default
            error stop "execute_basic_swcc: unsupported config type."

        end select

    end subroutine execute_basic_swcc

    subroutine setup_wrf_basic(input, material_id, config)
        implicit none
        class(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id
        class(type_config_wrf), intent(inout) :: config

        type(type_constant_id) :: L_unit
        real(real64) :: scale_pressure

        associate (material => input%basic%materials(material_id)%water_characteristic_curve)

            call config%reset()

            config%material_id = material_id
            config%swcc_model = SWCC_MODELS%to_object(material%model_number)
            L_unit = PHYSICS_UNITS%to_object(material%unit)

            config%theta_s = material%theta_s
            config%theta_r = material%theta_r
            config%alpha1 = material%alpha1

            select case (config%swcc_model%ID)

            case (SWCC_MODELS%VG%ID)
                config%n1 = material%n1
                config%m1 = 1.0d0 - 1.0d0 / material%n1

            case (SWCC_MODELS%MVG%ID)
                config%n1 = material%n1
                config%m1 = 1.0d0 - 1.0d0 / material%n1
                config%h_crit = material%h_crit

            case (SWCC_MODELS%DURNER%ID)
                config%n1 = material%n1
                config%m1 = 1.0d0 - 1.0d0 / material%n1
                config%alpha2 = material%alpha2
                config%n2 = material%n2
                config%m2 = 1.0d0 - 1.0d0 / material%n2
                config%w1 = material%w1
                config%w2 = 1.0d0 - material%w1

            case (SWCC_MODELS%DVGCH%ID)
                config%n1 = material%n1
                config%m1 = 1.0d0 - 1.0d0 / material%n1
                config%n2 = material%n2
                config%m2 = 1.0d0 - 1.0d0 / material%n2
                config%w1 = material%w1
                config%w2 = 1.0d0 - material%w1

            end select

            select case (L_unit%ID)
            case (PHYSICS_UNITS%M%ID)
                scale_pressure = 1000.0d0 * 9.80655d0
            case (PHYSICS_UNITS%CM%ID)
                scale_pressure = 1000.0d0 * 9.80655d0 * 1.0d-2
            case default
                scale_pressure = 1.0d0
            end select

            select case (config%swcc_model%ID)

            case (SWCC_MODELS%BC%ID, SWCC_MODELS%KO%ID)
                config%alpha1 = config%alpha1 * scale_pressure
                config%h_crit = config%h_crit * scale_pressure
                config%alpha2 = config%alpha2 * scale_pressure

            case (SWCC_MODELS%VG%ID, SWCC_MODELS%DVGCH%ID)
                config%alpha1 = config%alpha1 / scale_pressure

            case (SWCC_MODELS%MVG%ID)
                config%alpha1 = config%alpha1 / scale_pressure
                config%h_crit = config%h_crit * scale_pressure

            case (SWCC_MODELS%DURNER%ID)
                config%alpha1 = config%alpha1 / scale_pressure
                config%alpha2 = config%alpha2 / scale_pressure

            end select

        end associate

    end subroutine setup_wrf_basic

    module subroutine execute_basic_gcc(self, input, material_id, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id
        class(type_config_gcc), intent(inout) :: config

        if (material_id < 1 .or. material_id > input%basic%num_materials) then
            error stop "Input Error: material_id is out of range."
        end if

        select type (config)

            !==================================================
            ! GCC モデルの処理
            !==================================================
        class is (type_config_gcc)
            associate (material => input%basic%materials(material_id)%phase_change%equilibrium_model)

                call config%reset()

                config%material_id = material_id
                if (material%segregation) then
                    config%gcc_model = GCC_TYPES%SEGREGATION
                else
                    config%gcc_model = GCC_TYPES%NON_SEGREGATION
                end if

            end associate

        class default
            error stop "execute_basic_gcc: unsupported config type."
        end select

    end subroutine execute_basic_gcc

    module subroutine execute_basic_iteration(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        class(type_config_iteration), intent(inout) :: config

        select type (config)
        type is (type_config_iteration)
            associate (nls => input%basic%solver_settings%nonlinear_solver)
                config%nonlinear_solver_type = NONLINEAR_SOLVER%to_object(nls%method)
                if (config%nonlinear_solver_type == NONLINEAR_SOLVER%NONE) then
                    return
                end if

                call execute_basic_iteration_nonlinear(input, config%nonlinear)
            end associate
        end select

    end subroutine execute_basic_iteration

    subroutine execute_basic_iteration_nonlinear(input, config)
        implicit none
        class(type_input), intent(in) :: input
        class(type_config_iteration_nonlinear), intent(inout) :: config

        integer(int32) :: i

        select type (config)
        type is (type_config_iteration_nonlinear)
            associate (nls => input%basic%solver_settings%nonlinear_solver)
                config%max_iterations = nls%max_iterations
                config%update_frequency = nls%update_frequency

                config%norm_type = NORM_TYPES%to_object(nls%convergence%norm_type)
                config%combination_logic = NONLINEAR_LOGIC%to_object(nls%convergence%use_logic)
                config%convergence_norm_type = NONLINEAR_NORM_CRITERIA%to_object(nls%convergence%use_criteria)

                do i = 1, PHYSICS_TYPES%NUM_ID
                    call execute_basic_iteration_criterion(input, NONLINEAR_NORM_CRITERIA%RESIDUAL, config%residual(i))
                    call execute_basic_iteration_criterion(input, NONLINEAR_NORM_CRITERIA%UPDATE, config%update(i))
                end do

            end associate
        end select

    end subroutine execute_basic_iteration_nonlinear

    subroutine execute_basic_iteration_criterion(input, vector_norm_type, config)
        implicit none
        class(type_input), intent(in) :: input
        type(type_constant_id), intent(in) :: vector_norm_type
        class(type_config_iteration_criterion), intent(inout) :: config

        if (.not. NONLINEAR_NORM_CRITERIA%is_valid(vector_norm_type)) then
            error stop "Input Error: Invalid vector norm type for iteration criterion."
        end if

        select type (config)
        type is (type_config_iteration_criterion)
            associate (nls => input%basic%solver_settings%nonlinear_solver)
                select case (vector_norm_type%ID)
                case (NONLINEAR_NORM_CRITERIA%RESIDUAL%ID)
                    config%criterion = NONLINEAR_CRITERIA%to_object(nls%convergence%residual%criteria)
                    config%absolute_tolerance = nls%convergence%residual%absolute_tolerance
                    config%relative_tolerance = nls%convergence%residual%relative_tolerance
                case (NONLINEAR_NORM_CRITERIA%UPDATE%ID)
                    config%criterion = NONLINEAR_CRITERIA%to_object(nls%convergence%update%criteria)
                    config%absolute_tolerance = nls%convergence%update%absolute_tolerance
                    config%relative_tolerance = nls%convergence%update%relative_tolerance
                end select

            end associate
        end select

    end subroutine execute_basic_iteration_criterion

    module subroutine execute_basic_parallel_openmp(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        class(type_config_parallel_openmp), intent(inout) :: config

        select type (config)
        type is (type_config_parallel_openmp)
            associate (parallel => input%basic%solver_settings%parallel_settings%threads)
                config%is_parallel = parallel%is_parallel
                if (config%is_parallel) then
                    config%num_threads = parallel%num_threads
                    config%schedule = parallel%schedule
                    config%max_active_levels = parallel%max_active_levels
                end if
            end associate
        end select

    end subroutine execute_basic_parallel_openmp

    module subroutine execute_basic_control_manager(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        class(type_config_control_manager), intent(inout) :: config

        integer(int32) :: i, current_material_id, pid
        integer(int32) :: num_unique_regions
        integer(int32) :: max_region_id

        call input%geometry%vtk%get_active_region_info(config%unique_material_ids)

        if (.not. allocated(config%unique_material_ids) .or. size(config%unique_material_ids) == 0) then
            print *, "Error: No active material regions found."
            stop 1
        end if

        num_unique_regions = size(config%unique_material_ids)
        max_region_id = maxval(config%unique_material_ids)

        ! -------------------------
        ! 物理有効フラグ
        ! -------------------------
        call allocate_array(config%compute_active, source=input%basic%analysis_controls%is_active)

        ! -------------------------
        ! 材料別フラグ設定
        ! -------------------------
        call allocate_array(config%physics_active, PHYSICS_TYPES%NUM_ID, num_unique_regions)
        config%physics_active(:, :) = .false.

        do i = 1, num_unique_regions
            current_material_id = config%unique_material_ids(i)

            if (current_material_id > size(input%basic%materials)) cycle

            do pid = 1, PHYSICS_TYPES%NUM_ID
                if (config%compute_active(pid)) then
                    config%physics_active(pid, current_material_id) = config%compute_active(pid)
                end if
            end do
        end do

        ! -------------------------
        ! カップリングモードの設定
        ! -------------------------
        config%coupling_mode = COUPLING_MODES%to_object(input%basic%analysis_controls%coupling_mode)

    end subroutine execute_basic_control_manager

end submodule input_translator_basic
