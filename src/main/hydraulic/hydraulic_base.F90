submodule(main_hydraulic) hydraulic_base
    implicit none
contains
    module subroutine initialize_type_hydraulic(self, input, active_region_ids)
        implicit none
        class(type_hydraulic), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: active_region_ids(:)

        type(type_physics_info), allocatable :: density_info(:)
        integer(int32), allocatable :: wrf_ids(:)
        type(type_wrf_params), allocatable :: wrf_model_info(:)
        integer(int32), allocatable :: hcf_ids(:)
        type(type_hcf_params), allocatable :: hcf_model_info(:)
        integer(int32), allocatable :: gcc_model_ids(:)

        integer(int32) :: num_materials
        integer(int32) :: num_phases
        integer(int32) :: i, j

        num_materials = input%basic%num_materials

        allocate (density_info(num_materials))
        allocate (wrf_ids(num_materials))
        allocate (wrf_model_info(num_materials))
        allocate (hcf_ids(num_materials))
        allocate (hcf_model_info(num_materials))
        allocate (gcc_model_ids(num_materials))

        do i = 1, num_materials
            do j = 1, size(active_region_ids)
                if (input%basic%materials(i)%id == active_region_ids(j)) then
                    associate (material => input%basic%materials(i))
                        num_phases = material%phase

                        call density_info(i)%reset()
                        density_info(i)%num_phases = num_phases
                        if (num_phases >= 1) then
                            density_info(i)%solid = material%thermal%density(1)
                        end if
                        if (num_phases >= 2) then
                            density_info(i)%water = material%thermal%density(2)
                        end if
                        if (num_phases >= 3) then
                            density_info(i)%ice = material%thermal%density(3)
                        end if
                        if (num_phases >= 4) then
                            density_info(i)%vapor = material%thermal%density(4)
                        end if

                        wrf_ids(i) = i
                        wrf_model_info(i)%model_number = material%thermal%phase_change%wrf%model_number
                        !! TBI : 単位系対応
                        wrf_model_info(i)%unit_id = PHYSICS_UNIT_M
                        wrf_model_info(i)%alpha1 = material%thermal%phase_change%wrf%alpha1
                        wrf_model_info(i)%alpha2 = material%thermal%phase_change%wrf%alpha2
                        wrf_model_info(i)%h_crit = material%thermal%phase_change%wrf%h_crit
                        wrf_model_info(i)%theta_r = material%thermal%phase_change%wrf%theta_r
                        wrf_model_info(i)%theta_s = material%thermal%phase_change%wrf%theta_s
                        wrf_model_info(i)%n1 = material%thermal%phase_change%wrf%n1
                        wrf_model_info(i)%m1 = material%thermal%phase_change%wrf%m1
                        wrf_model_info(i)%n2 = material%thermal%phase_change%wrf%n2
                        wrf_model_info(i)%m2 = material%thermal%phase_change%wrf%m2
                        wrf_model_info(i)%w1 = material%thermal%phase_change%wrf%w1
                        wrf_model_info(i)%w2 = material%thermal%phase_change%wrf%w2

                        hcf_ids(i) = i
                        hcf_model_info(i)%model_number = material%hydraulic%model_number
                        hcf_model_info(i)%hcf_model_number = material%hydraulic%hcf%model_number
                            !! TBI : 単位系対応
                        hcf_model_info(i)%unit_id = PHYSICS_UNIT_M
                        hcf_model_info(i)%k_s = material%hydraulic%hydraulic_conductivity
                        hcf_model_info(i)%alpha1 = material%hydraulic%hcf%alpha1
                        hcf_model_info(i)%n1 = material%hydraulic%hcf%n1
                        hcf_model_info(i)%m1 = material%hydraulic%hcf%m1
                        hcf_model_info(i)%alpha2 = material%hydraulic%hcf%alpha2
                        hcf_model_info(i)%n2 = material%hydraulic%hcf%n2
                        hcf_model_info(i)%m2 = material%hydraulic%hcf%m2
                        hcf_model_info(i)%w1 = material%hydraulic%hcf%w1
                        hcf_model_info(i)%w2 = material%hydraulic%hcf%w2
                        hcf_model_info(i)%h_crit = material%hydraulic%hcf%h_crit
                        hcf_model_info(i)%gain_factor = 10.0d0
                        hcf_model_info(i)%omega = material%hydraulic%impedance_factor
                        hcf_model_info(i)%water_viscosity_model = material%hydraulic%water_viscosity_model

                        if (material%thermal%phase_change%gcc%is_segregation) then
                            gcc_model_ids(i) = GCC_SEGREGATION
                        else
                            gcc_model_ids(i) = GCC_NON_SEGREGATION
                        end if
                    end associate
                end if
            end do
        end do

        call self%physics%initialize(active_region_ids, &
                                     density_info=density_info, &
                                     wrf_ids=wrf_ids, &
                                     wrf_model_info=wrf_model_info, &
                                     hcf_ids=hcf_ids, &
                                     hcf_model_info=hcf_model_info, &
                                     gcc_model_ids=gcc_model_ids)
        self%computation_type = input%basic%simulation_settings%calculate_type
        self%computation_dimension = input%basic%simulation_settings%calculate_dimension

    end subroutine initialize_type_hydraulic

end submodule hydraulic_base
