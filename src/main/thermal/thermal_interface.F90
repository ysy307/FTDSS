module main_thermal
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_input, only:type_input
    use :: module_physics
    implicit none
    private

    public :: type_thermal

    type :: type_thermal
        type(type_physics_manager) :: physics
        ! Add thermal-specific components here
    contains
        procedure, pass(self) :: initialize => initialize_type_thermal
    end type type_thermal

contains
    subroutine initialize_type_thermal(self, input, active_region_ids)
        implicit none
        class(type_thermal), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: active_region_ids(:)

        integer(int32) :: num_materials
        integer(int32) :: num_phases
        integer(int32) :: i, j

        type(type_physics_info), allocatable :: density_info(:)
        type(type_physics_info), allocatable :: specific_heat_info(:)
        type(type_physics_info), allocatable :: heat_capacity_info(:)
        type(type_physics_info), allocatable :: thermal_conductivity_info(:)
        integer(int32), allocatable :: wrf_ids(:)
        type(type_wrf_params), allocatable :: wrf_model_info(:)
        integer(int32), allocatable :: hcf_ids(:)
        type(type_hcf_params), allocatable :: hcf_model_info(:)
        integer(int32), allocatable :: gcc_model_ids(:)

        ! Initialize thermal physics components

        num_materials = input%basic%num_materials

        allocate (density_info(num_materials))
        allocate (specific_heat_info(num_materials))
        allocate (heat_capacity_info(num_materials))
        allocate (thermal_conductivity_info(num_materials))
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
                        call specific_heat_info(i)%reset()
                        call heat_capacity_info(i)%reset()
                        call thermal_conductivity_info(i)%reset()

                        density_info(i)%num_phases = num_phases
                        specific_heat_info(i)%num_phases = num_phases
                        heat_capacity_info(i)%num_phases = num_phases
                        thermal_conductivity_info(i)%num_phases = num_phases

                        if (num_phases >= 1) then
                            density_info(i)%solid = material%thermal%density(1)
                            specific_heat_info(i)%solid = material%thermal%specific_heat(1)
                            heat_capacity_info(i)%solid = material%thermal%density(1) * material%thermal%specific_heat(1)
                            thermal_conductivity_info(i)%solid = material%thermal%thermal_conductivity(1)
                        end if

                        if (num_phases >= 2) then
                            density_info(i)%water = material%thermal%density(2)
                            specific_heat_info(i)%water = material%thermal%specific_heat(2)
                            heat_capacity_info(i)%water = material%thermal%density(2) * material%thermal%specific_heat(2)
                            thermal_conductivity_info(i)%water = material%thermal%thermal_conductivity(2)
                        end if

                        if (num_phases >= 3) then
                            density_info(i)%ice = material%thermal%density(3)
                            specific_heat_info(i)%ice = material%thermal%specific_heat(3)
                            heat_capacity_info(i)%ice = material%thermal%density(3) * material%thermal%specific_heat(3)
                            thermal_conductivity_info(i)%ice = material%thermal%thermal_conductivity(3)
                            if (allocated(material%thermal%thermal_conductivity_dispersity)) then
                                call allocate_array(thermal_conductivity_info(i)%dispersity, &
                                                    source=material%thermal%thermal_conductivity_dispersity)
                            end if
                        end if

                        if (num_phases >= 4) then
                            density_info(i)%vapor = material%thermal%density(4)
                            specific_heat_info(i)%vapor = material%thermal%specific_heat(4)
                            heat_capacity_info(i)%vapor = material%thermal%density(4) * material%thermal%specific_heat(4)
                            !! TBI: 体積熱容量の経験的パラメータ入力
                            thermal_conductivity_info(i)%vapor = material%thermal%thermal_conductivity(4)
                        end if

                        if (material%thermal%phase_change%gcc%is_segregation) then
                            gcc_model_ids(i) = GCC_SEGREGATION
                        else
                            gcc_model_ids(i) = GCC_NON_SEGREGATION
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

                        if (material%is_active(PHYSICS_TYPE_HYDRAULIC)) then
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
                        end if

                    end associate
                end if
            end do
        end do

        call self%physics%initialize(active_region_ids, density_info, &
                                     specific_heat_info, heat_capacity_info, thermal_conductivity_info, &
                                     wrf_ids, wrf_model_info, hcf_ids, hcf_model_info, gcc_model_ids)

    end subroutine initialize_type_thermal

end module main_thermal
