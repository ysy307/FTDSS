submodule(governing_thermal) thermal_base
    implicit none
contains
    module subroutine initialize_type_thermal(self, input, active_region_ids)
        implicit none
        class(type_thermal), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: active_region_ids(:)

        integer(int32) :: num_materials
        integer(int32) :: num_phases
        integer(int32) :: i, j

        type(type_config_constitutive), allocatable :: density_info(:)
        type(type_config_constitutive), allocatable :: specific_heat_info(:)
        type(type_config_constitutive), allocatable :: heat_capacity_info(:)
        type(type_config_constitutive), allocatable :: thermal_conductivity_info(:)
        type(type_config_wrf), allocatable :: wrf_model_info(:)
        type(type_config_hcf), allocatable :: hcf_model_info(:)
        type(type_config_gcc), allocatable :: gcc_model_info(:)

        ! Initialize thermal physics components

        num_materials = input%basic%num_materials

        allocate (density_info(num_materials))
        allocate (specific_heat_info(num_materials))
        allocate (heat_capacity_info(num_materials))
        allocate (thermal_conductivity_info(num_materials))
        allocate (wrf_model_info(num_materials))
        allocate (hcf_model_info(num_materials))
        allocate (gcc_model_info(num_materials))

        do i = 1, num_materials
            do j = 1, size(active_region_ids)
                if (input%basic%materials(i)%id == active_region_ids(j)) then

                    call density_info(i)%reset()
                    call specific_heat_info(i)%reset()
                    call heat_capacity_info(i)%reset()
                    call thermal_conductivity_info(i)%reset()

                    call input_translator%execute(input, i, CONSTITUTIVE_PROPERTIES%DENSITY, &
                                                  density_info(i))
                    call input_translator%execute(input, i, CONSTITUTIVE_PROPERTIES%SPECIFIC_HEAT, &
                                                  specific_heat_info(i))
                    call input_translator%execute(input, i, CONSTITUTIVE_PROPERTIES%VOLUMETRIC_HEAT_CAPACITY, &
                                                  heat_capacity_info(i))
                    call input_translator%execute(input, i, CONSTITUTIVE_PROPERTIES%THERMAL_CONDUCTIVITY, &
                                                  thermal_conductivity_info(i))

                    call input_translator%execute(input, i, wrf_model_info(i))
                    call input_translator%execute(input, i, hcf_model_info(i))
                    call input_translator%execute(input, i, gcc_model_info(i))
                end if
            end do
        end do

        call self%physics%initialize(active_region_ids, density_info, &
                                     specific_heat_info, heat_capacity_info, thermal_conductivity_info, &
                                     wrf_model_info, hcf_model_info, gcc_model_info)

        self%computation_type = input%basic%simulation_settings%calculate_type
        self%computation_dimension = input%basic%simulation_settings%calculate_dimension

    end subroutine initialize_type_thermal

    module subroutine destroy_type_thermal(self)
        implicit none
        class(type_thermal), intent(inout) :: self

        self%computation_dimension = 0
        self%computation_type = 0

    end subroutine destroy_type_thermal
end submodule thermal_base
