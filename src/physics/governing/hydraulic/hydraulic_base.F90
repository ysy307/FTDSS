submodule(physics_governing_hydraulic) hydraulic_base
    implicit none
contains
    module subroutine initialize_type_hydraulic(self, input, active_region_ids)
        implicit none
        class(type_hydraulic), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: active_region_ids(:)

        integer(int32) :: num_materials
        integer(int32) :: i, j

        type(type_config_constitutive), allocatable :: density_info(:)
        type(type_config_wrf), allocatable :: wrf_model_info(:)
        type(type_config_hcf), allocatable :: hcf_model_info(:)
        type(type_config_gcc), allocatable :: gcc_model_info(:)

        ! Initialize thermal physics components

        num_materials = input%basic%num_materials

        allocate (density_info(num_materials))
        allocate (wrf_model_info(num_materials))
        allocate (hcf_model_info(num_materials))
        allocate (gcc_model_info(num_materials))

        do i = 1, num_materials
            do j = 1, size(active_region_ids)
                if (input%basic%materials(i)%id == active_region_ids(j)) then

                    call density_info(i)%reset()

                    call input_translator%execute(input, i, CONSTITUTIVE_PROPERTIES%DENSITY, &
                                                  density_info(i))
                    call input_translator%execute(input, i, wrf_model_info(i))
                    call input_translator%execute(input, i, hcf_model_info(i))
                    call input_translator%execute(input, i, gcc_model_info(i))
                end if
            end do
        end do

        call self%physics%initialize(active_region_ids, &
                                     configs_density=density_info, &
                                     configs_wrf=wrf_model_info, &
                                     configs_hcf=hcf_model_info, &
                                     configs_gcc=gcc_model_info)

        self%computation_type = input%basic%simulation_settings%calculate_type
        self%computation_dimension = input%basic%simulation_settings%calculate_dimension

    end subroutine initialize_type_hydraulic

end submodule hydraulic_base
