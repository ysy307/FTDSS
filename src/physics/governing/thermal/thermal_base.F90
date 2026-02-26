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

        type(type_physics_info), allocatable :: density_info(:)
        type(type_physics_info), allocatable :: specific_heat_info(:)
        type(type_physics_info), allocatable :: heat_capacity_info(:)
        type(type_physics_info), allocatable :: thermal_conductivity_info(:)
        integer(int32), allocatable :: wrf_ids(:)
        type(type_wrf_params), allocatable :: wrf_model_info(:)
        integer(int32), allocatable :: hcf_ids(:)
        type(type_hcf_params), allocatable :: hcf_model_info(:)
        integer(int32), allocatable :: gcc_model_info(:)

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
        allocate (gcc_model_info(num_materials))

        gcc_model_info = 0 ! ID 0 は「モデルなし」として扱う
        hcf_ids = 0 ! ID 0 は「モデルなし」として扱う
        wrf_ids = 0

        do i = 1, num_materials
            do j = 1, size(active_region_ids)
                if (input%basic%materials(i)%ID == active_region_ids(j)) then

                    call density_info(i)%reset()
                    call specific_heat_info(i)%reset()
                    call heat_capacity_info(i)%reset()
                    call thermal_conductivity_info(i)%reset()

                    call input%get_density_info(i, density_info(i))
                    call input%get_specific_heat_info(i, specific_heat_info(i))
                    call input%get_volumetric_heat_capacity_info(i, heat_capacity_info(i))
                    call input%get_thermal_conductivity_info(i, thermal_conductivity_info(i))
                    call input%get_wrf_info(i, wrf_model_info(i))
                    call input%get_gcc_info(i, gcc_model_info(i))
                    call input%get_hcf_info(i, hcf_model_info(i))

                    wrf_ids(i) = i
                    hcf_ids(i) = i

                end if
            end do
        end do

        call self%physics%initialize(active_region_ids, density_info, &
                                     specific_heat_info, heat_capacity_info, thermal_conductivity_info, &
                                     wrf_ids, wrf_model_info, hcf_ids, hcf_model_info, gcc_model_info)

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
