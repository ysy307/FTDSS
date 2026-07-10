submodule(physics_governing_hydraulic) hydraulic_base
    implicit none
contains
    module subroutine initialize_type_hydraulic(self, input, active_region_ids)
        implicit none
        class(type_hydraulic), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: active_region_ids(:)

        integer(int32) :: num_materials
        integer(int32) :: num_active_materials
        integer(int32) :: i, j
        integer(int32) :: material_idx

        type(type_config_constitutive), allocatable :: density_info(:)
        type(type_config_wrf), allocatable :: wrf_model_info(:)
        type(type_config_hcf), allocatable :: hcf_model_info(:)
        type(type_config_gcc), allocatable :: gcc_model_info(:)

        ! Initialize thermal physics components

        num_materials = input%basic%num_materials
        num_active_materials = size(active_region_ids)

        allocate (density_info(num_active_materials))
        allocate (wrf_model_info(num_active_materials))
        allocate (hcf_model_info(num_active_materials))
        allocate (gcc_model_info(num_active_materials))

        do j = 1, num_active_materials
            material_idx = 0
            do i = 1, num_materials
                if (input%basic%materials(i)%id == active_region_ids(j)) then
                    material_idx = i
                    exit
                end if
            end do

            if (material_idx == 0) then
                if (num_materials == 1) then
                    material_idx = 1
                else
                    write (*, *) 'Error: No material definition matched active region id ', active_region_ids(j)
                    stop 1
                end if
            end if

            call density_info(j)%reset()

            call input_translator%execute(input, material_idx, CONSTITUTIVE_PROPERTIES%DENSITY, &
                                          density_info(j))
            call input_translator%execute(input, material_idx, wrf_model_info(j))
            call input_translator%execute(input, material_idx, hcf_model_info(j))
            call input_translator%execute(input, material_idx, gcc_model_info(j))
        end do

        call self%physics%initialize(active_region_ids, &
                                     configs_density=density_info, &
                                     configs_wrf=wrf_model_info, &
                                     configs_hcf=hcf_model_info, &
                                     configs_gcc=gcc_model_info)

        self%computation_type = input%basic%simulation_settings%calculate_type
        self%computation_dimension = input%basic%simulation_settings%calculate_dimension
        self%enable_vapor_transport = input%basic%analysis_controls%enable_vapor_transport
        self%enable_fringe_subcell_quadrature = input%basic%analysis_controls%enable_fringe_subcell_quadrature
        self%enable_fringe_K_averaging = input%basic%analysis_controls%enable_fringe_K_averaging

    end subroutine initialize_type_hydraulic

    module pure function is_vapor_transport_enabled_hydraulic(self) result(enabled)
        implicit none
        class(type_hydraulic), intent(in) :: self
        logical :: enabled

        enabled = self%enable_vapor_transport
    end function is_vapor_transport_enabled_hydraulic

end submodule hydraulic_base
