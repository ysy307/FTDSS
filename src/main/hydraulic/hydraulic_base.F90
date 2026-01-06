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
        integer(int32), allocatable :: gcc_model_info(:)

        integer(int32) :: num_materials
        integer(int32) :: num_phases
        integer(int32) :: i, j

        num_materials = input%basic%num_materials

        allocate (density_info(num_materials))
        allocate (wrf_ids(num_materials))
        allocate (wrf_model_info(num_materials))
        allocate (hcf_ids(num_materials))
        allocate (hcf_model_info(num_materials))
        allocate (gcc_model_info(num_materials))

        do i = 1, num_materials
            do j = 1, size(active_region_ids)
                if (input%basic%materials(i)%id == active_region_ids(j)) then
                    call density_info(i)%reset()

                    call input%get_density_info(i, density_info(i))
                    call input%get_wrf_info(i, wrf_model_info(i))
                    call input%get_gcc_info(i, gcc_model_info(i))
                    call input%get_hcf_info(i, hcf_model_info(i))

                    wrf_ids(i) = i
                    hcf_ids(i) = i

                end if
            end do
        end do

        call self%physics%initialize(active_region_ids, &
                                     density_info=density_info, &
                                     wrf_ids=wrf_ids, &
                                     wrf_model_info=wrf_model_info, &
                                     hcf_ids=hcf_ids, &
                                     hcf_model_info=hcf_model_info, &
                                     gcc_model_ids=gcc_model_info)
        self%computation_type = input%basic%simulation_settings%calculate_type
        self%computation_dimension = input%basic%simulation_settings%calculate_dimension

    end subroutine initialize_type_hydraulic

end submodule hydraulic_base
