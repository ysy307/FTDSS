submodule(main_hydraulic) hydraulic_base
    implicit none
contains
    module subroutine initialize_type_hydraulic(self, input, active_region_ids)
        implicit none
        class(type_hydraulic), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: active_region_ids(:)

        integer(int32), allocatable :: hcf_ids(:)
        type(type_hcf_params), allocatable :: hcf_model_info(:)

        integer(int32) :: num_materials
        integer(int32) :: num_phases
        integer(int32) :: i, j

        num_materials = input%basic%num_materials

        allocate (hcf_ids(num_materials))
        allocate (hcf_model_info(num_materials))

        do i = 1, num_materials
            do j = 1, size(active_region_ids)
                if (input%basic%materials(i)%id == active_region_ids(j)) then
                    associate (material => input%basic%materials(i))
                        num_phases = material%phase

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
                    end associate
                end if
            end do
        end do

        call self%physics%initialize(active_region_ids, &
                                     hcf_ids=hcf_ids, &
                                     hcf_model_info=hcf_model_info)

        self%computation_type = input%basic%simulation_settings%calculate_type
        self%computation_dimension = input%basic%simulation_settings%calculate_dimension

    end subroutine initialize_type_hydraulic

end submodule hydraulic_base
