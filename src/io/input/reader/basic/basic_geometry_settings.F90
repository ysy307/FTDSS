submodule(io_input_basic) input_basic_geometry_settings
    implicit none
    !!------------------------------------------------------------------------------------------------------------------------------
    ! JSON key names for geometry settings
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: geometry_settings = "geometry_settings"
    character(*), parameter :: mesh_file_name = "mesh_file_name"
    character(*), parameter :: integration = "integration"
    character(*), parameter :: integration_order = "integration_order"
    character(*), parameter :: integration_type = "integration_type"
    character(*), parameter :: valid_integration_types(3) = [character(len=16) :: "full", "reduced", "free"]
    character(*), parameter :: integration_points = "integration_points"
contains
    module subroutine read_geometry_settings(self, json)
        implicit none
        class(type_input_basic), intent(inout) :: self
        type(json_file), intent(inout) :: json

        character(256) :: buffer(3) = [character(256) :: geometry_settings, "", ""]

        buffer(2) = mesh_file_name
        call get_json_value(json, join(buffer), self%geometry_settings%file_name, &
                            is_required=.true.)

        buffer(2) = integration
        buffer(3) = integration_order
        call get_json_value(json, join(buffer), self%geometry_settings%integration_order, &
                            is_required=.true., default_value=2)
        ! buffer(3) = integration_type
        ! call get_json_value(json, join(buffer), self%geometry_settings%integration_type, &
        !                     is_required=.true., default_value="full", valid_list=valid_integration_types)

        ! if (strip(self%geometry_settings%integration_type) == "free") then
        !     buffer(3) = integration_points
        !     call get_json_value(json, join(buffer), self%geometry_settings%integration_points, &
        !                         is_required=.true., valid_range=[0.0d0, 1.0d0])
        ! end if

    end subroutine read_geometry_settings

    module subroutine display_geometry_settings(self)
        implicit none
        class(type_geometry_settings) :: self

        integer(int32) :: i

        write (*, '(a)') "Mesh File Name: "//strip(self%file_name)
        write (*, '(a)') "Integration Type: "//strip(self%integration_type)
        if (strip(self%integration_type) == "free") then
            do i = 1, size(self%integration_points)
                write (*, '(a)') "Integration Points: "//to_string(self%integration_points(i))
            end do
        end if

    end subroutine display_geometry_settings

end submodule input_basic_geometry_settings
