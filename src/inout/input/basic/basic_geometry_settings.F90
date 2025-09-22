submodule(inout_input_basic) inout_input_basic_geometry_settings
    implicit none
    !!------------------------------------------------------------------------------------------------------------------------------
    ! JSON key names for geometry settings
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: geometry_settings = "geometry_settings"
    character(*), parameter :: mesh_file_name = "mesh_file_name"
    character(*), parameter :: global_node_id_key = "global_node_id_key"
    character(*), parameter :: node_type_key = "node_type_key"
    character(*), parameter :: num_sharing_ranks_key = "num_sharing_ranks_key"
    character(*), parameter :: owner_ranks_key = "owner_ranks_key"
    character(*), parameter :: communication_partners_key = "communication_partners_key"
    character(*), parameter :: cell_id_key = "cell_id_key"
    character(*), parameter :: rank_key = "rank_key"
    character(*), parameter :: color_key = "color_key"
    character(*), parameter :: integration = "integration"
    character(*), parameter :: integration_type = "integration_type"
    character(*), parameter :: valid_integration_types(3) = [character(len=16) :: "full", "reduced", "free"]
    character(*), parameter :: integration_points = "integration_points"
contains
    module subroutine read_parameters_geometry_settings(self, json)
        implicit none
        class(type_input_basic), intent(inout) :: self
        type(json_file), intent(inout) :: json

        character(256) :: buffer(3) = [character(256) :: geometry_settings, "", ""]

        buffer(2) = mesh_file_name
        call get_json_value(json, join(buffer), self%geometry_settings%file_name, &
                            is_required=.true.)

        buffer(2) = global_node_id_key
        call get_json_value(json, join(buffer), self%geometry_settings%global_node_id_key, &
                            is_required=.true., default_value=global_node_id_key)
        buffer(2) = node_type_key
        call get_json_value(json, join(buffer), self%geometry_settings%node_type_key, &
                            is_required=.true., default_value=node_type_key)

        buffer(2) = num_sharing_ranks_key
        call get_json_value(json, join(buffer), self%geometry_settings%num_sharing_ranks_key, &
                            is_required=.true., default_value=num_sharing_ranks_key)

        buffer(2) = owner_ranks_key
        call get_json_value(json, join(buffer), self%geometry_settings%owner_ranks_key, &
                            is_required=.true., default_value=owner_ranks_key)

        buffer(2) = communication_partners_key
        call get_json_value(json, join(buffer), self%geometry_settings%communication_partners_key, &
                            is_required=.true., default_value=communication_partners_key)

        buffer(2) = cell_id_key
        call get_json_value(json, join(buffer), self%geometry_settings%cell_id_key, &
                            is_required=.true., default_value=cell_id_key)

        buffer(2) = rank_key
        call get_json_value(json, join(buffer), self%geometry_settings%rank_key, &
                            is_required=.true., default_value=rank_key)

        buffer(2) = color_key
        call get_json_value(json, join(buffer), self%geometry_settings%color_key, &
                            is_required=.true., default_value=color_key)

        buffer(2) = integration
        buffer(3) = integration_type
        call get_json_value(json, join(buffer), self%geometry_settings%integration_type, &
                            is_required=.true., default_value="full", valid_list=valid_integration_types)

        if (strip(self%geometry_settings%integration_type) == "free") then
            buffer(3) = integration_points
            call get_json_value(json, join(buffer), self%geometry_settings%integration_points, &
                                is_required=.true., default_value=0.5d0, valid_range=[0.0d0, 1.0d0])
        end if

    end subroutine read_parameters_geometry_settings

    module subroutine display_geometry_settings(self)
        implicit none
        class(type_geometry_settings) :: self

        write (*, '(a)') "Mesh File Name: "//strip(self%file_name)
        write (*, '(a)') "Global Node ID Key: "//strip(self%global_node_id_key)
        write (*, '(a)') "Node Type Key: "//strip(self%node_type_key)
        write (*, '(a)') "Owner Ranks Key: "//strip(self%owner_ranks_key)
        write (*, '(a)') "Communication Partners Key: "//strip(self%communication_partners_key)
        write (*, '(a)') "Cell ID Key: "//strip(self%cell_id_key)
        write (*, '(a)') "Rank Key: "//strip(self%rank_key)
        write (*, '(a)') "Color Key: "//strip(self%color_key)
        write (*, '(a)') "Integration Type: "//strip(self%integration_type)
        if (strip(self%integration_type) == "free") then
            write (*, '(a)') "Integration Points: "//to_string(self%integration_points)
        end if

    end subroutine display_geometry_settings

end submodule inout_input_basic_geometry_settings
