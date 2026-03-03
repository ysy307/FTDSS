submodule(io_output_observation) output_observation_point
    implicit none

contains

    module subroutine initialize_observation_point_node(self, node_id)
        implicit none
        class(type_observation_point_node), intent(inout) :: self
        integer(int32), intent(in) :: node_id

        self%point_type = OUTPUT_OBSERVATION_TYPES%NODE_IDS
        self%node_id = node_id
    end subroutine initialize_observation_point_node

    module subroutine extract_value_node(self, nodal_values, value)
        implicit none
        class(type_observation_point_node), intent(in) :: self
        real(real64), intent(in) :: nodal_values(:)
        real(real64), intent(inout) :: value

        if (value_in_range(self%node_id, 1, size(nodal_values))) then
            value = nodal_values(self%node_id)
        else
            value = 0.0d0
        end if
    end subroutine extract_value_node

    module subroutine initialize_observation_point_coordinate(self, coordinate, fe_id, coordinate_normalized, fe, connectivity)
        implicit none
        class(type_observation_point_coordinate), intent(inout) :: self
        type(type_coordinate_dp), intent(in) :: coordinate
        integer(int32), intent(in) :: fe_id
        type(type_coordinate_dp), intent(in) :: coordinate_normalized
        class(abst_fe), intent(in), pointer :: fe
        integer(int32), intent(in) :: connectivity(:)

        self%point_type = OUTPUT_OBSERVATION_TYPES%COORDINATES

        self%coordinate = coordinate
        self%fe_id = fe_id
        self%coordinate_normalized = coordinate_normalized
        self%fe => fe
        self%connectivity = connectivity
    end subroutine initialize_observation_point_coordinate

    module subroutine extract_value_coordinate(self, nodal_values, value)
        implicit none
        class(type_observation_point_coordinate), intent(in) :: self
        real(real64), intent(in) :: nodal_values(:)
        real(real64), intent(inout) :: value

        value = 0.0d0

        if (associated(self%fe) .and. allocated(self%connectivity)) then
            call self%fe%lerp(self%coordinate_normalized, nodal_values(self%connectivity), value)
        end if

    end subroutine extract_value_coordinate

    module subroutine extract_value_holder_observation_point(self, nodal_values, value)
        implicit none
        class(holder_observation_point), intent(in) :: self
        real(real64), intent(in) :: nodal_values(:)
        real(real64), intent(inout) :: value

        call self%point%extract_value(nodal_values, value)

    end subroutine extract_value_holder_observation_point

end submodule output_observation_point
