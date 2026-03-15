submodule(io_input_geometry) input_geometry_base
    use :: io_input
    implicit none
contains
!================================================================!
    ! Main initialization subroutine
    !================================================================!
    module subroutine initialize_type_input_geometry(self)
        implicit none
        class(type_input_geometry), intent(inout) :: self

        character(len=256), allocatable :: fields_to_read(:)

        character(len=256) :: fullpath

        select type (p => self%parent)
        type is (type_input)

            call self%collect_fields_from_conditions(fields_to_read)

            fullpath = trim(p%input_path)//trim(p%basic%geometry_settings%file_name)

            ! Check array size > 0 instead of using allocated
            if (size(fields_to_read) > 0) then
                if (ends_with(p%basic%geometry_settings%file_name, '.vtk')) then
                    call self%vtk%initialize_vtk( &
                        file_name=strip(fullpath), &
                        global_node_id_key=strip(p%basic%geometry_settings%global_node_id_key), &
                        node_type_key=strip(p%basic%geometry_settings%node_type_key), &
                        num_sharing_ranks_key=strip(p%basic%geometry_settings%num_sharing_ranks_key), &
                        owner_ranks_key=strip(p%basic%geometry_settings%owner_ranks_key), &
                        communication_partners_key=strip(p%basic%geometry_settings%communication_partners_key), &
                        cell_id_key=strip(p%basic%geometry_settings%cell_id_key), &
                        rank_key=strip(p%basic%geometry_settings%rank_key), &
                        color_key=strip(p%basic%geometry_settings%color_key), &
                        point_field_names=fields_to_read)

                else if (ends_with(p%basic%geometry_settings%file_name, '.vtu')) then
                    call self%vtk%initialize_vtu( &
                        file_name=strip(fullpath), &
                        global_node_id_key=strip(p%basic%geometry_settings%global_node_id_key), &
                        node_type_key=strip(p%basic%geometry_settings%node_type_key), &
                        num_sharing_ranks_key=strip(p%basic%geometry_settings%num_sharing_ranks_key), &
                        owner_ranks_key=strip(p%basic%geometry_settings%owner_ranks_key), &
                        communication_partners_key=strip(p%basic%geometry_settings%communication_partners_key), &
                        cell_id_key=strip(p%basic%geometry_settings%cell_id_key), &
                        rank_key=strip(p%basic%geometry_settings%rank_key), &
                        color_key=strip(p%basic%geometry_settings%color_key), &
                        point_field_names=fields_to_read)
                end if

                ! Save loaded field names for later reference
                allocate (self%point_data_names, source=fields_to_read)
                ! Moved deallocate(fields_to_read) to end of block

            else
                if (ends_with(p%basic%geometry_settings%file_name, '.vtk')) then
                    call self%vtk%initialize_vtk( &
                        file_name=strip(fullpath), &
                        global_node_id_key=strip(p%basic%geometry_settings%global_node_id_key), &
                        node_type_key=strip(p%basic%geometry_settings%node_type_key), &
                        num_sharing_ranks_key=strip(p%basic%geometry_settings%num_sharing_ranks_key), &
                        owner_ranks_key=strip(p%basic%geometry_settings%owner_ranks_key), &
                        communication_partners_key=strip(p%basic%geometry_settings%communication_partners_key), &
                        cell_id_key=strip(p%basic%geometry_settings%cell_id_key), &
                        rank_key=strip(p%basic%geometry_settings%rank_key), &
                        color_key=strip(p%basic%geometry_settings%color_key))

                else if (ends_with(p%basic%geometry_settings%file_name, '.vtu')) then
                    call self%vtk%initialize_vtu( &
                        file_name=strip(fullpath), &
                        global_node_id_key=strip(p%basic%geometry_settings%global_node_id_key), &
                        node_type_key=strip(p%basic%geometry_settings%node_type_key), &
                        num_sharing_ranks_key=strip(p%basic%geometry_settings%num_sharing_ranks_key), &
                        owner_ranks_key=strip(p%basic%geometry_settings%owner_ranks_key), &
                        communication_partners_key=strip(p%basic%geometry_settings%communication_partners_key), &
                        cell_id_key=strip(p%basic%geometry_settings%cell_id_key), &
                        rank_key=strip(p%basic%geometry_settings%rank_key), &
                        color_key=strip(p%basic%geometry_settings%color_key))
                end if
            end if

            ! Always deallocate here
            if (allocated(fields_to_read)) deallocate (fields_to_read)

        end select

    end subroutine initialize_type_input_geometry

    module subroutine collect_fields_from_conditions(self, field_list)
        implicit none
        class(type_input_geometry), intent(inout) :: self
        character(len=256), allocatable, intent(out) :: field_list(:)

        character(len=256) :: temp_list(IC_TARGETS%NUM_ID)
        character(len=256) :: current_field_name

        integer(int32) :: num_fields, i, k
        logical :: is_duplicate

        num_fields = 0

        select type (p => self%parent)
        type is (type_input)
            ! Loop over all initial conditions by index
            do i = 1, IC_TARGETS%NUM_ID

                ! Skip if this analysis type is not active
                if (.not. p%basic%analysis_controls%is_active(i)) cycle

                ! Process only if initial condition reads from file
                if (p%conditions%initial_conditions%physics(i)%type == IC_METHODS%FROM_FILE%NAME) then

                    ! Check if field name is assigned
                    if (allocated(p%conditions%initial_conditions%physics(i)%field_name)) then
                        current_field_name = p%conditions%initial_conditions%physics(i)%field_name

                        ! Check for duplicates
                        is_duplicate = .false.
                        do k = 1, num_fields
                            if (trim(temp_list(k)) == trim(current_field_name)) then
                                is_duplicate = .true.
                                exit
                            end if
                        end do

                        ! Add to list if not a duplicate
                        if (.not. is_duplicate) then
                            num_fields = num_fields + 1
                            temp_list(num_fields) = current_field_name
                        end if
                    end if
                end if
            end do

            ! Allocate return array with collected field names
            if (num_fields > 0) then
                allocate (field_list(num_fields))
                field_list = temp_list(1:num_fields)
            else
                ! If none found, allocate size-0 array and return
                allocate (field_list(0))
            end if
        end select
    end subroutine collect_fields_from_conditions

end submodule input_geometry_base
