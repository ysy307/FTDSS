submodule(conditions_boundary) conditions_boundary_base
    implicit none
contains

    module subroutine calculate_time_coefficient(time, arr_time, time_coefficient, idx)
        implicit none
        real(real64), intent(in) :: time
        real(real64), intent(in) :: arr_time(:)
        real(real64), intent(inout) :: time_coefficient
        integer(int32), intent(inout) :: idx

        integer(int32) :: i

        do i = 1, size(arr_time(:)) - 1
            if (arr_time(i) < time .and. time <= arr_time(i + 1)) then
                time_coefficient = (time - arr_time(i)) / (arr_time(i + 1) - arr_time(i))
                idx = i
                exit
            end if
        end do

        if (i > size(arr_time) - 1) then
            time_coefficient = 0.0d0
            idx = size(arr_time) - 1
        end if
    end subroutine calculate_time_coefficient

    module subroutine find_target_edges_by_group(domain, i_material, target_edges)
        implicit none
        type(type_domain), intent(inout) :: domain
        integer(int32), intent(in) :: i_material
        integer(int32), allocatable, intent(inout) :: target_edges(:, :)

        ! Local variables
        integer(int32) :: i, num_total_segments, current_segment_idx
        integer(int32) :: side_group, num_sides, side_order
        integer(int32), dimension(:), pointer :: p_conn => null()

        ! 1. Count segments
        num_total_segments = 0
        num_sides = domain%get_num_sides()
        do i = 1, num_sides
            if (allocated(domain%Sides(i)%s)) then
                side_group = domain%Sides(i)%s%get_group()
                if (side_group == i_material) then
                    side_order = domain%Sides(i)%s%get_order()
                    if (side_order == 1) then
                        num_total_segments = num_total_segments + 1
                    elseif (side_order == 2) then
                        num_total_segments = num_total_segments + 2
                    end if
                end if
            end if
        end do

        if (num_total_segments == 0) then
            if (allocated(target_edges)) deallocate (target_edges)
            call global_logger%log_warning(message="No target edges found for material ID: "//to_string(i_material))
            return
        end if

        ! 2. Allocate and fill the array
        call Allocate_Array(target_edges, 2_int32, num_total_segments)
        current_segment_idx = 0
        do i = 1, num_sides
            if (allocated(domain%Sides(i)%s)) then
                side_group = domain%Sides(i)%s%get_group()
                if (side_group == i_material) then
                    side_order = domain%Sides(i)%s%get_order()
                    p_conn => domain%Sides(i)%s%get_connectivity()
                    if (side_order == 1) then
                        current_segment_idx = current_segment_idx + 1
                        target_edges(:, current_segment_idx) = p_conn([1, 2])
                    elseif (side_order == 2) then
                        current_segment_idx = current_segment_idx + 1
                        target_edges(:, current_segment_idx) = p_conn([1, 3])
                        current_segment_idx = current_segment_idx + 1
                        target_edges(:, current_segment_idx) = p_conn([3, 2])
                    end if
                end if
            end if
        end do

    end subroutine find_target_edges_by_group

end submodule conditions_boundary_base
