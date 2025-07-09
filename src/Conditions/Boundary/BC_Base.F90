submodule(Condition_Boundary) Condition_Boundary_Base
    implicit none
contains

    module subroutine Calc_Time_Coefficients(time, arr_time, timeCoe, idx)
        implicit none
        real(real64), intent(in) :: Time
        real(real64), intent(in) :: arr_time(:)
        real(real64), intent(inout) :: timeCoe
        integer(int32), intent(inout) :: idx
        integer(int32) :: i

        do i = 1, size(arr_time(:)) - 1
            if (arr_time(i) < time .and. time <= arr_time(i + 1)) then
                timeCoe = (time - arr_time(i)) / (arr_time(i + 1) - arr_time(i))
                idx = i
                exit
            end if
        end do

        if (i > size(arr_time) - 1) then
            timeCoe = 0.0d0
            idx = size(arr_time) - 1
        end if
    end subroutine Calc_Time_Coefficients

    module subroutine Find_Target_Edges_By_Group(Domain, Input_BC, iGroup, target_edges)
        ! Ensure this subroutine has access to the Domain_Side module
        ! e.g., use :: Domain_Side, only: Abst_SideType, SideHolder
        ! e.g., use :: Core_Vtk, only: VTK_LINE, VTK_QUADRATIC_EDGE
        implicit none
        type(type_domain), intent(in) :: Domain
        type(Input_Boundary), intent(in) :: Input_BC
        integer(int32), intent(in) :: iGroup
        integer(int32), allocatable, intent(inout) :: target_edges(:, :)

        ! Local variables
        integer(int32) :: i, num_total_segments, current_segment_idx
        integer(int32) :: Side_Group, num_Sides, Side_type

        ! 1. Count segments
        num_total_segments = 0
        num_Sides = Domain%get_num_sides() ! Assuming Domain%get_numSides() is a valid function
        do i = 1, num_Sides
            ! Check if the polymorphic component 's' is allocated before using it
            if (allocated(Domain%Sides(i)%s)) then
                Side_Group = Domain%Sides(i)%s%get_group() ! CORRECT: Use the public 'get_group' function
                if (Side_Group == iGroup) then
                    Side_type = Domain%Sides(i)%s%get_type() ! CORRECT: Use the 'get_type' function
                    if (Side_type == VTK_LINE) then
                        num_total_segments = num_total_segments + 1
                    elseif (Side_type == VTK_QUADRATIC_EDGE) then
                        num_total_segments = num_total_segments + 2
                    end if
                end if
            end if
        end do

        if (num_total_segments == 0) then
            if (allocated(target_edges)) deallocate (target_edges)
            return
        end if

        ! 2. Allocate and fill the array
        call Allocate_Array(target_edges, 2_int32, num_total_segments)
        current_segment_idx = 0
        do i = 1, num_Sides
            if (allocated(Domain%Sides(i)%s)) then
                Side_Group = Domain%Sides(i)%s%get_group()
                if (Side_Group == iGroup) then
                    Side_type = Domain%Sides(i)%s%get_type()
                    if (Side_type == VTK_LINE) then
                        current_segment_idx = current_segment_idx + 1
                        ! CORRECT: 'connectivity' is the public component name
                        target_edges(:, current_segment_idx) = Domain%Sides(i)%s%connectivity([1, 2])
                    elseif (Side_type == VTK_QUADRATIC_EDGE) then
                        current_segment_idx = current_segment_idx + 1
                        target_edges(:, current_segment_idx) = Domain%Sides(i)%s%connectivity([1, 3])
                        current_segment_idx = current_segment_idx + 1
                        target_edges(:, current_segment_idx) = Domain%Sides(i)%s%connectivity([3, 2])
                    end if
                end if
            end if
        end do

    end subroutine Find_Target_Edges_By_Group

end submodule Condition_Boundary_Base
