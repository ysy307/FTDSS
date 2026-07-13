submodule(app_ftcms) ftcms_assimilation
    implicit none

contains

    !> Runs one ETKF assimilation cycle: fetches surface state, invokes controller.
    module subroutine run_assimilation_ftcms(self, current_time, current_doy)
        implicit none
        class(type_ftcms), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: current_doy

        real(real64) :: Ts, Pwater
        real(real64), pointer, contiguous :: u(:)
        integer(int32) :: i_patch, num_patches, entity_id, i, n
        integer(int32) :: node_id
        real(real64) :: Ts_sum, Pw_sum
        type(type_boundary_patch), pointer :: bc_patch
        logical, allocatable :: on_other_entity(:)

        if (.not. self%assimilation_enabled) return

        ! Build exclusion mask: nodes shared with any BC entity other than 3.
        call self%domain%get_num_bc_patches(num_patches)
        nullify (u)
        call self%temperature%get_current(u)
        if (associated(u)) then
            allocate (on_other_entity(size(u)), source=.false.)
            do i_patch = 1, num_patches
                nullify (bc_patch)
                call self%domain%get_bc_patch(i_patch, bc_patch)
                if (bc_patch%entity_id == 3) cycle
                if (.not. allocated(bc_patch%connectivity%col_ind)) cycle
                do i = 1, size(bc_patch%connectivity%col_ind)
                    node_id = bc_patch%connectivity%col_ind(i)
                    if (node_id >= 1 .and. node_id <= size(u)) on_other_entity(node_id) = .true.
                end do
            end do
        end if

        ! Average temperature over entity 3 nodes not shared with other entities.
        Ts_sum = 0.0d0; n = 0
        do i_patch = 1, num_patches
            nullify (bc_patch)
            call self%domain%get_bc_patch(i_patch, bc_patch)
            entity_id = bc_patch%entity_id
            if (entity_id /= 3) cycle
            if (.not. allocated(bc_patch%connectivity%col_ind)) cycle
            do i = 1, size(bc_patch%connectivity%col_ind)
                node_id = bc_patch%connectivity%col_ind(i)
                if (associated(u) .and. node_id >= 1 .and. node_id <= size(u)) then
                    if (allocated(on_other_entity)) then
                        if (on_other_entity(node_id)) cycle
                    end if
                    Ts_sum = Ts_sum + u(node_id)
                    n = n + 1
                end if
            end do
        end do
        if (n == 0 .and. associated(u)) then
            ! Fallback: use all entity-3 nodes if the exclusion mask removed everything.
            Ts_sum = 0.0d0; n = 0
            do i_patch = 1, num_patches
                nullify (bc_patch)
                call self%domain%get_bc_patch(i_patch, bc_patch)
                if (bc_patch%entity_id /= 3) cycle
                if (.not. allocated(bc_patch%connectivity%col_ind)) cycle
                do i = 1, size(bc_patch%connectivity%col_ind)
                    node_id = bc_patch%connectivity%col_ind(i)
                    if (node_id >= 1 .and. node_id <= size(u)) then
                        Ts_sum = Ts_sum + u(node_id)
                        n = n + 1
                    end if
                end do
            end do
        end if
        if (n > 0) then
            Ts = Ts_sum / real(n, real64)
        else
            Ts = 0.0d0
        end if
        nullify (u)

        call self%pressure%get_current(u)
        Pw_sum = 0.0d0; n = 0
        do i_patch = 1, num_patches
            nullify (bc_patch)
            call self%domain%get_bc_patch(i_patch, bc_patch)
            if (bc_patch%entity_id /= 3) cycle
            if (.not. allocated(bc_patch%connectivity%col_ind)) cycle
            do i = 1, size(bc_patch%connectivity%col_ind)
                node_id = bc_patch%connectivity%col_ind(i)
                if (associated(u) .and. node_id >= 1 .and. node_id <= size(u)) then
                    if (allocated(on_other_entity)) then
                        if (size(on_other_entity) == size(u)) then
                            if (on_other_entity(node_id)) cycle
                        end if
                    end if
                    Pw_sum = Pw_sum + u(node_id)
                    n = n + 1
                end if
            end do
        end do
        if (n == 0 .and. associated(u)) then
            Pw_sum = 0.0d0; n = 0
            do i_patch = 1, num_patches
                nullify (bc_patch)
                call self%domain%get_bc_patch(i_patch, bc_patch)
                if (bc_patch%entity_id /= 3) cycle
                if (.not. allocated(bc_patch%connectivity%col_ind)) cycle
                do i = 1, size(bc_patch%connectivity%col_ind)
                    node_id = bc_patch%connectivity%col_ind(i)
                    if (node_id >= 1 .and. node_id <= size(u)) then
                        Pw_sum = Pw_sum + u(node_id)
                        n = n + 1
                    end if
                end do
            end do
        end if
        Pwater = merge(Pw_sum / real(n, real64), 0.0d0, n > 0)
        if (allocated(on_other_entity)) deallocate (on_other_entity)
        nullify (u)

        call self%assimilation%set_surface_state(Ts, Pwater)
        call self%assimilation%execute_assimilation_cycle( &
            current_time, current_doy, &
            self%bc(PHYSICS_TYPES%THERMAL%ID), &
            self%bc(PHYSICS_TYPES%HYDRAULIC%ID))

    end subroutine run_assimilation_ftcms

end submodule ftcms_assimilation
