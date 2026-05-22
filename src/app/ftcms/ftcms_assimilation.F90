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
        integer(int32) :: surface_node_id

        if (.not. self%assimilation_enabled) return

        ! Surface node: node 1 (top of mesh, z=0).
        ! Assumes 1D mesh ordered top-to-bottom or the first node is the surface.
        nullify (u)
        surface_node_id = 1
        call self%temperature%get_current(u)
        if (associated(u) .and. size(u) >= surface_node_id) then
            Ts = u(surface_node_id)
        else
            Ts = 0.0d0
        end if
        nullify (u)

        call self%pressure%get_current(u)
        if (associated(u) .and. size(u) >= surface_node_id) then
            Pwater = u(surface_node_id)
        else
            Pwater = 0.0d0
        end if
        nullify (u)

        call self%assimilation%set_surface_state(Ts, Pwater)
        call self%assimilation%execute_assimilation_cycle( &
            current_time, current_doy, &
            self%bc(PHYSICS_TYPES%THERMAL%ID), &
            self%bc(PHYSICS_TYPES%HYDRAULIC%ID))

    end subroutine run_assimilation_ftcms

end submodule ftcms_assimilation
