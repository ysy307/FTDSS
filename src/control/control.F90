module module_control
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_input, only:type_input
    use :: control_time, only:type_time
    use :: control_time_profiler, only:type_profiler
    use :: control_iteration, only:type_iteration
    use :: control_output, only:type_output_manager
    use :: control_openmp, only:initialize_openmp
    implicit none
    private

    public :: type_time
    public :: type_iteration
    public :: type_controls

    type :: type_controls
        private
        logical :: is_active(PHYSICS_TYPES%NUM_ID) = .false.
        type(type_constant_id) :: coupling_mode
        ! integer(int32) :: coupling_mode
        logical, allocatable :: thermal(:)
        logical, allocatable :: hydraulic(:)
        logical, allocatable :: mechanical(:)

        type(type_iteration), public :: iteration
        type(type_time), public :: time
        type(type_profiler), public :: profiler

        type(type_output_manager), public :: out_field
        type(type_output_manager), public :: out_history

    contains
        procedure, pass(self), public :: initialize => initialize_type_controls
        procedure, pass(self), public :: is_physics_active => is_physics_active_control
        procedure, pass(self), public :: is_target => is_target_control
        procedure, pass(self), public :: get_coupling_mode => get_coupling_mode_control
        procedure, pass(self) :: display => display_controls
    end type type_controls

contains
    subroutine initialize_type_controls(self, input)
        implicit none
        class(type_controls), intent(inout) :: self
        class(type_input), intent(in) :: input

        integer(int32), allocatable :: unique_material_ids(:)
        integer(int32) :: ierr
        integer(int32) :: i, num_unique_regions, max_region_id
        integer(int32) :: current_material_id
        character(len=10), allocatable :: profiler_labels(:)
        real(real64) :: current_time_s

        ierr = 0
        call input%geometry%vtk%get_active_region_info(unique_material_ids)
        if (ierr /= 0) return
        if (.not. allocated(unique_material_ids) .or. size(unique_material_ids) == 0) then
            ierr = -1
            print *, "Error: No active material regions found."
            stop 1
        end if

        num_unique_regions = size(unique_material_ids)
        max_region_id = maxval(unique_material_ids)

        self%is_active(PHYSICS_TYPE_THERMAL) = input%basic%analysis_controls%is_active(PHYSICS_TYPE_THERMAL)
        if (self%is_active(PHYSICS_TYPE_THERMAL)) then
            allocate (self%thermal(max_region_id))
            self%thermal = .false.
        end if

        self%is_active(PHYSICS_TYPE_HYDRAULIC) = input%basic%analysis_controls%is_active(PHYSICS_TYPE_HYDRAULIC)
        if (self%is_active(PHYSICS_TYPE_HYDRAULIC)) then
            allocate (self%hydraulic(max_region_id))
            self%hydraulic = .false.
        end if

        self%is_active(PHYSICS_TYPE_MECHANICAL) = input%basic%analysis_controls%is_active(PHYSICS_TYPE_MECHANICAL)
        if (self%is_active(PHYSICS_TYPE_MECHANICAL)) then
            allocate (self%mechanical(max_region_id))
            self%mechanical = .false.
        end if

        do i = 1, num_unique_regions
            current_material_id = unique_material_ids(i)
            if (current_material_id > size(input%basic%materials)) cycle

            if (self%is_active(PHYSICS_TYPE_THERMAL)) then
                self%thermal(current_material_id) = &
                    input%basic%materials(current_material_id)%is_active(PHYSICS_TYPE_THERMAL)
            end if

            if (self%is_active(PHYSICS_TYPE_HYDRAULIC)) then
                self%hydraulic(current_material_id) = &
                    input%basic%materials(current_material_id)%is_active(PHYSICS_TYPE_HYDRAULIC)
            end if

            if (self%is_active(PHYSICS_TYPE_MECHANICAL)) then
                self%mechanical(current_material_id) = &
                    input%basic%materials(current_material_id)%is_active(PHYSICS_TYPE_MECHANICAL)
            end if
        end do

        self%coupling_mode = COUPLING_MODES%to_object(input%basic%analysis_controls%coupling_mode)

        call self%time%initialize(input)
        call self%iteration%initialize(input)
        call initialize_openmp(input)

        call self%time%get_time(current_time_s)
        associate (field_output => input%output_settings%field_output)
            call self%out_field%initialize(field_output%output_interval_step, &
                                           field_output%output_interval_unit, &
                                           field_output%output_time_unit, &
                                           field_output%file_format, &
                                           current_time_s)
        end associate
        associate (history_output => input%output_settings%history_output)
            call self%out_history%initialize(history_output%output_interval_step, &
                                             history_output%output_interval_unit, &
                                             history_output%output_time_unit, &
                                             history_output%file_format, &
                                             current_time_s)
        end associate

        call deallocate_array(unique_material_ids)

    end subroutine initialize_type_controls

    ! -----------------------------------------------------------------
    ! 指定された物理現象と材料IDが計算対象かどうかを判定する
    ! -----------------------------------------------------------------
    pure function is_target_control(self, target_physics, material_id) result(is_active)
        implicit none
        class(type_controls), intent(in) :: self
        integer, intent(in) :: target_physics
        integer(int32), intent(in) :: material_id
        logical :: is_active

        if (.not. self%is_active(target_physics)) then
            is_active = .false.
            return
        end if

        is_active = .false.

        select case (target_physics)
        case (PHYSICS_TYPE_THERMAL)
            if (allocated(self%thermal)) then
                if (material_id <= ubound(self%thermal, 1)) then
                    is_active = self%thermal(material_id)
                end if
            end if

        case (PHYSICS_TYPE_HYDRAULIC)
            if (allocated(self%hydraulic)) then
                if (material_id <= ubound(self%hydraulic, 1)) then
                    is_active = self%hydraulic(material_id)
                end if
            end if

        case (PHYSICS_TYPE_MECHANICAL)
            if (allocated(self%mechanical)) then
                if (material_id <= ubound(self%mechanical, 1)) then
                    is_active = self%mechanical(material_id)
                end if
            end if
        end select
    end function is_target_control

    !>
    !> 指定された物理定数が計算対象かどうかを判定する
    pure function is_physics_active_control(self, physics_type) result(is_active)
        implicit none
        !> Instance of control settings
        class(type_controls), intent(in) :: self
        !> Physics type identifier in PHYSICS_TYPES
        type(type_constant_id), intent(in) :: physics_type
        !> Returns `true` if the physics type is active
        logical :: is_active

        if (PHYSICS_TYPES%in_group(physics_type)) then
            if (physics_type%id < 1 .or. physics_type%id > PHYSICS_TYPES%NUM_ID) then
                is_active = .false.
            end if
            is_active = self%is_active(physics_type%id)
        else
            is_active = .false.
        end if

    end function is_physics_active_control

    subroutine get_coupling_mode_control(self, coupling_mode)
        implicit none
        class(type_controls), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: coupling_mode

        coupling_mode => self%coupling_mode
    end subroutine get_coupling_mode_control

    subroutine reset_controls(self)
        implicit none
        class(type_controls), intent(inout) :: self

        call self%iteration%reset()

    end subroutine reset_controls

    subroutine display_controls(self)
        implicit none
        class(type_controls), intent(in) :: self
        integer(int32) :: i

        write (*, '(a)') "# Control Settings"
        write (*, '(a)') "## Active Physics Types:"
        if (self%is_active(PHYSICS_TYPE_THERMAL)) then
            write (*, '(a)') "- Thermal"
        end if
        if (self%is_active(PHYSICS_TYPE_HYDRAULIC)) then
            write (*, '(a)') "- Hydraulic"
        end if
        if (self%is_active(PHYSICS_TYPE_MECHANICAL)) then
            write (*, '(a)') "- Mechanical"
        end if

        write (*, '(a)') "## Coupling Mode:"
        write (*, '(a)') "- "//trim(self%coupling_mode%name)

        if (allocated(self%thermal)) then
            write (*, '(a)') "### Thermal Material Flags:"
            do i = 1, size(self%thermal)
                write (*, '(a,i0,a,l1)') "- Material ID ", i, ": ", self%thermal(i)
            end do
        end if

        if (allocated(self%hydraulic)) then
            write (*, '(a)') "### Hydraulic Material Flags:"
            do i = 1, size(self%hydraulic)
                write (*, '(a,i0,a,l1)') "- Material ID ", i, ": ", self%hydraulic(i)
            end do
        end if

        if (allocated(self%mechanical)) then
            write (*, '(a)') "### Mechanical Material Flags:"
            do i = 1, size(self%mechanical)
                write (*, '(a,i0,a,l1)') "- Material ID ", i, ": ", self%mechanical(i)
            end do
        end if

        write (*, '(a)') "## Time and Iteration Settings"
        call self%time%display()
        ! call self%iteration%display()

        write (*, '(a)') "---"
    end subroutine display_controls

end module module_control
