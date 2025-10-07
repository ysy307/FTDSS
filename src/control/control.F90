module module_control
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_input, only:type_input
    use :: control_time, only:type_time
    use :: control_iteration, only:type_iteration
    use :: control_openmp, only:initialize_openmp
    implicit none
    private

    public :: type_time
    public :: type_iteration
    public :: type_controls

    type :: type_controls
        logical :: is_active(NUM_PHYSICS_TYPES) = .false.
        integer(int32) :: coupling_mode
        ! --- マテリアルごとのフラグ ---
        logical, allocatable :: thermal(:)
        logical, allocatable :: hydraulic(:)
        logical, allocatable :: mechanical(:)

        type(type_iteration) :: iteration
        type(type_time) :: time
    contains
        procedure :: initialize => initialize_type_controls
        procedure :: is_target => should_calculate_target
        procedure, pass(self) :: display => display_controls
    end type type_controls

contains
    subroutine initialize_type_controls(self, input)
        implicit none
        class(type_controls), intent(inout) :: self
        class(type_input), intent(in), optional :: input

        integer(int32), allocatable :: unique_material_ids(:)
        integer(int32) :: ierr
        integer(int32) :: i, num_unique_regions, max_region_id
        integer(int32) :: current_material_id
        character(len=10), allocatable :: profiler_labels(:)

        if (present(input)) then

            ierr = 0
            call input%geometry%vtk%get_active_region_info(unique_material_ids)
            if (ierr /= 0) return
            if (.not. allocated(unique_material_ids) .or. size(unique_material_ids) == 0) then
                ierr = -1 ! エラーコード
                print *, "Error: No active material regions found."
                stop 1
            end if

            num_unique_regions = size(unique_material_ids)
            max_region_id = maxval(unique_material_ids)

            ! [修正] PHYSICS_TYPE_* 定数を使用
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

            self%coupling_mode = input%basic%analysis_controls%coupling_mode

            call self%time%initialize(input=input)
            call self%iteration%initialize(input)
            call initialize_openmp(input)
        else
            profiler_labels = [character(len=10) :: "IO", "Setup", "Assemble", "Solve", "Total"]
            call self%time%initialize(profiler_sections=profiler_labels)
            call self%time%Record(TIME_RECORD_START)
            call self%time%Profile_Start("Total")
            call self%time%Profile_Start("IO")
        end if

        call deallocate_array(unique_material_ids)

    end subroutine initialize_type_controls

    ! -----------------------------------------------------------------
    ! 指定された物理現象と材料IDが計算対象かどうかを判定する
    ! -----------------------------------------------------------------
    pure function should_calculate_target(self, target_id, i_material) result(is_active)
        implicit none
        class(type_controls), intent(in) :: self
        integer, intent(in) :: target_id
        integer(int32), intent(in) :: i_material
        logical :: is_active

        ! [修正] まず全体フラグをチェックし、falseなら即座にリターン
        if (.not. self%is_active(target_id)) then
            is_active = .false.
            return
        end if

        is_active = .false.

        ! [修正] PHYSICS_TYPE_* 定数を使用
        select case (target_id)
        case (PHYSICS_TYPE_THERMAL)
            if (allocated(self%thermal)) then
#ifdef USE_DEBUG
                if (i_material <= ubound(self%thermal, 1)) then
#endif
                    is_active = self%thermal(i_material)
#ifdef USE_DEBUG
                end if
#endif
            end if

        case (PHYSICS_TYPE_HYDRAULIC)
            if (allocated(self%hydraulic)) then
#ifdef USE_DEBUG
                if (i_material <= ubound(self%hydraulic, 1)) then
#endif
                    is_active = self%hydraulic(i_material)
#ifdef USE_DEBUG
                end if
#endif
            end if

        case (PHYSICS_TYPE_MECHANICAL)
            if (allocated(self%mechanical)) then
#ifdef USE_DEBUG
                if (i_material <= ubound(self%mechanical, 1)) then
#endif
                    is_active = self%mechanical(i_material)
#ifdef USE_DEBUG
                end if
#endif
            end if
        end select
    end function should_calculate_target

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
        write (*, '(a)') "- "//trim(get_coupling_mode_string(self%coupling_mode))

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
        call self%iteration%display()

        write (*, '(a)') "---"
    end subroutine display_controls

end module module_control
