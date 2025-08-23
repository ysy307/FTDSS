module module_control
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: control_time, only:type_time
    use :: control_iteration, only:type_iteration
    use :: control_openmp, only:initialize_openmp
    use :: module_input, only:type_input
    implicit none
    private

    public :: type_time
    public :: type_iteration
    public :: type_controls

    type :: type_controls
        logical :: calculate_thermal
        logical :: calculate_hydraulic
        logical :: calculate_mechanical
        character(:), allocatable :: coupling_mode
        ! --- マテリアルごとのフラグ ---
        logical, allocatable :: thermal(:)
        logical, allocatable :: hydraulic(:)
        logical, allocatable :: mechanical(:)

        type(type_iteration) :: iteration
        type(type_time) :: time
    contains
        procedure :: initialize => initialize_type_controls
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

        ierr = 0
        ! アクティブなマテリアル領域の情報を取得
        call input%geometry%vtk%get_active_region_info(unique_material_ids, ierr)
        if (ierr /= 0) return
        if (.not. allocated(unique_material_ids) .or. size(unique_material_ids) == 0) then
            ierr = -1 ! エラーコード
            print *, "Error: No active material regions found."
            stop 1
        end if

        num_unique_regions = size(unique_material_ids)
        max_region_id = maxval(unique_material_ids)

        ! 全体的な計算フラグを設定し、配列を割り当てて初期化
        if (input%basic%analysis_controls%calculate_thermal) then
            self%calculate_thermal = .true.
            allocate (self%thermal(max_region_id))
            self%thermal = .false. ! 配列全体を .false. で初期化
        else
            self%calculate_thermal = .false.
        end if

        if (input%basic%analysis_controls%calculate_hydraulic) then
            self%calculate_hydraulic = .true.
            allocate (self%hydraulic(max_region_id))
            self%hydraulic = .false.
        else
            self%calculate_hydraulic = .false.
        end if

        if (input%basic%analysis_controls%calculate_mechanical) then
            self%calculate_mechanical = .true.
            allocate (self%mechanical(max_region_id))
            self%mechanical = .false.
        else
            self%calculate_mechanical = .false.
        end if

        ! アクティブな各マテリアル領域に対してフラグを立てる
        do i = 1, num_unique_regions
            current_material_id = unique_material_ids(i)

            if (self%calculate_thermal) then
                self%thermal(current_material_id) = input%basic%materials(i)%calculate_thermal
            end if

            if (self%calculate_hydraulic) then
                self%hydraulic(current_material_id) = input%basic%materials(i)%calculate_hydraulic
            end if

            if (self%calculate_mechanical) then
                self%mechanical(current_material_id) = input%basic%materials(i)%calculate_mechanical
            end if
        end do

        ! coupling_modeの設定
        self%coupling_mode = input%basic%analysis_controls%coupling_mode

        call self%time%initialize(input)
        call self%iteration%initialize(input)

        call initialize_openmp(input)

        call deallocate_array(unique_material_ids)

    end subroutine initialize_type_controls
end module module_control

