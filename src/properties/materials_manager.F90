module properties_material_manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_input, only:type_input
    use :: module_calculate, only: &
        holder_gccs, holder_wrfs, holder_dens, holder_sphs, holder_vhcs, holder_thcs, &
        abst_gcc, abst_wrf, abst_den, abst_sph, abst_vhc, abst_thc
    implicit none
    public

#ifdef USE_DEBUG
    logical, parameter, private :: debug_mode = .true.
#else
    logical, parameter, private :: debug_mode = .false.
#endif

    public :: type_material_manager

    type :: type_material_manager
        private
        type(holder_thcs), allocatable :: thc(:)
        type(holder_dens), allocatable :: den(:)
        type(holder_sphs), allocatable :: sph(:)
        type(holder_vhcs), allocatable :: vhc(:)
        type(holder_gccs), allocatable :: gcc(:)
        type(holder_wrfs), allocatable :: wrf(:)

        integer(int32), allocatable :: region_id_map(:)
    contains
        procedure, public, pass(self) :: initialize

        procedure, public, pass(self) :: get_thc => get_thc_ptr
        procedure, public, pass(self) :: get_den => get_den_ptr
        procedure, public, pass(self) :: get_sph => get_sph_ptr
        procedure, public, pass(self) :: get_vhc => get_vhc_ptr
        procedure, public, pass(self) :: get_gcc => get_gcc_ptr
        procedure, public, pass(self) :: get_wrf => get_wrf_ptr

    end type type_material_manager

contains

    ! 初期化（holder内部のinitialize呼ぶ）
    subroutine initialize(self, input, ierr)
        class(type_material_manager), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(inout) :: ierr

        integer(int32) :: model_idx
        integer(int32) :: num_unique_regions
        integer(int32) :: max_region_id
        integer(int32), allocatable :: unique_material_ids(:)
        integer(int32) :: current_material_id

        ierr = 0
        call input%geometry%vtk%get_active_region_info(unique_material_ids, ierr)
        if (ierr /= 0) return
        if (.not. allocated(unique_material_ids) .or. size(unique_material_ids) == 0) then
            ierr = -1 ! エラーコード
            print *, "Error: No active material regions found."
            stop 1
        end if

        ! 実際に存在するユニーク領域の数を正とする
        num_unique_regions = size(unique_material_ids)
        max_region_id = maxval(unique_material_ids)

        allocate (self%thc(num_unique_regions))
        allocate (self%den(num_unique_regions))
        allocate (self%sph(num_unique_regions))
        allocate (self%vhc(num_unique_regions))
        allocate (self%gcc(num_unique_regions))
        allocate (self%wrf(num_unique_regions))

        ! region_idの最大値でマップ配列を確保し、0(無効値)で初期化
        allocate (self%region_id_map(max_region_id), source=0)

        do model_idx = 1, num_unique_regions
            current_material_id = unique_material_ids(model_idx)
            call self%thc(model_idx)%initialize(input, current_material_id)
            call self%den(model_idx)%initialize(input, current_material_id)
            call self%sph(model_idx)%initialize(input, current_material_id)
            call self%vhc(model_idx)%initialize(input, current_material_id)
            call self%gcc(model_idx)%initialize(input, current_material_id)
            call self%wrf(model_idx)%initialize(input, current_material_id)

            ! 正しいマッピング: region_idをインデックスとして、配列のインデックスを格納
            self%region_id_map(current_material_id) = model_idx
        end do
    end subroutine initialize

    ! 以下はすべてポインタを返すgetterのみ（デバッグ時のみエラーチェック）

    ! THC getter
    subroutine get_thc_ptr(self, region_id, thc_ptr)
        implicit none
        class(type_material_manager), intent(in), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_thc), intent(inout), pointer :: thc_ptr

        integer(int32) :: model_index

        if (debug_mode) then
            if (region_id < 1 .or. region_id > size(self%region_id_map)) then
                print *, "Error: Invalid region_id in get_thc_ptr:", region_id
                nullify (thc_ptr)
                stop 1
            end if
        end if

        model_index = self%region_id_map(region_id)
        if (debug_mode) then
            if (model_index == 0) then
                print *, "Error: region_id not mapped in get_thc_ptr:", region_id
                nullify (thc_ptr)
                stop 1
            end if
        end if

        thc_ptr => self%thc(model_index)%p
    end subroutine get_thc_ptr

    ! DEN getter
    subroutine get_den_ptr(self, region_id, den_ptr)
        implicit none
        class(type_material_manager), intent(in), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_den), intent(inout), pointer :: den_ptr

        integer(int32) :: model_index

        if (debug_mode) then
            if (region_id < 1 .or. region_id > size(self%region_id_map)) then
                print *, "Error: Invalid region_id in get_den_ptr:", region_id
                nullify (den_ptr)
                stop 1
            end if
        end if

        model_index = self%region_id_map(region_id)
        if (debug_mode) then
            if (model_index == 0) then
                print *, "Error: region_id not mapped in get_den_ptr:", region_id
                nullify (den_ptr)
                stop 1
            end if
        end if

        den_ptr => self%den(model_index)%p
    end subroutine get_den_ptr

    ! SPH getter
    subroutine get_sph_ptr(self, region_id, sph_ptr)
        implicit none
        class(type_material_manager), intent(in), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_sph), intent(inout), pointer :: sph_ptr

        integer(int32) :: model_index

        if (debug_mode) then
            if (region_id < 1 .or. region_id > size(self%region_id_map)) then
                print *, "Error: Invalid region_id in get_sph_ptr:", region_id
                nullify (sph_ptr)
                stop 1
            end if
        end if

        model_index = self%region_id_map(region_id)
        if (debug_mode) then
            if (model_index == 0) then
                print *, "Error: region_id not mapped in get_sph_ptr:", region_id
                nullify (sph_ptr)
                stop 1
            end if
        end if

        sph_ptr => self%sph(model_index)%p
    end subroutine get_sph_ptr

    ! VHC getter
    subroutine get_vhc_ptr(self, region_id, vhc_ptr)
        implicit none
        class(type_material_manager), intent(in), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_vhc), intent(inout), pointer :: vhc_ptr

        integer(int32) :: model_index

        if (debug_mode) then
            if (region_id < 1 .or. region_id > size(self%region_id_map)) then
                print *, "Error: Invalid region_id in get_vhc_ptr:", region_id
                nullify (vhc_ptr)
                stop 1
            end if
        end if

        model_index = self%region_id_map(region_id)
        if (debug_mode) then
            if (model_index == 0) then
                print *, "Error: region_id not mapped in get_vhc_ptr:", region_id
                nullify (vhc_ptr)
                stop 1
            end if
        end if

        vhc_ptr => self%vhc(model_index)%p
    end subroutine get_vhc_ptr

    ! GCC getter
    subroutine get_gcc_ptr(self, region_id, gcc_ptr)
        implicit none
        class(type_material_manager), intent(in), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_gcc), intent(inout), pointer :: gcc_ptr

        integer(int32) :: model_index

        if (debug_mode) then
            if (region_id < 1 .or. region_id > size(self%region_id_map)) then
                print *, "Error: Invalid region_id in get_gcc_ptr:", region_id
                nullify (gcc_ptr)
                stop 1
            end if
        end if

        model_index = self%region_id_map(region_id)
        if (debug_mode) then
            if (model_index == 0) then
                print *, "Error: region_id not mapped in get_gcc_ptr:", region_id
                nullify (gcc_ptr)
                stop 1
            end if
        end if

        gcc_ptr => self%gcc(model_index)%p
    end subroutine get_gcc_ptr

    ! WRF getter
    subroutine get_wrf_ptr(self, region_id, wrf_ptr)
        implicit none
        class(type_material_manager), intent(in), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_wrf), intent(inout), pointer :: wrf_ptr

        integer(int32) :: model_index

        if (debug_mode) then
            if (region_id < 1 .or. region_id > size(self%region_id_map)) then
                print *, "Error: Invalid region_id in get_wrf_ptr:", region_id
                nullify (wrf_ptr)
                stop 1
            end if
        end if

        model_index = self%region_id_map(region_id)
        if (debug_mode) then
            if (model_index == 0) then
                print *, "Error: region_id not mapped in get_wrf_ptr:", region_id
                nullify (wrf_ptr)
                stop 1
            end if
        end if

        wrf_ptr => self%wrf(model_index)%p
    end subroutine get_wrf_ptr

end module properties_material_manager
