module physics_registry
    use, intrinsic :: iso_fortran_env, only: int32, real64
    ! use :: module_input, only:type_input
    use :: physics_material_thermal_conductivity, only:holder_thcs, abst_thc
    ! use :: physics_material_heat_capacity, only:holder_vhcs, abst_vhc
    use :: physics_material_density, only:holder_dens, abst_den
    use :: physics_material_specific_heat, only:holder_sphs, abst_sph
    ! use :: physics_models_hcf, only:holder_hcfs, abst_hcf
    ! use :: physics_models_gcc, only:holder_gccs, abst_gcc
    ! use :: physics_models_wrf, only:holder_wrfs, abst_wrf
    implicit none

    public :: type_physics_registry

    type :: type_physics_registry
        private
        type(holder_thcs), allocatable :: thc(:)
        type(holder_dens), allocatable :: den(:)
        type(holder_sphs), allocatable :: sph(:)
        ! type(holder_vhcs), allocatable :: vhc(:)
        ! type(holder_gccs), allocatable :: gcc(:)
        ! type(holder_wrfs), allocatable :: wrf(:)
        ! type(holder_hcfs), allocatable :: hcf(:)

        integer(int32), allocatable :: region_id_map(:)
    contains
        procedure, public, pass(self) :: initialize => initialize_type_physics_registry

        procedure, public, pass(self) :: get_thc => get_thc_ptr
        procedure, public, pass(self) :: get_den => get_den_ptr
        procedure, public, pass(self) :: get_sph => get_sph_ptr
        procedure, public, pass(self) :: get_vhc => get_vhc_ptr
        procedure, public, pass(self) :: get_gcc => get_gcc_ptr
        procedure, public, pass(self) :: get_wrf => get_wrf_ptr
        procedure, public, pass(self) :: get_hcf => get_hcf_ptr

    end type type_physics_registry

contains

    ! 初期化（holder内部のinitialize呼ぶ）
    subroutine initialize_type_physics_registry(self, input, ierr)
        class(type_physics_registry), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(inout) :: ierr

        integer(int32) :: model_idx
        integer(int32) :: num_unique_regions
        integer(int32) :: max_region_id
        integer(int32), allocatable :: unique_material_ids(:)
        integer(int32) :: current_material_id

        ierr = 0
        call input%geometry%vtk%get_active_region_info(unique_material_ids)
        if (ierr /= 0) return
        if (.not. allocated(unique_material_ids) .or. size(unique_material_ids) == 0) then
            ierr = -1 ! エラーコード
            print *, "Error: No active material regions found."
            stop 1
        end if

        ! 実際に存在するユニーク領域の数を正とする
        num_unique_regions = size(unique_material_ids)
        max_region_id = maxval(unique_material_ids)

        if (input%basic%analysis_controls%calculate_thermal) then
            allocate (self%thc(num_unique_regions))
            allocate (self%den(num_unique_regions))
            allocate (self%sph(num_unique_regions))
            allocate (self%vhc(num_unique_regions))
            allocate (self%gcc(num_unique_regions))
            allocate (self%wrf(num_unique_regions))
        end if

        if (input%basic%analysis_controls%calculate_hydraulic) then
            allocate (self%hcf(num_unique_regions))
        end if

        ! region_idの最大値でマップ配列を確保し、0(無効値)で初期化
        allocate (self%region_id_map(max_region_id), source=0)

        do model_idx = 1, num_unique_regions
            current_material_id = unique_material_ids(model_idx)
            if (input%basic%analysis_controls%calculate_thermal) then
                call self%thc(model_idx)%initialize(input, current_material_id)
                call self%den(model_idx)%initialize(input, current_material_id)
                call self%sph(model_idx)%initialize(input, current_material_id)
                call self%vhc(model_idx)%initialize(input, current_material_id)
                call self%gcc(model_idx)%initialize(input, current_material_id)
                call self%wrf(model_idx)%initialize(input, current_material_id)
            end if

            if (input%basic%analysis_controls%calculate_hydraulic) then
                call self%hcf(model_idx)%initialize(input, current_material_id)
            end if

            self%region_id_map(current_material_id) = model_idx
        end do
    end subroutine initialize_type_physics_registry

    ! THC getter
    function get_thc_ptr(self, region_id) result(thc_ptr)
        implicit none
        class(type_physics_registry), intent(in), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_thc), pointer :: thc_ptr

        integer(int32) :: model_index

#ifdef USE_DEBUG
        if (region_id < 1 .or. region_id > size(self%region_id_map)) then
            print *, "Error: Invalid region_id in get_thc_ptr:", region_id
            nullify (thc_ptr)
            stop 1
        end if
#endif

        model_index = self%region_id_map(region_id)

#ifdef USE_DEBUG
        if (model_index == 0) then
            print *, "Error: region_id not mapped in get_thc_ptr:", region_id
            nullify (thc_ptr)
            stop 1
        end if
#endif

        thc_ptr => self%thc(model_index)%p
    end function get_thc_ptr

    ! DEN getter
    function get_den_ptr(self, region_id) result(den_ptr)
        implicit none
        class(type_physics_registry), intent(in), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_den), pointer :: den_ptr

        integer(int32) :: model_index

#ifdef USE_DEBUG
        if (region_id < 1 .or. region_id > size(self%region_id_map)) then
            print *, "Error: Invalid region_id in get_den_ptr:", region_id
            nullify (den_ptr)
            stop 1
        end if
#endif

        model_index = self%region_id_map(region_id)

#ifdef USE_DEBUG
        if (model_index == 0) then
            print *, "Error: region_id not mapped in get_den_ptr:", region_id
            nullify (den_ptr)
            stop 1
        end if
#endif

        den_ptr => self%den(model_index)%p
    end function get_den_ptr

    ! SPH getter
    function get_sph_ptr(self, region_id) result(sph_ptr)
        implicit none
        class(type_physics_registry), intent(in), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_sph), pointer :: sph_ptr

        integer(int32) :: model_index

#ifdef USE_DEBUG
        if (region_id < 1 .or. region_id > size(self%region_id_map)) then
            print *, "Error: Invalid region_id in get_sph_ptr:", region_id
            nullify (sph_ptr)
            stop 1
        end if
#endif

        model_index = self%region_id_map(region_id)

#ifdef USE_DEBUG
        if (model_index == 0) then
            print *, "Error: region_id not mapped in get_sph_ptr:", region_id
            nullify (sph_ptr)
            stop 1
        end if
#endif

        sph_ptr => self%sph(model_index)%p
    end function get_sph_ptr

    ! VHC getter
    function get_vhc_ptr(self, region_id) result(vhc_ptr)
        implicit none
        class(type_physics_registry), intent(in), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_vhc), pointer :: vhc_ptr

        integer(int32) :: model_index

#ifdef USE_DEBUG
        if (region_id < 1 .or. region_id > size(self%region_id_map)) then
            print *, "Error: Invalid region_id in get_vhc_ptr:", region_id
            nullify (vhc_ptr)
            stop 1
        end if
#endif

        model_index = self%region_id_map(region_id)

#ifdef USE_DEBUG
        if (model_index == 0) then
            print *, "Error: region_id not mapped in get_vhc_ptr:", region_id
            nullify (vhc_ptr)
            stop 1
        end if
#endif

        vhc_ptr => self%vhc(model_index)%p
    end function get_vhc_ptr

    ! GCC getter
    function get_gcc_ptr(self, region_id) result(gcc_ptr)
        implicit none
        class(type_physics_registry), intent(in), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_gcc), pointer :: gcc_ptr

        integer(int32) :: model_index

#ifdef USE_DEBUG
        if (region_id < 1 .or. region_id > size(self%region_id_map)) then
            print *, "Error: Invalid region_id in get_gcc_ptr:", region_id
            nullify (gcc_ptr)
            stop 1
        end if
#endif

        model_index = self%region_id_map(region_id)

#ifdef USE_DEBUG
        if (model_index == 0) then
            print *, "Error: region_id not mapped in get_gcc_ptr:", region_id
            nullify (gcc_ptr)
            stop 1
        end if
#endif

        gcc_ptr => self%gcc(model_index)%p
    end function get_gcc_ptr

    ! WRF getter
    function get_wrf_ptr(self, region_id) result(wrf_ptr)
        implicit none
        class(type_physics_registry), intent(in), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_wrf), pointer :: wrf_ptr

        integer(int32) :: model_index

#ifdef USE_DEBUG
        if (region_id < 1 .or. region_id > size(self%region_id_map)) then
            print *, "Error: Invalid region_id in get_wrf_ptr:", region_id
            nullify (wrf_ptr)
            stop 1
        end if
#endif

        model_index = self%region_id_map(region_id)

#ifdef USE_DEBUG
        if (model_index == 0) then
            print *, "Error: region_id not mapped in get_wrf_ptr:", region_id
            nullify (wrf_ptr)
            stop 1
        end if
#endif

        wrf_ptr => self%wrf(model_index)%p
    end function get_wrf_ptr

    ! HCF getter
    function get_hcf_ptr(self, region_id) result(hcf_ptr)
        implicit none
        class(type_physics_registry), intent(in), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_hcf), pointer :: hcf_ptr

        integer(int32) :: model_index

#ifdef USE_DEBUG
        if (region_id < 1 .or. region_id > size(self%region_id_map)) then
            print *, "Error: Invalid region_id in get_hcf_ptr:", region_id
            nullify (hcf_ptr)
            stop 1
        end if
#endif

        model_index = self%region_id_map(region_id)

#ifdef USE_DEBUG
        if (model_index == 0) then
            print *, "Error: region_id not mapped in get_hcf_ptr:", region_id
            nullify (hcf_ptr)
            stop 1
        end if
#endif

        hcf_ptr => self%hcf(model_index)%p
    end function get_hcf_ptr

end module physics_registry
