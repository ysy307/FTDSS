module physics_registry
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state, type_physics_info
    use :: module_physics_materials, only:holder_dens, abst_den, holder_sphs, abst_sph, holder_vhcs, abst_vhc, holder_thcs, abst_thc
    use :: module_physics_models, only:holder_hcfs, abst_hcf, type_hcf_params, holder_gccs, abst_gcc, holder_wrfs, abst_wrf, type_wrf_params, type_evaporation
    implicit none

    public :: type_physics_registry
    public :: type_material_pointers

    type :: type_material_pointers
        class(abst_thc), pointer :: thc => null()
        class(abst_vhc), pointer :: vhc => null()
        class(abst_gcc), pointer :: gcc => null()
        class(abst_wrf), pointer :: wrf => null()
        class(abst_den), pointer :: den => null()
        class(abst_hcf), pointer :: hcf => null()
    end type type_material_pointers

    type :: type_physics_registry
        private
        type(type_iapws97) :: water
        type(type_iapws06) :: ice
        type(type_evaporation) :: evaporation
        type(holder_thcs), allocatable :: thc(:)
        type(holder_dens), allocatable :: den(:)
        type(holder_sphs), allocatable :: sph(:)
        type(holder_vhcs), allocatable :: vhc(:)
        type(holder_gccs), allocatable :: gcc(:)
        type(holder_wrfs), allocatable :: wrf(:)
        type(holder_hcfs), allocatable :: hcf(:)

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
        procedure, public, pass(self) :: get_evaporation => get_evaporation_ptr

    end type type_physics_registry

contains

    ! 初期化（holder内部のinitialize呼ぶ）
    subroutine initialize_type_physics_registry(self, unique_material_ids, flags_coumpute, density_info, &
                                                specific_heat_info, heat_capacity_info, thermal_conductivity_info, &
                                                gcc_model_ids, wrf_model_info, hcf_model_info)
        implicit none
        class(type_physics_registry), intent(inout) :: self
        integer(int32), intent(in) :: unique_material_ids(:)
        logical, intent(in) :: flags_coumpute(:)
        type(type_physics_info), intent(in), optional :: density_info(:)
        type(type_physics_info), intent(in), optional :: specific_heat_info(:)
        type(type_physics_info), intent(in), optional :: heat_capacity_info(:)
        type(type_physics_info), intent(in), optional :: thermal_conductivity_info(:)
        integer(int32), intent(in), optional :: gcc_model_ids(:)
        type(type_wrf_params), intent(in), optional :: wrf_model_info(:)
        type(type_hcf_params), intent(in), optional :: hcf_model_info(:)

        integer(int32) :: model_idx
        integer(int32) :: num_unique_regions
        integer(int32) :: max_region_id
        integer(int32) :: current_material_id
        integer(int32) :: status

        call self%water%initialize()
        call self%ice%initialize()

        call self%evaporation%initialize(self%water)

        num_unique_regions = size(unique_material_ids)
        max_region_id = maxval(unique_material_ids)

        if (flags_coumpute(1)) then
            allocate (self%thc(num_unique_regions), stat=status)
            if (status /= 0) then
                print *, "Error: Unable to allocate THC models in physics registry."
                stop 1
            end if
            allocate (self%den(num_unique_regions), stat=status)
            if (status /= 0) then
                print *, "Error: Unable to allocate DEN models in physics registry."
                stop 1
            end if
            allocate (self%sph(num_unique_regions), stat=status)
            if (status /= 0) then
                print *, "Error: Unable to allocate SPH models in physics registry."
                stop 1
            end if
            allocate (self%vhc(num_unique_regions), stat=status)
            if (status /= 0) then
                print *, "Error: Unable to allocate VHC models in physics registry."
                stop 1
            end if
            allocate (self%gcc(num_unique_regions), stat=status)
            if (status /= 0) then
                print *, "Error: Unable to allocate GCC models in physics registry."
                stop 1
            end if
            allocate (self%wrf(num_unique_regions), stat=status)
            if (status /= 0) then
                print *, "Error: Unable to allocate WRF models in physics registry."
                stop 1
            end if
        end if

        if (flags_coumpute(2)) then
            allocate (self%den(num_unique_regions), stat=status)
            if (status /= 0) then
                print *, "Error: Unable to allocate DEN models in physics registry."
                stop 1
            end if
            allocate (self%hcf(num_unique_regions), stat=status)
            if (status /= 0) then
                print *, "Error: Unable to allocate HCF models in physics registry."
                stop 1
            end if
        end if

        allocate (self%region_id_map(max_region_id), source=0)

        do model_idx = 1, num_unique_regions
            current_material_id = unique_material_ids(model_idx)
            if (flags_coumpute(1)) then
                if (.not. present(density_info) .or. .not. present(specific_heat_info) .or. &
                    .not. present(heat_capacity_info) .or. .not. present(thermal_conductivity_info) .or. &
                    .not. present(gcc_model_ids) .or. .not. present(wrf_model_info)) then
                    print *, "Error: Missing required physics info for computing model index ", model_idx
                    stop 1
                end if
                call self%thc(model_idx)%initialize(current_material_id, thermal_conductivity_info(model_idx), self%water, self%ice)
                call self%den(model_idx)%initialize(current_material_id, density_info(model_idx), self%water, self%ice)
                call self%sph(model_idx)%initialize(current_material_id, specific_heat_info(model_idx), self%water, self%ice)
                call self%vhc(model_idx)%initialize(current_material_id, heat_capacity_info(model_idx), self%water, self%ice)
                call self%gcc(model_idx)%initialize(current_material_id, gcc_model_ids(model_idx), self%water, self%ice)
                call self%wrf(model_idx)%initialize(current_material_id, wrf_model_info(model_idx))
            end if

            if (flags_coumpute(2)) then
                if (.not. present(hcf_model_info)) then
                    print *, "Error: Missing required HCF model info for computing model index ", model_idx
                    stop 1
                end if
                call self%hcf(model_idx)%initialize(current_material_id, hcf_model_info(model_idx), self%water)
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

    ! Evaporation getter
    function get_evaporation_ptr(self) result(evaporation_ptr)
        implicit none
        class(type_physics_registry), intent(in), target :: self
        class(type_evaporation), pointer :: evaporation_ptr

        evaporation_ptr => self%evaporation
    end function get_evaporation_ptr

end module physics_registry
