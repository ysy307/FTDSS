module properties_properties_manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_state
    use :: module_input, only:type_input
    use :: module_calculate, only:abst_gcc, abst_wrf, abst_den, abst_sph, abst_vhc, abst_thc, abst_hcf
    use :: module_control
    use :: properties_material_manager, only:type_material_manager

    implicit none
    private
    public :: type_properties_manager
    public :: type_phase_property

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Helper Derived Type to Hold Pointers for a Specific Region
    !-------------------------------------------------------------------------------------------------------------------------------
    ! Note: ポインタをまとめて保持するためのプライベートな派生型
    type :: type_material_pointers
        class(abst_thc), pointer :: thc => null()
        class(abst_vhc), pointer :: vhc => null()
        class(abst_gcc), pointer :: gcc => null()
        class(abst_wrf), pointer :: wrf => null()
        class(abst_den), pointer :: den => null()
        class(abst_hcf), pointer :: hcf => null()
    end type type_material_pointers

    type :: type_phase_property
        real(real64) :: solid
        real(real64) :: water
        real(real64) :: ice
        real(real64) :: gas
    end type type_phase_property

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Main Derived Type with Generic Type-Bound Procedures
    !-------------------------------------------------------------------------------------------------------------------------------
    type :: type_properties_manager
        type(type_material_manager) :: materials
    contains
        procedure, public :: initialize => initialize_properties_manager

        ! --- Public Generic Interfaces (API remains unchanged) ---
        generic, public :: calc_thc => calculate_thc_scalar, calculate_thc_array
        generic, public :: calc_vhc => calculate_vhc_scalar, calculate_vhc_array
        generic, public :: calc_qw => calculate_qw_scalar, calculate_qw_array
        generic, public :: calc_water_content => calculate_water_content, calculate_water_content_array
        generic, public :: calc_dQw_dT => calculate_dQw_dT, calculate_dQw_dT_array
        generic, public :: calc_thermal => calc_thermal_properties_scalar, calc_thermal_properties_array
        generic, public :: calc_hcf => calculate_hcf_scalar, calculate_hcf_array
        generic, public :: calc_hydraulic => calc_hydraulic_properties_scalar, calc_hydraulic_properties_array

        ! --- Wrapper Procedures (Public facing) ---
        procedure, private, pass(self) :: calculate_thc_scalar
        procedure, private, pass(self) :: calculate_thc_array
        procedure, private, pass(self) :: calculate_vhc_scalar
        procedure, private, pass(self) :: calculate_vhc_array
        procedure, private, pass(self) :: calculate_qw_scalar
        procedure, private, pass(self) :: calculate_qw_array
        procedure, private, pass(self) :: calc_thermal_properties_scalar
        procedure, private, pass(self) :: calc_thermal_properties_array
        procedure, private, pass(self) :: calculate_hcf_scalar
        procedure, private, pass(self) :: calculate_hcf_array
        procedure, private, pass(self) :: calc_hydraulic_properties_scalar
        procedure, private, pass(self) :: calc_hydraulic_properties_array

        ! --- Implementation Procedures (Private, do the actual work) ---
        procedure, private, pass(self) :: calculate_thc_impl_scalar
        procedure, private, pass(self) :: calculate_thc_impl_array
        procedure, private, pass(self) :: calculate_vhc_impl_scalar
        procedure, private, pass(self) :: calculate_vhc_impl_array
        procedure, private, pass(self) :: calc_thermal_properties_impl_scalar
        procedure, private, pass(self) :: calc_thermal_properties_impl_array
        procedure, private, pass(self) :: calculate_hcf_impl_scalar
        procedure, private, pass(self) :: calculate_hcf_impl_array
        procedure, private, pass(self) :: calc_hydraulic_properties_impl_scalar
        procedure, private, pass(self) :: calc_hydraulic_properties_impl_array

        procedure, private, nopass :: calculate_water_content
        procedure, private, nopass :: calculate_water_content_array
        procedure, private, nopass :: calculate_dQw_dT
        procedure, private, nopass :: calculate_dQw_dT_array

        procedure, public, pass(self) :: get_phase_dens
        procedure, public, pass(self) :: get_phase_vhcs
        procedure, public, pass(self) :: get_phase_thcs
        procedure, public, pass(self) :: get_phase_sphs

        procedure, private, pass(self) :: get_pointers_for_region
    end type type_properties_manager

contains

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Initialization
    !-------------------------------------------------------------------------------------------------------------------------------
    subroutine initialize_properties_manager(self, input, ierr)
        implicit none
        class(type_properties_manager), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(inout) :: ierr

        call self%materials%initialize(input, ierr)
        if (ierr /= 0) then
            print *, "Error: Failed to initialize materials manager."
        end if
    end subroutine initialize_properties_manager

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Helper: Get all necessary pointers for a given region
    !-------------------------------------------------------------------------------------------------------------------------------
    ! Note: 指定された領域IDのポインタを一括で取得するヘルパー
    subroutine get_pointers_for_region(self, target_id, material_id, ptrs)
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: target_id
        integer(int32), intent(in) :: material_id
        type(type_material_pointers), intent(inout) :: ptrs

        select case (target_id)
        case (calc_thermal)
            ptrs%thc => self%materials%get_thc(material_id)
            ptrs%vhc => self%materials%get_vhc(material_id)
            ptrs%gcc => self%materials%get_gcc(material_id)
            ptrs%wrf => self%materials%get_wrf(material_id)
            ptrs%den => self%materials%get_den(material_id)

#ifdef USE_DEBUG
            if (.not. (associated(ptrs%thc) .and. associated(ptrs%vhc) .and. &
                       associated(ptrs%gcc) .and. associated(ptrs%wrf) .and. associated(ptrs%den))) then
                stop "Error: Failed to associate one or more material pointers."
            end if
#endif
        case (calc_hydraulic)
            ptrs%den => self%materials%get_den(material_id)
            ptrs%hcf => self%materials%get_hcf(material_id)

#ifdef USE_DEBUG
            if (.not. (associated(ptrs%gcc) .and. associated(ptrs%wrf) .and. &
                       associated(ptrs%den) .and. associated(ptrs%hcf))) then
                stop "Error: Failed to associate one or more material pointers."
            end if
#endif
        end select
    end subroutine get_pointers_for_region

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Helper: Calculate common properties (water content)
    !-------------------------------------------------------------------------------------------------------------------------------
    pure elemental subroutine calculate_water_content(ptrs, state)
        implicit none
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: state

        state%density_water = ptrs%den%material2
        state%density_ice = ptrs%den%material3
        state%water_content = ptrs%wrf%calc(-ptrs%gcc%calc(state))
    end subroutine calculate_water_content

    pure subroutine calculate_water_content_array(ptrs, states)
        implicit none
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: states(:)

        call calculate_water_content(ptrs, states(:))

    end subroutine calculate_water_content_array

    pure elemental subroutine calculate_dQw_dT(ptrs, state)
        implicit none
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: state

        state%density_water = ptrs%den%material2
        state%density_ice = ptrs%den%material3
        state%dQw_dT = ptrs%wrf%deriv(-ptrs%gcc%calc(state)) * ptrs%gcc%deriv(state)

    end subroutine calculate_dQw_dT

    pure subroutine calculate_dQw_dT_array(ptrs, states)
        implicit none
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: states(:)

        call calculate_dQw_dT(ptrs, states(:))

    end subroutine calculate_dQw_dT_array

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Thermal Conductivity (THC)
    !-------------------------------------------------------------------------------------------------------------------------------
    ! --- Wrapper ---
    function calculate_thc_scalar(self, material_id, state) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        real(real64) :: val

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(calc_thermal, material_id, ptrs)
        val = self%calculate_thc_impl_scalar(ptrs, state)
    end function calculate_thc_scalar

    ! --- Implementation ---
    function calculate_thc_impl_scalar(self, ptrs, state) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: state
        real(real64) :: val

        call self%calculate_water_content(ptrs, state)
        val = ptrs%thc%calc(state)
    end function calculate_thc_impl_scalar

    ! --- Wrapper (Array) ---
    function calculate_thc_array(self, material_id, states) result(vals)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_state), intent(inout) :: states(:)
        integer(int32), intent(in) :: material_id
        real(real64) :: vals(size(states))

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(calc_thermal, material_id, ptrs)
        vals = self%calculate_thc_impl_array(ptrs, states)
    end function calculate_thc_array

    ! --- Implementation (Array) ---
    function calculate_thc_impl_array(self, ptrs, states) result(vals)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: states(:)
        real(real64) :: vals(size(states))

        integer(int32) :: i

        call self%calc_water_content(ptrs, states)
        do i = 1, size(states)
            vals(i) = self%calculate_thc_impl_scalar(ptrs, states(i))
        end do
    end function calculate_thc_impl_array

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Volumetric Heat Capacity (VHC) Implementation
    !-------------------------------------------------------------------------------------------------------------------------------
    ! --- Wrapper ---
    function calculate_vhc_scalar(self, material_id, state) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        real(real64) :: val

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(calc_thermal, material_id, ptrs)
        val = self%calculate_vhc_impl_scalar(ptrs, state)
    end function calculate_vhc_scalar

    ! --- Implementation ---
    function calculate_vhc_impl_scalar(self, ptrs, state) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: state
        real(real64) :: val

        call self%calculate_water_content(ptrs, state)
        call self%calculate_dQw_dT(ptrs, state)
        val = ptrs%vhc%calc(state=state, &
                            den=ptrs%den, &
                            latentheat=ptrs%gcc%lf, &
                            dQw_dT=state%dQw_dT)
    end function calculate_vhc_impl_scalar

    ! --- Wrapper (Array) ---
    function calculate_vhc_array(self, material_id, states) result(vals)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: states(:)
        real(real64) :: vals(size(states))

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(calc_thermal, material_id, ptrs)
        vals = self%calculate_vhc_impl_array(ptrs, states)
    end function calculate_vhc_array

    ! --- Implementation (Array) ---
    function calculate_vhc_impl_array(self, ptrs, states) result(vals)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: states(:)
        real(real64) :: vals(size(states))

        integer(int32) :: i

        call self%calc_water_content(ptrs, states)
        call self%calc_dQw_dT(ptrs, states)
        do i = 1, size(states)
            vals(i) = self%calculate_vhc_impl_scalar(ptrs, states(i))
        end do
    end function calculate_vhc_impl_array

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Water Content (qw) Implementation
    !-------------------------------------------------------------------------------------------------------------------------------
    ! --- Wrapper ---
    function calculate_qw_scalar(self, material_id, state) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        real(real64) :: val

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(calc_thermal, material_id, ptrs)
        call self%calc_water_content(ptrs, state)
        val = state%water_content
    end function calculate_qw_scalar

    ! --- Wrapper (Array) ---
    function calculate_qw_array(self, material_id, states) result(vals)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_state), intent(inout) :: states(:)
        integer(int32), intent(in) :: material_id
        real(real64) :: vals(size(states))

        type(type_material_pointers) :: ptrs
        integer(int32) :: i

        call self%get_pointers_for_region(calc_thermal, material_id, ptrs)
        call self%calc_water_content(ptrs, states)
        do i = 1, size(states)
            vals(i) = states(i)%water_content
        end do
    end function calculate_qw_array

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Thermal Properties Calculation
    !-------------------------------------------------------------------------------------------------------------------------------
    ! --- Wrapper ---
    subroutine calc_thermal_properties_scalar(self, material_id, state, thc, vhc)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: thc
        real(real64), intent(inout) :: vhc

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(calc_thermal, material_id, ptrs)
        call self%calc_thermal_properties_impl_scalar(state, ptrs, thc, vhc)
    end subroutine calc_thermal_properties_scalar

    ! --- Implementation ---
    subroutine calc_thermal_properties_impl_scalar(self, state, ptrs, thc, vhc)
        class(type_properties_manager), intent(in) :: self
        type(type_state), intent(inout) :: state
        type(type_material_pointers), intent(in) :: ptrs
        real(real64), intent(inout) :: thc
        real(real64), intent(inout) :: vhc

        call self%calculate_water_content(ptrs, state)
        call self%calculate_dQw_dT(ptrs, state)

        thc = ptrs%thc%calc(state)
        vhc = ptrs%vhc%calc(state=state, den=ptrs%den, latentheat=ptrs%gcc%lf, dQw_dT=state%dQw_dT)

    end subroutine calc_thermal_properties_impl_scalar

    ! --- Wrapper (Array) ---
    subroutine calc_thermal_properties_array(self, material_id, states, thcs, vhcs)
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: states(:)
        real(real64), intent(inout) :: thcs(size(states))
        real(real64), intent(inout) :: vhcs(size(states))

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(calc_thermal, material_id, ptrs)
        call self%calc_thermal_properties_impl_array(ptrs, states, thcs, vhcs)

    end subroutine calc_thermal_properties_array

    ! --- Implementation (Array) ---
    subroutine calc_thermal_properties_impl_array(self, ptrs, states, thcs, vhcs)
        class(type_properties_manager), intent(in) :: self
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: states(:)
        real(real64), intent(inout) :: thcs(size(states))
        real(real64), intent(inout) :: vhcs(size(states))

        integer(int32) :: i

        call self%calc_water_content(ptrs, states)
        call self%calc_dQw_dT(ptrs, states)

        do i = 1, size(states)
            thcs(i) = self%calculate_thc_impl_scalar(ptrs, states(i))
            vhcs(i) = self%calculate_vhc_impl_scalar(ptrs, states(i))
        end do
    end subroutine calc_thermal_properties_impl_array

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Hydraulic Conductivity (HCF) Implementation
    !-------------------------------------------------------------------------------------------------------------------------------
    ! --- Wrapper ---
    function calculate_hcf_scalar(self, material_id, state) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        real(real64) :: val

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(calc_hydraulic, material_id, ptrs)
        val = self%calculate_hcf_impl_scalar(ptrs, state)
    end function calculate_hcf_scalar

    ! --- Implementation ---
    function calculate_hcf_impl_scalar(self, ptrs, state) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: state
        real(real64) :: val

        call self%calc_water_content(ptrs, state)
        state%ice_content = state%porosity - state%water_content

        val = ptrs%hcf%calc_kflh(state)
    end function calculate_hcf_impl_scalar

    ! --- Wrapper (Array) ---
    function calculate_hcf_array(self, material_id, states) result(vals)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: states(:)
        real(real64) :: vals(size(states))

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(calc_hydraulic, material_id, ptrs)

        vals = self%calculate_hcf_impl_array(ptrs, states)
    end function calculate_hcf_array

    ! --- Implementation (Array) ---
    function calculate_hcf_impl_array(self, ptrs, states) result(vals)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: states(:)
        real(real64) :: vals(size(states))

        integer(int32) :: i

        ! 1. 水分量を配列全体に対して一度に計算
        call self%calc_water_content(ptrs, states)

        ! 2. ループ内で各点の氷含有量を計算し、HCFを求める
        do i = 1, size(states)
            states(i)%ice_content = states(i)%porosity - states(i)%water_content
            vals(i) = self%calculate_hcf_impl_scalar(ptrs, states(i))
        end do
    end function calculate_hcf_impl_array

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Hydraulic Properties Calculation
    !-------------------------------------------------------------------------------------------------------------------------------
    ! --- Wrapper ---
    subroutine calc_hydraulic_properties_scalar(self, material_id, state, kflh)
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: kflh

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(calc_hydraulic, material_id, ptrs)
        call self%calc_hydraulic_properties_impl_scalar(state, ptrs, kflh)
    end subroutine calc_hydraulic_properties_scalar

    ! --- Implementation ---
    subroutine calc_hydraulic_properties_impl_scalar(self, state, ptrs, kflh)
        class(type_properties_manager), intent(in) :: self
        type(type_state), intent(inout) :: state
        type(type_material_pointers), intent(in) :: ptrs
        real(real64), intent(inout) :: kflh

        kflh = ptrs%hcf%calc_kflh(state)
        state%density_water = ptrs%den%material2
        state%density_ice = ptrs%den%material3

    end subroutine calc_hydraulic_properties_impl_scalar

    ! --- Wrapper (Array) ---
    subroutine calc_hydraulic_properties_array(self, material_id, states, kflhs)
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: states(:)
        real(real64), intent(inout) :: kflhs(size(states))

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(calc_hydraulic, material_id, ptrs)
        call self%calc_hydraulic_properties_impl_array(ptrs, states, kflhs)

    end subroutine calc_hydraulic_properties_array

    ! --- Implementation (Array) ---
    subroutine calc_hydraulic_properties_impl_array(self, ptrs, states, kflhs)
        class(type_properties_manager), intent(in) :: self
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: states(:)
        real(real64), intent(inout) :: kflhs(size(states))

        integer(int32) :: i

        do i = 1, size(states)
            kflhs(i) = ptrs%hcf%calc_kflh(states(i))
            states(i)%density_water = ptrs%den%material2
            states(i)%density_ice = ptrs%den%material3
        end do
    end subroutine calc_hydraulic_properties_impl_array

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Get Phase Property
    !-------------------------------------------------------------------------------------------------------------------------------
    function get_phase_dens(self, material_id) result(phase_property)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_phase_property) :: phase_property

        class(abst_den), pointer :: den_ptr => null()

        den_ptr => self%materials%get_den(material_id)

#ifdef USE_DEBUG
        if (.not. associated(den_ptr)) then
            print *, "Error in get_phase_property: Failed to get density pointer for material_id =", material_id
            stop "FATAL ERROR: DENSITY POINTER NOT FOUND"
        end if
#endif

        phase_property%solid = den_ptr%material1
        phase_property%water = den_ptr%material2
        phase_property%ice = den_ptr%material3
        phase_property%gas = den_ptr%material4

    end function get_phase_dens

    function get_phase_sphs(self, material_id) result(phase_property)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_phase_property) :: phase_property

        class(abst_sph), pointer :: sph_ptr => null()

        sph_ptr => self%materials%get_sph(material_id)

#ifdef USE_DEBUG
        if (.not. associated(sph_ptr)) then
            print *, "Error in get_phase_property: Failed to get specific heat pointer for material_id =", material_id
            stop "FATAL ERROR: SPECIFIC HEAT POINTER NOT FOUND"
        end if
#endif

        phase_property%solid = sph_ptr%material1
        phase_property%water = sph_ptr%material2
        phase_property%ice = sph_ptr%material3
        phase_property%gas = sph_ptr%material4

    end function get_phase_sphs

    function get_phase_vhcs(self, material_id) result(phase_property)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_phase_property) :: phase_property

        class(abst_vhc), pointer :: vhc_ptr => null()

        vhc_ptr => self%materials%get_vhc(material_id)

#ifdef USE_DEBUG
        if (.not. associated(vhc_ptr)) then
            print *, "Error in get_phase_property: Failed to get volumetric heat capacity pointer for material_id =", material_id
            stop "FATAL ERROR: VOLUMETRIC HEAT CAPACITY POINTER NOT FOUND"
        end if
#endif

        phase_property%solid = vhc_ptr%material1
        phase_property%water = vhc_ptr%material2
        phase_property%ice = vhc_ptr%material3
        phase_property%gas = vhc_ptr%material4

    end function get_phase_vhcs

    function get_phase_thcs(self, material_id) result(phase_property)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_phase_property) :: phase_property

        class(abst_thc), pointer :: thc_ptr => null()

        thc_ptr => self%materials%get_thc(material_id)

#ifdef USE_DEBUG
        if (.not. associated(thc_ptr)) then
            print *, "Error in get_phase_property: Failed to get thermal heat capacity pointer for material_id =", material_id
            stop "FATAL ERROR: THERMAL HEAT CAPACITY POINTER NOT FOUND"
        end if
#endif

        phase_property%solid = thc_ptr%material1
        phase_property%water = thc_ptr%material2
        phase_property%ice = thc_ptr%material3
        phase_property%gas = thc_ptr%material4

    end function get_phase_thcs

end module properties_properties_manager
