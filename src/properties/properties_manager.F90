module properties_properties_manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_state
    use :: module_input, only:type_input
    use :: module_calculate, only:abst_gcc, abst_wrf, abst_den, abst_vhc, abst_thc, abst_hcf
    use :: properties_material_manager, only:type_material_manager

    implicit none
    private
    public :: type_properties_manager

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

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Main Derived Type with Generic Type-Bound Procedures
    !-------------------------------------------------------------------------------------------------------------------------------
    type :: type_properties_manager
        private
        type(type_material_manager) :: materials
    contains
        procedure, public :: initialize => initialize_properties_manager

        ! --- Public Generic Interfaces (API remains unchanged) ---
        generic, public :: get_thc => calculate_thc_scalar, calculate_thc_array
        generic, public :: get_vhc => calculate_vhc_scalar, calculate_vhc_array
        generic, public :: get_qw => calculate_qw_scalar, calculate_qw_array
        generic, public :: calc_thermal => calc_thermal_properties_scalar, calc_thermal_properties_array
        generic, public :: get_hcf => calculate_hcf_scalar, calculate_hcf_array

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

        ! --- Implementation Procedures (Private, do the actual work) ---
        procedure, private, pass(self) :: calculate_thc_impl_scalar
        procedure, private, pass(self) :: calculate_thc_impl_array
        procedure, private, pass(self) :: calculate_vhc_impl_scalar
        procedure, private, pass(self) :: calculate_vhc_impl_array
        procedure, private, pass(self) :: calc_thermal_properties_impl_scalar
        procedure, private, pass(self) :: calc_thermal_properties_impl_array
        procedure, private, pass(self) :: calculate_hcf_impl_scalar
        procedure, private, pass(self) :: calculate_hcf_impl_array

        procedure, private, nopass :: get_water_content => calculate_water_content
        procedure, private, nopass :: calc_water_content => calculate_water_content_array
        procedure, private, nopass :: get_dQi_dT => calculate_dQi_dT
        procedure, private, nopass :: calc_dQi_dT => calculate_dQi_dT_array

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
    subroutine get_pointers_for_region(self, region_id, target, ptrs)
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: region_id
        character(*), intent(in) :: target
        type(type_material_pointers), intent(inout) :: ptrs

        select case (trim(adjustl(target)))
        case ("thermal")
            call self%materials%get_thc(region_id, ptrs%thc)
            call self%materials%get_vhc(region_id, ptrs%vhc)
            call self%materials%get_gcc(region_id, ptrs%gcc)
            call self%materials%get_wrf(region_id, ptrs%wrf)
            call self%materials%get_den(region_id, ptrs%den)

#ifdef USE_DEBUG
            if (.not. (associated(ptrs%thc) .and. associated(ptrs%vhc) .and. &
                       associated(ptrs%gcc) .and. associated(ptrs%wrf) .and. associated(ptrs%den))) then
                stop "Error: Failed to associate one or more material pointers."
            end if
#endif
        case ("hydraulic")
            call self%materials%get_gcc(region_id, ptrs%gcc)
            call self%materials%get_wrf(region_id, ptrs%wrf)
            call self%materials%get_den(region_id, ptrs%den)
            call self%materials%get_hcf(region_id, ptrs%hcf)

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
    pure elemental function calculate_water_content(ptrs) result(state)
        implicit none
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state) :: state

        state%density_water = ptrs%den%material2
        state%density_ice = ptrs%den%material3
        state%water_content = ptrs%wrf%calc(-ptrs%gcc%calc(T=state%temperature, &
                                                           Pw=state%pressure, &
                                                           rhoW=state%density_water, &
                                                           rhoI=state%density_ice))
    end function calculate_water_content

    subroutine calculate_water_content_array(ptrs, states)
        implicit none
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: states(:)
        integer(int32) :: i

        do i = 1, size(states)
            states(i) = calculate_water_content(ptrs)
        end do
    end subroutine calculate_water_content_array

    pure elemental function calculate_dQi_dT(ptrs) result(state)
        implicit none
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state) :: state

        state%density_water = ptrs%den%material2
        state%density_ice = ptrs%den%material3
        state%dQi_dT = ptrs%wrf%deriv(-ptrs%gcc%calc(t=state%temperature, &
                                                     pw=state%pressure, &
                                                     rhow=state%density_water, &
                                                     rhoi=state%density_ice)) &
                       * ptrs%gcc%deriv(t=state%temperature, &
                                        pw=state%pressure, &
                                        rhow=state%density_water, &
                                        rhoi=state%density_ice)

    end function calculate_dQi_dT

    subroutine calculate_dQi_dT_array(ptrs, states)
        implicit none
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: states(:)
        integer(int32) :: i

        do i = 1, size(states)
            states(i) = calculate_dQi_dT(ptrs)
        end do

    end subroutine calculate_dQi_dT_array

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Thermal Conductivity (THC)
    !-------------------------------------------------------------------------------------------------------------------------------
    ! --- Wrapper ---
    function calculate_thc_scalar(self, region_id, state) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: region_id
        type(type_state), intent(inout) :: state
        real(real64) :: val

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(region_id, "thermal", ptrs)
        val = self%calculate_thc_impl_scalar(ptrs, state)
    end function calculate_thc_scalar

    ! --- Implementation ---
    function calculate_thc_impl_scalar(self, ptrs, state) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: state
        real(real64) :: val

        state = self%get_water_content(ptrs)
        val = ptrs%thc%calc(state)
    end function calculate_thc_impl_scalar

    ! --- Wrapper (Array) ---
    function calculate_thc_array(self, region_id, states) result(vals)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_state), intent(inout) :: states(:)
        integer(int32), intent(in) :: region_id
        real(real64) :: vals(size(states))

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(region_id, "thermal", ptrs)
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
    function calculate_vhc_scalar(self, region_id, state) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: region_id
        type(type_state), intent(inout) :: state
        real(real64) :: val

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(region_id, "thermal", ptrs)
        val = self%calculate_vhc_impl_scalar(ptrs, state)
    end function calculate_vhc_scalar

    ! --- Implementation ---
    function calculate_vhc_impl_scalar(self, ptrs, state) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: state
        real(real64) :: val

        state = self%get_water_content(ptrs)
        state = self%get_dQi_dT(ptrs)
        val = ptrs%vhc%calc(state=state, &
                            den=ptrs%den, &
                            latentheat=ptrs%gcc%lf, &
                            dQi_dT=state%dQi_dT)
    end function calculate_vhc_impl_scalar

    ! --- Wrapper (Array) ---
    function calculate_vhc_array(self, region_id, states) result(vals)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: region_id
        type(type_state), intent(inout) :: states(:)
        real(real64) :: vals(size(states))

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(region_id, "thermal", ptrs)
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
        call self%calc_dQi_dT(ptrs, states)
        do i = 1, size(states)
            vals(i) = self%calculate_vhc_impl_scalar(ptrs, states(i))
        end do
    end function calculate_vhc_impl_array

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Water Content (qw) Implementation
    !-------------------------------------------------------------------------------------------------------------------------------
    ! --- Wrapper ---
    function calculate_qw_scalar(self, region_id, state) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: region_id
        type(type_state), intent(inout) :: state
        real(real64) :: val

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(region_id, "thermal", ptrs)
        state = self%get_water_content(ptrs)
        val = state%water_content
    end function calculate_qw_scalar

    ! --- Wrapper (Array) ---
    function calculate_qw_array(self, region_id, states) result(vals)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_state), intent(inout) :: states(:)
        integer(int32), intent(in) :: region_id
        real(real64) :: vals(size(states))

        type(type_material_pointers) :: ptrs
        integer(int32) :: i

        call self%get_pointers_for_region(region_id, "thermal", ptrs)
        call self%calc_water_content(ptrs, states)
        do i = 1, size(states)
            vals(i) = states(i)%water_content
        end do
    end function calculate_qw_array

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Thermal Properties Calculation
    !-------------------------------------------------------------------------------------------------------------------------------
    ! --- Wrapper ---
    subroutine calc_thermal_properties_scalar(self, region_id, state, thc, vhc)
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: region_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: thc
        real(real64), intent(inout) :: vhc

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(region_id, "thermal", ptrs)
        call self%calc_thermal_properties_impl_scalar(state, ptrs, thc, vhc)
    end subroutine calc_thermal_properties_scalar

    ! --- Implementation ---
    subroutine calc_thermal_properties_impl_scalar(self, state, ptrs, thc, vhc)
        class(type_properties_manager), intent(in) :: self
        type(type_state), intent(inout) :: state
        type(type_material_pointers), intent(in) :: ptrs
        real(real64), intent(inout) :: thc
        real(real64), intent(inout) :: vhc

        state = self%get_water_content(ptrs)
        state = self%get_dQi_dT(ptrs)

        thc = ptrs%thc%calc(state)
        vhc = ptrs%vhc%calc(state=state, den=ptrs%den, latentheat=ptrs%gcc%lf, dQi_dT=state%dQi_dT)

    end subroutine calc_thermal_properties_impl_scalar

    ! --- Wrapper (Array) ---
    subroutine calc_thermal_properties_array(self, region_id, states, thcs, vhcs)
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: region_id
        type(type_state), intent(inout) :: states(:)
        real(real64), intent(inout) :: thcs(size(states))
        real(real64), intent(inout) :: vhcs(size(states))

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(region_id, "thermal", ptrs)
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
        call self%calc_dQi_dT(ptrs, states)

        do i = 1, size(states)
            thcs(i) = self%calculate_thc_impl_scalar(ptrs, states(i))
            vhcs(i) = self%calculate_vhc_impl_scalar(ptrs, states(i))
        end do
    end subroutine calc_thermal_properties_impl_array

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Hydraulic Conductivity (HCF) Implementation
    !-------------------------------------------------------------------------------------------------------------------------------
    ! --- Wrapper ---
    function calculate_hcf_scalar(self, region_id, state) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: region_id
        type(type_state), intent(inout) :: state
        real(real64) :: val

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(region_id, "hydraulic", ptrs)
        val = self%calculate_hcf_impl_scalar(ptrs, state)
    end function calculate_hcf_scalar

    ! --- Implementation ---
    function calculate_hcf_impl_scalar(self, ptrs, state) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_material_pointers), intent(in) :: ptrs
        type(type_state), intent(inout) :: state
        real(real64) :: val

        state = self%get_water_content(ptrs)
        state%ice_content = state%porosity - state%water_content

        val = ptrs%hcf%calc_kflh(state)
    end function calculate_hcf_impl_scalar

    ! --- Wrapper (Array) ---
    function calculate_hcf_array(self, region_id, states) result(vals)
        implicit none
        class(type_properties_manager), intent(in) :: self
        integer(int32), intent(in) :: region_id
        type(type_state), intent(inout) :: states(:)
        real(real64) :: vals(size(states)) ! Note: intent(out)相当なので inout は不要

        type(type_material_pointers) :: ptrs

        call self%get_pointers_for_region(region_id, "hydraulic", ptrs)

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
end module properties_properties_manager
