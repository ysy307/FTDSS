module properties_properties_manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_state
    use :: module_input, only:type_input
    use :: module_calculate, only:abst_gcc, abst_wrf, abst_den, abst_vhc, abst_thc
    use :: properties_material_manager, only:type_material_manager

    implicit none
    private
    public :: type_properties_manager

#ifdef USE_DEBUG
    logical, parameter, private :: debug_mode = .true.
#else
    logical, parameter, private :: debug_mode = .false.
#endif

    !====================================================================
    ! Main Derived Type with Generic Type-Bound Procedures
    !====================================================================
    type :: type_properties_manager
        private
        type(type_material_manager) :: materials
    contains
        procedure, public :: initialize => initialize_properties_manager

        procedure, private :: calculate_thc_scalar
        procedure, private :: calculate_thc_array
        generic, public :: get_thc => calculate_thc_scalar, calculate_thc_array

        procedure, private :: calculate_heat_capacity_scalar
        procedure, private :: calculate_heat_capacity_array
        generic, public :: get_vhc => calculate_heat_capacity_scalar, calculate_heat_capacity_array

        procedure, private :: calculate_water_content_scalar
        procedure, private :: calculate_water_content_array
        generic, public :: get_qw => calculate_water_content_scalar, calculate_water_content_array

        procedure, public :: calc_thermal_properties_scalar
        procedure, public :: calc_thermal_properties_array
        generic, public :: calc_thermal => calc_thermal_properties_scalar, calc_thermal_properties_array
    end type type_properties_manager

contains

    !====================================================================
    ! Initialization (simplified)
    !====================================================================
    subroutine initialize_properties_manager(self, input, ierr)
        implicit none
        class(type_properties_manager), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(inout) :: ierr

        ! ポインタ設定が不要になり、下位モジュールの初期化のみで完了
        call self%materials%initialize(input, ierr)
        if (ierr /= 0) then
            print *, "Error: Failed to initialize materials manager."
        end if
    end subroutine initialize_properties_manager

    !====================================================================
    ! Thermal Conductivity (THC) Implementation
    !====================================================================
    function calculate_thc_scalar(self, state, region_id) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64) :: val

        class(abst_thc), pointer :: local_thc

        call self%materials%get_thc(region_id, local_thc)

        if (debug_mode) then
            if (.not. associated(local_thc)) stop "Error: Failed to get thc pointer in calculate_thc_scalar."
        end if

        val = local_thc%calc(state)
    end function calculate_thc_scalar

    function calculate_thc_array(self, state, region_id) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_state), intent(in) :: state(:)
        integer(int32), intent(in) :: region_id
        real(real64) :: val(size(state))

        integer(int32) :: i
        class(abst_thc), pointer :: local_thc

        call self%materials%get_thc(region_id, local_thc)

        if (debug_mode) then
            if (.not. associated(local_thc)) stop "Error: Failed to get thc pointer in calculate_thc_array."
        end if

        do i = 1, size(state)
            val(i) = local_thc%calc(state(i))
        end do
    end function calculate_thc_array

    !====================================================================
    ! Volumetric Heat Capacity (VHC) Implementation
    !====================================================================
    function calculate_heat_capacity_scalar(self, state, region_id) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64) :: val

        class(abst_gcc), pointer :: local_gcc
        class(abst_wrf), pointer :: local_wrf
        class(abst_den), pointer :: local_den
        class(abst_vhc), pointer :: local_vhc

        call self%materials%get_gcc(region_id, local_gcc)
        call self%materials%get_wrf(region_id, local_wrf)
        call self%materials%get_den(region_id, local_den)
        call self%materials%get_vhc(region_id, local_vhc)

        if (debug_mode) then
            if (.not. (associated(local_gcc) .and. associated(local_wrf) .and. &
                       associated(local_den) .and. associated(local_vhc))) then
                stop "Error: Failed to get pointers in calculate_heat_capacity_scalar."
            end if
        end if

        val = local_vhc%calc(state=state, &
                             den=local_den, &
                             latentheat=local_gcc%lf, &
                             dqi_dt=local_wrf%deriv(-local_gcc%calc(t=state%temperature, &
                                                                    pw=state%pressure, &
                                                                    rhow=local_den%material2, &
                                                                    rhoi=local_den%material3)))
    end function calculate_heat_capacity_scalar

    function calculate_heat_capacity_array(self, state, region_id) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_state), intent(in) :: state(:)
        integer(int32), intent(in) :: region_id
        real(real64) :: val(size(state))

        integer(int32) :: i
        class(abst_gcc), pointer :: local_gcc
        class(abst_wrf), pointer :: local_wrf
        class(abst_den), pointer :: local_den
        class(abst_vhc), pointer :: local_vhc

        call self%materials%get_gcc(region_id, local_gcc)
        call self%materials%get_wrf(region_id, local_wrf)
        call self%materials%get_den(region_id, local_den)
        call self%materials%get_vhc(region_id, local_vhc)

        if (debug_mode) then
            if (.not. (associated(local_gcc) .and. associated(local_wrf) .and. &
                       associated(local_den) .and. associated(local_vhc))) then
                stop "Error: Failed to get pointers in calculate_heat_capacity_array."
            end if
        end if

        do i = 1, size(state)
            val(i) = local_vhc%calc(state=state(i), &
                                    den=local_den, &
                                    latentheat=local_gcc%lf, &
                                    dqi_dt=local_wrf%deriv(-local_gcc%calc(t=state(i)%temperature, &
                                                                           pw=state(i)%pressure, &
                                                                           rhow=local_den%material2, &
                                                                           rhoi=local_den%material3)))
        end do
    end function calculate_heat_capacity_array

    !====================================================================
    ! Water Content (qw) Implementation
    !====================================================================
    function calculate_water_content_scalar(self, state, region_id) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64) :: val

        class(abst_gcc), pointer :: local_gcc
        class(abst_wrf), pointer :: local_wrf
        class(abst_den), pointer :: local_den

        call self%materials%get_gcc(region_id, local_gcc)
        call self%materials%get_wrf(region_id, local_wrf)
        call self%materials%get_den(region_id, local_den)

        if (debug_mode) then
            if (.not. (associated(local_gcc) .and. associated(local_wrf) .and. associated(local_den))) then
                stop "Error: Failed to get pointers in calculate_water_content_scalar."
            end if
        end if

        val = local_wrf%calc(-local_gcc%calc(t=state%temperature, &
                                             pw=state%pressure, &
                                             rhow=local_den%material2, &
                                             rhoi=local_den%material3))
    end function calculate_water_content_scalar

    function calculate_water_content_array(self, state, region_id) result(val)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_state), intent(in) :: state(:)
        integer(int32), intent(in) :: region_id
        real(real64) :: val(size(state))

        integer(int32) :: i
        class(abst_gcc), pointer :: local_gcc
        class(abst_wrf), pointer :: local_wrf
        class(abst_den), pointer :: local_den

        call self%materials%get_gcc(region_id, local_gcc)
        call self%materials%get_wrf(region_id, local_wrf)
        call self%materials%get_den(region_id, local_den)

        if (debug_mode) then
            if (.not. (associated(local_gcc) .and. associated(local_wrf) .and. associated(local_den))) then
                stop "Error: Failed to get pointers in calculate_water_content_array."
            end if
        end if

        do i = 1, size(state)
            val(i) = local_wrf%calc(-local_gcc%calc(T=state(i)%temperature, &
                                                    Pw=state(i)%pressure, &
                                                    rhoW=local_den%material2, &
                                                    rhoI=local_den%material3))
        end do
    end function calculate_water_content_array

    subroutine calc_thermal_properties_scalar(self, state, region_id, thc, vhc)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64), intent(out) :: thc, vhc

        class(abst_thc), pointer :: local_thc
        class(abst_gcc), pointer :: local_gcc
        class(abst_wrf), pointer :: local_wrf
        class(abst_den), pointer :: local_den
        class(abst_vhc), pointer :: local_vhc
        real(real64) :: local_dqi_dt

        call self%materials%get_thc(region_id, local_thc)
        call self%materials%get_gcc(region_id, local_gcc)
        call self%materials%get_wrf(region_id, local_wrf)
        call self%materials%get_den(region_id, local_den)
        call self%materials%get_vhc(region_id, local_vhc)

        ! デバッグモードでのポインタ関連付けチェックは省略

        ! 熱伝導率 (THC) の計算
        thc = local_thc%calc(state)

        ! 凍結水の微分項を一度だけ計算
        local_dqi_dt = local_wrf%deriv(-local_gcc%calc(t=state%temperature, &
                                                       pw=state%pressure, &
                                                       rhow=local_den%material2, &
                                                       rhoi=local_den%material3))

        ! 体積熱容量 (VHC) の計算
        vhc = local_vhc%calc(state=state, &
                             den=local_den, &
                             latentheat=local_gcc%lf, &
                             dqi_dt=local_dqi_dt)
    end subroutine calc_thermal_properties_scalar

    subroutine calc_thermal_properties_array(self, state, region_id, thc, vhc)
        implicit none
        class(type_properties_manager), intent(in) :: self
        type(type_state), intent(in) :: state(:)
        integer(int32), intent(in) :: region_id
        real(real64), intent(out) :: thc(size(state)), vhc(size(state))

        integer(int32) :: i
        class(abst_thc), pointer :: local_thc
        class(abst_gcc), pointer :: local_gcc
        class(abst_wrf), pointer :: local_wrf
        class(abst_den), pointer :: local_den
        class(abst_vhc), pointer :: local_vhc
        real(real64) :: local_dqi_dt

        call self%materials%get_thc(region_id, local_thc)
        call self%materials%get_gcc(region_id, local_gcc)
        call self%materials%get_wrf(region_id, local_wrf)
        call self%materials%get_den(region_id, local_den)
        call self%materials%get_vhc(region_id, local_vhc)

        ! デバッグモードでのポインタ関連付けチェックは省略

        do i = 1, size(state)
            ! 熱伝導率 (THC) の計算
            thc(i) = local_thc%calc(state(i))

            ! 凍結水の微分項を一度だけ計算
            local_dqi_dt = local_wrf%deriv(-local_gcc%calc(t=state(i)%temperature, &
                                                           pw=state(i)%pressure, &
                                                           rhow=local_den%material2, &
                                                           rhoi=local_den%material3))

            ! 体積熱容量 (VHC) の計算
            vhc(i) = local_vhc%calc(state=state(i), &
                                    den=local_den, &
                                    latentheat=local_gcc%lf, &
                                    dqi_dt=local_dqi_dt)
        end do
    end subroutine calc_thermal_properties_array

end module properties_properties_manager
