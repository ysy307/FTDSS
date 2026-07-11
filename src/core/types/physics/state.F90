module core_types_physics_state
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: core_allocate, only:allocate_array
    use :: core_deallocate, only:deallocate_array
    use :: core_types_geometry_coordinate, only:type_coordinate_dp
    implicit none
    private

    public :: type_state

    ! ------------------------------------------------------------------
    ! 1. Real(real64) Wrapper Type
    ! ------------------------------------------------------------------
    type :: type_field_dp
        private
        real(real64) :: value = 0.0d0
        logical :: is_set = .false.
    contains
        procedure, pass(self), public :: set => set_field_dp
        procedure, pass(self), public :: get => get_field_dp
        procedure, pass(self), private :: reset => reset_field_dp
    end type type_field_dp

    ! ------------------------------------------------------------------
    ! 2. Real(real64) allocatable Wrapper Type
    ! ------------------------------------------------------------------
    type :: type_field_array_dp
        private
        real(real64), allocatable :: value(:)
        logical :: is_set = .false.
    contains
        procedure, pass(self), private :: set_field_array_dp
        procedure, pass(self), private :: set_index_field_array_dp
        generic, public :: set => set_field_array_dp, set_index_field_array_dp
        procedure, pass(self), public :: get => get_field_array_dp
        procedure, pass(self), private :: reset => reset_field_array_dp
    end type type_field_array_dp
    ! ------------------------------------------------------------------
    ! 3. Coordinate Wrapper Type
    ! ------------------------------------------------------------------
    type :: type_field_coord
        type(type_coordinate_dp), private :: value = type_coordinate_dp(0.0d0, 0.0d0, 0.0d0)
        logical, private :: is_set = .false.
    contains
        procedure, pass(self), public :: set => field_coord_set
        procedure, pass(self), public :: get => get_field_coord
        procedure, pass(self), private :: reset => reset_field_coord
    end type type_field_coord

    ! ------------------------------------------------------------------
    ! 3. Main State Type
    ! ------------------------------------------------------------------
    type :: type_state
        ! --- Thermodynamic & Physical Properties ---
        type(type_field_dp) :: temperature ! Temperature [C]
        type(type_field_dp) :: pressure ! Pore pressure [Pa]
        type(type_field_dp) :: water_content ! Water content [-]
        type(type_field_dp) :: ice_content ! Ice content [-]
        type(type_field_dp) :: vapor_content ! Vapor content [-]
        type(type_field_dp) :: air_content ! Air content [-]
        type(type_field_dp) :: porosity ! Porosity [-]
        ! Generalized suction p_c* = max(psi_cap, psi_cryo) [Pa].
        ! Set by update_water_phases; the liquid-phase potential is
        ! -effective_suction, and retention-based properties (theta_w,
        ! relative permeability) must be evaluated at this suction.
        type(type_field_dp) :: effective_suction ! [Pa]

        type(type_field_array_dp) :: temperature_history ! Temperature history [C]
        type(type_field_array_dp) :: pressure_history ! Pressure history [Pa]
        type(type_field_array_dp) :: porosity_history ! Porosity history [-]
        ! Prognostic ice content history [-] (A1 Clapeyron-pressure-constraint
        ! closure only). Level 1 is the current-iterate state ice (prognostic
        ! value plus in-step local phase change); levels j >= 2 are the stored
        ! BDF history of the prognostic ice variable. When set, the storage
        ! (transient) term evaluations substitute these values for the
        ! equilibrium ice recomputed at (T_hist, P_hist) - the equilibrium
        ! closure yields ~0 ice at nodes whose pressure is pinned at P_eq(T).
        ! Left unset by every other code path, in which case all consumers
        ! keep the equilibrium evaluation (bit-identical behavior).
        type(type_field_array_dp) :: ice_content_history ! Prognostic ice history [-]

        ! --- Thermal Properties ---
        type(type_field_dp) :: latent_heat_fusion ! [J/kg]
        type(type_field_dp) :: latent_heat_vaporization ! [J/kg]

        ! --- Derivatives ---
        type(type_field_dp) :: dQw_dT ! d(theta_water)/dT
        type(type_field_dp) :: dQv_dT ! d(theta_vapor)/dT
        type(type_field_dp) :: dQa_dT ! d(theta_air)/dT
        type(type_field_dp) :: dQi_dT ! d(theta_ice)/dT
        type(type_field_dp) :: dQw_dP ! d(theta_water)/dP
        type(type_field_dp) :: dQv_dP ! d(theta_vapor)/dP
        type(type_field_dp) :: dQa_dP ! d(theta_air)/dP
        type(type_field_dp) :: dQi_dP ! d(theta_ice)/dP

        ! type(type_field_dp) :: dot_T ! Temperature change rate [K/s]
        ! type(type_field_dp) :: dot_P ! Pressure change rate [Pa/s]
        type(type_field_coord) :: grad_T ! Temperature gradient [K/m]
        type(type_field_coord) :: grad_P ! Pressure gradient [Pa/m]

        ! --- Other ---
        type(type_field_dp) :: relative_humidity ! [-]
        type(type_field_dp) :: mass_fraction_clay ! [-]

        ! --- Vectors ---
        type(type_field_coord) :: water_flux ! Water flux vector [m/s]
        type(type_field_coord) :: vapor_flux ! Vapor flux vector [m/s]

    contains
        !> Bulk Setter
        procedure, public, pass(self) :: set => set_all_state
        !> Bulk Getter
        procedure, public, pass(self) :: get => get_all_state
        !> Reset All
        procedure, public, pass(self) :: reset => reset_all_state
        !> Clear only the prognostic ice history (A1 Clapeyron closure):
        !> returns the field to the unset state so storage-term evaluations
        !> fall back to the equilibrium path. Needed because the per-field
        !> reset is private and reused workspace Gauss-point states must not
        !> carry the history over from a previously processed element.
        procedure, public, pass(self) :: clear_ice_content_history => clear_ice_content_history_state
        !> Copy
        procedure, public, pass(self) :: copy => copy_state
    end type type_state

contains

    ! ==================================================================
    ! Implementation: Real64 Field
    ! ==================================================================

    subroutine set_field_dp(self, value)
        implicit none
        class(type_field_dp), intent(inout) :: self
        real(real64), intent(in) :: value

        self%value = value
        self%is_set = .true.
    end subroutine set_field_dp

    subroutine get_field_dp(self, value, is_set)
        implicit none
        class(type_field_dp), intent(in) :: self
        real(real64), intent(inout) :: value
        logical, intent(inout), optional :: is_set

        if (.not. self%is_set) then
            value = huge(0.0d0)
            if (present(is_set)) is_set = .false.
            return
        end if

        value = self%value
        if (present(is_set)) is_set = self%is_set

    end subroutine get_field_dp

    subroutine reset_field_dp(self)
        implicit none
        class(type_field_dp), intent(inout) :: self

        self%value = 0.0d0
        self%is_set = .false.
    end subroutine reset_field_dp

    ! ==================================================================
    ! Implementation: Real64 Array Field
    ! ==================================================================

    subroutine set_field_array_dp(self, value)
        implicit none
        class(type_field_array_dp), intent(inout) :: self
        real(real64), intent(in) :: value(:)

        if (allocated(self%value)) then
            ! Reallocate only if sizes differ
            if (size(self%value) /= size(value)) then
                deallocate (self%value)
                allocate (self%value(size(value)))
            end if
        else
            allocate (self%value(size(value)))
        end if
        self%value = value
        self%is_set = .true.
    end subroutine set_field_array_dp

    subroutine set_index_field_array_dp(self, index, value)
        implicit none
        class(type_field_array_dp), intent(inout) :: self
        integer(int32), intent(in) :: index
        real(real64), intent(in) :: value

        if (.not. self%is_set) return
        self%value(index) = value
    end subroutine set_index_field_array_dp

    subroutine get_field_array_dp(self, value, is_set)
        implicit none
        class(type_field_array_dp), intent(in), target :: self
        real(real64), intent(inout), pointer, contiguous, dimension(:) :: value
        logical, intent(inout), optional :: is_set

        if (.not. self%is_set) then
            nullify (value)
            if (present(is_set)) is_set = .false.
            return
        end if

        value => self%value
        if (present(is_set)) is_set = self%is_set

    end subroutine get_field_array_dp

    subroutine reset_field_array_dp(self)
        implicit none
        class(type_field_array_dp), intent(inout) :: self

        if (allocated(self%value)) then
            deallocate (self%value)
        end if
        self%is_set = .false.
    end subroutine reset_field_array_dp

    subroutine field_coord_set(self, value)
        class(type_field_coord), intent(inout) :: self
        type(type_coordinate_dp), intent(in) :: value

        self%value = value
        self%is_set = .true.
    end subroutine field_coord_set

    subroutine get_field_coord(self, value, is_set)
        implicit none
        class(type_field_coord), intent(in), target :: self
        type(type_coordinate_dp), intent(inout), pointer :: value
        logical, intent(inout), optional :: is_set

        if (self%is_set) then
            value => self%value
            if (present(is_set)) is_set = self%is_set
        else
            nullify (value)
            if (present(is_set)) is_set = .false.
        end if
    end subroutine get_field_coord

    subroutine reset_field_coord(self)
        implicit none
        class(type_field_coord), intent(inout) :: self

        self%value = type_coordinate_dp(0.0d0, 0.0d0, 0.0d0)
        self%is_set = .false.
    end subroutine reset_field_coord

    ! ==================================================================
    ! Implementation: State Methods
    ! ==================================================================

    ! Regular subroutine to avoid argument count limitations
    ! and compiler-dependent behavior.
    subroutine set_all_state(self, temperature, pressure, water_content, ice_content, &
                             vapor_content, air_content, porosity, &
                             temperature_history, pressure_history, porosity_history, &
                             latent_heat_fusion, latent_heat_vaporization, &
                             dQw_dT, dQv_dT, dQa_dT, dQi_dT, dQw_dP, dQv_dP, dQa_dP, dQi_dP, &
                             !  dot_T, dot_P, &
                             grad_T, grad_P, &
                             relative_humidity, mass_fraction_clay, &
                             water_flux, vapor_flux)
        implicit none
        class(type_state), intent(inout) :: self

        real(real64), intent(in), optional :: temperature, pressure
        real(real64), intent(in), optional :: water_content, ice_content
        real(real64), intent(in), optional :: vapor_content, air_content
        real(real64), intent(in), optional :: porosity
        real(real64), intent(in), optional :: temperature_history(:)
        real(real64), intent(in), optional :: pressure_history(:)
        real(real64), intent(in), optional :: porosity_history(:)
        real(real64), intent(in), optional :: latent_heat_fusion, latent_heat_vaporization
        real(real64), intent(in), optional :: dQw_dT, dQv_dT, dQa_dT, dQi_dT
        real(real64), intent(in), optional :: dQw_dP, dQv_dP, dQa_dP, dQi_dP
        type(type_coordinate_dp), intent(in), optional :: grad_T, grad_P
        real(real64), intent(in), optional :: relative_humidity, mass_fraction_clay
        type(type_coordinate_dp), intent(in), optional :: water_flux, vapor_flux

        if (present(temperature)) then
            call self%temperature%set(temperature)
        end if
        if (present(pressure)) then
            call self%pressure%set(pressure)
        end if
        if (present(water_content)) then
            call self%water_content%set(water_content)
        end if
        if (present(ice_content)) then
            call self%ice_content%set(ice_content)
        end if
        if (present(vapor_content)) then
            call self%vapor_content%set(vapor_content)
        end if
        if (present(air_content)) then
            call self%air_content%set(air_content)
        end if
        if (present(porosity)) then
            call self%porosity%set(porosity)
        end if

        if (present(temperature_history)) then
            call self%temperature_history%set(temperature_history)
        end if

        if (present(pressure_history)) then
            call self%pressure_history%set(pressure_history)
        end if

        if (present(porosity_history)) then
            call self%porosity_history%set(porosity_history)
        end if

        if (present(latent_heat_fusion)) then
            call self%latent_heat_fusion%set(latent_heat_fusion)
        end if
        if (present(latent_heat_vaporization)) then
            call self%latent_heat_vaporization%set(latent_heat_vaporization)
        end if

        if (present(dQw_dT)) then
            call self%dQw_dT%set(dQw_dT)
        end if
        if (present(dQv_dT)) then
            call self%dQv_dT%set(dQv_dT)
        end if
        if (present(dQa_dT)) then
            call self%dQa_dT%set(dQa_dT)
        end if
        if (present(dQi_dT)) then
            call self%dQi_dT%set(dQi_dT)
        end if
        if (present(dQw_dP)) then
            call self%dQw_dP%set(dQw_dP)
        end if
        if (present(dQv_dP)) then
            call self%dQv_dP%set(dQv_dP)
        end if
        if (present(dQa_dP)) then
            call self%dQa_dP%set(dQa_dP)
        end if
        if (present(dQi_dP)) then
            call self%dQi_dP%set(dQi_dP)
        end if

        if (present(grad_T)) then
            call self%grad_T%set(grad_T)
        end if
        if (present(grad_P)) then
            call self%grad_P%set(grad_P)
        end if

        if (present(relative_humidity)) then
            call self%relative_humidity%set(relative_humidity)
        end if
        if (present(mass_fraction_clay)) then
            call self%mass_fraction_clay%set(mass_fraction_clay)
        end if
        if (present(water_flux)) then
            call self%water_flux%set(water_flux)
        end if
        if (present(vapor_flux)) then
            call self%vapor_flux%set(vapor_flux)
        end if

    end subroutine set_all_state

    subroutine get_all_state(self, temperature, pressure, water_content, ice_content, &
                             vapor_content, air_content, porosity, &
                             temperature_history, pressure_history, porosity_history, &
                             latent_heat_fusion, latent_heat_vaporization, &
                             dQw_dT, dQv_dT, dQa_dT, dQi_dT, dQw_dP, dQv_dP, dQa_dP, dQi_dP, &
                             ! dot_T, dot_P, &
                             grad_T, grad_P, &
                             relative_humidity, mass_fraction_clay, &
                             water_flux, vapor_flux)
        implicit none
        class(type_state), intent(in) :: self
        real(real64), intent(inout), optional :: temperature, pressure
        real(real64), intent(inout), optional :: water_content, ice_content
        real(real64), intent(inout), optional :: vapor_content, air_content
        real(real64), intent(inout), optional :: porosity
        real(real64), pointer, contiguous, dimension(:), intent(inout), optional :: temperature_history
        real(real64), pointer, contiguous, dimension(:), intent(inout), optional :: pressure_history
        real(real64), pointer, contiguous, dimension(:), intent(inout), optional :: porosity_history
        real(real64), intent(inout), optional :: latent_heat_fusion, latent_heat_vaporization
        real(real64), intent(inout), optional :: dQw_dT, dQv_dT, dQa_dT, dQi_dT
        real(real64), intent(inout), optional :: dQw_dP, dQv_dP, dQa_dP, dQi_dP
        type(type_coordinate_dp), intent(inout), pointer, optional :: grad_T, grad_P
        real(real64), intent(inout), optional :: relative_humidity, mass_fraction_clay
        type(type_coordinate_dp), intent(inout), pointer, optional :: water_flux, vapor_flux

        if (present(temperature)) then
            call self%temperature%get(temperature)
        end if
        if (present(pressure)) then
            call self%pressure%get(pressure)
        end if
        if (present(water_content)) then
            call self%water_content%get(water_content)
        end if
        if (present(ice_content)) then
            call self%ice_content%get(ice_content)
        end if
        if (present(vapor_content)) then
            call self%vapor_content%get(vapor_content)
        end if

        if (present(air_content)) then
            call self%air_content%get(air_content)
        end if
        if (present(porosity)) then
            call self%porosity%get(porosity)
        end if
        if (present(temperature_history)) then
            call self%temperature_history%get(temperature_history)
        end if
        if (present(pressure_history)) then
            call self%pressure_history%get(pressure_history)
        end if
        if (present(porosity_history)) then
            call self%porosity_history%get(porosity_history)
        end if
        if (present(latent_heat_fusion)) then
            call self%latent_heat_fusion%get(latent_heat_fusion)
        end if
        if (present(latent_heat_vaporization)) then
            call self%latent_heat_vaporization%get(latent_heat_vaporization)
        end if
        if (present(dQw_dT)) then
            call self%dQw_dT%get(dQw_dT)
        end if
        if (present(dQv_dT)) then
            call self%dQv_dT%get(dQv_dT)
        end if
        if (present(dQa_dT)) then
            call self%dQa_dT%get(dQa_dT)
        end if
        if (present(dQi_dT)) then
            call self%dQi_dT%get(dQi_dT)
        end if
        if (present(dQw_dP)) then
            call self%dQw_dP%get(dQw_dP)
        end if
        if (present(dQv_dP)) then
            call self%dQv_dP%get(dQv_dP)
        end if
        if (present(dQa_dP)) then
            call self%dQa_dP%get(dQa_dP)
        end if
        if (present(dQi_dP)) then
            call self%dQi_dP%get(dQi_dP)
        end if
        if (present(grad_T)) then
            call self%grad_T%get(grad_T)
        end if
        if (present(grad_P)) then
            call self%grad_P%get(grad_P)
        end if
        if (present(relative_humidity)) then
            call self%relative_humidity%get(relative_humidity)
        end if
        if (present(mass_fraction_clay)) then
            call self%mass_fraction_clay%get(mass_fraction_clay)
        end if
        if (present(water_flux)) then
            call self%water_flux%get(water_flux)
        end if
        if (present(vapor_flux)) then
            call self%vapor_flux%get(vapor_flux)
        end if
    end subroutine get_all_state

    subroutine reset_all_state(self)
        implicit none
        class(type_state), intent(inout) :: self

        call self%temperature%reset()
        call self%pressure%reset()
        call self%water_content%reset()
        call self%ice_content%reset()
        call self%vapor_content%reset()
        call self%air_content%reset()
        call self%porosity%reset()
        call self%effective_suction%reset()
        call self%temperature_history%reset()
        call self%pressure_history%reset()
        call self%porosity_history%reset()
        call self%ice_content_history%reset()
        call self%latent_heat_fusion%reset()
        call self%latent_heat_vaporization%reset()
        call self%dQw_dT%reset()
        call self%dQv_dT%reset()
        call self%dQa_dT%reset()
        call self%dQi_dT%reset()
        call self%dQw_dP%reset()
        call self%dQv_dP%reset()
        call self%dQa_dP%reset()
        call self%dQi_dP%reset()
        call self%grad_T%reset()
        call self%grad_P%reset()
        call self%relative_humidity%reset()
        call self%mass_fraction_clay%reset()
        call self%water_flux%reset()
        call self%vapor_flux%reset()
    end subroutine reset_all_state

    !> See the type-bound declaration for the rationale.
    subroutine clear_ice_content_history_state(self)
        implicit none
        class(type_state), intent(inout) :: self

        call self%ice_content_history%reset()
    end subroutine clear_ice_content_history_state

    subroutine copy_state(self, source)
        implicit none
        class(type_state), intent(inout) :: self
        class(type_state), intent(in) :: source

        call copy_field(self%temperature, source%temperature)
        call copy_field(self%pressure, source%pressure)
        call copy_field(self%water_content, source%water_content)
        call copy_field(self%ice_content, source%ice_content)
        call copy_field(self%vapor_content, source%vapor_content)
        call copy_field(self%air_content, source%air_content)
        call copy_field(self%porosity, source%porosity)
        call copy_field(self%effective_suction, source%effective_suction)
        call copy_array_field(self%temperature_history, source%temperature_history)
        call copy_array_field(self%pressure_history, source%pressure_history)
        call copy_array_field(self%porosity_history, source%porosity_history)
        call copy_array_field(self%ice_content_history, source%ice_content_history)
        call copy_field(self%latent_heat_fusion, source%latent_heat_fusion)
        call copy_field(self%latent_heat_vaporization, source%latent_heat_vaporization)
        call copy_field(self%dQw_dT, source%dQw_dT)
        call copy_field(self%dQv_dT, source%dQv_dT)
        call copy_field(self%dQa_dT, source%dQa_dT)
        call copy_field(self%dQi_dT, source%dQi_dT)
        call copy_field(self%dQw_dP, source%dQw_dP)
        call copy_field(self%dQv_dP, source%dQv_dP)
        call copy_field(self%dQa_dP, source%dQa_dP)
        call copy_field(self%dQi_dP, source%dQi_dP)
        call copy_coord_field(self%grad_T, source%grad_T)
        call copy_coord_field(self%grad_P, source%grad_P)
        call copy_field(self%relative_humidity, source%relative_humidity)
        call copy_field(self%mass_fraction_clay, source%mass_fraction_clay)
        call copy_coord_field(self%water_flux, source%water_flux)
        call copy_coord_field(self%vapor_flux, source%vapor_flux)

    contains
        subroutine copy_field(dst, src)
            implicit none
            type(type_field_dp), intent(inout) :: dst
            type(type_field_dp), intent(in) :: src

            if (src%is_set) then
                call dst%set(src%value)
            else
                call dst%reset()
            end if
        end subroutine copy_field

        subroutine copy_array_field(dst, src)
            implicit none
            type(type_field_array_dp), intent(inout) :: dst
            type(type_field_array_dp), intent(in) :: src

            if (src%is_set) then
                call dst%set(src%value)
            else
                call dst%reset()
            end if
        end subroutine copy_array_field

        subroutine copy_coord_field(dst, src)
            implicit none
            type(type_field_coord), intent(inout) :: dst
            type(type_field_coord), intent(in) :: src

            if (src%is_set) then
                call dst%set(src%value)
            else
                call dst%reset()
            end if
        end subroutine copy_coord_field
    end subroutine copy_state

end module core_types_physics_state
