module core_types_physics
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: core_types_coordinate, only:type_coordinate_dp
    implicit none
    private

    public :: type_state
    public :: type_physics_info

! ------------------------------------------------------------------
    ! 1. Real(real64) Wrapper Type
    ! ------------------------------------------------------------------
    type :: type_field_r64
        private
        real(real64) :: value = 0.0d0
        logical :: is_set = .false.
    contains
        procedure, pass(self), public :: set => set_field_r64
        procedure, pass(self), public :: get => get_field_r64
        procedure, pass(self), private :: reset => reset_field_r64
    end type type_field_r64

    ! ------------------------------------------------------------------
    ! 2. Coordinate Wrapper Type
    ! ------------------------------------------------------------------
    type :: type_field_coord
        private
        type(type_coordinate_dp) :: value
        logical :: is_set = .false.
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
        type(type_field_r64) :: temperature ! Temperature [C]
        type(type_field_r64) :: pressure ! Pressure [m]
        type(type_field_r64) :: water_content ! Water content [-]
        type(type_field_r64) :: ice_content ! Ice content [-]
        type(type_field_r64) :: vapor_content ! Vapor content [-]
        type(type_field_r64) :: air_content ! Air content [-]
        type(type_field_r64) :: porosity ! Porosity [-]

        ! --- Thermal Properties ---
        type(type_field_r64) :: latent_heat_fusion ! [J/kg]
        type(type_field_r64) :: latent_heat_vaporization ! [J/kg]

        ! --- Derivatives ---
        type(type_field_r64) :: dQw_dT ! d(theta_w)/dT
        type(type_field_r64) :: dQv_dT ! d(theta_v)/dT

        ! --- Other ---
        type(type_field_r64) :: relative_humidity ! [-]
        type(type_field_r64) :: mass_fraction_clay ! [-]

        ! --- Vectors ---
        type(type_field_coord) :: water_flux ! Water flux vector [m/s]

    contains
        ! Bulk Setter (Optional arguments)
        procedure, public :: set => state_set_all
        ! Reset All (Pure Elemental)
        procedure, public :: reset => state_reset_all
    end type type_state

    type :: type_physics_info
        integer(int32) :: num_phases = 0
        real(real64) :: solid = 0.0d0
        real(real64) :: water = 0.0d0
        real(real64) :: ice = 0.0d0
        real(real64) :: vapor = 0.0d0
        real(real64) :: air = 0.0d0
        real(real64), allocatable :: dispersity(:)
        real(real64), allocatable :: params(:)
    end type type_physics_info

contains

    ! ==================================================================
    ! Implementation: Real64 Field
    ! ==================================================================

    ! Setter
    pure elemental subroutine set_field_r64(self, value)
        implicit none
        class(type_field_r64), intent(inout) :: self
        real(real64), intent(in) :: value

        self%value = value
        self%is_set = .true.
    end subroutine set_field_r64

    ! Getter (Subroutine style)
    pure elemental subroutine get_field_r64(self, value, is_set)
        implicit none
        class(type_field_r64), intent(in) :: self
        real(real64), intent(inout) :: value
        logical, intent(inout), optional :: is_set

        if (.not. self%is_set) then
            value = 0.0d0
            if (present(is_set)) is_set = .false.
            return
        end if

        value = self%value
        if (present(is_set)) is_set = self%is_set

    end subroutine get_field_r64

    ! Reset
    pure elemental subroutine reset_field_r64(self)
        implicit none
        class(type_field_r64), intent(inout) :: self

        self%value = 0.0d0
        self%is_set = .false.
    end subroutine reset_field_r64

    ! Setter
    pure elemental subroutine field_coord_set(self, value)
        class(type_field_coord), intent(inout) :: self
        type(type_coordinate_dp), intent(in) :: value

        self%value = value
        self%is_set = .true.
    end subroutine field_coord_set

    ! Getter
    pure elemental subroutine get_field_coord(self, val, is_set)
        implicit none
        class(type_field_coord), intent(in) :: self
        type(type_coordinate_dp), intent(inout) :: val
        logical, intent(inout), optional :: is_set

        if (self%is_set) then
            val = self%value
            if (present(is_set)) is_set = self%is_set
        else
            val = type_coordinate_dp(0.0d0, 0.0d0, 0.0d0)
            if (present(is_set)) is_set = .false.
        end if
    end subroutine get_field_coord

    ! Reset
    pure elemental subroutine reset_field_coord(self)
        implicit none
        class(type_field_coord), intent(inout) :: self

        self%value = type_coordinate_dp(0.0d0, 0.0d0, 0.0d0)
        self%is_set = .false.
    end subroutine reset_field_coord

    ! ==================================================================
    ! Implementation: State Methods
    ! ==================================================================

    ! Bulk Setter
    ! Pure Elemental には引数制限やコンパイラ依存があるため、
    ! ここでは通常の Subroutine として実装します（安全策）。
    subroutine state_set_all(self, temperature, pressure, water_content, ice_content, &
                             vapor_content, air_content, porosity, &
                             latent_heat_fusion, latent_heat_vaporization, &
                             dQw_dT, dQv_dT, &
                             relative_humidity, mass_fraction_clay, &
                             water_flux)
        implicit none
        class(type_state), intent(inout) :: self

        real(real64), intent(in), optional :: temperature, pressure
        real(real64), intent(in), optional :: water_content, ice_content
        real(real64), intent(in), optional :: vapor_content, air_content
        real(real64), intent(in), optional :: porosity
        real(real64), intent(in), optional :: latent_heat_fusion, latent_heat_vaporization
        real(real64), intent(in), optional :: dQw_dT, dQv_dT
        real(real64), intent(in), optional :: relative_humidity, mass_fraction_clay
        type(type_coordinate_dp), intent(in), optional :: water_flux

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

        if (present(relative_humidity)) then
            call self%relative_humidity%set(relative_humidity)
        end if
        if (present(mass_fraction_clay)) then
            call self%mass_fraction_clay%set(mass_fraction_clay)
        end if
        if (present(water_flux)) then
            call self%water_flux%set(water_flux)
        end if

    end subroutine state_set_all

    ! Reset All (Pure Elemental)
    pure elemental subroutine state_reset_all(self)
        implicit none
        class(type_state), intent(inout) :: self

        call self%temperature%reset()
        call self%pressure%reset()
        call self%water_content%reset()
        call self%ice_content%reset()
        call self%vapor_content%reset()
        call self%air_content%reset()
        call self%porosity%reset()
        call self%latent_heat_fusion%reset()
        call self%latent_heat_vaporization%reset()
        call self%dQw_dT%reset()
        call self%dQv_dT%reset()
        call self%relative_humidity%reset()
        call self%mass_fraction_clay%reset()
        call self%water_flux%reset()
    end subroutine state_reset_all

end module core_types_physics
