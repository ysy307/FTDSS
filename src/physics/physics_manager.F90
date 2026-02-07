module physics_service
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core
    use :: physics_base, only:type_iapws_wrapper
    use :: module_physics_materials, only:type_material_manager, type_thc_dispersivity
    use :: module_physics_models, only:type_models_manager, type_wrf_params, type_hcf_params
    implicit none
    private
    public :: type_physics_manager

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Main Derived Type with Generic Type-Bound Procedures
    !-------------------------------------------------------------------------------------------------------------------------------
    type :: type_physics_manager
        private
        type(type_iapws97) :: water
        type(type_iapws06) :: ice
        type(type_iapws_wrapper) :: iapws_wrapper
        type(type_material_manager), allocatable :: materials(:)
        type(type_models_manager), allocatable :: models(:)
        integer(int32), allocatable :: materials_id_map(:)
        integer(int32) :: num_materials
    contains
        procedure, public :: initialize => initialize_physics_manager
        procedure, private :: set_map => set_map_materials

        procedure, public :: calc_density
        procedure, public :: get_density_solid
        procedure, public :: calc_density_water
        procedure, public :: calc_density_ice
        procedure, public :: calc_density_vapor
        procedure, public :: calc_density_vapor_saturation
        procedure, public :: calc_density_water_derivatives
        procedure, public :: calc_density_ice_derivatives
        procedure, public :: calc_density_vapor_derivatives
        procedure, public :: calc_specific_heat
        procedure, public :: get_specific_heat_solid
        procedure, public :: calc_specific_heat_water
        procedure, public :: calc_specific_heat_ice
        procedure, public :: calc_specific_heat_vapor
        procedure, private :: calc_thermal_conductivity_nondispersivity
        procedure, private :: calc_thermal_conductivity_dispersivity
        generic, public :: calc_thermal_conductivity &
            => calc_thermal_conductivity_nondispersivity, calc_thermal_conductivity_dispersivity
        procedure, public :: calc_vol_heat_capacity
        procedure, public :: calc_latent_heat_fusion
        procedure, public :: calc_latent_heat_vaporization
        procedure, public :: calc_pressure_ice_water_derivative

        procedure, public :: update_water_phases
        procedure, public :: calc_Kflh
        procedure, public :: calc_KlT
        procedure, public :: calc_Kvh
        procedure, public :: calc_KvT

    end type type_physics_manager

contains

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Initialization
    !-------------------------------------------------------------------------------------------------------------------------------
    subroutine initialize_physics_manager(self, unique_material_ids, density_info, &
                                          specific_heat_info, heat_capacity_info, thermal_conductivity_info, &
                                          wrf_ids, wrf_model_info, hcf_ids, hcf_model_info, gcc_model_ids)
        implicit none
        class(type_physics_manager), intent(inout) :: self
        integer(int32), intent(in) :: unique_material_ids(:)
        type(type_physics_info), intent(in), optional :: density_info(:)
        type(type_physics_info), intent(in), optional :: specific_heat_info(:)
        type(type_physics_info), intent(in), optional :: heat_capacity_info(:)
        type(type_physics_info), intent(in), optional :: thermal_conductivity_info(:)
        integer(int32), intent(in), optional :: wrf_ids(:)
        type(type_wrf_params), intent(in), optional :: wrf_model_info(:)
        integer(int32), intent(in), optional :: hcf_ids(:)
        type(type_hcf_params), intent(in), optional :: hcf_model_info(:)
        integer(int32), intent(in), optional :: gcc_model_ids(:)

        integer(int32) :: stat
        integer(int32) :: model_idx, current_material_id

        call self%water%initialize()
        call self%ice%initialize()

        call self%set_map(unique_material_ids)
        call self%iapws_wrapper%initialize(self%water, self%ice)
        allocate (self%materials(self%num_materials), stat=stat)
        if (stat /= 0) then
            error stop "Error in properties_manager%initialize: Failed to allocate materials array."
        end if
        allocate (self%models(self%num_materials), stat=stat)
        if (stat /= 0) then
            error stop "Error in properties_manager%initialize: Failed to allocate models array."
        end if

        ! Density
        if (present(density_info)) then
            do model_idx = 1, self%num_materials
                current_material_id = unique_material_ids(model_idx)
                call self%materials(model_idx)%initialize(material_id=current_material_id, &
                                                          den_info=density_info(model_idx), &
                                                          water=self%water, ice=self%ice)
            end do
        end if

        ! Specific Heat
        if (present(specific_heat_info)) then
            do model_idx = 1, self%num_materials
                current_material_id = unique_material_ids(model_idx)
                call self%materials(model_idx)%initialize(material_id=current_material_id, &
                                                          sph_info=specific_heat_info(model_idx), &
                                                          water=self%water, ice=self%ice)
            end do
        end if

        ! Volumetric Heat Capacity
        if (present(heat_capacity_info)) then
            do model_idx = 1, self%num_materials
                current_material_id = unique_material_ids(model_idx)
                call self%materials(model_idx)%initialize(material_id=current_material_id, &
                                                          vhc_info=heat_capacity_info(model_idx), &
                                                          water=self%water, ice=self%ice)
            end do
        end if

        ! Thermal Conductivity
        if (present(thermal_conductivity_info)) then
            do model_idx = 1, self%num_materials
                current_material_id = unique_material_ids(model_idx)
                call self%materials(model_idx)%initialize(material_id=current_material_id, &
                                                          thc_info=thermal_conductivity_info(model_idx), &
                                                          water=self%water, ice=self%ice)
            end do
        end if

        ! WRF Model
        if (present(wrf_ids) .and. present(wrf_model_info)) then
            do model_idx = 1, self%num_materials
                current_material_id = unique_material_ids(model_idx)
                call self%models(model_idx)%initialize(material_id=current_material_id, &
                                                       wrf_id=wrf_ids(model_idx), wrf_params=wrf_model_info(model_idx), &
                                                       water=self%water, ice=self%ice)
            end do
        end if

        ! HCF Model
        if (present(hcf_ids) .and. present(hcf_model_info)) then
            do model_idx = 1, self%num_materials
                current_material_id = unique_material_ids(model_idx)
                call self%models(model_idx)%initialize(material_id=current_material_id, &
                                                       hcf_id=hcf_ids(model_idx), hcf_params=hcf_model_info(model_idx), &
                                                       water=self%water, ice=self%ice)
            end do
        end if

        ! GCC Model
        if (present(gcc_model_ids)) then
            do model_idx = 1, self%num_materials
                current_material_id = unique_material_ids(model_idx)
                call self%models(model_idx)%initialize(material_id=current_material_id, &
                                                       gcc_id=gcc_model_ids(model_idx), &
                                                       water=self%water, ice=self%ice)
            end do
        end if

    end subroutine initialize_physics_manager

    subroutine set_map_materials(self, unique_material_ids)
        implicit none
        class(type_physics_manager), intent(inout) :: self
        integer(int32), intent(in) :: unique_material_ids(:)

        integer(int32) :: max_region_id
        integer(int32) :: model_idx

        self%num_materials = size(unique_material_ids)
        max_region_id = maxval(unique_material_ids)

        allocate (self%materials_id_map(max_region_id), source=0)

        do model_idx = 1, self%num_materials
            self%materials_id_map(unique_material_ids(model_idx)) = model_idx
        end do

    end subroutine set_map_materials

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Generic Type-Bound Procedures
    !-------------------------------------------------------------------------------------------------------------------------------
    subroutine calc_density(self, material_id, state, density)
        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density

        call self%materials(self%materials_id_map(material_id))%calc_density(state, density)

    end subroutine calc_density

    subroutine get_density_solid(self, material_id, density_solid)
        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        real(real64), intent(inout) :: density_solid

        call self%materials(self%materials_id_map(material_id))%get_density_solid(density_solid)

    end subroutine get_density_solid

    subroutine calc_density_water(self, state, density_water)
        implicit none
        class(type_physics_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density_water

        call self%iapws_wrapper%calc_rho_water(state, density_water)

    end subroutine calc_density_water

    subroutine calc_density_ice(self, state, density_ice)
        implicit none
        class(type_physics_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density_ice

        call self%iapws_wrapper%calc_rho_ice(state, density_ice)

    end subroutine calc_density_ice

    subroutine calc_density_vapor(self, state, density_vapor)
        implicit none
        class(type_physics_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density_vapor

        call self%iapws_wrapper%calc_rho_vapor(state, density_vapor)

    end subroutine calc_density_vapor

    subroutine calc_density_vapor_saturation(self, state, density_vapor_saturation)
        implicit none
        class(type_physics_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density_vapor_saturation

        call self%iapws_wrapper%calc_rho_vapor_saturation(state, density_vapor_saturation)

    end subroutine calc_density_vapor_saturation

    subroutine calc_density_water_derivatives(self, material_id, state, dden_dT, dden_dP)
        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout), optional :: dden_dT
        real(real64), intent(inout), optional :: dden_dP

        call self%materials(self%materials_id_map(material_id))%calc_density_water_derivatives(state, dden_dT, dden_dP)
    end subroutine calc_density_water_derivatives

    subroutine calc_density_ice_derivatives(self, material_id, state, dden_dT, dden_dP)
        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout), optional :: dden_dT
        real(real64), intent(inout), optional :: dden_dP

        call self%materials(self%materials_id_map(material_id))%calc_density_ice_derivatives(state, dden_dT, dden_dP)
    end subroutine calc_density_ice_derivatives

    subroutine calc_density_vapor_derivatives(self, material_id, state, dden_dT, dden_dP)

        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout), optional :: dden_dT
        real(real64), intent(inout), optional :: dden_dP

        call self%materials(self%materials_id_map(material_id))%calc_density_vapor_derivatives(state, dden_dT, dden_dP)
    end subroutine calc_density_vapor_derivatives

    subroutine calc_specific_heat(self, material_id, state, specific_heat)
        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: specific_heat

        call self%materials(self%materials_id_map(material_id))%calc_specific_heat(state, specific_heat)

    end subroutine calc_specific_heat

    subroutine get_specific_heat_solid(self, material_id, cp)
        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        real(real64), intent(inout) :: cp

        call self%materials(self%materials_id_map(material_id))%get_specific_heat_solid(cp)

    end subroutine get_specific_heat_solid

    subroutine calc_specific_heat_water(self, state, specific_heat_water)
        implicit none
        class(type_physics_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: specific_heat_water

        call self%iapws_wrapper%calc_cp_water(state, specific_heat_water)

    end subroutine calc_specific_heat_water

    subroutine calc_specific_heat_ice(self, state, specific_heat_ice)
        implicit none
        class(type_physics_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: specific_heat_ice

        call self%iapws_wrapper%calc_cp_ice(state, specific_heat_ice)

    end subroutine calc_specific_heat_ice

    subroutine calc_specific_heat_vapor(self, state, specific_heat_vapor)
        implicit none
        class(type_physics_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: specific_heat_vapor

        call self%iapws_wrapper%calc_cp_vapor(state, specific_heat_vapor)

    end subroutine calc_specific_heat_vapor

    subroutine calc_thermal_conductivity_nondispersivity(self, material_id, state, thermal_conductivity)
        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: thermal_conductivity

        call self%materials(self%materials_id_map(material_id))%calc_thermal_conductivity(state, thermal_conductivity)

    end subroutine calc_thermal_conductivity_nondispersivity

    subroutine calc_thermal_conductivity_dispersivity(self, material_id, state, thermal_conductivity)
        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        type(type_thc_dispersivity), intent(inout) :: thermal_conductivity

        call self%materials(self%materials_id_map(material_id))%calc_thermal_conductivity(state, thermal_conductivity)

    end subroutine calc_thermal_conductivity_dispersivity

    subroutine calc_vol_heat_capacity(self, material_id, state, vol_heat_capacity)
        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: vol_heat_capacity

        call self%materials(self%materials_id_map(material_id))%calc_vol_heat_capacity(state, vol_heat_capacity)

    end subroutine calc_vol_heat_capacity

    subroutine calc_latent_heat_fusion(self, material_id, state, latent_heat_fusion)
        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: latent_heat_fusion

        call self%models(self%materials_id_map(material_id))%calc_latent_heat_fusion(state, latent_heat_fusion)
    end subroutine calc_latent_heat_fusion

    subroutine calc_latent_heat_vaporization(self, material_id, state, latent_heat_vaporization)
        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: latent_heat_vaporization

        call self%models(self%materials_id_map(material_id))%calc_latent_heat_vaporization(state, latent_heat_vaporization)
    end subroutine calc_latent_heat_vaporization

    subroutine calc_pressure_ice_water_derivative(self, material_id, state, deriv)
        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv

        call self%models(self%materials_id_map(material_id))%calc_pressure_ice_water_derivative(state, deriv)
    end subroutine calc_pressure_ice_water_derivative

    ! subroutine update_water_phases(self, material_id, state)
    subroutine update_water_phases(self, material_id, state)
        implicit none
        class(type_physics_manager), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state

        call self%models(self%materials_id_map(material_id))%update_water_phases(state)
    end subroutine update_water_phases

    subroutine calc_Kflh(self, material_id, state, Kflh)
        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: Kflh

        call self%models(self%materials_id_map(material_id))%calc_Kflh(state, Kflh)
    end subroutine calc_Kflh

    subroutine calc_KlT(self, material_id, state, KlT)
        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: KlT

        call self%models(self%materials_id_map(material_id))%calc_KlT(state, KlT)
    end subroutine calc_KlT

    subroutine calc_Kvh(self, material_id, state, Kvh)
        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: Kvh

        call self%models(self%materials_id_map(material_id))%calc_Kvh(state, Kvh)
    end subroutine calc_Kvh

    subroutine calc_KvT(self, material_id, state, KvT)
        implicit none
        class(type_physics_manager), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: KvT

        call self%models(self%materials_id_map(material_id))%calc_KvT(state, KvT)
    end subroutine calc_KvT

end module physics_service
