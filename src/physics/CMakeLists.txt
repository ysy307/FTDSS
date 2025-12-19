# ====================================================================
# 1. Physics Constants
# ====================================================================
# 定数は他のすべてから参照されるため、独立させておきます
add_library(Physics_Base STATIC
    physics_constants.F90
    physics_types.F90
)
target_include_directories(Physics_Base PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
enable_build_flags(Physics_Base)

# ====================================================================
# 2. Physics Materials Group
# ====================================================================
# 密度・比熱・熱伝導率などを「Physics_Materials」として1つに統合します。
# これにより、内部的な依存関係（Base -> Densityなど）をCMakeが自動解決します。

set(PHYSICS_MATERIALS_SOURCES
    # Base
    materials/materials_base.F90
    # Density
    materials/density/density_interface.F90
    materials/density/density_base.F90
    materials/density/density_multiphase.F90
    # Specific Heat
    materials/specific_heat/specific_heat_interface.F90
    materials/specific_heat/specific_heat_base.F90
    materials/specific_heat/specific_heat_multiphase.F90
    # Thermal Conductivity
    materials/thermal_conductivity/thermal_conductivity_interface.F90
    materials/thermal_conductivity/thermal_conductivity_base.F90
    materials/thermal_conductivity/thermal_conductivity_multiphase.F90
    # Volumetric Heat Capacity
    materials/heat_capacity/heat_capacity_interface.F90
    materials/heat_capacity/heat_capacity_base.F90
    materials/heat_capacity/heat_capacity_multiphase.F90
    # Entry / Aggregator
    materials/materials.F90
)

add_library(Physics_Materials STATIC ${PHYSICS_MATERIALS_SOURCES})

target_include_directories(Physics_Materials PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Physics_Materials PUBLIC
    Physics_Base
    Solver::Core
)
enable_build_flags(Physics_Materials)

# ====================================================================
# 3. Physics Models Group
# ====================================================================
# 透水係数・保水曲線・相変化などを「Physics_Models」として1つに統合します。

set(PHYSICS_MODELS_SOURCES
    # Hydraulic Conductivity
    models/hydraulic_conductivity/hcf_interface.F90
    models/hydraulic_conductivity/hcf_base.F90
    models/hydraulic_conductivity/hcf_bc.F90
    models/hydraulic_conductivity/hcf_vg.F90
    models/hydraulic_conductivity/hcf_ko.F90
    models/hydraulic_conductivity/hcf_mvg.F90
    models/hydraulic_conductivity/hcf_durner.F90
    models/hydraulic_conductivity/hcf_dvgch.F90
    models/hydraulic_conductivity/hcf_impedance.F90
    models/hydraulic_conductivity/hcf_viscosity.F90
    models/hydraulic_conductivity/hcf_vapor.F90
    # Water Retention
    models/water_retention/wrf_interface.f90
    models/water_retention/wrf_base.F90
    models/water_retention/wrf_bc.F90
    models/water_retention/wrf_vg.F90
    models/water_retention/wrf_ko.F90
    models/water_retention/wrf_mvg.F90
    models/water_retention/wrf_durner.F90
    models/water_retention/wrf_dvgch.F90
    # Phase Change
    models/phase_change/liquid_solid/gcc_interface.F90
    models/phase_change/liquid_solid/gcc_base.F90
    models/phase_change/liquid_solid/fusion.F90
    models/phase_change/liquid_vapor/vaporization.F90
    # Entry / Aggregator
    models/models.F90
)

add_library(Physics_Models STATIC ${PHYSICS_MODELS_SOURCES})

target_include_directories(Physics_Models PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Physics_Models PUBLIC
    Physics_Base
    Solver::Core
)
enable_build_flags(Physics_Models)

# ====================================================================
# 4. Service Layer (Physics Main)
# ====================================================================

set(PHYSICS_SERVICE_SOURCES
    physics_registry.F90
    # physics_service.F90
    physics.F90
)

add_library(Physics_Service STATIC ${PHYSICS_SERVICE_SOURCES})
target_include_directories(Physics_Service PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Physics_Service PUBLIC
    Physics_Materials
    Physics_Models
    Solver::Core
)
enable_build_flags(Physics_Service)

# ====================================================================
# 5. Public Interface Alias
# ====================================================================
# 外部公開用の名前（Solver::Physics）を定義
# 実体（Physics_Service）へのエイリアスとして定義するのが一般的です
add_library(Solver::Physics ALIAS Physics_Service)