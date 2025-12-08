# CMakeLists.txt for src/physics

# ====================================================================
# IAPWS Property Library
# ====================================================================
set(PHYSICS_CONSTANTS_SOURCES
    physics_constants.F90
)
add_library(Physics_Constants STATIC ${PHYSICS_CONSTANTS_SOURCES})
target_include_directories(Physics_Constants PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
enable_build_flags(Physics_Constants)

# ====================================================================
# IAPWS Property Library
# ====================================================================
# set(PHYSICS_MATERIAL_IAPWS_SOURCES
#     # Constants
#     materials/iapws/iapws_constants.F90
#     # Interface implementations
#     materials/iapws/interfaces/iapws06_iceIh_interface.F90
#     materials/iapws/interfaces/iapws08_interface.F90
#     materials/iapws/interfaces/iapws97_auxiliary_interface.F90
#     materials/iapws/interfaces/iapws97_region1_interface.F90
#     materials/iapws/interfaces/iapws97_region2_interface.F90
#     materials/iapws/interfaces/iapws97_region3_interface.F90
#     materials/iapws/interfaces/iapws97_region4_interface.F90
#     materials/iapws/interfaces/iapws97_region5_interface.F90
#     # Core implementations
#     materials/iapws/core/iapws06_base_Ih.F90
#     materials/iapws/core/iapws08_base.F90
#     materials/iapws/core/iapws97_auxiliary.F90
#     materials/iapws/core/iapws97_base_region1.F90
#     materials/iapws/core/iapws97_base_region2.F90
#     materials/iapws/core/iapws97_base_region3.F90
#     materials/iapws/core/iapws97_boundary_region3.F90
#     materials/iapws/core/iapws97_base_region4.F90
#     materials/iapws/core/iapws97_base_region5.F90
#     # Properties implementations
#     materials/iapws/properties/iapws06_IceIh.F90
#     materials/iapws/properties/iapws97_region1.F90
#     materials/iapws/properties/iapws97_region2.F90
#     materials/iapws/properties/iapws97_region3.F90
#     materials/iapws/properties/iapws97_region5.F90
#     # Wrapper interfaces
#     materials/iapws/interfaces/iapws_wrapper_interface.F90
#     # Wrapper implementations
#     materials/iapws/wrapper/iapws_determine_regions.F90
#     materials/iapws/wrapper/iapws_computations.F90
#     # Top level
#     materials/iapws/iapws.F90
# )
# add_library(Physics_Material_IAPWS STATIC ${PHYSICS_MATERIAL_IAPWS_SOURCES})
# target_include_directories(Physics_Material_IAPWS PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# target_link_libraries(Physics_Material_IAPWS PUBLIC
#     Physics_Constants
# )
# enable_build_flags(Physics_Material_IAPWS)
# enable_thirdparty (Physics_Material_IAPWS)

# --- Density ---
# set(PHYSICS_MATERIAL_DENSITY_SOURCES
#     materials/density/density_interface.F90
#     materials/density/density_base.F90
#     materials/density/density_3phase.F90
# )
# add_library(Physics_Material_Density STATIC ${PHYSICS_MATERIAL_DENSITY_SOURCES})
# target_include_directories(Physics_Material_Density PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# target_link_libraries(Physics_Material_Density PUBLIC 
#     Solver::Core
#     Physics_Material_IAPWS
# )
# enable_build_flags(Physics_Material_Density)

# # --- Specific Heat ---
# set(PHYSICS_MATERIAL_SPH_SOURCES
#     materials/specific_heat/specific_heat_interface.F90
#     materials/specific_heat/specific_heat_base.F90
#     materials/specific_heat/specific_heat_3phase.F90
# )
# add_library(Physics_Material_SpecificHeat STATIC ${PHYSICS_MATERIAL_SPH_SOURCES})
# target_include_directories(Physics_Material_SpecificHeat PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# target_link_libraries(Physics_Material_SpecificHeat PUBLIC 
#     Solver::Core
#     Physics_Material_IAPWS
# )
# enable_build_flags(Physics_Material_SpecificHeat)

# # --- Thermal Conductivity ---
# set(PHYSICS_MATERIAL_THC_SOURCES
#     materials/thermal_conductivity/thermal_conductivity_interface.F90
#     materials/thermal_conductivity/thermal_conductivity_base.F90
#     materials/thermal_conductivity/thermal_conductivity_3phase.F90
# )
# add_library(Physics_Material_ThermalConductivity STATIC ${PHYSICS_MATERIAL_THC_SOURCES})
# target_include_directories(Physics_Material_ThermalConductivity PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# target_link_libraries(Physics_Material_ThermalConductivity PUBLIC 
#     Solver::Core 
# )
# enable_build_flags(Physics_Material_ThermalConductivity)

# # --- Heat Capacity (depends on Density) ---
# set(PHYSICS_MATERIAL_VHC_SOURCES
#     materials/heat_capacity/heat_capacity_interface.F90
#     materials/heat_capacity/heat_capacity_base.F90
#     materials/heat_capacity/heat_capacity_3phase.F90
#     materials/heat_capacity/heat_capacity_3phase_apparent.F90
# )
# add_library(Physics_Material_HeatCapacity STATIC ${PHYSICS_MATERIAL_VHC_SOURCES})
# target_include_directories(Physics_Material_HeatCapacity PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# target_link_libraries(Physics_Material_HeatCapacity PUBLIC
#     Solver::Core
#     Physics_Material_Density
# )
# enable_build_flags(Physics_Material_HeatCapacity)

# --- Materials aggregator ---
# add_library(Physics_Materials INTERFACE)
# target_link_libraries(Physics_Materials INTERFACE
#     Physics_Material_IAPWS
    # Physics_Material_Density
    # Physics_Material_SpecificHeat
    # Physics_Material_ThermalConductivity
    # Physics_Material_HeatCapacity
# )

# ====================================================================
# 2. Constitutive Model Libraries (models/*)
# ====================================================================

# # Hydraulic Conductivity
# set(PHYSICS_MODEL_HCF_SOURCES
#     models/hydraulic_conductivity/hcf_interface.F90
#     models/hydraulic_conductivity/hcf_base.F90
#     models/hydraulic_conductivity/hcf_bc.F90
#     models/hydraulic_conductivity/hcf_vg.F90
#     models/hydraulic_conductivity/hcf_ko.F90
#     models/hydraulic_conductivity/hcf_mvg.F90
#     models/hydraulic_conductivity/hcf_durner.F90
#     models/hydraulic_conductivity/hcf_dvgch.F90
#     models/hydraulic_conductivity/hcf_impedance.F90
#     models/hydraulic_conductivity/hcf_viscosity.F90
# )
# add_library(Physics_Model_HydraulicConductivity STATIC ${PHYSICS_MODEL_HCF_SOURCES})
# target_include_directories(Physics_Model_HydraulicConductivity PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# target_link_libraries(Physics_Model_HydraulicConductivity PUBLIC
#     Solver::Core
#     Solver::Input
# )
# enable_build_flags(Physics_Model_HydraulicConductivity)

# # Water Retention
# set(PHYSICS_MODEL_WRF_SOURCES
#     models/water_retention/wrf_interface.f90
#     models/water_retention/wrf_base.F90
#     models/water_retention/wrf_bc.F90
#     models/water_retention/wrf_vg.F90
#     models/water_retention/wrf_ko.F90
#     models/water_retention/wrf_mvg.F90
#     models/water_retention/wrf_durner.F90
#     models/water_retention/wrf_dvgch.F90
# )
# add_library(Physics_Model_WaterRetention STATIC ${PHYSICS_MODEL_WRF_SOURCES})
# target_include_directories(Physics_Model_WaterRetention PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# target_link_libraries(Physics_Model_WaterRetention PUBLIC
#     Solver::Core
#     Solver::Input
# )
# enable_build_flags(Physics_Model_WaterRetention)

# # Phase Change
# set(PHYSICS_MODEL_GCC_SOURCES
#     models/phase_change/liquid_solid/gcc_interface.F90
#     models/phase_change/liquid_solid/gcc_base.F90
#     models/phase_change/liquid_solid/gcc_non_segregation_m.F90
#     models/phase_change/liquid_solid/gcc_non_segregation_pa.F90
#     models/phase_change/liquid_solid/gcc_segregation_m.F90
#     models/phase_change/liquid_solid/gcc_segregation_pa.F90
# )
# add_library(Physics_Model_PhaseChange STATIC ${PHYSICS_MODEL_GCC_SOURCES})
# target_include_directories(Physics_Model_PhaseChange PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# target_link_libraries(Physics_Model_PhaseChange PUBLIC Solver::Core Solver::Input)
# enable_build_flags(Physics_Model_PhaseChange)

# # Models aggregator
# add_library(Physics_Models INTERFACE)
# target_link_libraries(Physics_Models INTERFACE
#     Physics_Model_HydraulicConductivity
#     Physics_Model_WaterRetention
#     Physics_Model_PhaseChange
# )

# ====================================================================
# 3. Service Layer (トップレイヤー)
# ====================================================================

set(PHYSICS_SERVICE_SOURCES
    # physics_registry.F90
    # physics_service.F90
    physics.F90
)
add_library(Physics_Service STATIC ${PHYSICS_SERVICE_SOURCES})
target_include_directories(Physics_Service PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Physics_Service PUBLIC
    # Physics_Materials
    # Physics_Models
    Solver::Core
    # Solver::Control
    # Solver::Input
)
enable_build_flags(Physics_Service)

# Public Interface
add_library(Physics_Interface INTERFACE)
target_link_libraries(Physics_Interface INTERFACE
    Physics_Service
)
add_library(Solver::Physics ALIAS Physics_Interface)
