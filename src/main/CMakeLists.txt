# src/main/CMakeLists.txt

set(Main_Base_SOURCES
    base.F90
)
add_library(Main_Base STATIC ${Main_Base_SOURCES})
target_include_directories(Main_Base PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Main_Base PUBLIC
    Solver::Core
    Solver::Domain
)

# ====================================================================
# 1. Thermal Library
# ====================================================================
set(Main_THERMAL_SOURCES
    thermal/thermal_interface.F90
    thermal/thermal_base.F90
    thermal/thermal_matrix.F90
    thermal/thermal_coefficients.F90
    thermal/thermal.F90
)

add_library(Main_Thermal STATIC ${Main_THERMAL_SOURCES})
target_include_directories(Main_Thermal PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Main_Thermal PUBLIC
    Main_Base
    Solver::Core
    Solver::Control
    Solver::Input
    Solver::Physics
    )
add_library(Solver::Thermal ALIAS Main_Thermal)
    
# ====================================================================
# 2. Hydraulic Library
# ====================================================================
set(MAIN_HYDRAULIC_SOURCES
    hydraulic/hydraulic_interface.F90
    hydraulic/hydraulic_base.F90
    hydraulic/hydraulic_matrix.F90
    hydraulic/hydraulic_coefficients.F90
    hydraulic/hydraulic.F90
)
    
add_library(Main_Hydraulic STATIC ${MAIN_HYDRAULIC_SOURCES})
target_include_directories(Main_Hydraulic PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Main_Hydraulic PUBLIC
    Solver::Core
    Solver::Control
    Solver::Input
    Solver::Physics
)
add_library(Solver::Hydraulic ALIAS Main_Hydraulic)

# ====================================================================
# 3. FTDSS Library (High-level Logic)
# ====================================================================
set(MAIN_FTDSS_SOURCES
    ftdss/ftdss_interface.F90
    ftdss/ftdss_base.F90
    ftdss/ftdss_boundary.F90
    ftdss/ftdss_assemble.F90
    ftdss/ftdss_compute.F90
    ftdss/ftdss.F90
)

add_library(MAIN_FTDSS_LIB STATIC ${MAIN_FTDSS_SOURCES})
target_include_directories(MAIN_FTDSS_LIB PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(MAIN_FTDSS_LIB PUBLIC
    Solver::Thermal
    Solver::Hydraulic
    Solver::Control 
    Solver::Input
    Solver::Conditions
    Solver::Output
)

# ルートCMakeでのリンク名解決用エイリアス
add_library(Solver::Ftdss ALIAS MAIN_FTDSS_LIB)
add_library(Solver::FTDSS ALIAS MAIN_FTDSS_LIB)