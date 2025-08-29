# src/main/hydraulic/CMakeLists.txt

set(APP_HYDRAULIC_SOURCES
    hydraulic_assemble.F90
    hydraulic_interface.F90
    hydraulic_crs.F90
    hydraulic.F90
)
add_library(App_Hydraulic STATIC ${APP_HYDRAULIC_SOURCES})
target_include_directories(App_Hydraulic PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# 依存関係を新しいインターフェースライブラリに更新
target_link_libraries(App_Hydraulic PUBLIC
    Solver::Core
    Solver::InOut
    Solver::Domain
    Solver::Physics
    Solver::Conditions
    Solver::Solver
    Solver::Field
)
enable_build_flags(App_Hydraulic)

add_library(Hydraulic_Interface INTERFACE)
target_link_libraries(Hydraulic_Interface INTERFACE App_Hydraulic)
add_library(Solver::Hydraulic ALIAS Hydraulic_Interface)