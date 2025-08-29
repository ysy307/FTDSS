# src/main/ftdss/CMakeLists.txt

set(APP_FTDSS_SOURCES
    ftdss.F90
)
add_library(App_FTDSS STATIC ${APP_FTDSS_SOURCES})
target_include_directories(App_FTDSS PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# FTDSSは、ThermalとHydraulicの両方の機能に依存する
target_link_libraries(App_FTDSS PUBLIC
    Solver::Thermal
    Solver::Hydraulic
    # FTDSSが直接利用する他のモジュールもここに追加
    Solver::Control 
    Solver::InOut
)
enable_build_flags(App_FTDSS)

add_library(FTDSS_Interface INTERFACE)
target_link_libraries(FTDSS_Interface INTERFACE App_FTDSS)
add_library(Solver::FTDSS ALIAS FTDSS_Interface)