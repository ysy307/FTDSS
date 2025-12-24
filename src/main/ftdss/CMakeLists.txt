# src/main/ftdss/CMakeLists.txt

set(MAIN_FTDSS_SOURCES
    ftdss.F90
)
add_library(MAIN_FTDSS STATIC ${MAIN_FTDSS_SOURCES})
target_include_directories(MAIN_FTDSS PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# FTDSSは、ThermalとHydraulicの両方の機能に依存する
target_link_libraries(MAIN_FTDSS PUBLIC
    Solver::Thermal
    # Solver::Hydraulic
    # FTDSSが直接利用する他のモジュールもここに追加
    Solver::Control 
    Solver::Input
    Solver::Conditions
    # Solver::Output
)
enable_build_flags(MAIN_FTDSS)

add_library(FTDSS_Interface INTERFACE)
target_link_libraries(FTDSS_Interface INTERFACE MAIN_FTDSS)

# ★修正箇所: 自動生成される依存名(Solver::Ftdss)に合わせたエイリアスを追加
add_library(Solver::Ftdss ALIAS FTDSS_Interface)
# 念のため既存の大文字エイリアスも残す場合
add_library(Solver::FTDSS ALIAS FTDSS_Interface)