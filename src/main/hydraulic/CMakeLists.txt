# # src/main/hydraulic/CMakeLists.txt

# set(MAIN_HYDRAULIC_SOURCES
#     hydraulic_interface.F90
#     hydraulic_base.F90
#     # hydraulic_assemble.F90
#     # hydraulic_crs.F90
#     hydraulic.F90
# )
# add_library(Main_Hydraulic STATIC ${MAIN_HYDRAULIC_SOURCES})
# target_include_directories(Main_Hydraulic PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# # 依存関係を新しいインターフェースライブラリに更新
# target_link_libraries(Main_Hydraulic PUBLIC
#     Solver::Core
#     Solver::Input
#     Solver::Domain
#     Solver::Physics
#     Solver::Conditions
#     Solver::Solver
#     Solver::Field
# )
# enable_build_flags(Main_Hydraulic)

# add_library(Hydraulic_Interface INTERFACE)
# target_link_libraries(Hydraulic_Interface INTERFACE Main_Hydraulic)
# add_library(Solver::Hydraulic ALIAS Hydraulic_Interface)