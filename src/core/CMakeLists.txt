# ============================================================
# Core Library
# ============================================================

add_library(SOLVER_CORE STATIC)

# ============================================================
# 1. Foundational Sources
# ============================================================

# ------------------------------------------------------------
# Constants
# ------------------------------------------------------------
set(CORE_CONSTANTS_FYPP
    constants/constans_ids.fypp
    constants/constans_values.fypp
    constants/constans_error.fypp
)
fypp_F90("" "${CORE_CONSTANTS_FYPP}" CORE_CONSTANTS_GENERATED)

target_sources(SOLVER_CORE PRIVATE
    constants/constants_base.F90
    constants/constants.F90
    ${CORE_CONSTANTS_GENERATED}
)

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------
set(CORE_VALIDATION_FYPP
    validation/check_length.fypp
    validation/check_nan.fypp
    validation/check_range.fypp
)
fypp_F90("" "${CORE_VALIDATION_FYPP}" CORE_VALIDATION_GENERATED)

target_sources(SOLVER_CORE PRIVATE
    validation/error.F90
    validation/validation.F90
    ${CORE_VALIDATION_GENERATED}
)

# ------------------------------------------------------------
# Memory
# ------------------------------------------------------------
set(CORE_MEMORY_FYPP
    memory/allocate.fypp
    memory/deallocate.fypp
)
fypp_F90("-D MAX_RANK=3" "${CORE_MEMORY_FYPP}" CORE_MEMORY_GENERATED)

target_sources(SOLVER_CORE PRIVATE
    memory/memory.F90
    ${CORE_MEMORY_GENERATED}
)

# ------------------------------------------------------------
# Utils
# ------------------------------------------------------------
set(CORE_UTILS_FYPP
    utils/unique.fypp
    utils/findings.fypp
)
fypp_F90("" "${CORE_UTILS_FYPP}" CORE_UTILS_GENERATED)

target_sources(SOLVER_CORE PRIVATE
    utils/string_utils.F90
    utils/system_env.F90
    utils/utils.F90
    ${CORE_UTILS_GENERATED}
)

# ------------------------------------------------------------
# Types
# ------------------------------------------------------------
set(CORE_TYPES_FYPP
    types/variable.fypp
)
fypp_F90("" "${CORE_TYPES_FYPP}" CORE_TYPES_GENERATED)

target_sources(SOLVER_CORE PRIVATE
    types/types.F90
    types/coordinate_array.F90
    types/coordinate.F90
    types/pointer.F90
    types/graph.F90
    types/datetime.F90
    types/solar.F90
    types/physics/state.F90
    types/physics/physics_info.F90
    types/physics/meteorology.F90
    types/config/config_base.F90
    types/config/conditions/config_boundary.F90
    types/config/conditions/config_initial.F90
    types/config/conditions/config_conditions.F90
    types/config/physics/config_physics_base.F90
    types/config/physics/config_swcc.F90
    types/config/physics/config_gcc.F90
    types/config/physics/config_physics.F90
    types/config/control/config_acceleration.F90
    types/config/control/config_output_manager.F90
    types/config/control/config_control.F90
    types/config/control/config_iteration.F90
    types/config/control/config_time.F90
    types/config/control/config_parallel.F90
    types/config/control/config_control_manager.F90
    types/math/gauss.F90
    types/matrix/matrix_interface.F90
    types/matrix/matrix_dense.F90
    types/matrix/matrix_coo.F90
    types/matrix/matrix_csr.F90
    types/matrix/matrix_bsr.F90
    types/matrix/matrix_dia.F90
    types/matrix/matrix_factory.F90
    types/matrix/matrix.F90
    types/vector/vector_interface.F90
    types/vector/vector_dp.F90
    types/vector/vector_int.F90
    types/vector/vector.F90
    ${CORE_TYPES_GENERATED}
)

# ============================================================
# 2. Interop (C / C++ / Fortran wrappers)
# ============================================================

target_sources(SOLVER_CORE PRIVATE

    # --- Utils ---
    interop/interop_utils.F90

    # --- Memory Profiling ---
    interop/memory_profiling/memory_stats.c
    interop/memory_profiling/memory_stats.F90
    interop/memory_profiling/memory_stats_wrapper.F90

    # --- Signal Handling ---
    interop/signal_handling/signal_flag.c
    interop/signal_handling/signal_flag.F90
    interop/signal_handling/signal_flag_wrapper.F90

    # --- System Info ---
    interop/system_info/system_info.c
    interop/system_info/system_info.F90
    interop/system_info/system_info_wrapper.F90

    # --- VTK (C++) ---
    interop/vtk/reader_vtk.cpp
    interop/vtk/reader_vtu.cpp
    interop/vtk/c_wrapper.cpp

    # --- VTK (Fortran) ---
    interop/vtk/vtk_wrapper.F90
    interop/vtk/vtu_wrapper.F90
    interop/vtk/vtk_constants.F90
    interop/vtk/vtk_initialize.F90
    interop/vtk/vtu_initialize.F90
    interop/vtk/vtk_base.F90
    interop/vtk/vtk.F90
    interop/interop.F90
)

# ============================================================
# 3. Core Entry
# ============================================================

target_sources(SOLVER_CORE PRIVATE
    core.F90
)

# ============================================================
# 4. Target Properties
# ============================================================

# 生成モジュール用
target_include_directories(SOLVER_CORE
    PUBLIC
        ${CMAKE_CURRENT_BINARY_DIR}
)

# C++ standard
set_target_properties(SOLVER_CORE PROPERTIES
    CXX_STANDARD 17
)

# ============================================================
# 5. External Libraries
# ============================================================

target_link_libraries(SOLVER_CORE
    PUBLIC
        VTK::IOXML
        VTK::CommonCore
        VTK::CommonDataModel
        VTK::IOLegacy
)

# ============================================================
# 6. Project-specific flags (existing functions kept)
# ============================================================

enable_build_flags(SOLVER_CORE)
enable_thirdparty(SOLVER_CORE)

# ============================================================
# 7. Alias
# ============================================================

add_library(SOLVER::CORE ALIAS SOLVER_CORE)