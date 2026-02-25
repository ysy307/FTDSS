# ============================================================
# Input Library (single static archive)
# ============================================================
add_library(SOLVER_INPUT STATIC)

# ------------------------------------------------------------
# Sources
# ------------------------------------------------------------
target_sources(SOLVER_INPUT
    PRIVATE
        input_base.F90

        # --- Basic ---
        basic/basic_base.F90
        basic/basic_interface.F90
        basic/basic_simulation_setting.F90
        basic/basic_analysis_controls.F90
        basic/basic_geometry_settings.F90
        basic/basic_materials.F90
        basic/basic_solver_settings.F90

        # --- Conditions ---
        conditions/conditions_base.F90
        conditions/conditions_interface.F90
        conditions/conditions_time_controls.F90
        conditions/conditions_boundary.F90
        conditions/conditions_initial.F90

        # --- Output ---
        output/output_interface.F90
        output/output_fields.F90
        output/output_history.F90
        output/output_standard.F90

        # --- Geometry ---
        geometry/geometry_base.F90
        geometry/geometry_interface.F90

        # --- Translator ---
        input_interface.F90
        translator/translator_interface.F90
        translator/translator_basic.F90
        translator/translator_conditions.F90
        translator/translator_output.F90

        # --- Entry ---
        input.F90
)

# ------------------------------------------------------------
# Fortran module output directory
# ------------------------------------------------------------
target_include_directories(SOLVER_INPUT
    PUBLIC
        ${CMAKE_CURRENT_BINARY_DIR}
)

# ------------------------------------------------------------
# Dependencies
# ------------------------------------------------------------
target_link_libraries(SOLVER_INPUT
    PUBLIC
        SOLVER::CORE
        # Solver::Physics
)

# ------------------------------------------------------------
# Project-specific flags (kept as-is)
# ------------------------------------------------------------
enable_build_flags(SOLVER_INPUT)
enable_thirdparty(SOLVER_INPUT)

# ------------------------------------------------------------
# Namespaced alias
# ------------------------------------------------------------
add_library(SOLVER::INPUT ALIAS SOLVER_INPUT)