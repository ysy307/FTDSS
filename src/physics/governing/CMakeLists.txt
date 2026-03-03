# ============================================================
# Governing Equation Library
# ============================================================
add_library(SOLVER_GOVERNING STATIC)

# ------------------------------------------------------------
# Sources
# ------------------------------------------------------------
target_sources(SOLVER_GOVERNING PRIVATE
    # ====================================================
    # 1. Boundary Conditions
    # ====================================================
    condition/boundary/boundary_interface.F90
    condition/boundary/boundary_base.F90
    condition/boundary/boundary_manager.F90
    condition/boundary/boundary.F90
    # ====================================================
    # 2. Initial Conditions
    # ====================================================
    condition/initial/initial_interface.F90
    condition/initial/initial_uniform.F90
    condition/initial/initial_manager.F90
    condition/initial/initial.F90
    # ====================================================
    # 3. Common Base
    # ====================================================
    governing_base.F90
    # ====================================================
    # 4. Thermal
    # ====================================================
    thermal/thermal_interface.F90
    thermal/thermal_base.F90
    thermal/thermal_matrix.F90
    thermal/thermal_coefficients.F90
    # ====================================================
    # 5. Hydraulic
    # ====================================================
    hydraulic/hydraulic_interface.F90
    hydraulic/hydraulic_base.F90
    hydraulic/hydraulic_matrix.F90
    hydraulic/hydraulic_coefficients.F90
    # ====================================================
    # 6. Atmosphere
    # ====================================================
    atmosphere/atmosphere.F90
    # ====================================================
    # \
    # ====================================================
    governing.F90


)

# ------------------------------------------------------------
# Include directory
# ------------------------------------------------------------
target_include_directories(SOLVER_GOVERNING PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)

# ------------------------------------------------------------
# Dependencies
# ------------------------------------------------------------
target_link_libraries(SOLVER_GOVERNING PUBLIC
    SOLVER::CORE
    SOLVER::DOMAIN
    SOLVER::SYSTEM
    SOLVER::CONTROL
    SOLVER::INPUT
    SOLVER::LINALG
    SOLVER::CONSTITUTIVE
)

enable_build_flags(SOLVER_GOVERNING)
enable_thirdparty(SOLVER_GOVERNING)

# ------------------------------------------------------------
# Namespaced alias
# ------------------------------------------------------------
add_library(SOLVER::GOVERNING ALIAS SOLVER_GOVERNING)
