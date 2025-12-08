# CMakeLists.txt for test
# Defines test executables for various modules in the Solver library.
#
if(TEST_NAME STREQUAL "core")
    add_executable(test_core test_core.F90)
    set_target_properties(test_core PROPERTIES LINKER_LANGUAGE Fortran)
    target_link_libraries(test_core PRIVATE
        Solver::Core
    )
    add_test(NAME test_core COMMAND $<TARGET_FILE:test_core>)
elseif(TEST_NAME STREQUAL "input")
    add_executable(test_input test_input.F90)
    set_target_properties(test_input PROPERTIES LINKER_LANGUAGE Fortran)
    target_link_libraries(test_input PRIVATE
        Solver::Core
        Solver::Input
    )
    add_test(NAME test_input COMMAND $<TARGET_FILE:test_input>)
elseif(TEST_NAME STREQUAL "linalg")
    add_executable(test_linalg test_linalg.F90)
    set_target_properties(test_linalg PROPERTIES LINKER_LANGUAGE Fortran)
    target_link_libraries(test_linalg PRIVATE
        Solver::Core
        Solver::Linalg
    )
    add_test(NAME test_linalg COMMAND $<TARGET_FILE:test_linalg>)
elseif(TEST_NAME STREQUAL "physics")
    add_executable(test_physics test_physics.F90)
    enable_build_flags(test_physics)
    enable_thirdparty(test_physics)
    set_target_properties(test_physics PROPERTIES LINKER_LANGUAGE Fortran)
    target_link_libraries(test_physics PRIVATE
        Solver::Core
        Solver::Physics
    )
    add_test(NAME test_physics COMMAND $<TARGET_FILE:test_physics>)
elseif(TEST_NAME STREQUAL "solver")
    add_executable(test_solver test_solver.F90)
    set_target_properties(test_solver PROPERTIES LINKER_LANGUAGE Fortran)
    target_link_libraries(test_solver PRIVATE
        Solver::Core
        Solver::Linalg
        Solver::Solver
    )
    add_test(NAME test_solver COMMAND $<TARGET_FILE:test_solver>)

elseif(TEST_NAME STREQUAL "control")
    add_executable(test_control test_control.F90)
    set_target_properties(test_control PROPERTIES LINKER_LANGUAGE Fortran)
    target_link_libraries(test_control PRIVATE
        Solver::Core
        Solver::Input
        Solver::Linalg
        Solver::Control
    )
    add_test(NAME test_control COMMAND $<TARGET_FILE:test_control>)
elseif(TEST_NAME STREQUAL "domain")
    add_executable(test_domain test_domain.F90)
    set_target_properties(test_domain PROPERTIES LINKER_LANGUAGE Fortran)
    target_link_libraries(test_domain PRIVATE
        Solver::Core
        Solver::Input
        Solver::Linalg
        Solver::Control
        Solver::Conditions
        Solver::Domain
    )
    add_test(NAME test_domain COMMAND $<TARGET_FILE:test_domain>)
elseif(TEST_NAME STREQUAL "field")
    add_executable(test_field test_field.F90)
    set_target_properties(test_field PROPERTIES LINKER_LANGUAGE Fortran)
    target_link_libraries(test_field PRIVATE
        Solver::Core
        Solver::Input
        Solver::Linalg
        Solver::Control
        Solver::Conditions
        Solver::Domain
        Solver::Field
    )
    add_test(NAME test_field COMMAND $<TARGET_FILE:test_field>)
elseif(TEST_NAME STREQUAL "parallel")
    add_executable(test_parallel test_parallel.F90)
    set_target_properties(test_parallel PROPERTIES LINKER_LANGUAGE Fortran)
    target_link_libraries(test_parallel PRIVATE
        Solver::Core
        Solver::Input
        Solver::Parallel
    )
    add_test(NAME test_parallel COMMAND $<TARGET_FILE:test_parallel>)
    
endif()