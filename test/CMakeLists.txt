# Ensure log output directory exists
file(MAKE_DIRECTORY ${PROJECT_SOURCE_DIR}/log)

# ====================================================================
# Function: add_solver_test
# ====================================================================
function(add_solver_test test_name)
    set(target_name "test_${test_name}")

    if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${target_name}.F90")
        message(FATAL_ERROR "Test source file not found: ${target_name}.F90")
    endif()

    add_executable(${target_name} ${target_name}.F90)
    set_target_properties(${target_name} PROPERTIES LINKER_LANGUAGE Fortran)

    # Apply general build flags and third-party libraries by default
    enable_build_flags(${target_name})
    enable_thirdparty(${target_name})

    # Link internal solver libraries passed via ARGN
    if(ARGN)
        target_link_libraries(${target_name} PRIVATE ${ARGN})
    endif()

    # Link test-drive framework
    target_link_libraries(${target_name} PRIVATE test-drive::test-drive)

    # Register to CTest
    add_test(NAME ${target_name} COMMAND $<TARGET_FILE:${target_name}>)

    # Custom run target
    set(log_dir "${PROJECT_SOURCE_DIR}/log")
    set(log_file "${log_dir}/results_${test_name}.md")

    if(ENABLE_MPI AND MPI_Fortran_FOUND)
        add_custom_target(run_${test_name}
            COMMAND ${CMAKE_COMMAND} -E make_directory ${log_dir}
            COMMAND sh -c "${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 1 $<TARGET_FILE:${target_name}> > ${log_file}"
            DEPENDS ${target_name}
            WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
            VERBATIM
        )
    else()
        add_custom_target(run_${test_name}
            COMMAND ${CMAKE_COMMAND} -E make_directory ${log_dir}
            COMMAND sh -c "env OMP_NUM_THREADS=4 $<TARGET_FILE:${target_name}> > ${log_file}"
            DEPENDS ${target_name}
            WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
            VERBATIM
        )
    endif()
endfunction()

# --------------------------------------------------------------------
# Register each test with its specific dependencies
# --------------------------------------------------------------------

# test_mkl requires only default flags and third-party libraries
add_solver_test(mkl)

# test_main requires all solver libraries
add_solver_test(main
    SOLVER::CORE
    SOLVER::LINALG
    SOLVER::PARALLEL
    SOLVER::SOLVER
    SOLVER::SYSTEM
    SOLVER::DOMAIN
    SOLVER::INPUT
    SOLVER::OUTPUT
    SOLVER::CONSTITUTIVE
    SOLVER::GOVERNING
    SOLVER::FTCMS
)