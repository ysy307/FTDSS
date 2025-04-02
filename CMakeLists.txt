# # CMake Minimum Version
# cmake_minimum_required(VERSION 3.30)

# # Project Name and Language
# project(YSY_FC LANGUAGES Fortran)
# enable_language(Fortran)

# # Options
# option(ENABLE_OPTIMIZE "Enable optimization flags" ON)
# option(ENABLE_OPENMP   "Enable OpenMP support"     ON)
# option(ENABLE_MKL      "Enable MKL support"        ON)
# option(ENABLE_MPI      "Enable MPI support"        OFF)
# option(ENABLE_DEBUG    "Enable debug flags"        OFF)

# # Select the application to build
# set(BUILD_APP "" CACHE STRING "Specify the application to build: ysy_fc or test")

# # Output Directories
# set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/.mod)
# set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin)
# set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib)
# set(CMAKE_INCLUDE_PATH             ${PROJECT_SOURCE_DIR}/include)

# ### Compiler Configuration
# if(NOT DEFINED CMAKE_Fortran_COMPILER)
#     set(CMAKE_Fortran_COMPILER ifx)
# endif()

# ### Source Files
# add_subdirectory(src/Allocate)
# add_subdirectory(src/Calculate)
# add_subdirectory(src/Conditions)
# add_subdirectory(src/Inout)
# add_subdirectory(src/Main)
# add_subdirectory(src/Matrix)
# add_subdirectory(src/Solver)
# add_subdirectory(src/Type)

# ### Fortran compilation flags
# set(COMPILE_Fortran_FLAGS "-fpp -traceback")
# set(CMAKE_VERBOSE_MAKEFILE 1)


# if(ENABLE_DEBUG)
#     set(COMPILE_Fortran_FLAGS "${COMPILE_Fortran_FLAGS} -O0 -g -check all")
# elseif(ENABLE_OPTIMIZE)
#     set(COMPILE_Fortran_FLAGS "${COMPILE_Fortran_FLAGS} -O3 -flto -xCORE-AVX2")
# endif()
# set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp -traceback -O0 -g -check all")

# add_compile_options("${COMPILE_Fortran_FLAGS}")

# # Libraries
# file(GLOB_RECURSE MY_ALL_LIBRARIES ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/*.a)
# set(LINK_LIBRARIES ${MY_ALL_LIBRARIES})

# ### OpenMP Configuration
# if(ENABLE_OPENMP)
#     find_package(OpenMP REQUIRED)
#     list(APPEND LINK_LIBRARIES OpenMP::OpenMP_Fortran)
# endif()

# ### MKL Configuration
# if(ENABLE_MKL)
#     set(MKL_LINK "static")
#     find_package(MKL CONFIG REQUIRED PATHS $ENV{MKLROOT})
#     list(APPEND LINK_LIBRARIES MKL::MKL)
# endif()

# ### MPI Configuration
# if(ENABLE_MPI)
#     find_package(MPI REQUIRED)
#     list(APPEND LINK_LIBRARIES MPI::MPI_Fortran)
# endif()

# # Include Paths
# set(My_INCLUDE_PATH
#     ${CMAKE_INCLUDE_PATH}/Json-Fortran
# )

# # External Libraries
# set(fortran_stdlib_DIR "/workspaces/FTDSS/EXTERNAL/fortran_stdlib/lib/cmake/fortran_stdlib")
# find_package(fortran_stdlib REQUIRED)
# list(APPEND LINK_LIBRARIES fortran_stdlib::fortran_stdlib)
# message(STATUS "fortran_stdlib_DIR: ${fortran_stdlib_DIR}")
# message(STATUS "fortran_stdlib_INCLUDE_DIRS: ${fortran_stdlib_INCLUDE_DIRS}")

# # Debug Information
# message(STATUS "Fortran Compiler ID: ${CMAKE_Fortran_COMPILER_ID}")
# message(STATUS "Fortran Compiler IDd: ${My_INCLUDE_PATH}")

# set(MAIN_FILE "${PROJECT_SOURCE_DIR}/app/ysy_fc.F90")
# set(TEST_FILE "${PROJECT_SOURCE_DIR}/test/test_paradiso.F90")

# # add_definitions
# set(FORTRAN_STDLIB_INCLUDE_DIR "/workspaces/FTDSS/EXTERNAL/fortran_stdlib/include/fortran_stdlib/IntelLLVM-2025.0.4")
# include_directories(${My_INCLUDE_PATH})
# # include_directories() 
# # Build Targets
# if(BUILD_APP STREQUAL "ysy_fc")
#     # ysy_fc Target
#     add_executable(ysy_fc ${MAIN_FILE})
#     target_compile_options    (ysy_fc PRIVATE ${COMPILE_Fortran_FLAGS})
#     target_link_libraries     (ysy_fc PRIVATE ${LINK_LIBRARIES})
#     target_include_directories(ysy_fc PUBLIC  ${My_INCLUDE_PATH})

#     # Custom Run Target
#     add_custom_target(run_ysy_fc
#         COMMAND OMP_NUM_THREADS=4 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/ysy_fc
#         DEPENDS ysy_fc
#         WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
#     )

# elseif(BUILD_APP STREQUAL "test")
#     # Test Target
#     add_executable(test ${TEST_FILE})
#     target_link_libraries(test PUBLIC ${LINK_LIBRARIES} Allocate Calculate Conditions Inout Main Matrix Solver Type)
#     # target_include_directories(test PUBLIC ${My_INCLUDE_PATH})
#     target_include_directories(test PUBLIC ${My_INCLUDE_PATH} ${FORTRAN_STDLIB_INCLUDE_DIR})
#     # set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")

#     # Custom Run Target
#     add_custom_target(run_test
#         COMMAND OMP_NUM_THREADS=4 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test
#         DEPENDS test
#         WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
#     )
# endif()

# # Final Messages
# if(BUILD_APP STREQUAL "")
#     message(WARNING "No application selected to build. Set BUILD_APP to 'ysy_fc' or 'test'.")
# endif()
# CMake Minimum Version
cmake_minimum_required(VERSION 3.30)

# Project Name and Language
project(YSY_FC LANGUAGES Fortran)
enable_language(Fortran)
# Options
option(ENABLE_OPTIMIZE "Enable optimization flags" ON)
option(ENABLE_OPENMP   "Enable OpenMP support"     ON)
option(ENABLE_MKL      "Enable MKL support"        ON)
option(ENABLE_MPI      "Enable MPI support"        OFF)
option(ENABLE_DEBUG    "Enable debug flags"        OFF)

# Select the application to build
set(BUILD_APP "" CACHE STRING "Specify the application to build: ysy_fc or test")

# Output Directories
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/.mod)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib)
set(CMAKE_INCLUDE_PATH             ${PROJECT_SOURCE_DIR}/include)

### Compiler Configuration
if(NOT DEFINED CMAKE_Fortran_COMPILER)
    set(CMAKE_Fortran_COMPILER ifx)
endif()

# Source Files
file(GLOB_RECURSE ALL_SRC_FILES ${PROJECT_SOURCE_DIR}/src/**/**.F90)

# Libraries
file(GLOB_RECURSE MY_ALL_LIBRARIES ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/*.a)
set(LINK_LIBRARIES ${MY_ALL_LIBRARIES})

if(ENABLE_OPENMP)
    find_package(OpenMP REQUIRED)
    list(APPEND LINK_LIBRARIES OpenMP::OpenMP_Fortran)
endif()

if(ENABLE_MPI)
    find_package(MPI REQUIRED)
    list(APPEND LINK_LIBRARIES MPI::MPI_Fortran)
endif()

# Include Paths
set(My_INCLUDE_PATH
    ${CMAKE_INCLUDE_PATH}/Json-Fortran
)

# External Libraries
set(fortran_stdlib_DIR "/workspaces/FTDSS/EXTERNAL/fortran_stdlib/lib/cmake/fortran_stdlib")
find_package(fortran_stdlib REQUIRED)
list(APPEND LINK_LIBRARIES fortran_stdlib::fortran_stdlib)

# Debug Information
message(STATUS "Fortran Compiler ID: ${CMAKE_Fortran_COMPILER_ID}")

set(MAIN_FILE "${PROJECT_SOURCE_DIR}/app/ysy_fc.F90")
set(TEST_FILE "${PROJECT_SOURCE_DIR}/test/test_paradiso.F90")

# Build Targets
if(BUILD_APP STREQUAL "ysy_fc")
    # ysy_fc Target
    add_executable(ysy_fc ${MAIN_FILE} ${ALL_SRC_FILES})
    target_link_libraries(ysy_fc PRIVATE ${LINK_LIBRARIES})
    target_include_directories(ysy_fc PUBLIC ${My_INCLUDE_PATH})

    # Compilation Flags
    target_compile_options(test PUBLIC -fpp -traceback)
    target_link_options   (test PUBLIC -fpp -traceback)

    if(ENABLE_MKL)
        set(MKL_LINK "static")
        find_package(MKL CONFIG REQUIRED PATHS $ENV{MKLROOT})
        target_compile_options    (test PUBLIC $<TARGET_PROPERTY:MKL::MKL,INTERFACE_COMPILE_OPTIONS>)
        target_include_directories(test PUBLIC $<TARGET_PROPERTY:MKL::MKL,INTERFACE_INCLUDE_DIRECTORIES>)
        target_link_libraries     (test PUBLIC $<LINK_ONLY:MKL::MKL>)
        add_definitions(-D_MKL)
    endif()

    if(ENABLE_DEBUG)
        target_compile_options(test PUBLIC -O0 -g -check all)
        target_link_options   (test PUBLIC -O0 -g -check all)
    elseif(ENABLE_OPTIMIZE)
        target_compile_options(test PUBLIC -O3 -flto -xCORE-AVX2)
        target_link_options   (test PUBLIC -O3 -flto -xCORE-AVX2)
    endif()


    # Custom Run Target
    add_custom_target(run_ysy_fc
        COMMAND OMP_NUM_THREADS=4 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/ysy_fc
        DEPENDS ysy_fc
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    )

elseif(BUILD_APP STREQUAL "test")
    add_executable(test ${TEST_FILE} ${ALL_SRC_FILES})
    target_link_libraries(test PUBLIC ${LINK_LIBRARIES} )
    target_include_directories(test PUBLIC ${My_INCLUDE_PATH})
    # Compilation Flags
    target_compile_options(test PUBLIC -fpp -traceback)
    target_link_options   (test PUBLIC -fpp -traceback)

    if(ENABLE_MKL)
        set(MKL_LINK "static")
        find_package(MKL CONFIG REQUIRED PATHS $ENV{MKLROOT})
        target_compile_options    (test PUBLIC $<TARGET_PROPERTY:MKL::MKL,INTERFACE_COMPILE_OPTIONS>)
        target_include_directories(test PUBLIC $<TARGET_PROPERTY:MKL::MKL,INTERFACE_INCLUDE_DIRECTORIES>)
        target_link_libraries     (test PUBLIC $<LINK_ONLY:MKL::MKL>)
        add_definitions(-D_MKL)
    endif()

    if(ENABLE_DEBUG)
        target_compile_options(test PUBLIC -O0 -g -check all)
        target_link_options   (test PUBLIC -O0 -g -check all)
    elseif(ENABLE_OPTIMIZE)
        target_compile_options(test PUBLIC -O3 -flto -xCORE-AVX2)
        target_link_options   (test PUBLIC -O3 -flto -xCORE-AVX2)
    endif()


    # Custom Run Target
    add_custom_target(run_test
        COMMAND OMP_NUM_THREADS=4 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test
        DEPENDS test
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    )
endif()

# Final Messages
if(BUILD_APP STREQUAL "")
    message(WARNING "No application selected to build. Set BUILD_APP to 'ysy_fc' or 'test'.")
endif()