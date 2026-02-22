# ===== fypp include configuration =====

set(FYPP_INCLUDE_DIR
    ${PROJECT_SOURCE_DIR}/include
    CACHE PATH "fypp include directory")

# fypp から include されうる全ファイルを列挙（GLOB禁止）
set(FYPP_INCLUDE_FILES
    ${FYPP_INCLUDE_DIR}/common.fypp
    ${FYPP_INCLUDE_DIR}/constants.fypp
)

set(FYPP_DEFAULT_OPTS
    --line-length=132
)

# =====================================


# Generic preprocess function
function(preprocess preproc preprocopts srcext trgext srcfiles trgfiles)
    set(_trgfiles)
    foreach(srcfile IN LISTS srcfiles)
        string(REGEX REPLACE "\\.${srcext}$" ".${trgext}" trgfile ${srcfile})
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${trgfile}
            COMMAND ${preproc}
                    ${preprocopts}
                    -I${FYPP_INCLUDE_DIR}
                    ${CMAKE_CURRENT_SOURCE_DIR}/${srcfile}
                    ${CMAKE_CURRENT_BINARY_DIR}/${trgfile}
            MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${srcfile}
            DEPENDS ${FYPP_INCLUDE_FILES}
        )
        list(APPEND _trgfiles ${CMAKE_CURRENT_BINARY_DIR}/${trgfile})
    endforeach()
    set(${trgfiles} ${_trgfiles} PARENT_SCOPE)
endfunction()


# .fypp -> .F90
function(fypp_F90 preprocopts fyppfiles F90files)
    if(NOT FYPP)
        find_program(FYPP fypp REQUIRED)
        set(FYPP ${FYPP} CACHE PATH "Path to fypp preprocessor" FORCE)
    endif()

    set(_fypp_opts
        ${FYPP_DEFAULT_OPTS}
        ${preprocopts}
    )

    preprocess(
        "${FYPP}"
        "${_fypp_opts}"
        "fypp"
        "F90"
        "${fyppfiles}"
        _F90files
    )
    set(${F90files} ${_F90files} PARENT_SCOPE)
endfunction()
