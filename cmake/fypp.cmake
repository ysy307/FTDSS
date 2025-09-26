# Generic preprocess function
function(preprocess preproc preprocopts srcext trgext srcfiles trgfiles)
  set(_trgfiles)
  foreach(srcfile IN LISTS srcfiles)
    string(REGEX REPLACE "\\.${srcext}$" ".${trgext}" trgfile ${srcfile})
    add_custom_command(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${trgfile}
      COMMAND ${preproc} ${preprocopts} ${CMAKE_CURRENT_SOURCE_DIR}/${srcfile} ${CMAKE_CURRENT_BINARY_DIR}/${trgfile}
      MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${srcfile})
    list(APPEND _trgfiles ${CMAKE_CURRENT_BINARY_DIR}/${trgfile})
  endforeach()
  set(${trgfiles} ${_trgfiles} PARENT_SCOPE)
endfunction()

# .fypp -> .F90
function(fypp_F90 fyppopts fyppfiles F90files)
  if(NOT FYPP)
    find_program(FYPP fypp REQUIRED)
    set(FYPP ${FYPP} CACHE PATH "Path to fypp preprocessor" FORCE)
  endif()
  preprocess("${FYPP}" "${fyppopts}" "fypp" "F90" "${fyppfiles}" _F90files)
  set(${F90files} ${_F90files} PARENT_SCOPE)
endfunction()
