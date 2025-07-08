module core_core
    use :: core_fortran_utils
    use :: core_error, only:error_message
    use :: core_check_nan, only:has_nan
    use :: core_check_range, only:value_in_range
    use :: core_unique, only:unique
    use :: core_allocate, only:allocate_array, allocate_pointer
    use :: core_deallocate, only:deallocate_array

    use :: core_vtk_vtk, only:type_vtk, type_vtk_cells
    use :: core_vtk_vtk_constants
    implicit none

contains

end module core_core
