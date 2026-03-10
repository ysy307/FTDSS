module core_interop_vtk_writer_wrapper
    !> @brief ISO_C_BINDING interface module for the legacy VTK writer C API.
    !>
    !> This module mirrors the structure of @c core_interop_vtk_wrapper (reader
    !> side) and serves as a future extension point for a C++ legacy-format
    !> (.vtk ASCII/binary) writer.  All C function bindings for the
    !> VTK XML writer are provided in @c core_interop_vtu_writer_wrapper.
    !>
    !> Reserved for `c_vtk_writer_*` C functions once a legacy VTK
    !> format writer is added to @c c_wrapper.cpp.
    use, intrinsic :: iso_c_binding
    use :: core_interop_vtk_writer_base, only: abst_vtk_writer
    implicit none
    private

    public :: abst_vtk_writer

end module core_interop_vtk_writer_wrapper
