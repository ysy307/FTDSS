#include <petsc/finclude/petscdmplex.h>
#include <petsc/finclude/petscviewerhdf5.h>

!> Measure the shapes PETSc wrote, so an XDMF descriptor can be generated.
program vtu_probe
    use, intrinsic :: iso_fortran_env, only: output_unit
    use :: petscdmplex
    implicit none
    PetscErrorCode :: ierr
    PetscViewer :: viewer
    Vec :: v
    IS :: cells
    PetscInt :: n, bs

    call PetscInitialize(ierr)
    call PetscViewerHDF5Open(PETSC_COMM_WORLD, &
        "/workspaces/FTCMS/project/_1Domain-Square1st/Output/Files/Out_005.h5", &
        FILE_MODE_READ, viewer, ierr)

    ! geometry/vertices
    call PetscViewerHDF5PushGroup(viewer, "/geometry", ierr)
    call VecCreate(PETSC_COMM_WORLD, v, ierr)
    call PetscObjectSetName(PetscObjectCast(v), "vertices", ierr)
    call VecLoad(v, viewer, ierr)
    write (output_unit,'(A,I0)') "vertices load ierr=", ierr
    if (ierr == 0) then
        call VecGetSize(v, n, ierr)
        call VecGetBlockSize(v, bs, ierr)
        write (output_unit,'(A,I0,A,I0)') "  vertices: total=", n, "  blocksize=", bs
    end if
    call VecDestroy(v, ierr)
    call PetscViewerHDF5PopGroup(viewer, ierr)

    ! viz/topology/cells
    call PetscViewerHDF5PushGroup(viewer, "/viz/topology", ierr)
    call ISCreate(PETSC_COMM_WORLD, cells, ierr)
    call PetscObjectSetName(PetscObjectCast(cells), "cells", ierr)
    call ISLoad(cells, viewer, ierr)
    write (output_unit,'(A,I0)') "cells load ierr=", ierr
    if (ierr == 0) then
        call ISGetSize(cells, n, ierr)
        write (output_unit,'(A,I0)') "  cells: total entries=", n
    end if
    call ISDestroy(cells, ierr)
    call PetscViewerHDF5PopGroup(viewer, ierr)

    ! one field
    call PetscViewerHDF5PushGroup(viewer, "/fields", ierr)
    call VecCreate(PETSC_COMM_WORLD, v, ierr)
    call PetscObjectSetName(PetscObjectCast(v), "temperature", ierr)
    call VecLoad(v, viewer, ierr)
    write (output_unit,'(A,I0)') "temperature load ierr=", ierr
    if (ierr == 0) then
        call VecGetSize(v, n, ierr)
        write (output_unit,'(A,I0)') "  temperature: total=", n
    end if
    call VecDestroy(v, ierr)
    call PetscViewerHDF5PopGroup(viewer, ierr)

    call PetscViewerDestroy(viewer, ierr)
    call PetscFinalize(ierr)
end program vtu_probe
