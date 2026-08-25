#include <petsc/finclude/petscdmplex.h>
#include <petsc/finclude/petscviewerhdf5.h>

!> Probe: which groups did PETSc actually write into the HDF5 output?
program petscfe_probe
    use, intrinsic :: iso_fortran_env, only: output_unit
    use :: petscdmplex
    implicit none

    PetscErrorCode :: ierr
    PetscViewer :: viewer
    PetscBool :: has
    character(len=64) :: names(8)
    integer :: i

    call PetscInitialize(ierr)
    names(1) = "/geometry"
    names(2) = "/topology"
    names(3) = "/viz"
    names(4) = "/viz/geometry"
    names(5) = "/viz/topology"
    names(6) = "/vertex_fields"
    names(7) = "/cell_fields"
    names(8) = "/fields"

    call PetscViewerHDF5Open(PETSC_COMM_WORLD, &
        "/workspaces/FTCMS/project/_1Domain-Square1st/Output/Files/Out_005.h5", &
        FILE_MODE_READ, viewer, ierr)
    write (output_unit, '(A,I0)') "open ierr = ", ierr
    if (ierr /= 0) goto 100

    do i = 1, 8
        has = PETSC_FALSE
        call PetscViewerHDF5HasGroup(viewer, trim(names(i)), has, ierr)
        write (output_unit, '(A,A20,A,L1,A,I0)') "  ", trim(names(i)), " present=", has, "  ierr=", ierr
    end do

    call PetscViewerDestroy(viewer, ierr)
100 continue
    call PetscFinalize(ierr)
end program petscfe_probe
