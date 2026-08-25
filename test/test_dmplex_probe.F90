#include <petsc/finclude/petscdmplex.h>

!> Reads a Gmsh mesh with DMPlex and reports what PETSc sees: the strata, the
!> labels Gmsh physical groups become, and the per-rank split after
!> DMPlexDistribute. Run it against the same .msh the solver uses to check that
!> PETSc reproduces the mesh and the boundary patches the current reader builds.
program dmplex_probe
    use petscdmplex
    implicit none

    PetscErrorCode :: ierr
    DM :: dm, dm_dist
    PetscInt :: dim, c_start, c_end, v_start, v_end, f_start, f_end
    PetscInt :: num_labels, i, num_values, j, num_points
    PetscMPIInt :: rank, num_procs
    character(len=256) :: mesh_file
    character(len=:), allocatable :: label_name
    IS :: value_is, point_is
    PetscInt, pointer :: values(:)
    DMLabel :: label
    PetscBool :: has_file

    PetscCallA(PetscInitialize(ierr))
    PetscCallA(MPI_Comm_rank(PETSC_COMM_WORLD, rank, ierr))
    PetscCallA(MPI_Comm_size(PETSC_COMM_WORLD, num_procs, ierr))

    call PetscOptionsGetString(PETSC_NULL_OPTIONS, PETSC_NULL_CHARACTER, "-mesh", &
                               mesh_file, has_file, ierr)
    if (has_file .eqv. PETSC_FALSE) then
        if (rank == 0) write (*, '(A)') 'usage: dmplex_probe -mesh <file.msh>'
        PetscCallA(PetscFinalize(ierr))
        stop
    end if

    ! interpolate=TRUE builds the faces and edges, which is what boundary labels
    ! are attached to.
    PetscCallA(DMPlexCreateFromFile(PETSC_COMM_WORLD, trim(mesh_file), "mesh", PETSC_TRUE, dm, ierr))

    if (num_procs > 1) then
        PetscCallA(DMPlexDistribute(dm, 0_PETSC_INT_KIND, PETSC_NULL_SF, dm_dist, ierr))
        if (dm_dist /= PETSC_NULL_DM) then
            PetscCallA(DMDestroy(dm, ierr))
            dm = dm_dist
        end if
    end if

    PetscCallA(DMGetDimension(dm, dim, ierr))
    PetscCallA(DMPlexGetHeightStratum(dm, 0_PETSC_INT_KIND, c_start, c_end, ierr))
    PetscCallA(DMPlexGetHeightStratum(dm, 1_PETSC_INT_KIND, f_start, f_end, ierr))
    PetscCallA(DMPlexGetDepthStratum(dm, 0_PETSC_INT_KIND, v_start, v_end, ierr))

    write (*, '(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0)') &
        '[DMPLEX] rank ', rank, '/', num_procs, ': dim=', dim, &
        ' cells=', c_end - c_start, ' faces=', f_end - f_start, ' vertices=', v_end - v_start

    PetscCallA(DMGetNumLabels(dm, num_labels, ierr))
    do i = 0, num_labels - 1
        call get_label_name(dm, i, label_name, ierr)
        PetscCallA(DMGetLabel(dm, trim(label_name), label, ierr))
        PetscCallA(DMLabelGetNumValues(label, num_values, ierr))
        if (num_values <= 0) cycle

        PetscCallA(DMLabelGetValueIS(label, value_is, ierr))
        PetscCallA(ISGetIndices(value_is, values, ierr))
        do j = 1, num_values
            PetscCallA(DMLabelGetStratumIS(label, values(j), point_is, ierr))
            num_points = 0
            if (point_is /= PETSC_NULL_IS) then
                PetscCallA(ISGetSize(point_is, num_points, ierr))
                PetscCallA(ISDestroy(point_is, ierr))
            end if
            write (*, '(A,I0,A,A,A,I0,A,I0)') &
                '[DMPLEX] rank ', rank, ':   label "', trim(label_name), &
                '" value=', values(j), ' points=', num_points
        end do
        PetscCallA(ISRestoreIndices(value_is, values, ierr))
        PetscCallA(ISDestroy(value_is, ierr))
    end do

    ! What PETSc already knows about an element: its polytope and the vertex
    ! order of its closure. Neither has to be reconstructed from cone sizes.
    if (rank == 0 .and. c_end > c_start) then
        call report_cell(dm, c_start, ierr)
    end if

    PetscCallA(DMDestroy(dm, ierr))
    PetscCallA(PetscFinalize(ierr))

contains

    subroutine report_cell(dm, cell, ierr)
        implicit none
        DM :: dm
        PetscInt, intent(in) :: cell
        PetscErrorCode, intent(inout) :: ierr

        DMPolytopeType :: cell_type
        PetscInt, pointer :: closure(:)
        PetscInt :: vs, ve, i, num_vertices, num_closure
        Vec :: coords
        PetscScalar, pointer :: coord_array(:)
        PetscInt :: dim, offset

        call DMPlexGetCellType(dm, cell, cell_type, ierr)
        call DMPlexGetDepthStratum(dm, 0_PETSC_INT_KIND, vs, ve, ierr)
        call DMGetDimension(dm, dim, ierr)
        call DMGetCoordinatesLocal(dm, coords, ierr)
        call VecGetArrayRead(coords, coord_array, ierr)

        nullify (closure)
        call DMPlexGetTransitiveClosure(dm, cell, PETSC_TRUE, num_closure, closure, ierr)

        write (*, '(A,I0,A,I0)') '[DMPLEX] cell ', cell, ' polytope id = ', cell_type%v
        num_vertices = 0
        do i = 1, 2 * num_closure, 2
            if (closure(i) < vs .or. closure(i) >= ve) cycle
            num_vertices = num_vertices + 1
            offset = (closure(i) - vs) * dim
            write (*, '(A,I0,A,I0,A,2(1X,F12.6))') &
                '[DMPLEX]   closure vertex ', num_vertices, ' point=', closure(i), &
                ' xy =', coord_array(offset + 1), coord_array(offset + 2)
        end do

        call DMPlexRestoreTransitiveClosure(dm, cell, PETSC_TRUE, num_closure, closure, ierr)
        call VecRestoreArrayRead(coords, coord_array, ierr)
    end subroutine report_cell

    subroutine get_label_name(dm, index, name, ierr)
        implicit none
        DM :: dm
        PetscInt, intent(in) :: index
        character(len=:), allocatable, intent(out) :: name
        PetscErrorCode, intent(inout) :: ierr

        character(len=256) :: buffer

        call DMGetLabelName(dm, index, buffer, ierr)
        name = trim(buffer)
    end subroutine get_label_name

end program dmplex_probe
