program manual_fem
#include "petsc/finclude/petsc.h"
    use :: petsc
    implicit none

    DM :: dm
    Mat :: A
    Vec :: x, b
    KSP :: ksp
    PetscSection :: section

    PetscErrorCode :: ierr
    PetscMPIInt :: rank
    character(len=256) :: filename
    PetscBool :: flg
    PetscInt :: cell, cStart, cEnd
    PetscInt :: n_nodes
    PetscInt, pointer :: global_indices(:)
    PetscScalar :: ke(9)
    PetscScalar :: fe(3)
    PetscInt :: i
    PetscInt :: ids(4)
    IS :: is_parts(4), boundary_faces_is, boundary_nodes_is

    call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
    call MPI_Comm_rank(PETSC_COMM_WORLD, rank, ierr)
    call PetscOptionsGetString(PETSC_NULL_OBJECT, PETSC_NULL_CHARACTER, "-f", filename, flg, ierr)
    call DMPlexCreateFromFile(PETSC_COMM_WORLD, filename, PETSC_TRUE, dm, ierr)
    call DMSetFromOptions(dm, ierr)
    call DMGetLocalSection(dm, section, ierr)

    call DMCreateMatrix(dm, A, ierr)
    call DMCreateGlobalVector(dm, x, ierr)
    call VecDuplicate(x, b, ierr)
    call VecSet(b, 0.0_8, ierr)

    if (rank == 0) print *, "Assembling matrix and vector manually..."

    ! Set element stiffness matrix (ke) and load vector (fe) in row-major order
    ke(1) = 0.5_8; ke(2) = -0.5_8; ke(3) = 0.0_8
    ke(4) = -0.5_8; ke(5) = 1.0_8; ke(6) = -0.5_8
    ke(7) = 0.0_8; ke(8) = -0.5_8; ke(9) = 0.5_8
    fe = 1.0_8 / 6.0_8

    ! Element Loop
    call DMPlexGetHeightStratum(dm, 0, cStart, cEnd, ierr)
    do cell = cStart, cEnd - 1
        ! --- FIX 1: Use correct NULL types ---
        call DMPlexGetClosureIndices(dm, section, section, cell, PETSC_TRUE, n_nodes, global_indices, PETSC_NULL_OBJECT, PETSC_NULL_OBJECT, ierr)
        call MatSetValues(A, n_nodes, global_indices, n_nodes, global_indices, ke, ADD_VALUES, ierr)
        call VecSetValues(b, n_nodes, global_indices, fe, ADD_VALUES, ierr)
        call DMPlexRestoreClosureIndices(dm, section, section, cell, PETSC_TRUE, n_nodes, global_indices, PETSC_NULL_OBJECT, PETSC_NULL_OBJECT, ierr)
    end do

    call MatAssemblyBegin(A, MAT_FINAL_ASSEMBLY, ierr)
    call MatAssemblyEnd(A, MAT_FINAL_ASSEMBLY, ierr)
    call VecAssemblyBegin(b, ierr)
    call VecAssemblyEnd(b, ierr)

    ! --- FIX 2: Correct Boundary Condition Logic ---
    if (rank == 0) print *, "Applying boundary conditions..."
    ! (a) Get an IS of all boundary faces
    ids = [1, 2, 3, 4]
    do i = 1, 4
        call DMGetStratumIS(dm, 'Face Sets', ids(i), is_parts(i), ierr)
    end do
    call ISConcatenate(PETSC_COMM_WORLD, 4, is_parts, boundary_faces_is, ierr)
    do i = 1, 4
        call ISDestroy(is_parts(i), ierr)
    end do

    ! (b) Get the IS of all unique nodes on those faces
    call DMPlexGetClosureIS(dm, section, boundary_faces_is, PETSC_TRUE, boundary_nodes_is, PETSC_NULL_IS, ierr)
    call ISDestroy(boundary_faces_is, ierr)

    ! (c) Apply u=0 using the correct IS of nodes
    call MatZeroRowsIS(A, boundary_nodes_is, 1.0_8, x, b, ierr)
    call ISDestroy(boundary_nodes_is, ierr)

    if (rank == 0) print *, "Solving system..."
    call KSPCreate(PETSC_COMM_WORLD, ksp, ierr)
    call KSPSetOperators(ksp, A, A, ierr)
    call KSPSetFromOptions(ksp, ierr)
    call KSPSolve(ksp, b, x, ierr)
    if (rank == 0) print *, "System solved."

    call VecViewFromOptions(x, PETSC_NULL_OBJECT, '-vec_view', ierr)

    call KSPDestroy(ksp, ierr)
    call VecDestroy(x, ierr)
    call VecDestroy(b, ierr)
    call MatDestroy(A, ierr)
    call DMDestroy(dm, ierr)
    call PetscFinalize(ierr)

end program manual_fem
