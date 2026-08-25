#include <petsc/finclude/petscvec.h>

program petsc_smoke
    use petscvec
    implicit none

    PetscErrorCode :: ierr
    PetscInt :: n
    PetscScalar :: value
    PetscReal :: norm
    Vec :: x

    n = 10
    value = 2.0

    PetscCallA(PetscInitialize(ierr))

    PetscCallA(VecCreate(PETSC_COMM_WORLD, x, ierr))
    PetscCallA(VecSetSizes(x, PETSC_DECIDE, n, ierr))
    PetscCallA(VecSetFromOptions(x, ierr))

    PetscCallA(VecSet(x, value, ierr))
    PetscCallA(VecNorm(x, NORM_2, norm, ierr))

    PetscCallA(VecDestroy(x, ierr))
    PetscCallA(PetscFinalize(ierr))

end program petsc_smoke