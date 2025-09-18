program Poisson2D
#include <petsc/finclude/petsc.h>
    use petsc
    implicit none

    type(tDM) :: dm
    type(tSNES) :: snes
    type(tVec) :: u_glob
    type(tPetscDS) :: ds
    type(tDMLabel) :: boundaryLabel
    PetscErrorCode :: ierr
    PetscInt, parameter :: boundary_ids(1) = (/1/)
    character(len=256) :: mesh_file
    PetscBool :: flg
    PetscInt :: bd

    ! Initialize
    call PetscInitialize(ierr)

    ! Ensure FE parsing for msh if needed
    call PetscOptionsSetValue(PETSC_NULL_OPTIONS, '-dm_plex_msh_parse_fe', '', ierr)

    ! Read mesh filename from options
    call PetscOptionsGetString(PETSC_NULL_OPTIONS, PETSC_NULL_CHARACTER, '-mesh_file', mesh_file, flg, ierr)
    if (.not. flg) then
        print *, 'Error: supply -mesh_file <path>'
        call PetscFinalize(ierr)
        stop 1
    end if

    ! Create DM from file
    call DMPlexCreateFromFile(PETSC_COMM_WORLD, trim(mesh_file), "FTDSS", PETSC_TRUE, dm, ierr)
    call DMSetFromOptions(dm, ierr)
    call DMSetUp(dm, ierr)

    ! Set pointwise residuals/Jacobian in PetscDS
    call DMGetDS(dm, ds, ierr)
    call PetscDSSetResidual(ds, 0, f0_u, f1_u, ierr)
    call PetscDSSetJacobian(ds, 0, 0, PETSC_NULL_FUNCTION, PETSC_NULL_FUNCTION, PETSC_NULL_FUNCTION, g3_uu, ierr)

    ! Boundary: look up label and add essential BC (u=0 on ids in boundary_ids)
    call DMGetLabel(dm, 'Face Sets', boundaryLabel, ierr)
    call DMAddBoundary(dm, DM_BC_ESSENTIAL, 'wall', boundaryLabel, 1, boundary_ids, 0, 0, PETSC_NULL_INTEGER, u_exact, PETSC_NULL_FUNCTION, PETSC_NULL_OBJECT, bd, ierr)

    ! SNES setup and solve
    call SNESCreate(PETSC_COMM_WORLD, snes, ierr)
    call SNESSetDM(snes, dm, ierr)
    call SNESSetFromOptions(snes, ierr)

    call DMCreateGlobalVector(dm, u_glob, ierr)
    call VecSet(u_glob, 0.0d0, ierr)
    call PetscObjectSetName(u_glob, 'solution', ierr)

    call SNESSolve(snes, PETSC_NULL_VEC, u_glob, ierr)

    ! Output
    call DMViewFromOptions(dm, PETSC_NULL_OBJECT, '-dm_view', ierr)
    call VecViewFromOptions(u_glob, PETSC_NULL_OBJECT, '-vec_view', ierr)

    ! Cleanup
    call DMDestroy(dm, ierr)
    call SNESDestroy(snes, ierr)
    call VecDestroy(u_glob, ierr)
    call PetscFinalize(ierr)

contains

    ! f0: forcing term = -6.0
    subroutine f0_u(dim, Nf, NfAux, uOff, uOff_x, u, u_t, u_x, aOff, aOff_x, a, a_t, a_x, t, x, numConstants, constants, f0, ierr)
        implicit none
        PetscInt, intent(in) :: dim, Nf, NfAux, numConstants
        PetscInt, intent(in), dimension(*) :: uOff, uOff_x, aOff, aOff_x
        PetscScalar, intent(in), dimension(*) :: u, u_t, u_x, a, a_t, a_x, constants
        PetscReal, intent(in) :: t
        PetscReal, intent(in), dimension(*) :: x
        PetscScalar, intent(out), dimension(*) :: f0
        PetscErrorCode, intent(out) :: ierr

        f0(1) = -6.0d0
        ierr = 0
    end subroutine f0_u

    ! f1: gradient term (∇u)
    subroutine f1_u(dim, Nf, NfAux, uOff, uOff_x, u, u_t, u_x, aOff, aOff_x, a, a_t, a_x, t, x, numConstants, constants, f1, ierr)
        implicit none
        PetscInt, intent(in) :: dim, Nf, NfAux, numConstants
        PetscInt, intent(in), dimension(*) :: uOff, uOff_x, aOff, aOff_x
        PetscScalar, intent(in), dimension(*) :: u, u_t, u_x, a, a_t, a_x, constants
        PetscReal, intent(in) :: t
        PetscReal, intent(in), dimension(*) :: x
        PetscScalar, intent(out), dimension(*) :: f1
        PetscErrorCode, intent(out) :: ierr
        integer :: i

        do i = 1, dim
            ! uOff_x(1) is a C-based offset; Fortran indexing of u_x starts at 1.
            f1(i) = u_x(uOff_x(1) + i)
        end do
        ierr = 0
    end subroutine f1_u

    ! g3: derivative dF1/d(grad u) -> identity matrix in each block
    subroutine g3_uu(dim, Nf, NfAux, uOff, uOff_x, u, u_t, u_x, aOff, aOff_x, a, a_t, a_x, t, x, numConstants, constants, g3, ierr)
        implicit none
        PetscInt, intent(in) :: dim, Nf, NfAux, numConstants
        PetscInt, intent(in), dimension(*) :: uOff, uOff_x, aOff, aOff_x
        PetscScalar, intent(in), dimension(*) :: u, u_t, u_x, a, a_t, a_x, constants
        PetscReal, intent(in) :: t
        PetscReal, intent(in), dimension(*) :: x
        PetscScalar, intent(out), dimension(*) :: g3
        PetscErrorCode, intent(out) :: ierr
        integer :: i, j, n

        n = dim * dim
        do i = 1, n
            g3(i) = 0.0d0
        end do
        do i = 1, dim
            do j = 1, dim
                if (i .eq. j) then
                    g3((i - 1) * dim + j) = 1.0d0
                else
                    g3((i - 1) * dim + j) = 0.0d0
                end if
            end do
        end do
        ierr = 0
    end subroutine g3_uu

    ! Boundary value u = 0
    subroutine u_exact(dim, time, x, Nc, u, ctx, ierr)
        implicit none
        PetscInt, intent(in) :: dim, Nc
        PetscReal, intent(in) :: time
        PetscReal, intent(in), dimension(*) :: x
        type(tPetscObject), intent(in) :: ctx
        PetscScalar, intent(out), dimension(*) :: u
        PetscErrorCode, intent(out) :: ierr

        u(1) = 0.0d0
        ierr = 0
    end subroutine u_exact

end program Poisson2D
