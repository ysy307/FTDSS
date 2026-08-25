/*
 * Basis tabulation of a PetscFE, reachable from Fortran.
 *
 * PetscFE can tabulate its basis at any set of reference points, but the
 * generated Fortran binding hands back a PetscTabulation whose Fortran
 * descriptor is never built, so reading it from Fortran faults. This shim does
 * the call on the C side and copies the values into arrays the caller owns,
 * which is what the interop layer is for.
 */
#include <petscfe.h>
#include <string.h>

/*
 * Tabulate at `num_points` reference points.
 *
 *   points     : num_points * dim, point-major
 *   values     : num_points * num_basis            (may be NULL)
 *   gradients  : num_points * num_basis * dim      (may be NULL)
 *
 * `values` and `gradients` are filled only when non-NULL; ask for gradients and
 * the tabulation is built to first order, otherwise to zeroth.
 */
PetscErrorCode ftcms_petscfe_tabulate(PetscFE fe, PetscInt num_points, const PetscReal points[],
                                      PetscReal values[], PetscReal gradients[])
{
  PetscTabulation tab;
  PetscInt        K = gradients ? 1 : 0;
  PetscInt        Nb, Nc, cdim, count;

  PetscFunctionBegin;
  PetscCall(PetscFECreateTabulation(fe, 1, num_points, points, K, &tab));

  Nb   = tab->Nb;
  Nc   = tab->Nc;
  cdim = tab->cdim;

  if (values) {
    count = num_points * Nb * Nc;
    memcpy(values, tab->T[0], (size_t)count * sizeof(PetscReal));
  }
  if (gradients) {
    count = num_points * Nb * Nc * cdim;
    memcpy(gradients, tab->T[1], (size_t)count * sizeof(PetscReal));
  }

  PetscCall(PetscTabulationDestroy(&tab));
  PetscFunctionReturn(PETSC_SUCCESS);
}

/* Number of basis functions, components and reference dimension of a PetscFE. */
PetscErrorCode ftcms_petscfe_sizes(PetscFE fe, PetscInt *num_basis, PetscInt *num_components,
                                   PetscInt *reference_dim)
{
  PetscFunctionBegin;
  PetscCall(PetscFEGetDimension(fe, num_basis));
  PetscCall(PetscFEGetNumComponents(fe, num_components));
  PetscCall(PetscFEGetSpatialDimension(fe, reference_dim));
  PetscFunctionReturn(PETSC_SUCCESS);
}
