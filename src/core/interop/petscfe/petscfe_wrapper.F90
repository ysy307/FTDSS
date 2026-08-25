!> Fortran access to a PetscFE's basis tabulation.
!>
!> PetscFE can tabulate its basis at any reference point, but the generated
!> Fortran binding hands back a descriptor it never builds, so the values
!> cannot be read from Fortran. The C side of this module does the call and
!> copies the result into arrays the caller owns.
module core_interop_petscfe
    use, intrinsic :: iso_c_binding, only: c_int, c_double, c_ptr
    implicit none
    private

    public :: petscfe_tabulate
    public :: petscfe_sizes

    interface
        function ftcms_petscfe_tabulate(fe, num_points, points, values, gradients) &
            bind(c, name="ftcms_petscfe_tabulate") result(ierr)
            import :: c_int, c_double, c_ptr
            implicit none
            type(c_ptr), value :: fe
            integer(c_int), value :: num_points
            real(c_double), intent(in) :: points(*)
            real(c_double), intent(inout) :: values(*)
            real(c_double), intent(inout) :: gradients(*)
            integer(c_int) :: ierr
        end function ftcms_petscfe_tabulate

        function ftcms_petscfe_sizes(fe, num_basis, num_components, reference_dim) &
            bind(c, name="ftcms_petscfe_sizes") result(ierr)
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), value :: fe
            integer(c_int), intent(inout) :: num_basis
            integer(c_int), intent(inout) :: num_components
            integer(c_int), intent(inout) :: reference_dim
            integer(c_int) :: ierr
        end function ftcms_petscfe_sizes
    end interface

contains

    !> Basis values and reference gradients at the given reference points.
    !>
    !> points is (dim, num_points); values is (num_basis, num_points);
    !> gradients is (dim, num_basis, num_points).
    subroutine petscfe_tabulate(fe, points, values, gradients, ierr)
        implicit none
        type(c_ptr), intent(in) :: fe
        real(c_double), intent(in) :: points(:, :)
        real(c_double), intent(inout) :: values(:, :)
        real(c_double), intent(inout) :: gradients(:, :, :)
        integer(c_int), intent(inout) :: ierr

        ierr = ftcms_petscfe_tabulate(fe, int(size(points, 2), c_int), points, values, gradients)
    end subroutine petscfe_tabulate

    subroutine petscfe_sizes(fe, num_basis, num_components, reference_dim, ierr)
        implicit none
        type(c_ptr), intent(in) :: fe
        integer(c_int), intent(inout) :: num_basis, num_components, reference_dim
        integer(c_int), intent(inout) :: ierr

        ierr = ftcms_petscfe_sizes(fe, num_basis, num_components, reference_dim)
    end subroutine petscfe_sizes

end module core_interop_petscfe
