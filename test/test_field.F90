program test_field
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: mpi_f08
    use :: module_core
    use :: module_input
    use :: module_control
    use :: module_domain
    use :: module_field
    implicit none
    type(type_input) :: input
    type(type_control) :: controls
    type(type_domain) :: domain
    type(type_jacobian_matrix) :: J
    type(type_residual_vector) :: R
    integer(int32) :: ierr
    integer(int32) :: myrank
    integer(int32) :: nsize

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)

    call controls%initialize()
    call input%initialize()
    call controls%initialize(input)

    call domain%initialize(input, controls)

    call J%initialize(domain, MATRIX_CRS)
    call R%initialize(domain)

    call J%display()
    call R%display()

    call MPI_Finalize(ierr)

end program test_field
