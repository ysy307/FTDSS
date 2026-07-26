program test_coupling_assembly
    use, intrinsic :: iso_fortran_env, only:error_unit, int32, output_unit, real64
#ifdef _MPI
    use :: mpi_f08
#endif
    use :: module_core, only:PHYSICS_TYPES, abst_matrix, type_matrix_bsr
    use :: module_ftcms, only:type_ftcms
    implicit none

    type(type_ftcms) :: ftcms
    class(abst_matrix), pointer :: matrix
    real(real64), pointer :: values(:, :, :)
    real(real64) :: norm_th, norm_ht
    integer(int32) :: ierr
#ifdef _MPI
    logical :: mpi_is_initialized, mpi_is_finalized
#endif

#ifdef _MPI
    call MPI_Initialized(mpi_is_initialized, ierr)
    if (.not. mpi_is_initialized) call MPI_Init(ierr)
#endif

    nullify (matrix, values)
    call ftcms%initialize()
    call ftcms%assemble()
    matrix => ftcms%K%get_matrix()
    if (.not. associated(matrix)) then
        write (error_unit, '(A)') "FAIL: monolithic Jacobian matrix is not associated"
        error stop 1
    end if

    select type (matrix)
    type is (type_matrix_bsr)
        values => matrix%get_val()
    class default
        write (error_unit, '(A)') "FAIL: monolithic Jacobian is not BSR"
        error stop 1
    end select
    if (.not. associated(values)) then
        write (error_unit, '(A)') "FAIL: monolithic Jacobian values are not associated"
        error stop 1
    end if

    norm_th = maxval(abs(values(PHYSICS_TYPES%THERMAL%ID, PHYSICS_TYPES%HYDRAULIC%ID, :)))
    norm_ht = maxval(abs(values(PHYSICS_TYPES%HYDRAULIC%ID, PHYSICS_TYPES%THERMAL%ID, :)))
    if (norm_th <= tiny(1.0d0)) then
        write (error_unit, '(A,ES12.5)') "FAIL: K_TH is zero, max norm=", norm_th
        error stop 1
    end if
    if (norm_ht <= tiny(1.0d0)) then
        write (error_unit, '(A,ES12.5)') "FAIL: K_HT is zero, max norm=", norm_ht
        error stop 1
    end if
    write (output_unit, '(A,2(1X,ES12.5))') "PASS: coupled Modified-Picard K_TH/K_HT max norms:", norm_th, norm_ht

    call ftcms%destroy()

#ifdef _MPI
    call MPI_Initialized(mpi_is_initialized, ierr)
    call MPI_Finalized(mpi_is_finalized, ierr)
    if (mpi_is_initialized .and. (.not. mpi_is_finalized)) call MPI_Finalize(ierr)
#endif

end program test_coupling_assembly
