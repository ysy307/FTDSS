program test_input
    use, intrinsic :: iso_fortran_env, only: int32, real64
#ifdef _MPI
    use :: mpi_f08
#endif
    use :: module_input
    implicit none

    integer(int32) :: ierr
    integer(int32) :: myrank

#ifdef _MPI
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
#endif

    ! Call test function
    call run_test_input()

#ifdef _MPI
    call MPI_Finalize(ierr)
#endif

contains

    !> Test loading and logic consistency of the Input module
    !>
    subroutine run_test_input()
        type(type_input) :: input
        integer(int32) :: calc_type, calc_dim
        character(:), allocatable :: title
        logical :: success
        integer(int32) :: myrank_sub, ierr_sub

#ifdef _MPI
        call MPI_Comm_rank(MPI_COMM_WORLD, myrank_sub, ierr_sub)
#else
        myrank_sub = 0
#endif

        if (myrank_sub == 0) print *, "--- [Input] Testing Loading & Logic ---"

        ! 1. Initialize (load JSON and apply auto-configuration)
        call input%initialize()

        ! 2. Retrieve values
        calc_type = input%basic%simulation_settings%calculate_type
        calc_dim = input%basic%simulation_settings%calculate_dimension
        title = input%basic%simulation_settings%title

        success = .true.

        if (myrank_sub == 0) then
            print *, "  Loaded Parameters:"
            print *, "    Calculate Type:     ", calc_type
            print *, "    Derived Dimension:  ", calc_dim

            ! Logic verification
            if (calc_type == 1 .or. calc_type == 2) then
                if (calc_dim /= 2) then
                    print *, "  [FAIL] Logic Error: Type 1/2 must be Dim 2. Got:", calc_dim
                    success = .false.
                end if
            else if (calc_type == 3) then
                if (calc_dim /= 3) then
                    print *, "  [FAIL] Logic Error: Type 3 must be Dim 3. Got:", calc_dim
                    success = .false.
                end if
            end if

            if (success) then
                print *, "  [PASS] Input logic verification successful."
            else
                print *, "  [FAIL] Input verification failed."
            end if

            ! Display contents for verification
            call input%display()
        end if

    end subroutine run_test_input

end program test_input
