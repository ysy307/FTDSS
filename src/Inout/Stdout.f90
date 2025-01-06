module Inout_Stdout
    use omp_lib
    use, intrinsic :: iso_fortran_env, only : int32, real64
	use :: Types
    implicit none
    private

    public :: init_omp_config

    contains

    subroutine init_omp_config(Solver)
		type(SolverInfo), intent(inout) :: Solver
		! OpenMPの設定
		! call omp_set_dynamic(TRUE)
		call omp_set_max_active_levels(1)

		if (Solver%Flags%isStdOut) write(*,'(a)')    ''
		if (Solver%Flags%isStdOut) write(*,'(a)')    'OpenMP Properties'
		if (Solver%Flags%isStdOut) write(*,'(a)')    '-------------------------------------'
		if (Solver%Flags%isStdOut) write(*,'(a,i0)') 'Max number of threads           = ', omp_get_max_threads()
		if (Solver%Flags%isStdOut) write(*,'(a,i0)') 'Number of processors availables = ', omp_get_num_procs()
		if (Solver%Flags%isStdOut) write(*,'(a)')    '-------------------------------------'
		if (Solver%Flags%isStdOut) write(*,'(a)')    ''
	end subroutine init_omp_config

end module Inout_Stdout