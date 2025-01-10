module Calculate_BLAS
    use omp_lib
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    implicit none
    private

    public :: norm_2
    public :: ddots

contains

    function norm_2(N, x) result(norm)
        implicit none
        integer(int32), intent(in)    :: N
        real(real64), intent(in)    :: x(:)
        real(real64)                  :: norm
        integer(int32)                :: iN, ithread, nthreads
        real(real64), allocatable     :: partial_sums(:)

        ! nthreads = omp_get_max_threads()
        ! allocate(partial_sums(nthreads))
        ! partial_sums = 0.0d0
        norm = 0.0d0

        ! $omp parallel private(iN, ithread)
        ! ithread = omp_get_thread_num() + 1
        ! $omp do
        do iN = 1, N
            norm = norm + x(iN)**2
        end do
        ! $omp end do
        ! $omp end parallel

        ! メモリ解放
        deallocate (partial_sums)
    end function norm_2

    function ddots(N, x, y) result(dot)
        implicit none
        integer(int32), intent(in)    :: N
        real(real64), intent(in)    :: x(:), y(:)
        real(real64)                  :: dot
        integer(int32)                :: iN, ithread, nthreads
        real(real64), allocatable     :: partial_sums(:)

        ! nthreads = omp_get_max_threads()
        ! allocate(partial_sums(nthreads))
        ! partial_sums = 0.0d0
        dot = 0.0d0

        ! $omp parallel private(iN, ithread)
        ! ithread = omp_get_thread_num() + 1
        ! $omp do
        do iN = 1, N
            dot = dot + x(iN) * y(iN)
        end do
        ! $omp end do
        ! $omp end parallel

        ! dot = sum(partial_sums)
    end function ddots

end module Calculate_BLAS
