module linalg_vector_ops
    use, intrinsic :: iso_fortran_env, only: int32, real64
#ifdef _MPI
    use :: mpi
#endif
    implicit none
    private

    ! =========================================================================
    ! 1. 公開インターフェース (API)
    ! =========================================================================
    public :: norm_1, norm_2, norm_inf, dot

    ! =========================================================================
    ! 2. 手続きポインタと、そのための抽象インターフェース
    ! =========================================================================
    abstract interface
        function real64_from_vec_t(x)
            import :: real64
            real(real64), intent(in) :: x(:)
            real(real64) :: real64_from_vec_t
        end function
        function real64_from_2vec_t(x, y)
            import :: real64
            real(real64), intent(in) :: x(:), y(:)
            real(real64) :: real64_from_2vec_t
        end function
        function real64_from_vec_inf_t(x)
            import :: real64
            real(real64), intent(in) :: x(:)
            real(real64) :: real64_from_vec_inf_t
        end function
    end interface

    procedure(real64_from_vec_t), pointer, private :: norm_1_impl => null()
    procedure(real64_from_vec_t), pointer, private :: norm_2_impl => null()
    procedure(real64_from_vec_inf_t), pointer, private :: norm_inf_impl => null()
    procedure(real64_from_2vec_t), pointer, private :: dot_impl => null()

    logical, private :: is_initialized = .false.

    ! =========================================================================
    ! 3. 外部関数(MKL)のインターフェース定義
    !    エラー修正のため、CONTAINS の前に移動しました。
    ! =========================================================================
#ifdef _MKL
    interface
#ifdef _MPI
        function pdasum(n, x, incx)
            import :: int32, real64
            integer(int32), intent(in) :: n, incx
            real(real64), intent(in) :: x(*)
            real(real64) :: pdasum
        end function
        function pdnrm2(n, x, incx)
            import :: int32, real64
            integer(int32), intent(in) :: n, incx
            real(real64), intent(in) :: x(*)
            real(real64) :: pdnrm2
        end function
        function pddot(n, x, incx, y, incy)
            import :: int32, real64
            integer(int32), intent(in) :: n, incx, incy
            real(real64), intent(in) :: x(*), y(*)
            real(real64) :: pddot
        end function
#else
        function dasum(n, x, incx)
            import :: int32, real64
            integer(int32), intent(in) :: n, incx
            real(real64), intent(in) :: x(*)
            real(real64) :: dasum
        end function
        function dnrm2(n, x, incx)
            import :: int32, real64
            integer(int32), intent(in) :: n, incx
            real(real64), intent(in) :: x(*)
            real(real64) :: dnrm2
        end function
        function ddot(n, x, incx, y, incy)
            import :: int32, real64
            integer(int32), intent(in) :: n, incx, incy
            real(real64), intent(in) :: x(*), y(*)
            real(real64) :: ddot
        end function
#endif
        function idamax(n, x, incx)
            import :: int32, real64
            integer(int32), intent(in) :: n, incx
            real(real64), intent(in) :: x(*)
            integer(int32) :: idamax
        end function
    end interface
#endif

contains

    ! =========================================================================
    ! 4. 公開関数 (ラッパー)
    ! =========================================================================
    function norm_1(x) result(norm)
        real(real64), intent(in) :: x(:); real(real64) :: norm
        if (.not. is_initialized) call initialize_backend()
        norm = norm_1_impl(x)
    end function norm_1

    function norm_2(x) result(norm)
        real(real64), intent(in) :: x(:); real(real64) :: norm
        if (.not. is_initialized) call initialize_backend()
        norm = norm_2_impl(x)
    end function norm_2

    function norm_inf(x) result(norm)
        implicit none
        real(real64), intent(in) :: x(:); real(real64) :: norm
        if (.not. is_initialized) call initialize_backend()
        norm = norm_inf_impl(x)
    end function norm_inf

    function dot(x, y) result(prod)
        real(real64), intent(in) :: x(:), y(:); real(real64) :: prod
        if (.not. is_initialized) call initialize_backend()
        prod = dot_impl(x, y)
    end function dot

    ! =========================================================================
    ! 5. バックエンドの初期化処理
    ! =========================================================================
    subroutine initialize_backend()
        if (is_initialized) return
#ifdef _MKL
        norm_1_impl => norm_1_mkl
        norm_2_impl => norm_2_mkl
        norm_inf_impl => norm_inf_mkl
        dot_impl => dot_mkl
        write (*, '(A)') "INFO: linalg_vector_ops initialized with MKL backend."
#else
        norm_1_impl => norm_1_native
        norm_2_impl => norm_2_native
        norm_inf_impl => norm_inf_native
        dot_impl => dot_native
        write (*, '(A)') "INFO: linalg_vector_ops initialized with native Fortran backend."
#endif
        is_initialized = .true.
    end subroutine initialize_backend

    ! =========================================================================
    ! 6. 実際の計算処理 (実装)
    ! =========================================================================

    ! -------------------------------------------------------------------------
    ! 6a. MKLバックエンドの実装
    ! -------------------------------------------------------------------------
#ifdef _MKL
    function norm_1_mkl(x) result(norm)
        real(real64), intent(in) :: x(:); real(real64) :: norm
#ifdef _MPI
        norm = pdasum(int(size(x), int32), x, 1)
#else
        norm = dasum(int(size(x), int32), x, 1)
#endif
    end function norm_1_mkl

    function norm_2_mkl(x) result(norm)
        real(real64), intent(in) :: x(:); real(real64) :: norm
#ifdef _MPI
        norm = pdnrm2(int(size(x), int32), x, 1)
#else
        norm = dnrm2(int(size(x), int32), x, 1)
#endif
    end function norm_2_mkl

    function norm_inf_mkl(x) result(norm)
        real(real64), intent(in) :: x(:); real(real64) :: norm
        if (size(x) > 0) then
            norm = abs(x(idamax(int(size(x), int32), x, 1)))
        else
            norm = 0.0_real64
        end if
    end function norm_inf_mkl

    function dot_mkl(x, y) result(prod)
        real(real64), intent(in) :: x(:), y(:); real(real64) :: prod
        if (size(x) /= size(y)) error stop "Error: dot_mkl - array sizes do not match."
#ifdef _MPI
        prod = pddot(int(size(x), int32), x, 1, y, 1)
#else
        prod = ddot(int(size(x), int32), x, 1, y, 1)
#endif
    end function dot_mkl
#endif

    ! -------------------------------------------------------------------------
    ! 6b. 標準Fortranバックエンドの実装
    ! -------------------------------------------------------------------------
    function norm_1_native(x) result(norm)
        real(real64), intent(in) :: x(:); real(real64) :: norm
        norm = sum(abs(x))
    end function norm_1_native

    function norm_2_native(x) result(norm)
        real(real64), intent(in) :: x(:); real(real64) :: norm
        norm = norm2(x)
    end function norm_2_native

    function norm_inf_native(x) result(norm)
        real(real64), intent(in) :: x(:); real(real64) :: norm
        if (size(x) > 0) then
            norm = maxval(abs(x))
        else
            norm = 0.0_real64
        end if
    end function norm_inf_native

    function dot_native(x, y) result(prod)
        real(real64), intent(in) :: x(:), y(:); real(real64) :: prod
        if (size(x) /= size(y)) error stop "Error: dot_native - array sizes do not match."
        prod = dot_product(x, y)
    end function dot_native

end module linalg_vector_ops
