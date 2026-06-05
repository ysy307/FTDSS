!>
!> Provides advanced vector operations and operator overloads.
!> This module offers a hybrid approach to arithmetic:
!> * **Operator Overloads**: For fixed-size, lightweight `coordinate` and
!>     `coordinate_array` types, it provides convenient function-based
!>     operators (+, -, *, /).
!> * **Subroutines**: For variable-size, potentially large `vector` types, it
!>     provides performance-oriented subroutines.
!> It also features a backend-switching capability for norm and dot-product
!> calculations, utilizing MKL when available.
!>
module linalg_vector_operations
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use :: mpi_f08
    use :: module_core
    use :: linalg_mkl_backend

    implicit none
    private

    ! =========================================================================
    ! 1. Public Interface (API)
    ! =========================================================================
    public :: initialize_linalg
    public :: vector_norm1
    public :: vector_norm2
    public :: vector_norminf
    public :: vector_dot
    public :: vector_axpy
    public :: vector_xpay
    public :: vector_axpyz
    public :: vector_scale
    public :: vector_abs
    public :: vector_reciprocal
    public :: vector_shift

    interface vector_norm1
        module procedure :: norm_1_native
        module procedure :: norm_1_vector_dp
    end interface

    interface vector_norm2
        module procedure :: norm_2_native
        module procedure :: norm_2_vector_dp
    end interface

    interface vector_norminf
        module procedure :: norm_inf_native
        module procedure :: norm_inf_vector_dp
    end interface

    interface vector_dot
        module procedure :: dot_native
        module procedure :: dot_vector_dp
    end interface

    public :: operator( + ) !&
    public :: operator( - ) !&
    public :: operator( * ) !&
    public :: operator( / ) !&
    public :: add
    public :: subtract
    public :: multiply
    public :: divide
    public :: assignment(=)

    interface operator(+)
        module procedure :: add_coordinate_dp
        module procedure :: add_scalar_1_coordinate_dp
        module procedure :: add_scalar_2_coordinate_dp
        module procedure :: add_coordinate_int
        module procedure :: add_scalar_1_coordinate_int
        module procedure :: add_scalar_2_coordinate_int
        module procedure :: add_coordinate_array_dp
        module procedure :: add_scalar_1_coordinate_array_dp
        module procedure :: add_scalar_2_coordinate_array_dp
        module procedure :: add_coordinate_array_int
        module procedure :: add_scalar_1_coordinate_array_int
        module procedure :: add_scalar_2_coordinate_array_int
    end interface

    interface operator(-)
        module procedure :: subtract_coordinate_dp
        module procedure :: subtract_scalar_1_coordinate_dp
        module procedure :: subtract_scalar_2_coordinate_dp
        module procedure :: subtract_coordinate_int
        module procedure :: subtract_scalar_1_coordinate_int
        module procedure :: subtract_scalar_2_coordinate_int
        module procedure :: subtract_coordinate_array_dp
        module procedure :: subtract_scalar_1_coordinate_array_dp
        module procedure :: subtract_scalar_2_coordinate_array_dp
        module procedure :: subtract_coordinate_array_int
        module procedure :: subtract_scalar_1_coordinate_array_int
        module procedure :: subtract_scalar_2_coordinate_array_int
    end interface

    interface operator(*)
        module procedure :: multiply_coordinate_dp
        module procedure :: multiply_scalar_1_coordinate_dp
        module procedure :: multiply_scalar_2_coordinate_dp
        module procedure :: multiply_coordinate_int
        module procedure :: multiply_scalar_1_coordinate_int
        module procedure :: multiply_scalar_2_coordinate_int
        module procedure :: multiply_coordinate_array_dp
        module procedure :: multiply_scalar_1_coordinate_array_dp
        module procedure :: multiply_scalar_2_coordinate_array_dp
        module procedure :: multiply_coordinate_array_int
        module procedure :: multiply_scalar_1_coordinate_array_int
        module procedure :: multiply_scalar_2_coordinate_array_int
    end interface

    interface operator(/)
        module procedure :: divide_coordinate_dp
        module procedure :: divide_scalar_1_coordinate_dp
        module procedure :: divide_scalar_2_coordinate_dp
        module procedure :: divide_coordinate_array_dp
        module procedure :: divide_scalar_1_coordinate_array_dp
        module procedure :: divide_scalar_2_coordinate_array_dp
    end interface

    interface add
        module procedure :: add_vector_dp
        module procedure :: add_vector_int
    end interface

    interface subtract
        module procedure :: subtract_vector_dp
        module procedure :: subtract_vector_int
    end interface

    interface multiply
        module procedure :: multiply_vector_dp
        module procedure :: multiply_vector_int
    end interface

    interface divide
        module procedure :: divide_vector_dp
    end interface

    interface assignment(=)
        module procedure :: assign_coordinate_dp
        module procedure :: assign_coordinate_int
        module procedure :: assign_coordinate_array_dp
        module procedure :: assign_coordinate_array_int
        module procedure :: assign_vector_dp
        module procedure :: assign_vector_int
    end interface

    interface vector_axpy
        module procedure :: axpy_vector_dp
        module procedure :: axpy_vector_int
    end interface

    interface vector_xpay
        module procedure :: xpay_vector_dp
        module procedure :: xpay_vector_int
    end interface

    interface vector_axpyz
        module procedure :: axpyz_vector_dp
        module procedure :: axpyz_vector_int
    end interface

    interface vector_scale
        module procedure :: scale_vector_dp
        module procedure :: scale_vector_int
    end interface

    interface vector_abs
        module procedure :: abs_vector_dp
        module procedure :: abs_vector_int
    end interface

    interface vector_reciprocal
        module procedure :: reciprocal_vector_dp
        module procedure :: reciprocal_vector_int
    end interface

    interface vector_shift
        module procedure :: shift_vector_dp
        module procedure :: shift_vector_int
    end interface

    ! =========================================================================
    ! 2. Private Helper Interfaces and Pointers
    ! =========================================================================

    abstract interface
        function abst_real_from_one_vector_function(vector) result(res)
            import :: real64
            implicit none
            real(real64), intent(in) :: vector(:)
            real(real64) :: res
        end function

        function abst_real_from_two_vectors_function(vector_a, vector_b) result(res)
            import :: real64
            implicit none
            real(real64), intent(in) :: vector_a(:)
            real(real64), intent(in) :: vector_b(:)
            real(real64) :: res
        end function

        function abst_real_from_vector_for_inf_norm_function(vector) result(res)
            import :: real64
            implicit none
            real(real64), intent(in) :: vector(:)
            real(real64) :: res
        end function

        subroutine abst_axpy_subroutine(alpha, x, y)
            import :: real64
            implicit none
            real(real64), intent(in) :: alpha
            real(real64), intent(in) :: x(:)
            real(real64), intent(inout) :: y(:)
        end subroutine
    end interface

    !> Procedure pointer for the backend implementation of the 1-norm.
    procedure(abst_real_from_one_vector_function), pointer, private :: compute_norm_1_backend => null()
    !> Procedure pointer for the backend implementation of the 2-norm.
    procedure(abst_real_from_one_vector_function), pointer, private :: compute_norm_2_backend => null()
    !> Procedure pointer for the backend implementation of the infinity-norm.
    procedure(abst_real_from_vector_for_inf_norm_function), pointer, private :: compute_norm_inf_backend => null()
    !> Procedure pointer for the backend implementation of the dot product.
    procedure(abst_real_from_two_vectors_function), pointer, private :: compute_dot_product_backend => null()
    !> Procedure pointer for the backend implementation of AXPY (y <- alpha*x + y).
    procedure(abst_axpy_subroutine), pointer, private :: compute_axpy_backend => null()

    !> Flag to ensure the backend pointers are initialized only once.
    logical, private :: is_mkl_initialized = .false.

contains

    ! =========================================================================
    ! 3. Private Helper Subroutines
    ! =========================================================================

    !>
    !> Initializes the linear algebra backend.
    !> This should be called once before using norm or dot product functions.
    !>
    subroutine initialize_linalg()
        implicit none

        if (.not. is_mkl_initialized) call initialize_mkl_backend()
    end subroutine initialize_linalg

    !>
    !> Points the backend procedure pointers to the appropriate implementation (MKL or native).
    !> This selection is controlled by the `_MKL` preprocessor macro.
    !>
    subroutine initialize_mkl_backend()
        implicit none

        if (is_mkl_initialized) return

#ifdef _MKL

        compute_norm_1_backend => norm_1_mkl
        compute_norm_2_backend => norm_2_mkl
        compute_norm_inf_backend => norm_inf_mkl
        compute_dot_product_backend => dot_mkl
        compute_axpy_backend => axpy_mkl
#else
        compute_norm_1_backend => norm_1_native
        compute_norm_2_backend => norm_2_native
        compute_norm_inf_backend => norm_inf_native
        compute_dot_product_backend => dot_native
        compute_axpy_backend => axpy_native
#endif
        is_mkl_initialized = .true.
    end subroutine initialize_mkl_backend

    ! =========================================================================
    ! 4. Public Norms and Dot Product
    ! =========================================================================

    !>
    !> Computes the 1-norm of a vector, \( \sum |x_i| \), using the initialized backend.
    !>
    function norm_1_native(x) result(norm_value)
        implicit none
        !> The input vector.
        real(real64), intent(in) :: x(:)
        !> The computed 1-norm.
        real(real64) :: norm_value

        if (.not. is_mkl_initialized) call initialize_mkl_backend()
        norm_value = compute_norm_1_backend(x)
    end function norm_1_native

    !>
    !> Computes the 1-norm of a vector object, \( \sum |x_i| \), using the initialized backend.
    function norm_1_vector_dp(x) result(norm_value)
        implicit none
        !> The input vector.
        type(type_vector_dp), intent(in) :: x
        !> The computed 1-norm.
        real(real64) :: norm_value

        real(real64), dimension(:), pointer :: vec_data

        if (.not. is_mkl_initialized) call initialize_mkl_backend()
        vec_data => x%get_data()
        norm_value = norm_1_native(vec_data)
    end function norm_1_vector_dp

    !>
    !> Computes the 2-norm (Euclidean norm) of a vector, \( \sqrt{\sum x_i^2} \), using the initialized backend.
    !>
    function norm_2_native(x) result(norm_value)
        implicit none
        !> The input vector.
        real(real64), intent(in) :: x(:)
        !> The computed 2-norm.
        real(real64) :: norm_value

        if (.not. is_mkl_initialized) call initialize_mkl_backend()
        norm_value = compute_norm_2_backend(x)
    end function norm_2_native

    !>
    !> Computes the 2-norm (Euclidean norm) of a vector object, \( \sqrt{\sum x_i^2} \), using the initialized backend.
    function norm_2_vector_dp(x) result(norm_value)
        implicit none
        !> The input vector.
        type(type_vector_dp), intent(in) :: x
        !> The computed 2-norm.
        real(real64) :: norm_value

        real(real64), dimension(:), pointer :: vec_data

        if (.not. is_mkl_initialized) call initialize_mkl_backend()
        vec_data => x%get_data()
        norm_value = norm_2_native(vec_data)
    end function norm_2_vector_dp

    !>
    !> Computes the infinity-norm (maximum absolute value), \( \max(|x_i|) \), using the initialized backend.
    !>
    function norm_inf_native(x) result(norm_value)
        implicit none
        !> The input vector.
        real(real64), intent(in) :: x(:)
        !> The computed infinity-norm.
        real(real64) :: norm_value

        if (.not. is_mkl_initialized) call initialize_mkl_backend()
        norm_value = compute_norm_inf_backend(x)
    end function norm_inf_native

    !>
    !> Computes the infinity-norm (maximum absolute value) of a vector object, \( \max(|x_i|) \), using the initialized backend.
    function norm_inf_vector_dp(x) result(norm_value)
        implicit none
        !> The input vector.
        type(type_vector_dp), intent(in) :: x
        !> The computed infinity-norm.
        real(real64) :: norm_value

        real(real64), dimension(:), pointer :: vec_data

        if (.not. is_mkl_initialized) call initialize_mkl_backend()
        vec_data => x%get_data()
        norm_value = norm_inf_native(vec_data)
    end function norm_inf_vector_dp

    !>
    !> Computes the dot product of two vectors, \( \sum x_i y_i \), using the initialized backend.
    !>
    function dot_native(x, y) result(product)
        implicit none
        !> The first input vector.
        real(real64), intent(in) :: x(:)
        !> The second input vector.
        real(real64), intent(in) :: y(:)
        !> The computed dot product.
        real(real64) :: product

#ifdef USE_DEBUG
        call check_match_length(x, y, 'dot')
#endif
        if (.not. is_mkl_initialized) call initialize_mkl_backend()
        product = compute_dot_product_backend(x, y)
    end function dot_native

    !> Computes the dot product of two vector objects, \( \sum x_i y_i \), using the initialized backend.
    function dot_vector_dp(x, y) result(product)
        implicit none
        !> The first input vector.
        type(type_vector_dp), intent(in) :: x
        !> The second input vector.
        type(type_vector_dp), intent(in) :: y
        !> The computed dot product.
        real(real64) :: product

        real(real64), dimension(:), pointer :: vec_data_x
        real(real64), dimension(:), pointer :: vec_data_y

        vec_data_x => x%get_data()
        vec_data_y => y%get_data()

        call check_match_length(vec_data_x, vec_data_y, 'dot_vector_dp')

        if (.not. is_mkl_initialized) call initialize_mkl_backend()
        product = dot_native(vec_data_x, vec_data_y)
    end function dot_vector_dp

    ! =========================================================================
    ! 5. Operator Overload Implementations for Coordinate Types
    ! =========================================================================

    ! -------------------------------------------------------------------------
    ! 5a. Double Precision Coordinate Operators
    ! -------------------------------------------------------------------------
    !>
    !> Performs component-wise addition of two double precision coordinates.
    !>
    function add_coordinate_dp(a, b) result(c)
        implicit none
        !> The first coordinate.
        type(type_coordinate_dp), intent(in) :: a
        !> The second coordinate.
        type(type_coordinate_dp), intent(in) :: b
        !> The resulting coordinate.
        type(type_coordinate_dp) :: c
        c%x = a%x + b%x
        c%y = a%y + b%y
        c%z = a%z + b%z
    end function add_coordinate_dp
    !>
    !> Performs component-wise addition of a scalar and a coordinate.
    !>
    function add_scalar_1_coordinate_dp(scalar, coord) result(c)
        implicit none
        !> The scalar value.
        real(real64), intent(in) :: scalar
        !> The coordinate.
        type(type_coordinate_dp), intent(in) :: coord
        !> The resulting coordinate.
        type(type_coordinate_dp) :: c
        c%x = scalar + coord%x
        c%y = scalar + coord%y
        c%z = scalar + coord%z
    end function add_scalar_1_coordinate_dp
    !>
    !> Performs component-wise addition of a coordinate and a scalar.
    !>
    function add_scalar_2_coordinate_dp(coord, scalar) result(c)
        implicit none
        !> The coordinate.
        type(type_coordinate_dp), intent(in) :: coord
        !> The scalar value.
        real(real64), intent(in) :: scalar
        !> The resulting coordinate.
        type(type_coordinate_dp) :: c
        c%x = coord%x + scalar
        c%y = coord%y + scalar
        c%z = coord%z + scalar
    end function add_scalar_2_coordinate_dp
    !>
    !> Performs component-wise subtraction of two double precision coordinates.
    !>
    function subtract_coordinate_dp(a, b) result(c)
        implicit none
        !> The first coordinate (minuend).
        type(type_coordinate_dp), intent(in) :: a
        !> The second coordinate (subtrahend).
        type(type_coordinate_dp), intent(in) :: b
        !> The resulting coordinate.
        type(type_coordinate_dp) :: c
        c%x = a%x - b%x
        c%y = a%y - b%y
        c%z = a%z - b%z
    end function subtract_coordinate_dp
    !>
    !> Performs component-wise subtraction of a coordinate from a scalar.
    !>
    function subtract_scalar_1_coordinate_dp(scalar, coord) result(c)
        implicit none
        !> The scalar value (minuend).
        real(real64), intent(in) :: scalar
        !> The coordinate (subtrahend).
        type(type_coordinate_dp), intent(in) :: coord
        !> The resulting coordinate.
        type(type_coordinate_dp) :: c
        c%x = scalar - coord%x
        c%y = scalar - coord%y
        c%z = scalar - coord%z
    end function subtract_scalar_1_coordinate_dp
    !>
    !> Performs component-wise subtraction of a scalar from a coordinate.
    !>
    function subtract_scalar_2_coordinate_dp(coord, scalar) result(c)
        implicit none
        !> The coordinate (minuend).
        type(type_coordinate_dp), intent(in) :: coord
        !> The scalar value (subtrahend).
        real(real64), intent(in) :: scalar
        !> The resulting coordinate.
        type(type_coordinate_dp) :: c
        c%x = coord%x - scalar
        c%y = coord%y - scalar
        c%z = coord%z - scalar
    end function subtract_scalar_2_coordinate_dp
    !>
    !> Performs component-wise multiplication of two double precision coordinates.
    !>
    function multiply_coordinate_dp(a, b) result(c)
        implicit none
        !> The first coordinate.
        type(type_coordinate_dp), intent(in) :: a
        !> The second coordinate.
        type(type_coordinate_dp), intent(in) :: b
        !> The resulting coordinate.
        type(type_coordinate_dp) :: c
        c%x = a%x * b%x
        c%y = a%y * b%y
        c%z = a%z * b%z
    end function multiply_coordinate_dp
    !>
    !> Performs component-wise multiplication of a scalar and a coordinate.
    !>
    function multiply_scalar_1_coordinate_dp(scalar, coord) result(c)
        implicit none
        !> The scalar value.
        real(real64), intent(in) :: scalar
        !> The coordinate.
        type(type_coordinate_dp), intent(in) :: coord
        !> The resulting coordinate.
        type(type_coordinate_dp) :: c
        c%x = scalar * coord%x
        c%y = scalar * coord%y
        c%z = scalar * coord%z
    end function multiply_scalar_1_coordinate_dp
    !>
    !> Performs component-wise multiplication of a coordinate and a scalar.
    !>
    function multiply_scalar_2_coordinate_dp(coord, scalar) result(c)
        implicit none
        !> The coordinate.
        type(type_coordinate_dp), intent(in) :: coord
        !> The scalar value.
        real(real64), intent(in) :: scalar
        !> The resulting coordinate.
        type(type_coordinate_dp) :: c
        c%x = coord%x * scalar
        c%y = coord%y * scalar
        c%z = coord%z * scalar
    end function multiply_scalar_2_coordinate_dp
    !>
    !> Performs component-wise division of two double precision coordinates.
    !>
    function divide_coordinate_dp(a, b) result(c)
        implicit none
        !> The first coordinate (numerator).
        type(type_coordinate_dp), intent(in) :: a
        !> The second coordinate (denominator).
        type(type_coordinate_dp), intent(in) :: b
        !> The resulting coordinate.
        type(type_coordinate_dp) :: c

#ifdef USE_DEBUG
        if (b%x == 0.0d0 .or. b%y == 0.0d0 .or. b%z == 0.0d0) then
            write (*, '(A)') "ERROR in divide_coordinate_dp: Division by zero."
            error stop 1
        end if
#endif
        c%x = a%x / b%x
        c%y = a%y / b%y
        c%z = a%z / b%z
    end function divide_coordinate_dp
    !>
    !> Performs component-wise division of a scalar by a coordinate.
    !>
    function divide_scalar_1_coordinate_dp(scalar, coord) result(c)
        implicit none
        !> The scalar value (numerator).
        real(real64), intent(in) :: scalar
        !> The coordinate (denominator).
        type(type_coordinate_dp), intent(in) :: coord
        !> The resulting coordinate.
        type(type_coordinate_dp) :: c

#ifdef USE_DEBUG
        if (coord%x == 0.0d0 .or. coord%y == 0.0d0 .or. coord%z == 0.0d0) then
            write (*, '(A)') "ERROR in divide_scalar_1_coordinate_dp: Division by zero."
            error stop 1
        end if
#endif
        c%x = scalar / coord%x
        c%y = scalar / coord%y
        c%z = scalar / coord%z
    end function divide_scalar_1_coordinate_dp
    !>
    !> Performs component-wise division of a coordinate by a scalar.
    !>
    function divide_scalar_2_coordinate_dp(coord, scalar) result(c)
        implicit none
        !> The coordinate (numerator).
        type(type_coordinate_dp), intent(in) :: coord
        !> The scalar value (denominator).
        real(real64), intent(in) :: scalar
        !> The resulting coordinate.
        type(type_coordinate_dp) :: c

#ifdef USE_DEBUG
        if (scalar == 0.0d0) then
            write (*, '(A)') "ERROR in divide_scalar_2_coordinate_dp: Division by zero."
            error stop 1
        end if
#endif
        c%x = coord%x / scalar
        c%y = coord%y / scalar
        c%z = coord%z / scalar
    end function divide_scalar_2_coordinate_dp

    ! -------------------------------------------------------------------------
    ! 5b. Integer Coordinate Operators
    ! -------------------------------------------------------------------------
    !>
    !> Performs component-wise addition of two integer coordinates.
    !>
    function add_coordinate_int(a, b) result(c)
        implicit none
        !> The first coordinate.
        type(type_coordinate_int), intent(in) :: a
        !> The second coordinate.
        type(type_coordinate_int), intent(in) :: b
        !> The resulting coordinate.
        type(type_coordinate_int) :: c

        c%x = a%x + b%x
        c%y = a%y + b%y
        c%z = a%z + b%z
    end function add_coordinate_int
    !>
    !> Performs component-wise addition of a scalar and a coordinate.
    !>
    function add_scalar_1_coordinate_int(scalar, coord) result(c)
        implicit none
        !> The scalar value.
        integer(int32), intent(in) :: scalar
        !> The coordinate.
        type(type_coordinate_int), intent(in) :: coord
        !> The resulting coordinate.
        type(type_coordinate_int) :: c

        c%x = scalar + coord%x
        c%y = scalar + coord%y
        c%z = scalar + coord%z
    end function add_scalar_1_coordinate_int
    !>
    !> Performs component-wise addition of a coordinate and a scalar.
    !>
    function add_scalar_2_coordinate_int(coord, scalar) result(c)
        implicit none
        !> The coordinate.
        type(type_coordinate_int), intent(in) :: coord
        !> The scalar value.
        integer(int32), intent(in) :: scalar
        !> The resulting coordinate.
        type(type_coordinate_int) :: c

        c%x = coord%x + scalar
        c%y = coord%y + scalar
        c%z = coord%z + scalar
    end function add_scalar_2_coordinate_int
    !>
    !> Performs component-wise subtraction of two integer coordinates.
    !>
    function subtract_coordinate_int(a, b) result(c)
        implicit none
        !> The first coordinate (minuend).
        type(type_coordinate_int), intent(in) :: a
        !> The second coordinate (subtrahend).
        type(type_coordinate_int), intent(in) :: b
        !> The resulting coordinate.
        type(type_coordinate_int) :: c

        c%x = a%x - b%x
        c%y = a%y - b%y
        c%z = a%z - b%z
    end function subtract_coordinate_int
    !>
    !> Performs component-wise subtraction of a coordinate from a scalar.
    !>
    function subtract_scalar_1_coordinate_int(scalar, coord) result(c)
        implicit none
        !> The scalar value (minuend).
        integer(int32), intent(in) :: scalar
        !> The coordinate (subtrahend).
        type(type_coordinate_int), intent(in) :: coord
        !> The resulting coordinate.
        type(type_coordinate_int) :: c

        c%x = scalar - coord%x
        c%y = scalar - coord%y
        c%z = scalar - coord%z
    end function subtract_scalar_1_coordinate_int
    !>
    !> Performs component-wise subtraction of a scalar from a coordinate.
    !>
    function subtract_scalar_2_coordinate_int(coord, scalar) result(c)
        implicit none
        !> The coordinate (minuend).
        type(type_coordinate_int), intent(in) :: coord
        !> The scalar value (subtrahend).
        integer(int32), intent(in) :: scalar
        !> The resulting coordinate.
        type(type_coordinate_int) :: c

        c%x = coord%x - scalar
        c%y = coord%y - scalar
        c%z = coord%z - scalar
    end function subtract_scalar_2_coordinate_int
    !>
    !> Performs component-wise multiplication of two integer coordinates.
    !>
    function multiply_coordinate_int(a, b) result(c)
        implicit none
        !> The first coordinate.
        type(type_coordinate_int), intent(in) :: a
        !> The second coordinate.
        type(type_coordinate_int), intent(in) :: b
        !> The resulting coordinate.
        type(type_coordinate_int) :: c

        c%x = a%x * b%x
        c%y = a%y * b%y
        c%z = a%z * b%z
    end function multiply_coordinate_int
    !>
    !> Performs component-wise multiplication of a scalar and a coordinate.
    !>
    function multiply_scalar_1_coordinate_int(scalar, coord) result(c)
        implicit none
        !> The scalar value.
        integer(int32), intent(in) :: scalar
        !> The coordinate.
        type(type_coordinate_int), intent(in) :: coord
        !> The resulting coordinate.
        type(type_coordinate_int) :: c

        c%x = scalar * coord%x
        c%y = scalar * coord%y
        c%z = scalar * coord%z
    end function multiply_scalar_1_coordinate_int
    !>
    !> Performs component-wise multiplication of a coordinate and a scalar.
    !>
    function multiply_scalar_2_coordinate_int(coord, scalar) result(c)
        implicit none
        !> The coordinate.
        type(type_coordinate_int), intent(in) :: coord
        !> The scalar value.
        integer(int32), intent(in) :: scalar
        !> The resulting coordinate.
        type(type_coordinate_int) :: c

        c%x = coord%x * scalar
        c%y = coord%y * scalar
        c%z = coord%z * scalar
    end function multiply_scalar_2_coordinate_int

    ! -------------------------------------------------------------------------
    ! 5c. Double Precision Coordinate Array Operators
    ! -------------------------------------------------------------------------
    !>
    !> Performs element-wise addition of two coordinate arrays.
    !>
    function add_coordinate_array_dp(a, b) result(c)
        implicit none
        !> The first coordinate array.
        type(type_coordinate_array_dp), intent(in) :: a
        !> The second coordinate array.
        type(type_coordinate_array_dp), intent(in) :: b
        !> The resulting coordinate array.
        type(type_coordinate_array_dp) :: c

        c%x = a%x + b%x
        c%y = a%y + b%y
        c%z = a%z + b%z
    end function add_coordinate_array_dp
    !>
    !> Adds a scalar to each component of each coordinate in an array.
    !>
    function add_scalar_1_coordinate_array_dp(scalar, array_coord) result(c)
        implicit none
        !> The scalar value.
        real(real64), intent(in) :: scalar
        !> The coordinate array.
        type(type_coordinate_array_dp), intent(in) :: array_coord
        !> The resulting coordinate array.
        type(type_coordinate_array_dp) :: c

        c%x = scalar + array_coord%x
        c%y = scalar + array_coord%y
        c%z = scalar + array_coord%z
    end function add_scalar_1_coordinate_array_dp
    !>
    !> Adds a scalar to each component of each coordinate in an array.
    !>
    function add_scalar_2_coordinate_array_dp(array_coord, scalar) result(c)
        implicit none
        !> The coordinate array.
        type(type_coordinate_array_dp), intent(in) :: array_coord
        !> The scalar value.
        real(real64), intent(in) :: scalar
        !> The resulting coordinate array.
        type(type_coordinate_array_dp) :: c

        c%x = array_coord%x + scalar
        c%y = array_coord%y + scalar
        c%z = array_coord%z + scalar
    end function add_scalar_2_coordinate_array_dp
    !>
    !> Performs element-wise subtraction of two coordinate arrays.
    !>
    function subtract_coordinate_array_dp(a, b) result(c)
        implicit none
        !> The first coordinate array (minuend).
        type(type_coordinate_array_dp), intent(in) :: a
        !> The second coordinate array (subtrahend).
        type(type_coordinate_array_dp), intent(in) :: b
        !> The resulting coordinate array.
        type(type_coordinate_array_dp) :: c

        c%x = a%x - b%x
        c%y = a%y - b%y
        c%z = a%z - b%z
    end function subtract_coordinate_array_dp
    !>
    !> Subtracts each coordinate component in an array from a scalar.
    !>
    function subtract_scalar_1_coordinate_array_dp(scalar, array_coord) result(c)
        implicit none
        !> The scalar value (minuend).
        real(real64), intent(in) :: scalar
        !> The coordinate array (subtrahend).
        type(type_coordinate_array_dp), intent(in) :: array_coord
        !> The resulting coordinate array.
        type(type_coordinate_array_dp) :: c

        c%x = scalar - array_coord%x
        c%y = scalar - array_coord%y
        c%z = scalar - array_coord%z
    end function subtract_scalar_1_coordinate_array_dp
    !>
    !> Subtracts a scalar from each coordinate component in an array.
    !>
    function subtract_scalar_2_coordinate_array_dp(array_coord, scalar) result(c)
        implicit none
        !> The coordinate array (minuend).
        type(type_coordinate_array_dp), intent(in) :: array_coord
        !> The scalar value (subtrahend).
        real(real64), intent(in) :: scalar
        !> The resulting coordinate array.
        type(type_coordinate_array_dp) :: c

        c%x = array_coord%x - scalar
        c%y = array_coord%y - scalar
        c%z = array_coord%z - scalar
    end function subtract_scalar_2_coordinate_array_dp
    !>
    !> Performs element-wise multiplication of two coordinate arrays.
    !>
    function multiply_coordinate_array_dp(a, b) result(c)
        implicit none
        !> The first coordinate array.
        type(type_coordinate_array_dp), intent(in) :: a
        !> The second coordinate array.
        type(type_coordinate_array_dp), intent(in) :: b
        !> The resulting coordinate array.
        type(type_coordinate_array_dp) :: c

        c%x = a%x * b%x
        c%y = a%y * b%y
        c%z = a%z * b%z
    end function multiply_coordinate_array_dp
    !>
    !> Multiplies each coordinate component in an array by a scalar.
    !>
    function multiply_scalar_1_coordinate_array_dp(scalar, array_coord) result(c)
        implicit none
        !> The scalar value.
        real(real64), intent(in) :: scalar
        !> The coordinate array.
        type(type_coordinate_array_dp), intent(in) :: array_coord
        !> The resulting coordinate array.
        type(type_coordinate_array_dp) :: c

        c%x = scalar * array_coord%x
        c%y = scalar * array_coord%y
        c%z = scalar * array_coord%z
    end function multiply_scalar_1_coordinate_array_dp
    !>
    !> Multiplies each coordinate component in an array by a scalar.
    !>
    function multiply_scalar_2_coordinate_array_dp(array_coord, scalar) result(c)
        implicit none
        !> The coordinate array.
        type(type_coordinate_array_dp), intent(in) :: array_coord
        !> The scalar value.
        real(real64), intent(in) :: scalar
        !> The resulting coordinate array.
        type(type_coordinate_array_dp) :: c

        c%x = array_coord%x * scalar
        c%y = array_coord%y * scalar
        c%z = array_coord%z * scalar
    end function multiply_scalar_2_coordinate_array_dp
    !>
    !> Performs element-wise division of two coordinate arrays.
    !>
    function divide_coordinate_array_dp(a, b) result(c)
        implicit none
        !> The first coordinate array (numerator).
        type(type_coordinate_array_dp), intent(in) :: a
        !> The second coordinate array (denominator).
        type(type_coordinate_array_dp), intent(in) :: b
        !> The resulting coordinate array.
        type(type_coordinate_array_dp) :: c

#ifdef USE_DEBUG
        if (any(b%x == 0.0d0) .or. any(b%y == 0.0d0) .or. any(b%z == 0.0d0)) then
            write (*, '(A)') "ERROR in divide_coordinate_array_dp: Division by zero."
            error stop 1
        end if
#endif
        c%x = a%x / b%x
        c%y = a%y / b%y
        c%z = a%z / b%z
    end function divide_coordinate_array_dp
    !>
    !> Divides a scalar by each coordinate component in an array.
    !>
    function divide_scalar_1_coordinate_array_dp(scalar, array_coord) result(c)
        implicit none
        !> The scalar value (numerator).
        real(real64), intent(in) :: scalar
        !> The coordinate array (denominator).
        type(type_coordinate_array_dp), intent(in) :: array_coord
        !> The resulting coordinate array.
        type(type_coordinate_array_dp) :: c

#ifdef USE_DEBUG
        if (any(array_coord%x == 0.0d0) .or. any(array_coord%y == 0.0d0) .or. any(array_coord%z == 0.0d0)) then
            write (*, '(A)') "ERROR in divide_scalar_1_coordinate_array_dp: Division by zero."
            error stop 1
        end if
#endif
        c%x = scalar / array_coord%x
        c%y = scalar / array_coord%y
        c%z = scalar / array_coord%z
    end function divide_scalar_1_coordinate_array_dp
    !>
    !> Divides each coordinate component in an array by a scalar.
    !>
    function divide_scalar_2_coordinate_array_dp(array_coord, scalar) result(c)
        implicit none
        !> The coordinate array (numerator).
        type(type_coordinate_array_dp), intent(in) :: array_coord
        !> The scalar value (denominator).
        real(real64), intent(in) :: scalar
        !> The resulting coordinate array.
        type(type_coordinate_array_dp) :: c

#ifdef USE_DEBUG
        if (scalar == 0.0d0) then
            write (*, '(A)') "ERROR in divide_scalar_2_coordinate_array_dp: Division by zero."
            error stop 1
        end if
#endif
        c%x = array_coord%x / scalar
        c%y = array_coord%y / scalar
        c%z = array_coord%z / scalar
    end function divide_scalar_2_coordinate_array_dp

    ! -------------------------------------------------------------------------
    ! 5d. Integer Coordinate Array Operators
    ! -------------------------------------------------------------------------
    !>
    !> Performs element-wise addition of two integer coordinate arrays.
    !>
    function add_coordinate_array_int(a, b) result(c)
        implicit none
        !> The first coordinate array.
        type(type_coordinate_array_int), intent(in) :: a
        !> The second coordinate array.
        type(type_coordinate_array_int), intent(in) :: b
        !> The resulting coordinate array.
        type(type_coordinate_array_int) :: c

        c%x = a%x + b%x
        c%y = a%y + b%y
        c%z = a%z + b%z
    end function add_coordinate_array_int
    !>
    !> Adds a scalar to each component of each coordinate in an array.
    !>
    function add_scalar_1_coordinate_array_int(scalar, array_coord) result(c)
        implicit none
        !> The scalar value.
        integer(int32), intent(in) :: scalar
        !> The coordinate array.
        type(type_coordinate_array_int), intent(in) :: array_coord
        !> The resulting coordinate array.
        type(type_coordinate_array_int) :: c

        c%x = scalar + array_coord%x
        c%y = scalar + array_coord%y
        c%z = scalar + array_coord%z
    end function add_scalar_1_coordinate_array_int
    !>
    !> Adds a scalar to each component of each coordinate in an array.
    !>
    function add_scalar_2_coordinate_array_int(array_coord, scalar) result(c)
        implicit none
        !> The coordinate array.
        type(type_coordinate_array_int), intent(in) :: array_coord
        !> The scalar value.
        integer(int32), intent(in) :: scalar
        !> The resulting coordinate array.
        type(type_coordinate_array_int) :: c

        c%x = array_coord%x + scalar
        c%y = array_coord%y + scalar
        c%z = array_coord%z + scalar
    end function add_scalar_2_coordinate_array_int
    !>
    !> Performs element-wise subtraction of two integer coordinate arrays.
    !>
    function subtract_coordinate_array_int(a, b) result(c)
        implicit none
        !> The first coordinate array (minuend).
        type(type_coordinate_array_int), intent(in) :: a
        !> The second coordinate array (subtrahend).
        type(type_coordinate_array_int), intent(in) :: b
        !> The resulting coordinate array.
        type(type_coordinate_array_int) :: c

        c%x = a%x - b%x
        c%y = a%y - b%y
        c%z = a%z - b%z
    end function subtract_coordinate_array_int
    !>
    !> Subtracts each coordinate component in an array from a scalar.
    !>
    function subtract_scalar_1_coordinate_array_int(scalar, array_coord) result(c)
        implicit none
        !> The scalar value (minuend).
        integer(int32), intent(in) :: scalar
        !> The coordinate array (subtrahend).
        type(type_coordinate_array_int), intent(in) :: array_coord
        !> The resulting coordinate array.
        type(type_coordinate_array_int) :: c

        c%x = scalar - array_coord%x
        c%y = scalar - array_coord%y
        c%z = scalar - array_coord%z
    end function subtract_scalar_1_coordinate_array_int
    !>
    !> Subtracts a scalar from each coordinate component in an array.
    !>
    function subtract_scalar_2_coordinate_array_int(array_coord, scalar) result(c)
        implicit none
        !> The coordinate array (minuend).
        type(type_coordinate_array_int), intent(in) :: array_coord
        !> The scalar value (subtrahend).
        integer(int32), intent(in) :: scalar
        !> The resulting coordinate array.
        type(type_coordinate_array_int) :: c

        c%x = array_coord%x - scalar
        c%y = array_coord%y - scalar
        c%z = array_coord%z - scalar
    end function subtract_scalar_2_coordinate_array_int
    !>
    !> Performs element-wise multiplication of two integer coordinate arrays.
    !>
    function multiply_coordinate_array_int(a, b) result(c)
        implicit none
        !> The first coordinate array.
        type(type_coordinate_array_int), intent(in) :: a
        !> The second coordinate array.
        type(type_coordinate_array_int), intent(in) :: b
        !> The resulting coordinate array.
        type(type_coordinate_array_int) :: c

        c%x = a%x * b%x
        c%y = a%y * b%y
        c%z = a%z * b%z
    end function multiply_coordinate_array_int
    !>
    !> Multiplies each coordinate component in an array by a scalar.
    !>
    function multiply_scalar_1_coordinate_array_int(scalar, array_coord) result(c)
        implicit none
        !> The scalar value.
        integer(int32), intent(in) :: scalar
        !> The coordinate array.
        type(type_coordinate_array_int), intent(in) :: array_coord
        !> The resulting coordinate array.
        type(type_coordinate_array_int) :: c

        c%x = scalar * array_coord%x
        c%y = scalar * array_coord%y
        c%z = scalar * array_coord%z
    end function multiply_scalar_1_coordinate_array_int
    !>
    !> Multiplies each coordinate component in an array by a scalar.
    !>
    function multiply_scalar_2_coordinate_array_int(array_coord, scalar) result(c)
        implicit none
        !> The coordinate array.
        type(type_coordinate_array_int), intent(in) :: array_coord
        !> The scalar value.
        integer(int32), intent(in) :: scalar
        !> The resulting coordinate array.
        type(type_coordinate_array_int) :: c

        c%x = array_coord%x * scalar
        c%y = array_coord%y * scalar
        c%z = array_coord%z * scalar
    end function multiply_scalar_2_coordinate_array_int

    ! =========================================================================
    ! 6. Public Subroutines for Vector Arithmetic
    ! =========================================================================

    ! -------------------------------------------------------------------------
    ! 6a. Double Precision Vector Arithmetic Subroutines
    ! -------------------------------------------------------------------------
    !>
    !> Performs element-wise addition of two vectors: c = a + b.
    !>
    subroutine add_vector_dp(a, b, c)
        implicit none
        !> The first input vector.
        class(type_vector_dp), intent(in) :: a
        !> The second input vector.
        class(type_vector_dp), intent(in) :: b
        !> The output vector to store the result.
        class(type_vector_dp), intent(inout) :: c
        real(real64), dimension(:), pointer :: ptr_a, ptr_b, ptr_c

        ptr_a => a%get_data()
        ptr_b => b%get_data()
        ptr_c => c%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_a, ptr_b, 'add_vector_dp')
        call check_match_length(ptr_a, ptr_c, 'add_vector_dp')
#endif
        ptr_c = ptr_a + ptr_b
    end subroutine add_vector_dp
    !>
    !> Adds a scalar to each element of a vector: result = scalar + vector.
    !>
    subroutine add_scalar_1_vector_dp(scalar, vector, result_vec)
        implicit none
        !> The scalar value to add.
        real(real64), intent(in) :: scalar
        !> The input vector.
        class(type_vector_dp), intent(in) :: vector
        !> The output vector to store the result.
        class(type_vector_dp), intent(inout) :: result_vec
        real(real64), dimension(:), pointer :: ptr_v, ptr_r

        ptr_v => vector%get_data()
        ptr_r => result_vec%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_v, ptr_r, 'add_scalar_1_vector_dp')
#endif
        ptr_r = scalar + ptr_v
    end subroutine add_scalar_1_vector_dp
    !>
    !> Adds a scalar to each element of a vector: result = vector + scalar.
    !>
    subroutine add_scalar_2_vector_dp(vector, scalar, result_vec)
        implicit none
        !> The input vector.
        class(type_vector_dp), intent(in) :: vector
        !> The scalar value to add.
        real(real64), intent(in) :: scalar
        !> The output vector to store the result.
        class(type_vector_dp), intent(inout) :: result_vec
        real(real64), dimension(:), pointer :: ptr_v, ptr_r

        ptr_v => vector%get_data()
        ptr_r => result_vec%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_v, ptr_r, 'add_scalar_2_vector_dp')
#endif
        ptr_r = ptr_v + scalar
    end subroutine add_scalar_2_vector_dp
    !>
    !> Performs element-wise subtraction of two vectors: c = a - b.
    !>
    subroutine subtract_vector_dp(a, b, c)
        implicit none
        !> The first input vector (minuend).
        class(type_vector_dp), intent(in) :: a
        !> The second input vector (subtrahend).
        class(type_vector_dp), intent(in) :: b
        !> The output vector to store the result.
        class(type_vector_dp), intent(inout) :: c
        real(real64), dimension(:), pointer :: ptr_a, ptr_b, ptr_c

        ptr_a => a%get_data()
        ptr_b => b%get_data()
        ptr_c => c%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_a, ptr_b, 'subtract_vector_dp')
        call check_match_length(ptr_a, ptr_c, 'subtract_vector_dp')
#endif
        ptr_c = ptr_a - ptr_b
    end subroutine subtract_vector_dp
    !>
    !> Subtracts each element of a vector from a scalar: result = scalar - vector.
    !>
    subroutine subtract_scalar_1_vector_dp(scalar, vector, result_vec)
        implicit none
        !> The scalar value (minuend).
        real(real64), intent(in) :: scalar
        !> The input vector (subtrahend).
        class(type_vector_dp), intent(in) :: vector
        !> The output vector to store the result.
        class(type_vector_dp), intent(inout) :: result_vec
        real(real64), dimension(:), pointer :: ptr_v, ptr_r

        ptr_v => vector%get_data()
        ptr_r => result_vec%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_v, ptr_r, 'subtract_scalar_1_vector_dp')
#endif
        ptr_r = scalar - ptr_v
    end subroutine subtract_scalar_1_vector_dp
    !>
    !> Subtracts a scalar from each element of a vector: result = vector - scalar.
    !>
    subroutine subtract_scalar_2_vector_dp(vector, scalar, result_vec)
        implicit none
        !> The input vector (minuend).
        class(type_vector_dp), intent(in) :: vector
        !> The scalar value (subtrahend).
        real(real64), intent(in) :: scalar
        !> The output vector to store the result.
        class(type_vector_dp), intent(inout) :: result_vec
        real(real64), dimension(:), pointer :: ptr_v, ptr_r

        ptr_v => vector%get_data()
        ptr_r => result_vec%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_v, ptr_r, 'subtract_scalar_2_vector_dp')
#endif
        ptr_r = ptr_v - scalar
    end subroutine subtract_scalar_2_vector_dp
    !>
    !> Performs element-wise multiplication of two vectors: c = a * b.
    !>
    subroutine multiply_vector_dp(a, b, c)
        implicit none
        !> The first input vector.
        class(type_vector_dp), intent(in) :: a
        !> The second input vector.
        class(type_vector_dp), intent(in) :: b
        !> The output vector to store the result.
        class(type_vector_dp), intent(inout) :: c
        real(real64), dimension(:), pointer :: ptr_a, ptr_b, ptr_c

        ptr_a => a%get_data()
        ptr_b => b%get_data()
        ptr_c => c%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_a, ptr_b, 'multiply_vector_dp')
        call check_match_length(ptr_a, ptr_c, 'multiply_vector_dp')
#endif
        ptr_c = ptr_a * ptr_b
    end subroutine multiply_vector_dp
    !>
    !> Multiplies each element of a vector by a scalar: result = scalar * vector.
    !>
    subroutine multiply_scalar_1_vector_dp(scalar, vector, result_vec)
        implicit none
        !> The scalar value to multiply by.
        real(real64), intent(in) :: scalar
        !> The input vector.
        class(type_vector_dp), intent(in) :: vector
        !> The output vector to store the result.
        class(type_vector_dp), intent(inout) :: result_vec
        real(real64), dimension(:), pointer :: ptr_v, ptr_r

        ptr_v => vector%get_data()
        ptr_r => result_vec%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_v, ptr_r, 'multiply_scalar_1_vector_dp')
#endif
        ptr_r = scalar * ptr_v
    end subroutine multiply_scalar_1_vector_dp
    !>
    !> Multiplies each element of a vector by a scalar: result = vector * scalar.
    !>
    subroutine multiply_scalar_2_vector_dp(vector, scalar, result_vec)
        implicit none
        !> The input vector.
        class(type_vector_dp), intent(in) :: vector
        !> The scalar value to multiply by.
        real(real64), intent(in) :: scalar
        !> The output vector to store the result.
        class(type_vector_dp), intent(inout) :: result_vec
        real(real64), dimension(:), pointer :: ptr_v, ptr_r

        ptr_v => vector%get_data()
        ptr_r => result_vec%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_v, ptr_r, 'multiply_scalar_2_vector_dp')
#endif
        ptr_r = ptr_v * scalar
    end subroutine multiply_scalar_2_vector_dp
    !>
    !> Performs element-wise division of two vectors: c = a / b.
    !>
    subroutine divide_vector_dp(a, b, c)
        implicit none
        !> The first input vector (numerator).
        class(type_vector_dp), intent(in) :: a
        !> The second input vector (denominator).
        class(type_vector_dp), intent(in) :: b
        !> The output vector to store the result.
        class(type_vector_dp), intent(inout) :: c
        real(real64), dimension(:), pointer :: ptr_a, ptr_b, ptr_c

        ptr_a => a%get_data()
        ptr_b => b%get_data()
        ptr_c => c%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_a, ptr_b, 'divide_vector_dp')
        call check_match_length(ptr_a, ptr_c, 'divide_vector_dp')
        if (any(ptr_b == 0.0d0)) error stop "ERROR in divide_vector_dp: Division by zero."
#endif
        ptr_c = ptr_a / ptr_b
    end subroutine divide_vector_dp
    !>
    !> Divides a scalar by each element of a vector: result = scalar / vector.
    !>
    subroutine divide_scalar_1_vector_dp(scalar, vector, result_vec)
        implicit none
        !> The scalar value (numerator).
        real(real64), intent(in) :: scalar
        !> The input vector (denominator).
        class(type_vector_dp), intent(in) :: vector
        !> The output vector to store the result.
        class(type_vector_dp), intent(inout) :: result_vec
        real(real64), dimension(:), pointer :: ptr_v, ptr_r

        ptr_v => vector%get_data()
        ptr_r => result_vec%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_v, ptr_r, 'divide_scalar_1_vector_dp')
        if (any(ptr_v == 0.0d0)) error stop "ERROR in divide_scalar_1_vector_dp: Division by zero."
#endif
        ptr_r = scalar / ptr_v
    end subroutine divide_scalar_1_vector_dp
    !>
    !> Divides each element of a vector by a scalar: result = vector / scalar.
    !>
    subroutine divide_scalar_2_vector_dp(vector, scalar, result_vec)
        implicit none
        !> The input vector (numerator).
        class(type_vector_dp), intent(in) :: vector
        !> The scalar value (denominator).
        real(real64), intent(in) :: scalar
        !> The output vector to store the result.
        class(type_vector_dp), intent(inout) :: result_vec
        real(real64), dimension(:), pointer :: ptr_v, ptr_r

        ptr_v => vector%get_data()
        ptr_r => result_vec%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_v, ptr_r, 'divide_scalar_2_vector_dp')
        if (scalar == 0.0d0) error stop "ERROR in divide_scalar_2_vector_dp: Division by zero."
#endif
        ptr_r = ptr_v / scalar
    end subroutine divide_scalar_2_vector_dp

    ! -------------------------------------------------------------------------
    ! 6b. Integer Vector Arithmetic Subroutines
    ! -------------------------------------------------------------------------
    !>
    !> Performs element-wise addition of two integer vectors: c = a + b.
    !>
    subroutine add_vector_int(a, b, c)
        implicit none
        !> The first input vector.
        class(type_vector_int), intent(in) :: a
        !> The second input vector.
        class(type_vector_int), intent(in) :: b
        !> The output vector to store the result.
        class(type_vector_int), intent(inout) :: c

        integer(int32), dimension(:), pointer :: ptr_a, ptr_b, ptr_c

        ptr_a => a%get_data()
        ptr_b => b%get_data()
        ptr_c => c%get_data()

#ifdef USE_DEBUG
        call check_match_length(ptr_a, ptr_b, 'add_vector_int')
        call check_match_length(ptr_a, ptr_c, 'add_vector_int')
#endif

        ptr_c = ptr_a + ptr_b
    end subroutine add_vector_int
    !>
    !> Adds a scalar to each element of a vector: result = scalar + vector.
    !>
    subroutine add_scalar_1_vector_int(scalar, vector, result_vec)
        implicit none
        !> The scalar value to add.
        integer(int32), intent(in) :: scalar
        !> The input vector.
        class(type_vector_int), intent(in) :: vector
        !> The output vector to store the result.
        class(type_vector_int), intent(inout) :: result_vec

        integer(int32), dimension(:), pointer :: ptr_v, ptr_r

        ptr_v => vector%get_data()
        ptr_r => result_vec%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_v, ptr_r, 'add_scalar_1_vector_int')
#endif
        ptr_r = scalar + ptr_v
    end subroutine add_scalar_1_vector_int
    !>
    !> Adds a scalar to each element of a vector: result = vector + scalar.
    !>
    subroutine add_scalar_2_vector_int(vector, scalar, result_vec)
        implicit none
        !> The input vector.
        class(type_vector_int), intent(in) :: vector
        !> The scalar value to add.
        integer(int32), intent(in) :: scalar
        !> The output vector to store the result.
        class(type_vector_int), intent(inout) :: result_vec
        integer(int32), dimension(:), pointer :: ptr_v, ptr_r

        ptr_v => vector%get_data()
        ptr_r => result_vec%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_v, ptr_r, 'add_scalar_2_vector_int')
#endif
        ptr_r = ptr_v + scalar
    end subroutine add_scalar_2_vector_int
    !>
    !> Performs element-wise subtraction of two integer vectors: c = a - b.
    !>
    subroutine subtract_vector_int(a, b, c)
        implicit none
        !> The first input vector (minuend).
        class(type_vector_int), intent(in) :: a
        !> The second input vector (subtrahend).
        class(type_vector_int), intent(in) :: b
        !> The output vector to store the result.
        class(type_vector_int), intent(inout) :: c
        integer(int32), dimension(:), pointer :: ptr_a, ptr_b, ptr_c

        ptr_a => a%get_data()
        ptr_b => b%get_data()
        ptr_c => c%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_a, ptr_b, 'subtract_vector_int')
        call check_match_length(ptr_a, ptr_c, 'subtract_vector_int')
#endif
        ptr_c = ptr_a - ptr_b
    end subroutine subtract_vector_int
    !>
    !> Subtracts each element of a vector from a scalar: result = scalar - vector.
    !>
    subroutine subtract_scalar_1_vector_int(scalar, vector, result_vec)
        implicit none
        !> The scalar value (minuend).
        integer(int32), intent(in) :: scalar
        !> The input vector (subtrahend).
        class(type_vector_int), intent(in) :: vector
        !> The output vector to store the result.
        class(type_vector_int), intent(inout) :: result_vec
        integer(int32), dimension(:), pointer :: ptr_v, ptr_r

        ptr_v => vector%get_data()
        ptr_r => result_vec%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_v, ptr_r, 'subtract_scalar_1_vector_int')
#endif
        ptr_r = scalar - ptr_v
    end subroutine subtract_scalar_1_vector_int
    !>
    !> Subtracts a scalar from each element of a vector: result = vector - scalar.
    !>
    subroutine subtract_scalar_2_vector_int(vector, scalar, result_vec)
        implicit none
        !> The input vector (minuend).
        class(type_vector_int), intent(in) :: vector
        !> The scalar value (subtrahend).
        integer(int32), intent(in) :: scalar
        !> The output vector to store the result.
        class(type_vector_int), intent(inout) :: result_vec
        integer(int32), dimension(:), pointer :: ptr_v, ptr_r

        ptr_v => vector%get_data()
        ptr_r => result_vec%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_v, ptr_r, 'subtract_scalar_2_vector_int')
#endif
        ptr_r = ptr_v - scalar
    end subroutine subtract_scalar_2_vector_int
    !>
    !> Performs element-wise multiplication of two integer vectors: c = a * b.
    !>
    subroutine multiply_vector_int(a, b, c)
        implicit none
        !> The first input vector.
        class(type_vector_int), intent(in) :: a
        !> The second input vector.
        class(type_vector_int), intent(in) :: b
        !> The output vector to store the result.
        class(type_vector_int), intent(inout) :: c
        integer(int32), dimension(:), pointer :: ptr_a, ptr_b, ptr_c

        ptr_a => a%get_data()
        ptr_b => b%get_data()
        ptr_c => c%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_a, ptr_b, 'multiply_vector_int')
        call check_match_length(ptr_a, ptr_c, 'multiply_vector_int')
#endif
        ptr_c = ptr_a * ptr_b
    end subroutine multiply_vector_int
    !>
    !> Multiplies each element of a vector by a scalar: result = scalar * vector.
    !>
    subroutine multiply_scalar_1_vector_int(scalar, vector, result_vec)
        implicit none
        !> The scalar value to multiply by.
        integer(int32), intent(in) :: scalar
        !> The input vector.
        class(type_vector_int), intent(in) :: vector
        !> The output vector to store the result.
        class(type_vector_int), intent(inout) :: result_vec
        integer(int32), dimension(:), pointer :: ptr_v, ptr_r

        ptr_v => vector%get_data()
        ptr_r => result_vec%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_v, ptr_r, 'multiply_scalar_1_vector_int')
#endif
        ptr_r = scalar * ptr_v
    end subroutine multiply_scalar_1_vector_int
    !>
    !> Multiplies each element of a vector by a scalar: result = vector * scalar.
    !>
    subroutine multiply_scalar_2_vector_int(vector, scalar, result_vec)
        implicit none
        !> The input vector.
        class(type_vector_int), intent(in) :: vector
        !> The scalar value to multiply by.
        integer(int32), intent(in) :: scalar
        !> The output vector to store the result.
        class(type_vector_int), intent(inout) :: result_vec
        integer(int32), dimension(:), pointer :: ptr_v, ptr_r

        ptr_v => vector%get_data()
        ptr_r => result_vec%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_v, ptr_r, 'multiply_scalar_2_vector_int')
#endif
        ptr_r = ptr_v * scalar
    end subroutine multiply_scalar_2_vector_int

    ! =========================================================================
    ! 7. Public Assignment Subroutines
    ! =========================================================================
    !>
    !> Overloads the assignment operator (=) for the double precision coordinate type.
    !>
    subroutine assign_coordinate_dp(lhs, rhs)
        implicit none
        !> The destination object (left-hand side).
        type(type_coordinate_dp), intent(inout) :: lhs
        !> The source object (right-hand side).
        type(type_coordinate_dp), intent(in) :: rhs

        lhs%x = rhs%x
        lhs%y = rhs%y
        lhs%z = rhs%z
    end subroutine assign_coordinate_dp

    !>
    !> Overloads the assignment operator (=) for the integer coordinate type.
    !>
    subroutine assign_coordinate_int(lhs, rhs)
        implicit none
        !> The destination object (left-hand side).
        type(type_coordinate_int), intent(inout) :: lhs
        !> The source object (right-hand side).
        type(type_coordinate_int), intent(in) :: rhs

        lhs%x = rhs%x
        lhs%y = rhs%y
        lhs%z = rhs%z
    end subroutine assign_coordinate_int

    !>
    !> Overloads the assignment operator (=) for the double precision coordinate array type.
    !>
    subroutine assign_coordinate_array_dp(lhs, rhs)
        implicit none
        !> The destination object (left-hand side).
        type(type_coordinate_array_dp), intent(inout) :: lhs
        !> The source object (right-hand side).
        type(type_coordinate_array_dp), intent(in) :: rhs

        lhs%x = rhs%x
        lhs%y = rhs%y
        lhs%z = rhs%z
    end subroutine assign_coordinate_array_dp

    !>
    !> Overloads the assignment operator (=) for the integer coordinate array type.
    !>
    subroutine assign_coordinate_array_int(lhs, rhs)
        implicit none
        !> The destination object (left-hand side).
        type(type_coordinate_array_int), intent(inout) :: lhs
        !> The source object (right-hand side).
        type(type_coordinate_array_int), intent(in) :: rhs

        lhs%x = rhs%x
        lhs%y = rhs%y
        lhs%z = rhs%z
    end subroutine assign_coordinate_array_int

    !>
    !> Overloads the assignment operator (=) for the double precision vector type.
    !>
    subroutine assign_vector_dp(lhs, rhs)
        implicit none
        !> The destination vector (left-hand side).
        class(type_vector_dp), intent(inout) :: lhs
        !> The source vector (right-hand side).
        class(type_vector_dp), intent(in) :: rhs

        real(real64), dimension(:), pointer :: ptr_rhs

        if (.not. rhs%is_initialized()) then
            error stop "ERROR in assign_vector_dp: RHS vector is not initialized."
        end if

        ptr_rhs => rhs%get_data()
        call lhs%set(VECTOR_OPS%INS, ptr_rhs)
    end subroutine assign_vector_dp

    !>
    !> Overloads the assignment operator (=) for the integer vector type.
    !>
    subroutine assign_vector_int(lhs, rhs)
        implicit none
        !> The destination vector (left-hand side).
        class(type_vector_int), intent(inout) :: lhs
        !> The source vector (right-hand side).
        class(type_vector_int), intent(in) :: rhs

        integer(int32), dimension(:), pointer :: ptr_rhs

        if (.not. rhs%is_initialized()) then
            error stop "ERROR in assign_vector_int: RHS vector is not initialized."
        end if
        ptr_rhs => rhs%get_data()

        call lhs%set(VECTOR_OPS%INS, ptr_rhs)
    end subroutine assign_vector_int

    subroutine axpy_vector_dp(alpha, x, y)
        implicit none
        !> The scalar multiplier.
        real(real64), intent(in) :: alpha
        !> The input vector x.
        class(type_vector_dp), intent(in) :: x
        !> The output vector y to store the result.
        class(type_vector_dp), intent(inout) :: y

        real(real64), dimension(:), pointer :: ptr_x, ptr_y

        ptr_x => x%get_data()
        ptr_y => y%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_x, ptr_y, 'axpy_vector_dp')
#endif
        if (.not. is_mkl_initialized) call initialize_mkl_backend()
        call compute_axpy_backend(alpha, ptr_x, ptr_y)
    end subroutine axpy_vector_dp

    subroutine axpy_vector_int(alpha, x, y)
        implicit none
        !> The scalar multiplier.
        integer(int32), intent(in) :: alpha
        !> The input vector x.
        class(type_vector_int), intent(in) :: x
        !> The output vector y to store the result.
        class(type_vector_int), intent(inout) :: y

        integer(int32), dimension(:), pointer :: ptr_x, ptr_y

        ptr_x => x%get_data()
        ptr_y => y%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_x, ptr_y, 'axpy_vector_int')
#endif
        ptr_y = ptr_y + alpha * ptr_x
    end subroutine axpy_vector_int

    subroutine xpay_vector_dp(alpha, x, y)
        implicit none
        !> The scalar multiplier.
        real(real64), intent(in) :: alpha
        !> The input vector x.
        class(type_vector_dp), intent(in) :: x
        !> The output vector y to store the result.
        class(type_vector_dp), intent(inout) :: y

        real(real64), dimension(:), pointer :: ptr_x, ptr_y

        ptr_x => x%get_data()
        ptr_y => y%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_x, ptr_y, 'xpay_vector_dp')
#endif
        ! Current semantics: y <- alpha*x + y (identical to AXPY). Route through MKL backend.
        if (.not. is_mkl_initialized) call initialize_mkl_backend()
        call compute_axpy_backend(alpha, ptr_x, ptr_y)
    end subroutine xpay_vector_dp

    subroutine xpay_vector_int(alpha, x, y)
        implicit none
        !> The scalar multiplier.
        integer(int32), intent(in) :: alpha
        !> The input vector x.
        class(type_vector_int), intent(in) :: x
        !> The output vector y to store the result.
        class(type_vector_int), intent(inout) :: y

        integer(int32), dimension(:), pointer :: ptr_x, ptr_y

        ptr_x => x%get_data()
        ptr_y => y%get_data()
#ifdef USE_DEBUG
        call check_match_length(ptr_x, ptr_y, 'xpay_vector_int')
#endif
        ptr_y = alpha * ptr_x + ptr_y
    end subroutine xpay_vector_int

    subroutine axpyz_vector_dp(alpha, x, y, z)
        implicit none
        !> The scalar multiplier.
        real(real64), intent(in) :: alpha
        !> The input vector x.
        class(type_vector_dp), intent(in) :: x
        !> The input vector y.
        class(type_vector_dp), intent(in) :: y
        !> The output vector z to store the result.
        class(type_vector_dp), intent(inout) :: z

        real(real64), dimension(:), pointer :: ptr_x, ptr_y, ptr_z

        ptr_x => x%get_data()
        ptr_y => y%get_data()
        ptr_z => z%get_data()

        if (.not. associated(ptr_x) .or. .not. associated(ptr_y)) then
            error stop "axpyz_vector_dp: input vector data is not initialized"
        end if
        if (.not. associated(ptr_z)) then
            call z%initialize(size(ptr_y))
            ptr_z => z%get_data()
        end if

#ifdef USE_DEBUG
        call check_match_length(ptr_x, ptr_y, 'axpyz_vector_dp')
        call check_match_length(ptr_x, ptr_z, 'axpyz_vector_dp')
#endif
        ptr_z = ptr_y + alpha * ptr_x
    end subroutine axpyz_vector_dp

    subroutine axpyz_vector_int(alpha, x, y, z)
        implicit none
        !> The scalar multiplier.
        integer(int32), intent(in) :: alpha
        !> The input vector x.
        class(type_vector_int), intent(in) :: x
        !> The input vector y.
        class(type_vector_int), intent(in) :: y
        !> The output vector z to store the result.
        class(type_vector_int), intent(inout) :: z

        integer(int32), dimension(:), pointer :: ptr_x, ptr_y, ptr_z

        ptr_x => x%get_data()
        ptr_y => y%get_data()
        ptr_z => z%get_data()

        if (.not. associated(ptr_x) .or. .not. associated(ptr_y)) then
            error stop "axpyz_vector_int: input vector data is not initialized"
        end if
        if (.not. associated(ptr_z)) then
            call z%initialize(size(ptr_y))
            ptr_z => z%get_data()
        end if

#ifdef USE_DEBUG
        call check_match_length(ptr_x, ptr_y, 'axpyz_vector_int')
        call check_match_length(ptr_x, ptr_z, 'axpyz_vector_int')
#endif
        ptr_z = ptr_y + alpha * ptr_x
    end subroutine axpyz_vector_int

    subroutine scale_vector_dp(alpha, x)
        implicit none
        !> The scalar multiplier.
        real(real64), intent(in) :: alpha
        !> The input/output vector x to be scaled.
        class(type_vector_dp), intent(inout) :: x

        real(real64), dimension(:), pointer :: ptr_x

        ptr_x => x%get_data()

        ptr_x = alpha * ptr_x
    end subroutine scale_vector_dp

    subroutine scale_vector_int(a, x)
        implicit none
        !> The scalar multiplier.
        integer(int32), intent(in) :: a
        !> The input/output vector x to be scaled.
        class(type_vector_int), intent(inout) :: x

        integer(int32), dimension(:), pointer :: ptr_x

        ptr_x => x%get_data()

        ptr_x = a * ptr_x

    end subroutine scale_vector_int

    subroutine abs_vector_dp(x)
        implicit none
        !> The input/output vector x to be modified.
        class(type_vector_dp), intent(inout) :: x

        real(real64), dimension(:), pointer :: ptr_x

        ptr_x => x%get_data()

        ptr_x = abs(ptr_x)

    end subroutine abs_vector_dp

    subroutine abs_vector_int(x)
        implicit none
        !> The input/output vector x to be modified.
        class(type_vector_int), intent(inout) :: x

        integer(int32), dimension(:), pointer :: ptr_x

        ptr_x => x%get_data()

        ptr_x = abs(ptr_x)

    end subroutine abs_vector_int

    subroutine reciprocal_vector_dp(x)
        implicit none
        !> The input/output vector x to be modified.
        class(type_vector_dp), intent(inout) :: x

        real(real64), dimension(:), pointer :: ptr_x
        real(real64), parameter :: epsilon = 1.0d-20

        ptr_x => x%get_data()

        where (abs(ptr_x) < epsilon)
            ptr_x = 0.0d0
        else where
            ptr_x = 1.0d0 / ptr_x
        end where

    end subroutine reciprocal_vector_dp

    subroutine reciprocal_vector_int(x)
        implicit none
        !> The input/output vector x to be modified.
        class(type_vector_int), intent(inout) :: x

        integer(int32), dimension(:), pointer :: ptr_x

        ptr_x => x%get_data()

        where (ptr_x == 0)
            ptr_x = 0
        else where
            ptr_x = 1 / ptr_x
        end where

    end subroutine reciprocal_vector_int

    subroutine shift_vector_dp(shift_amount, x)
        implicit none
        !> The amount to shift by.
        integer(int32), intent(in) :: shift_amount
        !> The input/output vector x to be shifted.
        class(type_vector_dp), intent(inout) :: x

        real(real64), dimension(:), pointer :: ptr_x

        ptr_x => x%get_data()

        ptr_x = ptr_x - shift_amount

    end subroutine shift_vector_dp

    subroutine shift_vector_int(shift_amount, x)
        implicit none
        !> The amount to shift by.
        integer(int32), intent(in) :: shift_amount
        !> The input/output vector x to be shifted.
        class(type_vector_int), intent(inout) :: x

        integer(int32), dimension(:), pointer :: ptr_x

        ptr_x => x%get_data()

        ptr_x = ptr_x - shift_amount

    end subroutine shift_vector_int

end module linalg_vector_operations
