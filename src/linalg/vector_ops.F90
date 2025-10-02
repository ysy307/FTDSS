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
module linalg_vector_ops
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: mpi_f08
    use :: module_core
    use :: linalg_vector
    use :: linalg_mkl_backend

    implicit none
    private

    ! =========================================================================
    ! 1. Public Interface (API)
    ! =========================================================================
    public :: initialize_linalg
    public :: norm_1
    public :: norm_2
    public :: norm_inf
    public :: dot

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

    ! =========================================================================
    ! 2. Private Helper Interfaces and Pointers
    ! =========================================================================

    interface check_sizes_match
        module procedure :: check_sizes_match_dp
        module procedure :: check_sizes_match_int
    end interface

    abstract interface
        function abst_real_from_one_vector_function(vector)
            import :: real64
            implicit none
            real(real64), intent(in) :: vector(:)
            real(real64) :: abst_real_from_one_vector_function
        end function

        function abst_real_from_two_vectors_function(vector_a, vector_b)
            import :: real64
            implicit none
            real(real64), intent(in) :: vector_a(:)
            real(real64), intent(in) :: vector_b(:)
            real(real64) :: abst_real_from_two_vectors_function
        end function

        function abst_real_from_vector_for_inf_norm_function(vector)
            import :: real64
            implicit none
            real(real64), intent(in) :: vector(:)
            real(real64) :: abst_real_from_vector_for_inf_norm_function
        end function
    end interface

    !> Procedure pointer for the backend implementation of the 1-norm.
    procedure(abst_real_from_one_vector_function), pointer, private :: compute_norm_1_backend => null()
    !> Procedure pointer for the backend implementation of the 2-norm.
    procedure(abst_real_from_one_vector_function), pointer, private :: compute_norm_2_backend => null()
    !> Procedure pointer for the backend implementation of the infinity-norm.
    procedure(abst_real_from_vector_for_inf_norm_function), pointer, private :: compute_norm_inf_backend => null()
    !> Procedure pointer for the backend implementation of the dot product.
    procedure(abst_real_from_two_vectors_function), pointer, private :: compute_dot_product_backend => null()

    !> Flag to ensure the backend pointers are initialized only once.
    logical, private :: is_mkl_initialized = .false.

contains

    ! =========================================================================
    ! 3. Private Helper Subroutines
    ! =========================================================================

    !>
    !> Checks if two double precision arrays have the same size.
    !>
    subroutine check_sizes_match_dp(a, b, routine_name)
        implicit none
        !> The first array.
        real(real64), intent(in) :: a(:)
        !> The second array.
        real(real64), intent(in) :: b(:)
        !> The name of the calling routine for error messages.
        character(len=*), intent(in) :: routine_name

        if (size(a) /= size(b)) then
            write (*, '(A,A,A)') "ERROR in ", trim(routine_name), ": Array sizes do not match."
            error stop 1
        end if
    end subroutine check_sizes_match_dp

    !>
    !> Checks if two integer arrays have the same size.
    !>
    subroutine check_sizes_match_int(a, b, routine_name)
        implicit none
        !> The first array.
        integer(int32), intent(in) :: a(:)
        !> The second array.
        integer(int32), intent(in) :: b(:)
        !> The name of the calling routine for error messages.
        character(len=*), intent(in) :: routine_name

        if (size(a) /= size(b)) then
            write (*, '(A,A,A)') "ERROR in ", trim(routine_name), ": Array sizes do not match."
            error stop 1
        end if
    end subroutine check_sizes_match_int

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
#else
        compute_norm_1_backend => norm_1_native
        compute_norm_2_backend => norm_2_native
        compute_norm_inf_backend => norm_inf_native
        compute_dot_product_backend => dot_native
#endif
        is_mkl_initialized = .true.
    end subroutine initialize_mkl_backend

    ! =========================================================================
    ! 4. Public Norms and Dot Product
    ! =========================================================================

    !>
    !> Computes the 1-norm of a vector, \( \sum |x_i| \), using the initialized backend.
    !>
    function norm_1(x) result(norm_value)
        implicit none
        !> The input vector.
        real(real64), intent(in) :: x(:)
        !> The computed 1-norm.
        real(real64) :: norm_value

#ifdef USE_DEBUG
        if (.not. is_mkl_initialized) call initialize_mkl_backend()
#endif
        norm_value = compute_norm_1_backend(x)
    end function norm_1

    !>
    !> Computes the 2-norm (Euclidean norm) of a vector, \( \sqrt{\sum x_i^2} \), using the initialized backend.
    !>
    function norm_2(x) result(norm_value)
        implicit none
        !> The input vector.
        real(real64), intent(in) :: x(:)
        !> The computed 2-norm.
        real(real64) :: norm_value

#ifdef USE_DEBUG
        if (.not. is_mkl_initialized) call initialize_mkl_backend()
#endif
        norm_value = compute_norm_2_backend(x)
    end function norm_2

    !>
    !> Computes the infinity-norm (maximum absolute value), \( \max(|x_i|) \), using the initialized backend.
    !>
    function norm_inf(x) result(norm_value)
        implicit none
        !> The input vector.
        real(real64), intent(in) :: x(:)
        !> The computed infinity-norm.
        real(real64) :: norm_value

#ifdef USE_DEBUG
        if (.not. is_mkl_initialized) call initialize_mkl_backend()
#endif
        norm_value = compute_norm_inf_backend(x)
    end function norm_inf

    !>
    !> Computes the dot product of two vectors, \( \sum x_i y_i \), using the initialized backend.
    !>
    function dot(x, y) result(product)
        implicit none
        !> The first input vector.
        real(real64), intent(in) :: x(:)
        !> The second input vector.
        real(real64), intent(in) :: y(:)
        !> The computed dot product.
        real(real64) :: product

        call check_sizes_match(x, y, 'dot')

#ifdef USE_DEBUG
        if (.not. is_mkl_initialized) call initialize_mkl_backend()
#endif
        product = compute_dot_product_backend(x, y)
    end function dot

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
        call check_sizes_match(ptr_a, ptr_b, 'add_vector_dp')
        call check_sizes_match(ptr_a, ptr_c, 'add_vector_dp')
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
        call check_sizes_match(ptr_v, ptr_r, 'add_scalar_1_vector_dp')
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
        call check_sizes_match(ptr_v, ptr_r, 'add_scalar_2_vector_dp')
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
        call check_sizes_match(ptr_a, ptr_b, 'subtract_vector_dp')
        call check_sizes_match(ptr_a, ptr_c, 'subtract_vector_dp')
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
        call check_sizes_match(ptr_v, ptr_r, 'subtract_scalar_1_vector_dp')
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
        call check_sizes_match(ptr_v, ptr_r, 'subtract_scalar_2_vector_dp')
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
        call check_sizes_match(ptr_a, ptr_b, 'multiply_vector_dp')
        call check_sizes_match(ptr_a, ptr_c, 'multiply_vector_dp')
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
        call check_sizes_match(ptr_v, ptr_r, 'multiply_scalar_1_vector_dp')
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
        call check_sizes_match(ptr_v, ptr_r, 'multiply_scalar_2_vector_dp')
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
        call check_sizes_match(ptr_a, ptr_b, 'divide_vector_dp')
        call check_sizes_match(ptr_a, ptr_c, 'divide_vector_dp')
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
        call check_sizes_match(ptr_v, ptr_r, 'divide_scalar_1_vector_dp')
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
        call check_sizes_match(ptr_v, ptr_r, 'divide_scalar_2_vector_dp')
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
        call check_sizes_match(ptr_a, ptr_b, 'add_vector_int')
        call check_sizes_match(ptr_a, ptr_c, 'add_vector_int')
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
        call check_sizes_match(ptr_v, ptr_r, 'add_scalar_1_vector_int')
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
        call check_sizes_match(ptr_v, ptr_r, 'add_scalar_2_vector_int')
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
        call check_sizes_match(ptr_a, ptr_b, 'subtract_vector_int')
        call check_sizes_match(ptr_a, ptr_c, 'subtract_vector_int')
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
        call check_sizes_match(ptr_v, ptr_r, 'subtract_scalar_1_vector_int')
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
        call check_sizes_match(ptr_v, ptr_r, 'subtract_scalar_2_vector_int')
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
        call check_sizes_match(ptr_a, ptr_b, 'multiply_vector_int')
        call check_sizes_match(ptr_a, ptr_c, 'multiply_vector_int')
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
        call check_sizes_match(ptr_v, ptr_r, 'multiply_scalar_1_vector_int')
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
        call check_sizes_match(ptr_v, ptr_r, 'multiply_scalar_2_vector_int')
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
        call lhs%set(ptr_rhs)
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

        call lhs%set(ptr_rhs)
    end subroutine assign_vector_int

end module linalg_vector_ops
