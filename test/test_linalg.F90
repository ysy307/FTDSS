program test_linalg
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
    use :: module_linalg ! Imports vector types and ops
    implicit none

    print *, "========================================"
    print *, "   Linear Algebra Test Suite"
    print *, "========================================"
    print *

    ! Initialize the linear algebra backend (MKL or Native)
    call initialize_linalg()

    call run_test_vector_basic_dp()
    print *
    ! call run_test_vector_ops_dp()

    print *
    print *, "========================================"
    print *, "   Linalg tests completed."
    print *, "========================================"

contains

    !>
    !> Basic functionality tests for type_vector_dp
    !> Creation, Setting, Getting, Copying
    !>
    subroutine run_test_vector_basic_dp()
        implicit none
        type(type_vector_dp) :: v, v_copy
        integer(int32), parameter :: N = 5
        real(real64), allocatable :: vals(:)
        integer(int32) :: i

        print *, "--- Testing Basic Vector Functions (DP) ---"

        !-------------------------------------------------------
        ! 1. Initialization
        !-------------------------------------------------------
        call v%initialize(N)
        if (v%get_size() == N) then
            print *, "PASS: Initialization (Size=", N, ")"
        else
            print *, "FAIL: Initialization size mismatch."
        end if

        !-------------------------------------------------------
        ! 2. Set Scalar
        !-------------------------------------------------------
        call v%set(OP_INS, 1.0d0)
        if (all(v%get_data() == 1.0d0)) then
            print *, "PASS: Set Scalar (1.0)"
        else
            print *, "FAIL: Set Scalar mismatch."
        end if

        !-------------------------------------------------------
        ! 3. Set Array
        !-------------------------------------------------------
        allocate (vals(N))
        vals = [(dble(i), i=1, N)] ! [1.0, 2.0, ..., 5.0]

        call v%set(OP_INS, vals)

        if (all(v%get_data() == vals)) then
            print *, "PASS: Set Array [1..5]"
        else
            print *, "FAIL: Set Array mismatch."
            call v%display()
        end if

        !-------------------------------------------------------
        ! 4. Set at Index & Scatter
        !-------------------------------------------------------
        call v%set(OP_INS, 1, 10.0d0) ! v[1] = 10
        call v%set(OP_INS, [2, 4], [-2.0d0, -4.0d0]) ! v[2] = -2, v[4] = -4

        if (all(v%get_data() == [10.0d0, -2.0d0, 3.0d0, -4.0d0, 5.0d0])) then
            print *, "PASS: Set at Index & Scatter"
        else
            print *, "FAIL: Set at Index & Scatter mismatch."
            call v%display()
        end if

        !-------------------------------------------------------
        ! 5. Copy
        !-------------------------------------------------------
        call v_copy%initialize(N)
        call v_copy%copy(v)

        if (all(v_copy%get_data() == v%get_data())) then
            print *, "PASS: Copy Vector"
        else
            print *, "FAIL: Copy Vector"
        end if

        !-------------------------------------------------------
        ! 6. Zero
        !-------------------------------------------------------
        call v_copy%zero()
        if (all(v_copy%get_data() == 0.0d0)) then
            print *, "PASS: Zero Vector"
        else
            print *, "FAIL: Zero Vector"
        end if

        call v%destroy()
        call v_copy%destroy()
    end subroutine run_test_vector_basic_dp

    !>
    !> Operation tests for type_vector_dp
    !> Arithmetic, Norms, Dot Product
    !>
    ! subroutine run_test_vector_ops_dp()
    !     type(type_vector_dp) :: v1, v2, res
    !     integer(int32), parameter :: N = 5
    !     real(real64) :: n1, n2, ninf, dot_val

    !     print *, "--- Testing Vector Operations (DP) ---"

    !     call v1%initialize(N)
    !     call v2%initialize(N)
    !     call res%initialize(N)

    !     ! Setup Data
    !     ! v1 = [1, -2, 3, -4, 5]
    !     call v1%set([1.0d0, -2.0d0, 3.0d0, -4.0d0, 5.0d0])
    !     ! v2 = [1, 1, 1, 1, 1]
    !     call v2%set(1.0d0)

    !     ! ---------------------------------------------------------
    !     ! Norms
    !     ! ---------------------------------------------------------
    !     n1 = vector_norm1(v1)
    !     n2 = vector_norm2(v1)
    !     ninf = vector_norminf(v1)

    !     print *, "Norm1 (Expect 15.0): ", n1
    !     print *, "Norm2 (Expect ~7.416): ", n2
    !     print *, "NormInf (Expect 5.0): ", ninf

    !     ! ---------------------------------------------------------
    !     ! Dot Product
    !     ! ---------------------------------------------------------
    !     ! v1 . v2 = 1-2+3-4+5 = 3
    !     dot_val = vector_dot(v1, v2)
    !     print *, "Dot Product (Expect 3.0): ", dot_val

    !     ! ---------------------------------------------------------
    !     ! Arithmetic Subroutines
    !     ! ---------------------------------------------------------
    !     ! Add
    !     call add(v1, v2, res) ! res = [2, -1, 4, -3, 6]
    !     print *, "Add (v1 + v2) [First elem expect 2.0]: ", res%get_data() (1)

    !     ! Subtract
    !     call subtract(v1, v2, res) ! res = [0, -3, 2, -5, 4]
    !     print *, "Subtract (v1 - v2) [First elem expect 0.0]: ", res%get_data() (1)

    !     ! Multiply (Element-wise)
    !     call multiply(v1, v2, res) ! res = [1, -2, 3, -4, 5]
    !     print *, "Multiply (v1 * v2) [First elem expect 1.0]: ", res%get_data() (1)

    !     ! Divide
    !     call divide(v1, v2, res) ! res = [1, -2, 3, -4, 5]
    !     print *, "Divide (v1 / v2) [First elem expect 1.0]: ", res%get_data() (1)

    !     ! ---------------------------------------------------------
    !     ! Scalar Operations (Subroutines)
    !     ! ---------------------------------------------------------
    !     call vector_scale(2.0d0, v2) ! v2 becomes [2, 2, 2, 2, 2]
    !     print *, "Scale (v2 * 2.0) [First elem expect 2.0]: ", v2%get_data() (1)

    !     ! AXPY: v2 = 2.0*v1 + v2
    !     ! v2 was [2...], v1 is [1, -2, 3, -4, 5]
    !     ! v2[1] = 2*1 + 2 = 4
    !     ! v2[2] = 2*(-2) + 2 = -2
    !     call vector_axpy(2.0d0, v1, v2)
    !     print *, "AXPY (2.0*v1 + v2) [Expect 4.0, -2.0...]:"
    !     call v2%display()

    !     ! ---------------------------------------------------------
    !     ! Utilities
    !     ! ---------------------------------------------------------
    !     call v1%set(-10.0d0)
    !     call vector_abs(v1)
    !     print *, "Abs (-10.0 -> 10.0): ", v1%get_data() (1)

    !     call v1%set(4.0d0)
    !     call vector_reciprocal(v1)
    !     print *, "Reciprocal (4.0 -> 0.25): ", v1%get_data() (1)

    !     call v1%set(10.0d0)
    !     call vector_shift(1, v1)
    !     print *, "Shift (10.0 - 1 -> 9.0): ", v1%get_data() (1)

    !     ! ---------------------------------------------------------
    !     ! Assignment Operator (=)
    !     ! ---------------------------------------------------------
    !     res = v1 ! res should be 9.0
    !     print *, "Assignment (=) [Expect 9.0]: ", res%get_data() (1)

    !     call v1%destroy()
    !     call v2%destroy()
    !     call res%destroy()

    ! end subroutine run_test_vector_ops_dp

end program test_linalg
