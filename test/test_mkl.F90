program test_example
    use, intrinsic :: iso_fortran_env, only : error_unit
    use :: testdrive, only : &
        new_unittest, unittest_type, &
        run_testsuite, &
        error_type, check
    implicit none

    integer :: stat

    stat = 0

    call run_testsuite( &
        collect_tests, &
        error_unit, &
        stat &
    )

    if (stat /= 0) then
        error stop 1
    end if

contains

    subroutine collect_tests(tests)
        implicit none
        type(unittest_type), allocatable, intent(out) :: tests(:)

        tests = [ &
            new_unittest("square_test", test_square) &
        ]
    end subroutine collect_tests

    subroutine test_square(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        call check(error, 3 * 3, 9)
    end subroutine test_square

end program test_example