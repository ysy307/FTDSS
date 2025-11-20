program test_core
    use, intrinsic :: iso_fortran_env, only: int32
    use :: module_core
    implicit none

    class(abst_matrix), allocatable :: my_matrix

    ! Create a dense matrix with 100 nodes
    my_matrix = create_matrix(MATRIX_DENSE, 100)
    call my_matrix%display()

    ! Further test code would go here...

end program test_core
