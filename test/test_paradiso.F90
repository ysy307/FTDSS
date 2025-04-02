program pardiso_example
    use, intrinsic :: iso_fortran_env, only: real64, int64, int32
    use :: Solver_Solve
    use :: Matrix_CRS
    ! implicit none
    integer(int32) :: N, MAXFCT, MNUM, MTYPE, PHASE, NRHS, MSGLVL, ERROR
    integer(int32) :: nnz
    real(real64), allocatable :: B(:), X(:)
    real(real64), allocatable :: BF(:), XF(:)
    real(real64), allocatable :: AF(:, :)

    type(Type_CRS) :: A_CRS
    class(Abstract_Solver_CRS), allocatable :: solver, solver_2
    class(Abstract_Solver_Full), allocatable :: solver_3
    integer(int32) :: status
    real(real64), allocatable :: bb(:)
    real(real64), allocatable :: xx(:)

    ! 行列の次元 (例として 5x5 行列)
    N = 5
    MAXFCT = 1
    MNUM = 1
    MTYPE = 1 ! 対称行列、実数値
    PHASE = 13 ! 解法のフェーズ
    NRHS = 1
    MSGLVL = 0

    nnz = 15

    allocate (A_CRS%Ind(nnz))
    allocate (A_CRS%Val(nnz))
    allocate (A_CRS%Ptr(N + 1))
    A_CRS%Val = [4.0d0, -1.0d0, -1.0d0, -1.0d0, 4.0d0, -1.0d0, -1.0d0, 4.0d0, -1.0d0, -1.0d0, 4.0d0, -1.0d0, -1.0d0, -1.0d0, 4.0d0]
    A_CRS%Ptr = [1, 4, 7, 10, 13, 16]
    A_CRS%Ind = [1, 2, 5, 1, 2, 3, 2, 3, 4, 3, 4, 5, 1, 4, 5]
    A_CRS%nnz = nnz
    A_CRS%nrow = N

    allocate (B(N))
    allocate (X(N))
    allocate (bb(N))
    allocate (xx(N))
    B(:) = [15.0d0, 10.0d0, 10.0d0, 10.0d0, 15.0d0]
    bb(:) = [15.0d0, 10.0d0, 10.0d0, 10.0d0, 15.0d0]

    solver = Solver_CRS_LU_Constructor(N, MAXFCT, MNUM, MTYPE, PHASE, NRHS, MSGLVL, A_CRS)
    call solver%Solve(A_CRS, B, X, status)

    print *, '解ベクトル X:'
    print *, X

    ! deallocate (solver)

    solver_2 = solver_CRS_BiCGSTAB_Constructor(5, 1.0d-6, 1000, 1)

    call solver_2%Solve(A_CRS, bb, xx, status)
    call solver_2%Check(status, 0.0d0)

    print *, '解ベクトル X:'
    print *, xx

    solver_3 = Solver_Full_LU_Constructor(N)

    allocate (AF(N, N))
    allocate (BF(N))
    allocate (XF(N))
    ! AF(:, :) = 0.0d0
    AF(1, :) = [4.0d0, -1.0d0, 0.0d0, 0.0d0, -1.0d0]
    AF(2, :) = [-1.0d0, 4.0d0, -1.0d0, 0.0d0, 0.0d0]
    AF(3, :) = [0.0d0, -1.0d0, 4.0d0, -1.0d0, 0.0d0]
    AF(4, :) = [0.0d0, 0.0d0, -1.0d0, 4.0d0, -1.0d0]
    AF(5, :) = [-1.0d0, 0.0d0, 0.0d0, -1.0d0, 4.0d0]

    BF(:) = [15.0d0, 10.0d0, 10.0d0, 10.0d0, 15.0d0]
    XF(:) = 0.0d0
    call solver_3%Solve(AF, BF, XF, status)
    call solver_3%Check(status, 0.0d0)
    print *, '解ベクトル X:'
    print *, XF

end program pardiso_example
