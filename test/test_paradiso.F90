program pardiso_example
#include "mkl.fi"

    ! implicit none

    ! 変数の宣言
    integer :: N, MAXFCT, MNUM, MTYPE, PHASE, NRHS, MSGLVL, ERROR
    integer, allocatable :: IA(:), JA(:), PERM(:), IPARM(:)
    real(KIND=8), allocatable :: A(:), B(:), X(:)
    type(MKL_PARDISO_HANDLE), allocatable :: PT(:) ! ← 修正 (MKL_PARDISO_HANDLE は不要)

    ! 行列の次元 (例として 5x5 行列)
    N = 5
    MAXFCT = 1
    MNUM = 1
    MTYPE = 1 ! 対称行列、実数値
    PHASE = 13 ! 解法のフェーズ
    NRHS = 1
    MSGLVL = 0

    ! PARDISO のハンドルと配列を確保
    allocate (PT(64)) ! ← integer(KIND=8) に修正
    allocate (IPARM(64))
    allocate (PERM(N))
    allocate (IA(N + 1), JA(15)) ! ← integer(KIND=4) にする
    allocate (A(15)) ! ← real(KIND=8) で統一
    allocate (B(N))
    allocate (X(N))

    ! 行列 A の初期化
    A = [4.0d0, -1.0d0, -1.0d0, -1.0d0, 4.0d0, -1.0d0, -1.0d0, 4.0d0, -1.0d0, -1.0d0, 4.0d0, -1.0d0, -1.0d0, -1.0d0, 4.0d0]
    IA = [1, 4, 7, 10, 13, 16]
    JA = [1, 2, 5, 1, 2, 3, 2, 3, 4, 3, 4, 5, 1, 4, 5]
    B = [15.0, 10.0, 10.0, 10.0, 15.0]

    ! ! PARDISO の呼び出し
    ! call PARDISO(PT, MAXFCT, MNUM, MTYPE, PHASE, N, A, IA, JA, PERM, NRHS, IPARM, MSGLVL, B, X, ERROR)
    ! if (ERROR /= 0) then
    !     print *, 'PARDISO 初期化エラー'
    !     stop
    ! end if

    ! PHASE = 13
    call PARDISO(PT, MAXFCT, MNUM, MTYPE, PHASE, N, A, IA, JA, PERM, NRHS, IPARM, MSGLVL, B, X, ERROR)
    if (ERROR /= 0) then
        print *, 'PARDISO 解法エラー'
        stop
    end if

    print *, '解ベクトル X:'
    print *, X

    deallocate (PT, IPARM, PERM, IA, JA, A, B, X)

end program pardiso_example
