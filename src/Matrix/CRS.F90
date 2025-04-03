module Matrix_CRS
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Allocate_Allocate, only:Allocate_Array
    implicit none
    private

    public :: Type_CRS

    type :: Type_CRS
        integer(int32) :: nnz !! number of non-zero elements
        integer(int32) :: nrow !! number of rows
        integer(int32), allocatable :: Ptr(:) !! pointer to the start of each row
        integer(int32), allocatable :: Ind(:) !! index of the non-zero elements
        real(real64), allocatable :: Val(:) !! values of the non-zero elements
    contains
        procedure, public, pass(self) :: Find => Find_CRS_Location
        procedure, public, pass(self) :: Copy => Copy_CRS
    end type Type_CRS

    public :: operator(*)
    public :: operator(+)

    interface operator(*)
        module procedure Matrix_Vector_Product_CRS
    end interface
    interface operator(+)
        module procedure Matrix_Addition_CRS
    end interface

    interface Type_CRS
        module procedure Initialize_CRS
    end interface

contains

    function Initialize_CRS(Elements, nNode) result(A)
        implicit none
        integer(int32), intent(in) :: Elements(:, :)
        integer(int32), intent(in) :: nNode
        type(Type_CRS) :: A

        integer(int32) :: iN, iE, iT, irT, iNC, iNNZ, row_nnz
        integer(int32) :: nElment, nTop
        integer(int32), allocatable :: vertex(:), row(:), tmpInd(:)

        nTop = size(Elements, 1)
        nElment = size(Elements, 2)
        A%nrow = nNode + 1

        ! 1-origin用に配列を割り当て
        ! CRS のポインタ配列は nNode+1 個（A%Ptr(1)=1, A%Ptr(i+1)=row_iの開始位置を保持）
        call Allocate_Array(A%Ptr, nNode + 1_int32)
        call Allocate_Array(row, nNode)
        call Allocate_Array(tmpInd, 8_int32 * nNode)
        call Allocate_Array(vertex, nTop)

        A%Ptr(1) = 1 ! 1-originの最初の行は1番目から
        A%nnz = 0
        do iN = 1, nNode
            row(:) = 0
            row_nnz = 0
            do iE = 1, nElment
                vertex(:) = Elements(:, iE)
                ! 要素内のいずれかの頂点が iN と一致する場合，
                ! その要素の各頂点に対して非ゼロ数をインクリメント
                increament_vector: do iT = 1, nTop
                    if (vertex(iT) == iN) then
                        do irT = 1, nTop
                            row(vertex(irT)) = row(vertex(irT)) + 1
                        end do
                        exit increament_vector
                    end if
                end do increament_vector
            end do
            ! row配列の非ゼロ要素の個数を数え，仮のインデックス配列に格納
            do iNC = 1, nNode
                if (row(iNC) > 0) then
                    tmpInd(row_nnz + A%nnz + 1) = iNC
                    row_nnz = row_nnz + 1
                end if
            end do
            A%nnz = A%nnz + row_nnz
            ! 各行の終端ポインタは、非ゼロ要素の次の位置となる
            A%Ptr(iN + 1) = A%nnz + 1
        end do

        ! CRS行列のインデックスと値配列を1-originで割り当て
        call Allocate_Array(A%Ind, A%nnz)
        call Allocate_Array(A%val, A%nnz)

        ! CRSの初期化（値は0.0、インデックスは仮配列からコピー）
        do iNNZ = 1, A%nnz
            A%val(iNNZ) = 0.0d0
            A%Ind(iNNZ) = tmpInd(iNNZ)
        end do

        deallocate (vertex)
        deallocate (row)
        deallocate (tmpInd)
    end function Initialize_CRS

    function Matrix_Vector_Product_CRS(A, x) result(y)
        !* Matrix-Vector Product in CRS format
        implicit none
        type(Type_CRS), intent(in) :: A
        real(real64), intent(in) :: x(A%nrow)
        real(real64) :: y(A%nrow)
        real(real64) :: vtemp
        integer(int32) :: i, j, is, ie

        ! Initialize the result vector
        y(:) = 0.0d0

        !$omp parallel do private(vtemp, i, j, is, ie)
        do i = 1, A%nrow
            vtemp = 0.0d0

            is = A%ptr(i)
            ie = A%ptr(i + 1) - 1
            do j = is, ie
                vtemp = vtemp + A%val(j) * x(A%ind(j))
            end do
            y(i) = vtemp
        end do
        !$omp end parallel do
    end function Matrix_Vector_Product_CRS

    subroutine Find_CRS_Location(self, serch_column, serch_index, index)
        implicit none
        class(Type_CRS) :: self
        integer(int32), intent(in) :: serch_column, serch_index
        integer(int32), intent(inout) :: index
        integer(int32) :: i, start_index, end_index

        index = 0
        ! serch_columnのindex範囲を取得
        start_index = self%Ptr(serch_column)
        end_index = self%Ptr(serch_column + 1) - 1

        ! 範囲内でserch_indexになる値のインデックスを見つける
        do i = start_index, end_index
            if (self%Ind(i) == serch_index) then
                index = i
                exit
            end if
        end do

    end subroutine Find_CRS_Location

    function Matrix_Addition_CRS(A, B) result(C)
        !* Matrix-Matrix Product in CRS format
        implicit none
        type(Type_CRS), intent(in) :: A
        type(Type_CRS), intent(in) :: B
        type(Type_CRS) :: C

        C%Val(:) = A%Val(:) + B%Val(:)

    end function Matrix_Addition_CRS

    function Copy_CRS(self) result(B)
        !* Copy CRS matrix
        implicit none
        class(Type_CRS) :: self
        type(Type_CRS) :: B

        call Allocate_Array(B%Ptr, self%nrow + 1_int32)
        call Allocate_Array(B%Ind, self%nnz)
        call Allocate_Array(B%Val, self%nnz)

        B%nnz = self%nnz
        B%nrow = self%nrow
        B%Ptr(:) = self%Ptr(:)
        B%Ind(:) = self%Ind(:)
        B%Val(:) = self%Val(:)

    end function Copy_CRS

end module Matrix_CRS
