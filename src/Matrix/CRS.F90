module Matrix_CRS
    use, intrinsic :: iso_fortran_env, only: int32, real64, logical32
    use :: stdlib_sorting, only:sort
    use :: core_core, only:allocate_array
    use :: Domain_Module, only:Domain_t
    implicit none
    private

    public :: Type_CRS
    public :: operator(*)
    public :: operator(+)
    ! public :: Transpose_CRS

    type :: Type_CRS
        integer(int32) :: nnz ! number of non-zero elements
        integer(int32) :: nrow ! number of rows
        integer(int32) :: nptr ! size of Ptr (nrow+1 entries)
        integer(int32), allocatable :: Ptr(:) ! pointers to row starts (1-based)
        integer(int32), allocatable :: Ind(:) ! column indices of non-zeros
        real(real64), allocatable :: Val(:) ! non-zero values
    contains
        procedure, public, pass(self) :: Find => Find_CRS_Location
        procedure, public, pass(self) :: Copy => Copy_CRS
    end type Type_CRS

    interface operator(*)
        module procedure Matrix_Vector_Product_CRS
        module procedure Multiplication_Scalar_Matrix_CRS
        module procedure Multiplication_Matrix_Scalar_CRS
    end interface
    interface operator(+)
        module procedure Matrix_Addition_CRS
    end interface

    interface Type_CRS
        module procedure Initialize_CRS
    end interface

contains

    function Initialize_CRS(Domain) result(A)
        implicit none
        class(Domain_t), intent(in) :: Domain
        type(Type_CRS) :: A

        ! --- ローカル変数宣言 ---
        integer(int32) :: iN, iE, iT, irT, iNC, iNNZ, row_nnz, nsize
        integer(int32) :: nNode, nElement
        ! ★★★★★ 修正点1: rowCountをintegerからlogicalに変更 ★★★★★
        logical(logical32), allocatable :: rowCount(:)
        integer(int32), allocatable :: tmpInd(:)

        ! --- RCM適用時専用の変数 ---
        ! integer(int32), allocatable :: inv_perm(:)
        integer(int32), allocatable :: cols_for_this_row(:)
        integer(int32) :: old_iN, col_count

        ! --- 初期設定 ---
        nNode = Domain%get_numNode()
        nElement = Domain%get_numElement()
        A%nrow = nNode
        A%nptr = nNode + 1

        call allocate_array(A%Ptr, A%nptr)
        call allocate_array(rowCount, nNode)
        call allocate_array(tmpInd, 30_int32 * nNode) ! 十分なサイズを確保

        A%Ptr(1) = 1
        A%nnz = 0

        ! if (present(perm)) then
        !----------------------------------
        ! RCM適用時の処理 (perm がある場合)
        !----------------------------------

        ! call allocate_array(inv_perm, nNode)
        ! do iN = 1, nNode
        !     inv_perm(perm(iN)) = iN
        ! end do

        !--- RCM 適用時 ---
        ! 1) inv_perm / perm は Domain に既に格納されている想定
        do iN = 1, nNode
            ! iN は「RCM順での行番号」
            old_iN = Domain%RCM_perm(iN) ! 元ノード番号

            rowCount = .false.
            ! ツブし：その元ノード old_iN が属する要素を探し、
            !  隣接ノード群を rowCount(:) = .true. にする
            do iE = 1, nElement
                nsize = Domain%Elements(iE)%e%get_size()
                ! old_iN がこの要素のどこに入っているか探索
                do iT = 1, nsize
                    if (Domain%Elements(iE)%e%conn(iT) == old_iN) then
                        do irT = 1, nsize
                            rowCount(Domain%Elements(iE)%e%conn(irT)) = .true.
                        end do
                        exit
                    end if
                end do
            end do

            ! 2) 列数をカウント & RCMノード番号配列を作成
            col_count = count(rowCount)
            allocate (cols_for_this_row(col_count))
            row_nnz = 0
            do iNC = 1, nNode
                if (rowCount(iNC)) then
                    row_nnz = row_nnz + 1
                    ! iNC（元ノード） → RCM順ノード
                    cols_for_this_row(row_nnz) = Domain%RCM_inv_perm(iNC)
                end if
            end do

            call sort(cols_for_this_row) ! RCM順の列をソート

            ! 3) 一時バッファに格納
            tmpInd(A%nnz + 1:A%nnz + row_nnz) = cols_for_this_row
            A%nnz = A%nnz + row_nnz
            A%Ptr(iN + 1) = A%nnz + 1

            deallocate (cols_for_this_row)
        end do
        ! deallocate (inv_perm)

        ! else
        !     !--------------------------------------
        !     ! 通常の処理 (perm がない場合)
        !     !--------------------------------------
        !     do iN = 1, nNode
        !         rowCount(:) = .false. ! logical配列として初期化
        !         row_nnz = 0

        !         do iE = 1, nElement
        !             nsize = Domain%Elements(iE)%e%get_size()
        !             do iT = 1, nsize
        !                 if (Domain%Elements(iE)%e%conn(iT) == iN) then
        !                     do irT = 1, nsize
        !                         rowCount(Domain%Elements(iE)%e%conn(irT)) = .true.
        !                     end do
        !                     exit
        !                 end if
        !             end do
        !         end do

        !         do iNC = 1, nNode
        !             if (rowCount(iNC)) then
        !                 row_nnz = row_nnz + 1
        !                 tmpInd(A%nnz + row_nnz) = iNC
        !             end if
        !         end do

        !         A%nnz = A%nnz + row_nnz
        !         A%Ptr(iN + 1) = A%nnz + 1
        !     end do
        ! end if

        ! --- 最終的なCRS配列を確保・コピー ---
        call allocate_array(A%Ind, A%nnz)
        call allocate_array(A%Val, A%nnz)
        do iNNZ = 1, A%nnz
            A%Ind(iNNZ) = tmpInd(iNNZ)
            A%Val(iNNZ) = 0.0d0
        end do

        deallocate (rowCount, tmpInd)

    end function Initialize_CRS

    function Matrix_Vector_Product_CRS(A, x) result(y)
        implicit none
        type(Type_CRS), intent(in) :: A
        real(real64), intent(in) :: x(A%nrow)
        real(real64) :: y(A%nrow)
        integer(int32) :: i, j, is, ie
        real(real64) :: sum

        y(:) = 0.0d0
        do i = 1, A%nrow
            sum = 0.0d0
            is = A%Ptr(i)
            ie = A%Ptr(i + 1) - 1
            do j = is, ie
                sum = sum + A%Val(j) * x(A%Ind(j))
            end do
            y(i) = sum
        end do
    end function Matrix_Vector_Product_CRS

    function Matrix_Addition_CRS(A, B) result(C)
        implicit none
        type(Type_CRS), intent(in) :: A, B
        type(Type_CRS) :: C
        integer(int32) :: k

        ! Assume same sparsity structure
        C = A%Copy()
        do k = 1, A%nnz
            C%Val(k) = A%Val(k) + B%Val(k)
        end do
    end function Matrix_Addition_CRS

    function Multiplication_Scalar_Matrix_CRS(A, b) result(C)
        implicit none
        type(Type_CRS), intent(in) :: A
        real(real64), intent(in) :: b
        type(Type_CRS) :: C
        integer(int32) :: k

        ! Assume same sparsity structure
        C = A%Copy()
        do k = 1, A%nnz
            C%Val(k) = A%Val(k) * b
        end do
    end function Multiplication_Scalar_Matrix_CRS

    function Multiplication_Matrix_Scalar_CRS(a, B) result(C)
        implicit none
        real(real64), intent(in) :: a
        type(Type_CRS), intent(in) :: B
        type(Type_CRS) :: C
        integer(int32) :: k

        ! Assume same sparsity structure
        C = B%Copy()
        do k = 1, B%nnz
            C%Val(k) = B%Val(k) * a
        end do
    end function Multiplication_Matrix_Scalar_CRS

    subroutine Find_CRS_Location(self, column, index_in, loc)
        implicit none
        class(Type_CRS), intent(in) :: self
        integer(int32), intent(in) :: column, index_in
        integer(int32), intent(out) :: loc
        integer(int32) :: i, start, endp

        loc = 0
        start = self%Ptr(column)
        endp = self%Ptr(column + 1) - 1
        do i = start, endp
            if (self%Ind(i) == index_in) then
                loc = i; return
            end if
        end do
    end subroutine Find_CRS_Location

    function Copy_CRS(self) result(B)
        implicit none
        class(Type_CRS) :: self
        type(Type_CRS) :: B
        integer(int32) :: k

        B%nrow = self%nrow
        B%nptr = self%nptr
        B%nnz = self%nnz
        call allocate_array(B%Ptr, self%nptr)
        call allocate_array(B%Ind, self%nnz)
        call allocate_array(B%Val, self%nnz)
        B%Ptr(:) = self%Ptr(:)
        B%Ind(:) = self%Ind(:)
        B%Val(:) = self%Val(:)
    end function Copy_CRS

end module Matrix_CRS
