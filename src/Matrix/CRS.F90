module Matrix_CRS
    use, intrinsic :: iso_fortran_env, only: int32, real64, logical32
    use :: stdlib_sorting, only:sort
    use :: module_core, only:allocate_array, deallocate_array
    use :: module_domain, only:type_domain
    implicit none
    private

    public :: type_crs
    public :: operator(*)
    public :: operator(+)
    ! public :: Transpose_CRS

    type :: type_crs
        integer(int32) :: nnz ! number of non-zero elements
        integer(int32) :: nrow ! number of rows
        integer(int32) :: nptr ! size of Ptr (nrow+1 entries)
        integer(int32), allocatable :: Ptr(:) ! pointers to row starts (1-based)
        integer(int32), allocatable :: Ind(:) ! column indices of non-zeros
        real(real64), allocatable :: Val(:) ! non-zero values
    contains
        procedure, public, pass(self) :: Find => Find_CRS_Location
        procedure, public, pass(self) :: Copy => Copy_CRS
    end type type_crs

    interface operator(*)
        module procedure Matrix_Vector_Product_CRS
        module procedure Multiplication_Scalar_Matrix_CRS
        module procedure Multiplication_Matrix_Scalar_CRS
    end interface
    interface operator(+)
        module procedure Matrix_Addition_CRS
    end interface

    interface type_crs
        module procedure construct_type_crs
    end interface

contains

    function construct_type_crs(domain) result(A)
        implicit none
        class(type_domain), intent(in) :: domain
        type(type_crs) :: A

        ! --- ローカル変数宣言 ---
        integer(int32) :: iN, i, iNNZ, nNode, nnz_count
        integer(int32), allocatable :: tmpInd(:)

        ! --- RCM適用時専用の変数 ---
        integer(int32) :: old_iN, original_neighbor
        integer(int32), allocatable :: neighbor_nodes(:), rcm_cols(:)

        ! --- 初期設定 ---
        nNode = domain%get_num_nodes()
        A%nrow = nNode
        A%nptr = nNode + 1

        call allocate_array(A%Ptr, A%nptr)
        ! tmpIndは、最大非ゼロ要素数を見積もって確保。node_adjacencyから取得するのが理想
        ! ここでは簡潔のため、元のコードのサイズを流用
        call allocate_array(tmpInd, 30_int32 * nNode)

        A%Ptr(1) = 1
        nnz_count = 0

        !===============================================================
        ! CRS構造の構築 (RCM適用 & node_adjacency使用)
        !===============================================================
        do iN = 1, nNode
            ! ... (1. 元の節点番号を取得 までは同じ) ...
            call domain%rcm%reorder_original(iN, old_iN)

            ! 2. 隣接"以外"のノード群を取得
            call domain%node_adjacency%get_neighbors(old_iN, neighbor_nodes)

            ! 3.【修正】隣接ノード群(+自分自身)を「RCM順の列番号」に変換
            !         配列サイズは隣接ノード数 + 1 (対角成分用)
            allocate (rcm_cols(size(neighbor_nodes) + 1))

            ! 隣接ノードを変換
            do i = 1, size(neighbor_nodes)
                original_neighbor = neighbor_nodes(i)
                call domain%rcm%reorder_to_rcm(original_neighbor, rcm_cols(i))
            end do
            deallocate (neighbor_nodes)

            ! 自分自身（対角成分）を変換して追加
            call domain%rcm%reorder_to_rcm(old_iN, rcm_cols(size(rcm_cols)))

            ! 4. CRSフォーマットのため、列番号をソート
            call sort(rcm_cols)

            ! 5. 一時バッファに格納
            tmpInd(nnz_count + 1:nnz_count + size(rcm_cols)) = rcm_cols
            nnz_count = nnz_count + size(rcm_cols)
            A%Ptr(iN + 1) = nnz_count + 1

            deallocate (rcm_cols)
        end do

        A%nnz = nnz_count

        ! --- 最終的なCRS配列を確保・コピー ---
        call allocate_array(A%Ind, A%nnz)
        call allocate_array(A%Val, A%nnz)
        A%Ind(1:A%nnz) = tmpInd(1:A%nnz)
        A%Val = 0.0d0

        deallocate (tmpInd)

    end function construct_type_crs

    function Matrix_Vector_Product_CRS(A, x) result(y)
        implicit none
        type(type_crs), intent(in) :: A
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
        type(type_crs), intent(in) :: A, B
        type(type_crs) :: C
        integer(int32) :: k

        ! Assume same sparsity structure
        C = A%Copy()
        do k = 1, A%nnz
            C%Val(k) = A%Val(k) + B%Val(k)
        end do
    end function Matrix_Addition_CRS

    function Multiplication_Scalar_Matrix_CRS(A, b) result(C)
        implicit none
        type(type_crs), intent(in) :: A
        real(real64), intent(in) :: b
        type(type_crs) :: C
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
        type(type_crs), intent(in) :: B
        type(type_crs) :: C
        integer(int32) :: k

        ! Assume same sparsity structure
        C = B%Copy()
        do k = 1, B%nnz
            C%Val(k) = B%Val(k) * a
        end do
    end function Multiplication_Matrix_Scalar_CRS

    subroutine Find_CRS_Location(self, column, index_in, loc)
        implicit none
        class(type_crs), intent(in) :: self
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
        class(type_crs) :: self
        type(type_crs) :: B
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
