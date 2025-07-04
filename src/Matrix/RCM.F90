module Matrix_RCM
    use, intrinsic :: iso_fortran_env, only: int32, real64, logical32, int64
    use :: stdlib_sorting, only:sort_index
    use :: Core_Allocate, only:Allocate_Array
    use :: Domain_Module, only:Domain_t

    implicit none
    private

    public :: RCM_Reorder
    public :: RCM_Reorder_Inverse

    public :: Reorder_to_Original

contains

    !=======================================================================
    ! メインサブルーチン: RCM法による節点の並べ替え
    !=======================================================================
    subroutine RCM_Reorder(domain, perm, istat)
        implicit none
        ! --- 引数宣言 ---
        class(Domain_t), intent(in) :: domain
        integer(int32), allocatable, intent(inout) :: perm(:)
        integer(int32), intent(inout) :: istat

        ! --- ローカル変数宣言 ---
        integer(int32) :: num_nodes
        integer(int32), allocatable :: adj_ptr(:)
        integer(int32), allocatable :: adj_data(:)
        integer(int32), allocatable :: degree(:)
        integer(int32), allocatable :: R(:)
        integer(int32), allocatable :: Q(:)
        logical(logical32), allocatable :: visited(:)
        integer(int32) :: R_count
        integer(int32) :: start_node
        integer(int32) :: i

        ! --- 処理開始 ---
        istat = 0
        num_nodes = domain%get_numNode()

        ! 隣接関係を効率的な方法で構築
        call build_node_adjacency(domain, num_nodes, adj_ptr, adj_data, degree, istat)
        if (istat /= 0) return

        ! --- RCM本体で利用する配列を確保 ---
        call Allocate_Array(perm, num_nodes)
        call Allocate_Array(R, num_nodes)
        call Allocate_Array(Q, num_nodes)
        call Allocate_Array(visited, num_nodes)

        visited = .false.
        R_count = 0

        do while (R_count < num_nodes)
            call find_start_node(num_nodes, degree, visited, start_node, istat)
            if (istat /= 0) return

            call execute_cm_ordering(start_node, adj_ptr, adj_data, degree, &
                                     visited, Q, R, R_count)
        end do

        ! Cuthill-McKee順序(R)を逆順にしてReverse Cuthill-McKee順序(perm)を作成
        do i = 1, num_nodes
            perm(i) = R(num_nodes - i + 1)
        end do
        ! --- メモリ解放 ---
        deallocate (adj_ptr)
        deallocate (adj_data)
        deallocate (degree)
        deallocate (R)
        deallocate (Q)
        deallocate (visited)

    end subroutine RCM_Reorder

    !=======================================================================
    ! 内部サブルーチン群
    !=======================================================================

    !
    ! [修正箇所] 密行列を使わずに、スケーラブルな方法で隣接リストを構築する
    !
    subroutine build_node_adjacency(domain, num_nodes, adj_ptr, adj_data, degree, istat)
        implicit none
        ! --- 引数宣言 ---
        class(Domain_t), intent(in) :: domain
        integer(int32), intent(in) :: num_nodes
        integer(int32), allocatable, intent(inout) :: adj_ptr(:)
        integer(int32), allocatable, intent(inout) :: adj_data(:)
        integer(int32), allocatable, intent(inout) :: degree(:)
        integer(int32), intent(inout) :: istat

        ! --- ローカル変数宣言 ---
        integer(int32) :: i
        integer(int32) :: j
        integer(int32) :: k
        integer(int32) :: n1
        integer(int32) :: n2
        integer(int32) :: num_items
        integer(int32) :: nodes_per_item
        integer(int32) :: edge_count
        integer(int32) :: max_edges
        integer(int32) :: unique_edge_count
        integer(int32), allocatable :: edge_i(:)
        integer(int32), allocatable :: edge_j(:)
        integer(int64), allocatable :: sort_keys(:)
        integer(int32), allocatable :: p(:)
        integer(int32), allocatable :: temp_counters(:)
        integer(int32) :: num_adj

        integer(int64) :: conv = 1_int64

        istat = 0

        ! --- 1. 全てのエッジを一時配列 (COO形式) に格納する ---
        ! 最大エッジ数を多めに見積もる (三角要素:3辺, 四角要素:6辺(対角線含む))
        max_edges = domain%get_numElement() * 30 + domain%get_numSide() * 20
        call Allocate_Array(edge_i, max_edges)
        call Allocate_Array(edge_j, max_edges)

        edge_count = 0

        ! 領域要素 (Element) からエッジを抽出
        num_items = domain%get_numElement()
        do i = 1, num_items
            nodes_per_item = domain%Elements(i)%e%get_size()
            do j = 1, nodes_per_item
                do k = j + 1, nodes_per_item
                    edge_count = edge_count + 1
                    if (edge_count > max_edges) then
                        istat = -1 ! Error: max_edges exceeded
                        return
                    end if
                    n1 = domain%Elements(i)%e%conn(j)
                    n2 = domain%Elements(i)%e%conn(k)
                    ! 小さい方をi, 大きい方をjに格納し、ソートを容易にする
                    if (n1 < n2) then
                        edge_i(edge_count) = n1
                        edge_j(edge_count) = n2
                    else
                        edge_i(edge_count) = n2
                        edge_j(edge_count) = n1
                    end if
                end do
            end do
        end do

        ! 境界要素 (Side) からエッジを抽出
        num_items = domain%get_numSide()
        do i = 1, num_items
            nodes_per_item = domain%Sides(i)%s%get_size()
            do j = 1, nodes_per_item
                do k = j + 1, nodes_per_item
                    edge_count = edge_count + 1
                    if (edge_count > max_edges) then
                        istat = -1 ! Error: max_edges exceeded
                        return
                    end if
                    n1 = domain%Sides(i)%s%conn(j)
                    n2 = domain%Sides(i)%s%conn(k)
                    if (n1 < n2) then
                        edge_i(edge_count) = n1
                        edge_j(edge_count) = n2
                    else
                        edge_i(edge_count) = n2
                        edge_j(edge_count) = n1
                    end if
                end do
            end do
        end do

        ! --- 2. エッジをソートして重複を削除する ---
        call Allocate_Array(sort_keys, transfer(edge_count, conv))
        call Allocate_Array(p, edge_count)

        ! (i, j) のペアをソートするため、64bit整数の一意なキーを作成
        sort_keys = int(edge_i(1:edge_count), int64) * int(num_nodes, int64) + int(edge_j(1:edge_count), int64)
        call sort_index(sort_keys, p)

        ! ソートされたインデックスを使って、重複のないエッジリストを再構築
        unique_edge_count = 0
        if (edge_count > 0) then
            unique_edge_count = 1
            edge_i(1) = edge_i(p(1))
            edge_j(1) = edge_j(p(1))
            do i = 2, edge_count
                if (edge_i(p(i)) /= edge_i(p(i - 1)) .or. edge_j(p(i)) /= edge_j(p(i - 1))) then
                    unique_edge_count = unique_edge_count + 1
                    edge_i(unique_edge_count) = edge_i(p(i))
                    edge_j(unique_edge_count) = edge_j(p(i))
                end if
            end do
        end if
        deallocate (sort_keys)
        deallocate (p)

        ! --- 3. 次数(degree)を計算し、CSR形式を構築する ---
        call Allocate_Array(degree, num_nodes)
        degree = 0
        do i = 1, unique_edge_count
            degree(edge_i(i)) = degree(edge_i(i)) + 1
            degree(edge_j(i)) = degree(edge_j(i)) + 1
        end do

        num_adj = sum(degree)
        call Allocate_Array(adj_ptr, num_nodes + 1)
        call Allocate_Array(adj_data, num_adj)

        adj_ptr(1) = 1
        do i = 1, num_nodes
            adj_ptr(i + 1) = adj_ptr(i) + degree(i)
        end do

        ! CSRのadj_dataを埋めるためのカウンタを準備
        call Allocate_Array(temp_counters, num_nodes)
        temp_counters = 0
        do i = 1, unique_edge_count
            n1 = edge_i(i)
            n2 = edge_j(i)
            adj_data(adj_ptr(n1) + temp_counters(n1)) = n2
            temp_counters(n1) = temp_counters(n1) + 1
            adj_data(adj_ptr(n2) + temp_counters(n2)) = n1
            temp_counters(n2) = temp_counters(n2) + 1
        end do

        ! --- 一時配列を解放 ---
        deallocate (edge_i)
        deallocate (edge_j)
        deallocate (temp_counters)

        ! RCM本体で次数が必要なので、ここで再計算しておく
        do i = 1, num_nodes
            degree(i) = adj_ptr(i + 1) - adj_ptr(i)
        end do
    end subroutine build_node_adjacency

    ! --- ここから下の内部サブルーチンは変更なし (引数intentのみ修正) ---
    subroutine find_start_node(num_nodes, degree, visited, start_node, istat)
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in) :: degree(:)
        logical(logical32), intent(in) :: visited(:)
        integer(int32), intent(inout) :: start_node
        integer(int32), intent(inout) :: istat

        integer(int32) :: i
        integer(int32) :: min_deg
        istat = 0
        min_deg = num_nodes + 1
        start_node = -1
        do i = 1, num_nodes
            if (.not. visited(i) .and. degree(i) < min_deg) then
                min_deg = degree(i)
                start_node = i
            end if
        end do
        if (start_node == -1) then
            istat = 1
        end if
    end subroutine find_start_node

    subroutine execute_cm_ordering(start_node, adj_ptr, adj_data, degree, &
                                   visited, Q, R, R_count)
        integer(int32), intent(in) :: start_node
        integer(int32), intent(in) :: adj_ptr(:)
        integer(int32), intent(in) :: adj_data(:)
        integer(int32), intent(in) :: degree(:)
        logical(logical32), intent(inout) :: visited(:)
        integer(int32), intent(inout) :: Q(:)
        integer(int32), intent(inout) :: R(:)
        integer(int32), intent(inout) :: R_count

        integer(int32) :: q_head
        integer(int32) :: q_tail
        integer(int32) :: current_node
        q_head = 1
        q_tail = 1
        Q(1) = start_node
        visited(start_node) = .true.
        do while (q_head <= q_tail)
            current_node = Q(q_head)
            q_head = q_head + 1
            R_count = R_count + 1
            R(R_count) = current_node
            call sort_and_enqueue_neighbors(current_node, adj_ptr, adj_data, degree, visited, Q, q_tail)
        end do
    end subroutine execute_cm_ordering

    subroutine sort_and_enqueue_neighbors(node, adj_ptr, adj_data, degree, visited, Q, q_tail)
        integer(int32), intent(in) :: node
        integer(int32), intent(in) :: adj_ptr(:)
        integer(int32), intent(in) :: adj_data(:)
        integer(int32), intent(in) :: degree(:)
        logical(logical32), intent(inout) :: visited(:)
        integer(int32), intent(inout) :: Q(:)
        integer(int32), intent(inout) :: q_tail

        integer(int32), allocatable :: neighbors(:)
        integer(int32), allocatable :: neighbor_degrees(:)
        integer(int32), allocatable :: sorted_indices(:)
        integer(int32) :: i
        integer(int32) :: p
        integer(int32) :: neighbor_count
        integer(int32) :: start_idx
        integer(int32) :: end_idx
        start_idx = adj_ptr(node)
        end_idx = adj_ptr(node + 1) - 1
        neighbor_count = end_idx - start_idx + 1
        if (neighbor_count == 0) return
        call Allocate_Array(neighbors, neighbor_count)
        call Allocate_Array(neighbor_degrees, neighbor_count)
        call Allocate_Array(sorted_indices, neighbor_count)
        neighbors = adj_data(start_idx:end_idx)
        do i = 1, neighbor_count
            neighbor_degrees(i) = degree(neighbors(i))
        end do
        call sort_index(neighbor_degrees, sorted_indices)
        do i = 1, neighbor_count
            p = neighbors(sorted_indices(i))
            if (.not. visited(p)) then
                visited(p) = .true.
                q_tail = q_tail + 1
                Q(q_tail) = p
            end if
        end do
        deallocate (neighbors)
        deallocate (neighbor_degrees)
        deallocate (sorted_indices)
    end subroutine sort_and_enqueue_neighbors

    !=======================================================================
    ! RCMで得られた順方向の並べ替え配列(perm)から、
    ! 元の順序に戻すための逆方向の配列(iperm)を作成する。
    ! iperm は一度計算すれば、メッシュが変わらない限り再利用可能。
    !-----------------------------------------------------------------------
    subroutine RCM_Reorder_Inverse(perm, iperm, istat)
        implicit none
        integer(int32), intent(in) :: perm(:)
        integer(int32), allocatable, intent(inout) :: iperm(:)
        integer(int32), intent(inout) :: istat

        integer(int32) :: i
        integer(int32) :: n
        istat = 0
        n = size(perm)

        if (allocated(iperm)) then
            deallocate (iperm)
        end if
        call Allocate_Array(iperm, n)

        do i = 1, n
            ! [修正] コメントを現実に即したものに修正
            ! perm(RCM後のIndex) = 元のIndex
            ! iperm(元のIndex)  = RCM後のIndex
            iperm(perm(i)) = i
        end do
    end subroutine RCM_Reorder_Inverse

    !=======================================================================
    ! RCM順序のベクトルを受け取り、元の節点順序に並べ替えたベクトルを返す。
    ! ファイル出力などの後処理で利用する。
    !-----------------------------------------------------------------------
    subroutine Reorder_to_Original(vector_rcm, vector_original, perm, istat)
        implicit none
        real(real64), intent(in) :: vector_rcm(:)
        real(real64), intent(inout) :: vector_original(:)
        integer(int32), intent(in) :: perm(:) ! <--- ipermではなくpermを受け取る
        integer(int32), intent(inout) :: istat

        integer(int32) :: n
        integer(int32) :: i

        istat = 0
        n = size(vector_rcm)

        ! サイズチェック
        if (size(perm) /= n .or. size(vector_original) /= n) then
            istat = -1
            return
        end if

        ! =========================================================
        ! ▼▼▼ ここが正しいロジックです ▼▼▼
        ! =========================================================
        do i = 1, n
            ! 「RCM順でi番目の値」は、「元の節点番号perm(i)番目の場所」に入る
            vector_original(perm(i)) = vector_rcm(i)
        end do
        ! =========================================================

    end subroutine Reorder_to_Original

end module Matrix_RCM
