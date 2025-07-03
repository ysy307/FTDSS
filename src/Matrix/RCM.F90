module Matrix_RCM
    use, intrinsic :: iso_fortran_env, only: int32, logical32
    use :: stdlib_sorting, only:sort_index
    use :: Core_Allocate, only:Allocate_Array
    use :: Domain_Module, only:Domain_t

    implicit none
    private

    public :: RCM_Reorder

contains

    !=======================================================================
    ! メインサブルーチン: RCM法による節点の並べ替え
    !=======================================================================
    subroutine RCM_Reorder(domain, perm, istat)
        ! --- 引数宣言 ---
        class(Domain_t), intent(in) :: domain
        integer(int32), allocatable, intent(inout) :: perm(:)
        integer(int32), intent(inout) :: istat

        ! --- ローカル変数宣言 ---
        integer(int32) :: num_nodes
        integer(int32), allocatable :: adj_ptr(:), adj_data(:), degree(:)
        integer(int32), allocatable :: R(:), Q(:)
        logical(logical32), allocatable :: visited(:)
        integer(int32) :: R_count, start_node

        ! --- 処理開始 ---
        istat = 0
        num_nodes = domain%get_numNode()

        call build_node_adjacency(domain, num_nodes, adj_ptr, adj_data, degree, istat)
        if (istat /= 0) return

        ! --- RCM本体ロジックは変更なし ---

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

        do R_count = 1, num_nodes
            perm(R(R_count)) = num_nodes - R_count + 1
        end do

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

    subroutine build_node_adjacency(domain, num_nodes, adj_ptr, adj_data, degree, istat)
        class(Domain_t), intent(in) :: domain
        integer(int32), intent(in) :: num_nodes
        integer(int32), allocatable, intent(inout) :: adj_ptr(:), adj_data(:), degree(:)
        integer(int32), intent(inout) :: istat

        logical(logical32), allocatable :: adj_matrix(:, :)
        integer(int32) :: i, j, k, n1, n2, num_adj
        integer(int32) :: num_items, nodes_per_item

        istat = 0
        call Allocate_Array(degree, num_nodes)
        call Allocate_Array(adj_matrix, num_nodes, num_nodes)
        degree = 0; adj_matrix = .false.

        ! --- 1. 領域要素 (Element) から隣接関係を構築 ---
        num_items = domain%get_numElement()
        do i = 1, num_items
            nodes_per_item = domain%Elements(i)%e%get_size()
            do j = 1, nodes_per_item
                do k = j + 1, nodes_per_item
                    n1 = domain%Elements(i)%e%conn(j)
                    n2 = domain%Elements(i)%e%conn(k)
                    if (.not. adj_matrix(n1, n2)) then
                        adj_matrix(n1, n2) = .true.; adj_matrix(n2, n1) = .true.
                        degree(n1) = degree(n1) + 1; degree(n2) = degree(n2) + 1
                    end if
                end do
            end do
        end do

        ! --- 2. 境界要素 (Side) から隣接関係を構築 ---
        num_items = domain%get_numSide()
        do i = 1, num_items
            nodes_per_item = domain%Sides(i)%s%get_size()
            do j = 1, nodes_per_item
                do k = j + 1, nodes_per_item
                    n1 = domain%Sides(i)%s%conn(j)
                    n2 = domain%Sides(i)%s%conn(k)
                    if (.not. adj_matrix(n1, n2)) then
                        adj_matrix(n1, n2) = .true.; adj_matrix(n2, n1) = .true.
                        degree(n1) = degree(n1) + 1; degree(n2) = degree(n2) + 1
                    end if
                end do
            end do
        end do

        ! --- 3. adj_matrixからCSR形式の隣接リストを作成 (この部分は変更なし) ---
        num_adj = sum(degree)
        call Allocate_Array(adj_ptr, num_nodes + 1_int32)
        call Allocate_Array(adj_data, num_adj)
        adj_ptr(1) = 1
        do i = 1, num_nodes
            adj_ptr(i + 1) = adj_ptr(i) + degree(i)
        end do

        degree = 0
        do i = 1, num_nodes
            do j = i + 1, num_nodes
                if (adj_matrix(i, j)) then
                    adj_data(adj_ptr(i) + degree(i)) = j
                    adj_data(adj_ptr(j) + degree(j)) = i
                    degree(i) = degree(i) + 1
                    degree(j) = degree(j) + 1
                end if
            end do
        end do
        deallocate (adj_matrix)

        do i = 1, num_nodes
            degree(i) = adj_ptr(i + 1) - adj_ptr(i)
        end do
    end subroutine build_node_adjacency

    ! --- ここから下の内部サブルーチンは変更なし ---
    subroutine find_start_node(num_nodes, degree, visited, start_node, istat)
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in) :: degree(:)
        logical(logical32), intent(in) :: visited(:)
        integer(int32), intent(inout) :: start_node
        integer(int32), intent(inout) :: istat

        integer(int32) :: i, min_deg
        istat = 0
        min_deg = num_nodes + 1
        start_node = -1
        do i = 1, num_nodes
            if (.not. visited(i) .and. degree(i) < min_deg) then
                min_deg = degree(i)
                start_node = i
            end if
        end do
        if (start_node == -1) istat = 1
    end subroutine find_start_node

    subroutine execute_cm_ordering(start_node, adj_ptr, adj_data, degree, &
                                   visited, Q, R, R_count)
        integer(int32), intent(in) :: start_node
        integer(int32), intent(in) :: adj_ptr(:), adj_data(:), degree(:)
        logical(logical32), intent(inout) :: visited(:)
        integer(int32), intent(inout) :: Q(:), R(:), R_count

        integer(int32) :: q_head, q_tail, current_node
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
        integer(int32), intent(in) :: adj_ptr(:), adj_data(:), degree(:)
        logical(logical32), intent(inout) :: visited(:)
        integer(int32), intent(inout) :: Q(:), q_tail

        integer(int32), allocatable :: neighbors(:)
        integer(int32), allocatable :: neighbor_degrees(:)
        integer(int32), allocatable :: sorted_indices(:)
        integer(int32) :: i, p, neighbor_count, start_idx, end_idx
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

end module Matrix_RCM
