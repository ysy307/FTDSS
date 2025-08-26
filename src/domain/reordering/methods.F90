!================================================================!
! submodule (domain_reordering) reordering_methods
!
! リオーダリングアルゴリズムの具体的な実装
!================================================================!
submodule(domain_reordering) reordering_methods
    implicit none

contains

    !================================================================!
    !【メソッド】RCM (Reverse Cuthill-McKee) 並べ替え
    !================================================================!
    module subroutine rcm_reorder_method(self, node_adj)
        implicit none
        class(type_reordering), intent(inout) :: self
        class(type_node_adjacency), intent(in) :: node_adj

        integer(int32), allocatable :: cm_perm(:)
        integer(int32) :: i, num_nodes

        num_nodes = node_adj%get_num_nodes()
        if (num_nodes == 0) return

        ! 1. コアとなるCM並べ替えを実行
        call execute_reordering_core(node_adj, cm_perm)
        if (.not. allocated(cm_perm)) return

        ! 2. 結果を逆順にしてRCMの順列を作成
        if (allocated(self%perm)) call deallocate_array(self%perm)
        call allocate_array(self%perm, length=num_nodes)
        do i = 1, num_nodes
            self%perm(i) = cm_perm(num_nodes - i + 1)
        end do

        self%num_nodes = num_nodes
        self%is_reordered_perm = .true.
        self%is_reordered_iperm = .false.

        call deallocate_array(cm_perm)
    end subroutine rcm_reorder_method

    !================================================================!
    !【メソッド】CM (Cuthill-McKee) 並べ替え
    !================================================================!
    module subroutine cm_reorder_method(self, node_adj)
        implicit none
        class(type_reordering), intent(inout) :: self
        class(type_node_adjacency), intent(in) :: node_adj

        integer(int32) :: num_nodes

        num_nodes = node_adj%get_num_nodes()
        if (num_nodes == 0) return

        ! 1. コアとなるCM並べ替えを実行
        !    allocatableのムーブ代入を利用するため、self%permを直接渡す
        call execute_reordering_core(node_adj, self%perm)
        if (.not. allocated(self%perm)) return

        self%num_nodes = num_nodes
        self%is_reordered_perm = .true.
        self%is_reordered_iperm = .false.
    end subroutine cm_reorder_method

    !================================================================!
    !【メソッド】逆順列(iperm)を作成 (共通化)
    !================================================================!
    module subroutine create_inverse_permutation(self)
        implicit none
        class(type_reordering), intent(inout) :: self
        integer(int32) :: i

        if (.not. self%is_reordered_perm) then
            ! call error_message(932, c_opt="Inverse Permutation")
            return
        end if
        if (self%num_nodes <= 0) return

        if (allocated(self%iperm)) call deallocate_array(self%iperm)
        call allocate_array(self%iperm, self%num_nodes)
        do i = 1, self%num_nodes
            self%iperm(self%perm(i)) = i
        end do
        self%is_reordered_iperm = .true.
    end subroutine create_inverse_permutation

    !================================================================!
    ! ヘルパーサブルーチン群 (private)
    !================================================================!

    !================================================================!
    !【コアロジック】CM並べ替えを実行する共通サブルーチン
    !================================================================!
    subroutine execute_reordering_core(node_adj, cm_perm)
        implicit none
        class(type_node_adjacency), intent(in) :: node_adj
        integer(int32), allocatable, intent(out) :: cm_perm(:)

        integer(int32) :: num_nodes, i, r_count, start_node, istat
        integer(int32), allocatable :: degree(:), Q(:)
        logical, allocatable :: visited(:)

        num_nodes = node_adj%get_num_nodes()
        if (num_nodes == 0) return

        ! 1. 全ノードの次数を効率的に取得
        call allocate_array(degree, length=num_nodes)
        do i = 1, num_nodes
            degree(i) = node_adj%get_degree(i)
        end do

        ! 2. 作業用配列の確保と初期化
        call allocate_array(cm_perm, length=num_nodes)
        call allocate_array(Q, length=num_nodes)
        call allocate_array(visited, length=num_nodes)
        visited = .false.
        r_count = 0

        ! 3. 全ての連結成分を処理するまでループ
        do while (r_count < num_nodes)
            call find_start_node(num_nodes, degree, visited, start_node, istat)
            if (istat /= 0) then
                ! call error_message(931, c_opt="CM/RCM Reordering")
                if (allocated(cm_perm)) deallocate (cm_perm)
                return
            end if
            call execute_bfs_ordering(start_node, node_adj, degree, visited, Q, cm_perm, r_count)
        end do

        call deallocate_array(degree)
        call deallocate_array(Q)
        call deallocate_array(visited)
    end subroutine execute_reordering_core

    !================================================================!
    ! 幅優先探索(BFS)による並べ替え
    !================================================================!
    subroutine execute_bfs_ordering(start_node, node_adj, degree, visited, Q, R, R_count)
        implicit none
        integer(int32), intent(in) :: start_node
        class(type_node_adjacency), intent(in) :: node_adj
        integer(int32), intent(in) :: degree(:)
        logical, intent(inout) :: visited(:)
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
            call sort_and_enqueue_neighbors(current_node, node_adj, degree, visited, Q, q_tail)
        end do
    end subroutine execute_bfs_ordering

    !================================================================!
    ! 次数が小さい順にソートしてキューに追加
    !================================================================!
    subroutine sort_and_enqueue_neighbors(node, node_adj, degree, visited, Q, q_tail)
        implicit none
        integer(int32), intent(in) :: node
        class(type_node_adjacency), intent(in) :: node_adj
        integer(int32), intent(in) :: degree(:)
        logical, intent(inout) :: visited(:)
        integer(int32), intent(inout) :: Q(:), q_tail
        integer(int32), allocatable :: neighbors(:), neighbor_degrees(:), sorted_indices(:)
        integer(int32) :: i, p, n_count

        call node_adj%get_neighbors(node, neighbors)
        n_count = size(neighbors)
        if (n_count == 0) return

        call allocate_array(neighbor_degrees, length=n_count)
        call allocate_array(sorted_indices, length=n_count)
        do i = 1, n_count
            neighbor_degrees(i) = degree(neighbors(i))
        end do

        call sort_index(neighbor_degrees, sorted_indices)

        do i = 1, n_count
            p = neighbors(sorted_indices(i))
            if (.not. visited(p)) then
                visited(p) = .true.
                q_tail = q_tail + 1
                Q(q_tail) = p
            end if
        end do
        call deallocate_array(neighbors)
        call deallocate_array(neighbor_degrees)
        call deallocate_array(sorted_indices)
    end subroutine sort_and_enqueue_neighbors

    !================================================================!
    ! 開始ノードを探索
    !================================================================!
    subroutine find_start_node(num_nodes, degree, visited, start_node, istat)
        implicit none
        integer(int32), intent(in) :: num_nodes, degree(:)
        logical, intent(in) :: visited(:)
        integer(int32), intent(out) :: start_node, istat
        integer(int32) :: i, min_deg

        istat = 0
        min_deg = num_nodes + 2 ! 次数の最大値+1より大きい値
        start_node = -1
        do i = 1, num_nodes
            if (.not. visited(i) .and. degree(i) < min_deg) then
                min_deg = degree(i)
                start_node = i
            end if
        end do
        if (start_node == -1) istat = 1
    end subroutine find_start_node

end submodule reordering_methods
