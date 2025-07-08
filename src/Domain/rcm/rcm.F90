module domain_rcm
    use, intrinsic :: iso_fortran_env, only: int32, real64, logical32
    use :: core_core, only:allocate_array, deallocate_array
    use :: domain_adjacency, only:type_node_adjacency
    use :: stdlib_sorting, only:sort_index
    implicit none
    private

    public :: type_rcm

    ! RCMの結果と関連操作をカプセル化する型
    type :: type_rcm
        integer(int32) :: num_nodes = 0
        integer(int32), allocatable :: perm(:)
        integer(int32), allocatable :: iperm(:)
    contains
        procedure, public, pass(self) :: reorder => rcm_reorder_method
        procedure, public, pass(self) :: invert => rcm_inverse_method
        procedure, public, pass(self) :: restore_vector => reorder_to_original_method
        final :: final_destroy_rcm
    end type type_rcm

contains

    !================================================================!
    !【メソッド】節点隣接グラフを元にRCM並べ替えを実行
    !================================================================!
    subroutine rcm_reorder_method(self, node_adj)
        class(type_rcm), intent(inout) :: self
        class(type_node_adjacency), intent(in) :: node_adj

        integer(int32) :: n_nodes, i, r_count, start_node, istat
        integer(int32), allocatable :: degree(:), R(:), Q(:)
        logical(logical32), allocatable :: visited(:)

        n_nodes = node_adj%get_num_nodes()
        self%num_nodes = n_nodes

        if (n_nodes == 0) then
            if (allocated(self%perm)) call deallocate_array(self%perm)
            if (allocated(self%iperm)) call deallocate_array(self%iperm)
            return
        end if

        call allocate_array(degree, n_nodes)
        do i = 1, n_nodes
            degree(i) = node_adj%get_degree(i)
        end do

        call allocate_array(R, n_nodes)
        call allocate_array(Q, n_nodes)
        call allocate_array(visited, n_nodes)
        visited = .false.
        r_count = 0

        do while (r_count < n_nodes)
            call find_start_node(n_nodes, degree, visited, start_node, istat)
            if (istat /= 0) then
                error stop "domain_rcm::reorder: Could not find a starting node."
            end if
            call execute_cm_ordering(start_node, node_adj, degree, visited, Q, R, r_count)
        end do

        if (allocated(self%perm)) call deallocate_array(self%perm)
        call allocate_array(self%perm, n_nodes)
        do i = 1, n_nodes
            self%perm(i) = R(n_nodes - i + 1)
        end do

        call deallocate_array(degree)
        call deallocate_array(R)
        call deallocate_array(Q)
        call deallocate_array(visited)
    end subroutine rcm_reorder_method

    !================================================================!
    !【メソッド】逆順列(iperm)を作成
    !================================================================!
    subroutine rcm_inverse_method(self)
        class(type_rcm), intent(inout) :: self
        integer(int32) :: i

        if (.not. allocated(self%perm)) then
            error stop "domain_rcm::invert: 'perm' is not allocated. Call 'reorder' first."
        end if

        if (allocated(self%iperm)) call deallocate_array(self%iperm)
        call allocate_array(self%iperm, self%num_nodes)
        do i = 1, self%num_nodes
            self%iperm(self%perm(i)) = i
        end do
    end subroutine rcm_inverse_method

    !================================================================!
    !【メソッド】RCM順序のベクトルを元の順序に戻す
    !================================================================!
    subroutine reorder_to_original_method(self, vector_rcm, vector_original)
        class(type_rcm), intent(in) :: self
        real(real64), intent(in) :: vector_rcm(:)
        real(real64), intent(out) :: vector_original(:)
        integer(int32) :: i

        if (size(vector_rcm) /= self%num_nodes .or. size(vector_original) /= self%num_nodes) then
            error stop "domain_rcm::restore_vector: Vector size mismatch."
        end if
        if (.not. allocated(self%perm)) then
            error stop "domain_rcm::restore_vector: 'perm' is not allocated. Call 'reorder' first."
        end if

        do i = 1, self%num_nodes
            vector_original(self%perm(i)) = vector_rcm(i)
        end do
    end subroutine reorder_to_original_method

    !================================================================!
    ! RCMアルゴリズム用のプライベート・ヘルパーサブルーチン群
    !================================================================!
    subroutine find_start_node(num_nodes, degree, visited, start_node, istat)
        integer(int32), intent(in) :: num_nodes, degree(:)
        logical(logical32), intent(in) :: visited(:)
        integer(int32), intent(out) :: start_node, istat
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
        if (start_node == -1) then
            istat = 1
        end if
    end subroutine find_start_node

    subroutine execute_cm_ordering(start_node, node_adj, degree, visited, Q, R, R_count)
        integer(int32), intent(in) :: start_node
        class(type_node_adjacency), intent(in) :: node_adj
        integer(int32), intent(in) :: degree(:)
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
            call sort_and_enqueue_neighbors(current_node, node_adj, degree, visited, Q, q_tail)
        end do
    end subroutine execute_cm_ordering

    subroutine sort_and_enqueue_neighbors(node, node_adj, degree, visited, Q, q_tail)
        integer(int32), intent(in) :: node
        class(type_node_adjacency), intent(in) :: node_adj
        integer(int32), intent(in) :: degree(:)
        logical(logical32), intent(inout) :: visited(:)
        integer(int32), intent(inout) :: Q(:), q_tail
        integer(int32), allocatable :: neighbors(:), neighbor_degrees(:), sorted_indices(:)
        integer(int32) :: i, p, n_count

        n_count = node_adj%get_degree(node)
        if (n_count == 0) return

        call allocate_array(neighbors, n_count)
        call allocate_array(neighbor_degrees, n_count)
        call allocate_array(sorted_indices, n_count)

        neighbors = node_adj%ind(node_adj%ptr(node):node_adj%ptr(node + 1) - 1)
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
    !【解放処理】
    !================================================================!
    subroutine final_destroy_rcm(self)
        type(type_rcm), intent(inout) :: self
        if (allocated(self%perm)) then
            call deallocate_array(self%perm)
        end if
        if (allocated(self%iperm)) then
            call deallocate_array(self%iperm)
        end if
    end subroutine final_destroy_rcm

end module domain_rcm
