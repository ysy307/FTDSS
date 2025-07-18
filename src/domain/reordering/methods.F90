submodule(domain_reordering) reordering_methods
    implicit none

contains

    !================================================================!
    !【メソッド】要素リストを元にRCM並べ替えを実行
    !================================================================!
    module subroutine rcm_reorder_method(self, elements)
        implicit none
        class(type_reordering), intent(inout) :: self
        type(holder_elements), intent(in) :: elements(:)

        type(type_node_adjacency) :: local_node_adj
        integer(int32) :: n_nodes, i, r_count, start_node, istat
        integer(int32), allocatable :: degree(:), R(:), Q(:)
        logical, allocatable :: visited(:)

        call build_node_adjacency_from_elements(elements, local_node_adj)
        n_nodes = local_node_adj%get_num_nodes()
        self%num_nodes = n_nodes
        if (n_nodes == 0) return

        call allocate_array(degree, length=n_nodes)
        do i = 1, n_nodes
            degree(i) = local_node_adj%get_degree(i)
        end do

        call allocate_array(R, length=n_nodes)
        call allocate_array(Q, length=n_nodes)
        call allocate_array(visited, length=n_nodes)
        visited = .false.
        r_count = 0

        do while (r_count < n_nodes)
            call find_start_node(n_nodes, degree, visited, start_node, istat)
            if (istat /= 0) then
                call error_message(931, c_opt="RCM Reordering")
            end if
            call execute_cm_ordering(start_node, local_node_adj, degree, visited, Q, R, r_count)
        end do

        if (allocated(self%perm)) call deallocate_array(self%perm)
        call allocate_array(self%perm, length=n_nodes)
        do i = 1, n_nodes
            self%perm(i) = R(n_nodes - i + 1)
        end do

        self%is_reordered_perm = .true.
        self%is_reordered_iperm = .false.

        call deallocate_array(degree)
        call deallocate_array(R)
        call deallocate_array(Q)
        call deallocate_array(visited)
    end subroutine rcm_reorder_method

    module subroutine cm_reorder_method(self, elements)
        implicit none
        class(type_reordering), intent(inout) :: self
        type(holder_elements), intent(in) :: elements(:)

        type(type_node_adjacency) :: local_node_adj
        integer(int32) :: n_nodes, i, r_count, start_node, istat
        integer(int32), allocatable :: degree(:), R(:), Q(:)
        logical, allocatable :: visited(:)

        call build_node_adjacency_from_elements(elements, local_node_adj)
        n_nodes = local_node_adj%get_num_nodes()
        self%num_nodes = n_nodes
        if (n_nodes == 0) return

        call allocate_array(degree, length=n_nodes)
        do i = 1, n_nodes
            degree(i) = local_node_adj%get_degree(i)
        end do

        call allocate_array(R, length=n_nodes)
        call allocate_array(Q, length=n_nodes)
        call allocate_array(visited, length=n_nodes)
        visited = .false.
        r_count = 0

        do while (r_count < n_nodes)
            call find_start_node(n_nodes, degree, visited, start_node, istat)
            if (istat /= 0) then
                call error_message(931, c_opt="CM Reordering")
            end if
            call execute_cm_ordering(start_node, local_node_adj, degree, visited, Q, R, r_count)
        end do

        call deallocate_array(self%perm)
        call allocate_array(self%perm, length=n_nodes)
        self%perm(:) = R(:)

        self%is_reordered_perm = .true.
        self%is_reordered_iperm = .false.

        call deallocate_array(degree)
        call deallocate_array(R)
        call deallocate_array(Q)
        call deallocate_array(visited)
    end subroutine cm_reorder_method

    !================================================================!
    !【メソッド】逆順列(iperm)を作成
    !================================================================!
    module subroutine rcm_inverse_method(self)
        implicit none
        class(type_reordering), intent(inout) :: self
        integer(int32) :: i

        if (.not. self%is_reordered_perm) call error_message(932, c_opt="RCM Inversion")

        call deallocate_array(self%iperm)
        call allocate_array(self%iperm, self%num_nodes)
        do i = 1, self%num_nodes
            self%iperm(self%perm(i)) = i
        end do

        self%is_reordered_iperm = .true.
    end subroutine rcm_inverse_method

    module subroutine cm_inverse_method(self)
        implicit none
        class(type_reordering), intent(inout) :: self
        integer(int32) :: i

        if (.not. self%is_reordered_perm) call error_message(932, c_opt="CM Inversion")

        call deallocate_array(self%iperm)
        call allocate_array(self%iperm, self%num_nodes)
        do i = 1, self%num_nodes
            self%iperm(self%perm(i)) = i
        end do

        self%is_reordered_iperm = .true.
    end subroutine cm_inverse_method

    !================================================================!
    ! ヘルパーサブルーチン群
    !================================================================!
    subroutine build_node_adjacency_from_elements(elements, node_adj)
        implicit none
        type(holder_elements), intent(in) :: elements(:)
        type(type_node_adjacency), intent(inout) :: node_adj
        integer(int32) :: num_elements, num_nodes, total_conn_size
        integer(int32), allocatable :: elements_conn_data(:), elements_ptr(:)
        integer(int32) :: i, k, current_pos

        num_elements = size(elements)
        if (num_elements == 0) then
            call node_adj%initialize(0, 0, [integer(int32) ::], [integer(int32) :: 1])
            return
        end if
        num_nodes = 0
        total_conn_size = 0
        do i = 1, num_elements
            if (size(elements(i)%e%connectivity) > 0) then
                num_nodes = max(num_nodes, maxval(elements(i)%e%connectivity))
            end if
            total_conn_size = total_conn_size + size(elements(i)%e%connectivity)
        end do
        call allocate_array(elements_ptr, length=num_elements + 1_int32)
        call allocate_array(elements_conn_data, length=total_conn_size)
        current_pos = 1
        elements_ptr(1) = 1
        do i = 1, num_elements
            k = size(elements(i)%e%connectivity)
            if (k > 0) then
                elements_conn_data(current_pos:current_pos + k - 1) = elements(i)%e%connectivity
            end if
            current_pos = current_pos + k
            elements_ptr(i + 1) = current_pos
        end do
        call node_adj%initialize(num_nodes, num_elements, elements_conn_data, elements_ptr)
        call deallocate_array(elements_ptr)
        call deallocate_array(elements_conn_data)
    end subroutine build_node_adjacency_from_elements

    subroutine execute_cm_ordering(start_node, node_adj, degree, visited, Q, R, R_count)
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
    end subroutine execute_cm_ordering

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

    subroutine find_start_node(num_nodes, degree, visited, start_node, istat)
        implicit none
        integer(int32), intent(in) :: num_nodes, degree(:)
        logical, intent(in) :: visited(:)
        integer(int32), intent(inout) :: start_node, istat
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

end submodule reordering_methods
