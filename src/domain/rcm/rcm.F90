module domain_rcm
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_sorting, only:sort, sort_index
    use :: module_core, only:allocate_array, deallocate_array
    use :: domain_element, only:holder_elements
    use :: domain_adjacency_adjacency_node, only:type_node_adjacency

    implicit none
    private
    public :: type_rcm

    ! RCMの結果と関連操作をカプセル化する型
    type :: type_rcm
        private
        integer(int32) :: num_nodes = 0
        integer(int32), allocatable :: perm(:) ! RCMインデックス -> 元のインデックス
        integer(int32), allocatable :: iperm(:) ! 元のインデックス -> RCMインデックス
        ! ★★★ 状態管理フラグを追加 ★★★
        logical :: is_reordered_original = .false. ! permが作成済みか
        logical :: is_reordered_rcm = .false. ! ipermが作成済みか
    contains
        procedure, public, pass(self) :: reorder => rcm_reorder_method
        procedure, public, pass(self) :: invert => rcm_inverse_method
        ! publicな総称(generic)インターフェース
        procedure, private, pass(self) :: reorder_to_original_vector_int32
        procedure, private, pass(self) :: reorder_to_original_vector_real64
        procedure, private, pass(self) :: reorder_to_original_index
        procedure, private, pass(self) :: reorder_to_original_coo_indices
        ! ★★★ 新しいメソッドを追加 ★★★
        generic, public :: reorder_to_original => reorder_to_original_vector_real64, & !&
                                                  reorder_to_original_vector_int32, & !&
                                                  reorder_to_original_index, & !&
                                                  reorder_to_original_coo_indices !&

        ! RCM順序 -> 元の順序
        procedure, private, pass(self) :: reorder_to_rcm_vector_int32
        procedure, private, pass(self) :: reorder_to_rcm_vector_real64
        procedure, private, pass(self) :: reorder_to_rcm_index
        procedure, private, pass(self) :: reorder_to_rcm_coo_indices
        generic, public :: reorder_to_rcm => reorder_to_rcm_vector_real64, & !&
                                             reorder_to_rcm_vector_int32, & !&
                                             reorder_to_rcm_index, & !&
                                             reorder_to_rcm_coo_indices !&
        final :: final_destroy_rcm
    end type type_rcm

contains

    !================================================================!
    !【メソッド】要素リストを元にRCM並べ替えを実行
    !================================================================!
    subroutine rcm_reorder_method(self, elements)
        implicit none
        class(type_rcm), intent(inout) :: self
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
            if (istat /= 0) error stop "domain_rcm::reorder: Could not find a starting node."
            call execute_cm_ordering(start_node, local_node_adj, degree, visited, Q, R, r_count)
        end do

        if (allocated(self%perm)) call deallocate_array(self%perm)
        call allocate_array(self%perm, length=n_nodes)
        do i = 1, n_nodes
            self%perm(i) = R(n_nodes - i + 1)
        end do

        ! ★★★ 状態フラグを更新 ★★★
        self%is_reordered_original = .true.
        self%is_reordered_rcm = .false.
        if (allocated(self%iperm)) call deallocate_array(self%iperm)

        call deallocate_array(degree)
        call deallocate_array(R)
        call deallocate_array(Q)
        call deallocate_array(visited)
    end subroutine rcm_reorder_method

    !================================================================!
    !【メソッド】逆順列(iperm)を作成
    !================================================================!
    subroutine rcm_inverse_method(self)
        implicit none
        class(type_rcm), intent(inout) :: self
        integer(int32) :: i
        ! ★★★ permが作成済みかフラグで確認 ★★★
        if (.not. self%is_reordered_original) then
            error stop "domain_rcm::invert: 'perm' is not ready. Call 'reorder' first."
        end if
        if (allocated(self%iperm)) call deallocate_array(self%iperm)
        call allocate_array(self%iperm, self%num_nodes)
        do i = 1, self%num_nodes
            self%iperm(self%perm(i)) = i
        end do
        ! ★★★ 状態フラグを更新 ★★★
        self%is_reordered_rcm = .true.
    end subroutine rcm_inverse_method

    !================================================================!
    ! RCM順序 -> 元の順序
    !================================================================!
    subroutine reorder_to_original_vector_int32(self, vector_rcm, vector_original)
        implicit none
        class(type_rcm), intent(in) :: self
        integer(int32), intent(in) :: vector_rcm(:)
        integer(int32), intent(inout) :: vector_original(:)
        integer(int32) :: i
        if (size(vector_rcm) /= self%num_nodes .or. size(vector_original) /= self%num_nodes) error stop "Size mismatch"
        if (.not. self%is_reordered_original) error stop "'perm' not ready. Call 'reorder' first."
        do i = 1, self%num_nodes
            vector_original(self%perm(i)) = vector_rcm(i)
        end do
    end subroutine reorder_to_original_vector_int32

    subroutine reorder_to_original_vector_real64(self, vector_rcm, vector_original)
        implicit none
        class(type_rcm), intent(in) :: self
        real(real64), intent(in) :: vector_rcm(:)
        real(real64), intent(inout) :: vector_original(:)
        integer(int32) :: i
        if (size(vector_rcm) /= self%num_nodes .or. size(vector_original) /= self%num_nodes) error stop "Size mismatch"
        if (.not. self%is_reordered_original) error stop "'perm' not ready. Call 'reorder' first."
        do i = 1, self%num_nodes
            vector_original(self%perm(i)) = vector_rcm(i)
        end do
    end subroutine reorder_to_original_vector_real64

    subroutine reorder_to_original_index(self, index_rcm, index_original)
        implicit none
        class(type_rcm), intent(in) :: self
        integer(int32), intent(in) :: index_rcm
        integer(int32), intent(inout) :: index_original
        if (.not. self%is_reordered_original) error stop "'perm' not ready. Call 'reorder' first."
        index_original = self%perm(index_rcm)
    end subroutine reorder_to_original_index

    subroutine reorder_to_original_coo_indices(self, row_in, col_in, row_out, col_out)
        class(type_rcm), intent(in) :: self
        integer(int32), intent(in) :: row_in(:), col_in(:)
        integer(int32), intent(out) :: row_out(:), col_out(:)
        integer(int32) :: i

        ! permが作成済みかフラグで確認
        if (.not. self%is_reordered_original) then
            error stop "domain_rcm::reorder_to_original_coo_indices: 'perm' is not ready. Call 'reorder' first."
        end if
        ! 入力と出力のサイズが一致しているか確認
        if (size(row_in) /= size(row_out) .or. size(col_in) /= size(col_out) .or. size(row_in) /= size(col_in)) then
            error stop "domain_rcm::reorder_to_original_coo_indices: All input/output arrays must have the same size."
        end if

        ! permを使って、各(row, col)インデックスを同時に元の順序のインデックスに変換
        do i = 1, size(row_in)
            row_out(i) = self%perm(row_in(i))
            col_out(i) = self%perm(col_in(i))
        end do
    end subroutine reorder_to_original_coo_indices
    !================================================================!
    ! 元の順序 -> RCM順序
    !================================================================!
    subroutine reorder_to_rcm_vector_int32(self, vector_original, vector_reordered)
        implicit none
        class(type_rcm), intent(in) :: self
        integer(int32), intent(in) :: vector_original(:)
        integer(int32), intent(inout) :: vector_reordered(:)
        integer(int32) :: i

        if (size(vector_original) /= self%num_nodes .or. size(vector_reordered) /= self%num_nodes) error stop "Size mismatch"
        ! ★★★ ipermが作成済みかフラグで確認 ★★★
        if (.not. self%is_reordered_rcm) error stop "'iperm' not ready. Call 'invert' first."
        ! ★★★ iperm を使ったロジックに修正 (Scatter) ★★★
        do i = 1, self%num_nodes
            vector_reordered(self%iperm(i)) = vector_original(i)
        end do
    end subroutine reorder_to_rcm_vector_int32

    subroutine reorder_to_rcm_vector_real64(self, vector_original, vector_reordered)
        implicit none
        class(type_rcm), intent(in) :: self
        real(real64), intent(in) :: vector_original(:)
        real(real64), intent(inout) :: vector_reordered(:)
        integer(int32) :: i

        if (size(vector_original) /= self%num_nodes .or. size(vector_reordered) /= self%num_nodes) error stop "Size mismatch"
        ! ★★★ ipermが作成済みかフラグで確認 ★★★
        if (.not. self%is_reordered_rcm) error stop "'iperm' not ready. Call 'invert' first."
        ! ★★★ iperm を使ったロジックに修正 (Scatter) ★★★
        do i = 1, self%num_nodes
            vector_reordered(self%iperm(i)) = vector_original(i)
        end do
    end subroutine reorder_to_rcm_vector_real64

    subroutine reorder_to_rcm_index(self, index_original, index_reordered)
        implicit none
        class(type_rcm), intent(in) :: self
        integer(int32), intent(in) :: index_original
        integer(int32), intent(inout) :: index_reordered
        ! ★★★ ipermが作成済みかフラグで確認 ★★★
        if (.not. self%is_reordered_rcm) error stop "'iperm' not ready. Call 'invert' first."
        index_reordered = self%iperm(index_original)
    end subroutine reorder_to_rcm_index

    subroutine reorder_to_rcm_coo_indices(self, row_in, col_in, row_out, col_out)
        class(type_rcm), intent(in) :: self
        integer(int32), intent(in) :: row_in(:), col_in(:)
        integer(int32), intent(out) :: row_out(:), col_out(:)
        integer(int32) :: i

        ! ipermが作成済みかフラグで確認
        if (.not. self%is_reordered_rcm) then
            error stop "domain_rcm::reorder_to_rcm_coo_indices: 'iperm' is not ready. Call 'invert' first."
        end if
        ! 入力と出力のサイズが一致しているか確認
        if (size(row_in) /= size(row_out) .or. size(col_in) /= size(col_out) .or. size(row_in) /= size(col_in)) then
            error stop "domain_rcm::reorder_to_rcm_coo_indices: All input/output arrays must have the same size."
        end if

        ! ipermを使って、各(row, col)インデックスを同時にRCM順序のインデックスに変換
        do i = 1, size(row_in)
            row_out(i) = self%iperm(row_in(i))
            col_out(i) = self%iperm(col_in(i))
        end do
    end subroutine reorder_to_rcm_coo_indices

    !================================================================!
    !【解放処理】
    !================================================================!
    subroutine final_destroy_rcm(self)
        implicit none
        type(type_rcm), intent(inout) :: self

        call deallocate_array(self%perm)
        call deallocate_array(self%iperm)
    end subroutine final_destroy_rcm

    !================================================================!
    ! (以下、プライベートなヘルパーサブルーチン群 - 変更なし)
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

end module domain_rcm
