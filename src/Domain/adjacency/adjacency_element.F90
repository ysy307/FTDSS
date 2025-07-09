module domain_adjacency_adjacency_element
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use :: stdlib_sorting, only:sort
    use :: module_core, only:allocate_array, deallocate_array

    implicit none
    private

    public :: type_element_adjacency

    type :: type_element_adjacency
        integer(int32) :: num_elements = 0
        integer(int32), allocatable :: ptr(:)
        integer(int32), allocatable :: ind(:)
    contains
        procedure, pass(self), public :: initialize => initialize_adjacency
        procedure, pass(self), public :: is_adjacent => check_adjacent_sparse
        procedure, pass(self), public :: get_degree => get_degree_sparse
        procedure, pass(self), public :: get_num_elements => get_num_elements
        procedure, pass(self), public :: destroy => destroy_adjacency
    end type type_element_adjacency

contains

    !================================================================!
    !【コントローラー】初期化処理のメインフロー
    !================================================================!
    subroutine initialize_adjacency(self, num_nodes, elements_conn_data, elements_ptr)
        class(type_element_adjacency), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in) :: elements_conn_data(:)
        integer(int32), intent(in) :: elements_ptr(:)

        integer(int32), allocatable :: node_to_elem_ptr(:)
        integer(int32), allocatable :: node_to_elem_data(:)
        integer(int32), allocatable :: adj_i(:)
        integer(int32), allocatable :: adj_j(:)
        integer(int32) :: pair_count

        self%num_elements = size(elements_ptr) - 1

        ! ステップ1: ノード->要素の逆引きマップ作成
        call build_node_to_element_map(num_nodes, self%num_elements, &
                                       elements_conn_data, elements_ptr, &
                                       node_to_elem_ptr, node_to_elem_data)

        ! ステップ2: 隣接ペア列挙
        call generate_adjacent_pairs(num_nodes, node_to_elem_ptr, node_to_elem_data, &
                                     adj_i, adj_j, pair_count)

        call deallocate_array(node_to_elem_ptr)
        call deallocate_array(node_to_elem_data)

        ! ステップ3: CSR 構築
        call build_csr_from_pairs(self, adj_i, adj_j, pair_count)

        call deallocate_array(adj_i)
        call deallocate_array(adj_j)
    end subroutine initialize_adjacency

    !================================================================!
    !【ステップ1】ノード->要素の逆引きマップ作成 (ロジック修正版)
    !================================================================!
    subroutine build_node_to_element_map(num_nodes, num_elems, data, ptr_in, node_ptr, node_data)
        integer(int32), intent(in) :: num_nodes, num_elems
        integer(int32), intent(in) :: data(:), ptr_in(:)
        integer(int32), allocatable, intent(out) :: node_ptr(:), node_data(:)

        integer(int32), allocatable :: temp_counts(:)
        integer(int32) :: i, j, id, total

        call allocate_array(temp_counts, num_nodes)
        temp_counts = 0

        do i = 1, num_elems
            do j = ptr_in(i), ptr_in(i + 1) - 1
                id = data(j)
                if (id >= 1 .and. id <= num_nodes) then
                    temp_counts(id) = temp_counts(id) + 1
                end if
            end do
        end do

        call allocate_array(node_ptr, num_nodes + 1_int32)
        node_ptr(1) = 1
        do i = 1, num_nodes
            node_ptr(i + 1) = node_ptr(i) + temp_counts(i)
        end do

        total = node_ptr(num_nodes + 1) - 1
        call allocate_array(node_data, total)

        temp_counts = node_ptr(1:num_nodes)

        do i = 1, num_elems
            do j = ptr_in(i), ptr_in(i + 1) - 1
                id = data(j)
                if (id >= 1 .and. id <= num_nodes) then
                    node_data(temp_counts(id)) = i
                    temp_counts(id) = temp_counts(id) + 1
                end if
            end do
        end do

        call deallocate_array(temp_counts)
    end subroutine build_node_to_element_map

    !================================================================!
    !【ステップ2】隣接ペア列挙 (ロジック修正版)
    !================================================================!
    subroutine generate_adjacent_pairs(num_nodes, node_ptr, node_data, adj_i, adj_j, count)
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in) :: node_ptr(:), node_data(:)
        integer(int32), allocatable, intent(out) :: adj_i(:), adj_j(:)
        integer(int32), intent(out) :: count

        integer(int32) :: est, i, j, k, s, e, n1, n2

        est = size(node_data) * 30 ! 推定サイズ
        call allocate_array(adj_i, est)
        call allocate_array(adj_j, est)
        count = 0

        do i = 1, num_nodes
            s = node_ptr(i)
            e = node_ptr(i + 1) - 1
            do j = s, e - 1
                do k = j + 1, e
                    count = count + 1
                    if (count > est) then
                        ! 必要であればここで配列を拡張する処理を追加
                        error stop "generate_adjacent_pairs: estimation size exceeded."
                    end if
                    n1 = node_data(j)
                    n2 = node_data(k)
                    if (n1 < n2) then
                        adj_i(count) = n1
                        adj_j(count) = n2
                    else
                        adj_i(count) = n2
                        adj_j(count) = n1
                    end if
                end do
            end do
        end do
    end subroutine generate_adjacent_pairs

    !================================================================!
    !【ステップ3】CSR 構築 (重複削除処理を追加した修正版)
    !================================================================!
    subroutine build_csr_from_pairs(self, adj_i_in, adj_j_in, count_in)
        class(type_element_adjacency), intent(inout) :: self
        integer(int32), intent(in) :: adj_i_in(:), adj_j_in(:)
        integer(int32), intent(in) :: count_in

        integer(int64), allocatable :: sort_keys(:)
        integer(int32) :: unique_count, i, n1, n2, total_adj
        integer(int32), allocatable :: deg(:), pos(:)

        if (count_in == 0) then
            call allocate_array(self%ptr, self%num_elements + 1_int32)
            self%ptr = 1
            call allocate_array(self%ind, 0_int32)
            return
        end if

        ! --- ペアのソートと重複削除 (重要) ---
        call allocate_array(sort_keys, int(count_in, int64))
        do i = 1, count_in
            sort_keys(i) = int(adj_i_in(i), int64) * self%num_elements + int(adj_j_in(i), int64)
        end do
        call sort(sort_keys)

        unique_count = 1
        do i = 2, count_in
            if (sort_keys(i) > sort_keys(i - 1)) then
                unique_count = unique_count + 1
                sort_keys(unique_count) = sort_keys(i)
            end if
        end do

        ! --- CSR構築 ---
        call allocate_array(deg, self%num_elements)
        deg = 0
        do i = 1, unique_count
            n1 = int(sort_keys(i) / self%num_elements)
            n2 = int(mod(sort_keys(i), int(self%num_elements, int64)))
            deg(n1) = deg(n1) + 1
            deg(n2) = deg(n2) + 1
        end do

        call allocate_array(self%ptr, self%num_elements + 1_int32)
        self%ptr(1) = 1
        do i = 1, self%num_elements
            self%ptr(i + 1) = self%ptr(i) + deg(i)
        end do

        total_adj = self%ptr(self%num_elements + 1) - 1
        call allocate_array(self%ind, total_adj)

        call allocate_array(pos, self%num_elements)
        pos = self%ptr(1:self%num_elements)

        do i = 1, unique_count
            n1 = int(sort_keys(i) / self%num_elements)
            n2 = int(mod(sort_keys(i), int(self%num_elements, int64)))
            self%ind(pos(n1)) = n2
            pos(n1) = pos(n1) + 1
            self%ind(pos(n2)) = n1
            pos(n2) = pos(n2) + 1
        end do

        call deallocate_array(sort_keys)
        call deallocate_array(deg)
        call deallocate_array(pos)
    end subroutine build_csr_from_pairs

    !================================================================!
    ! 照会・取得・解放用サブルーチン
    !================================================================!
    function check_adjacent_sparse(self, i, j) result(is_adj)
        class(type_element_adjacency), intent(in) :: self
        integer(int32), intent(in) :: i, j
        logical :: is_adj
        integer(int32) :: k, startp, endp

        is_adj = .false.
        if (i < 1 .or. i > self%num_elements) then
            return
        end if
        if (j < 1 .or. j > self%num_elements) then
            return
        end if

        startp = self%ptr(i)
        endp = self%ptr(i + 1) - 1
        do k = startp, endp
            if (self%ind(k) == j) then
                is_adj = .true.
                return
            end if
        end do
    end function check_adjacent_sparse

    function get_degree_sparse(self, i) result(deg)
        class(type_element_adjacency), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32) :: deg

        if (i < 1 .or. i > self%num_elements) then
            deg = 0
        else
            deg = self%ptr(i + 1) - self%ptr(i)
        end if
    end function get_degree_sparse

    function get_num_elements(self) result(n)
        class(type_element_adjacency), intent(in) :: self
        integer(int32) :: n
        n = self%num_elements
    end function get_num_elements

    subroutine destroy_adjacency(self)
        class(type_element_adjacency), intent(inout) :: self
        if (allocated(self%ptr)) then
            call deallocate_array(self%ptr)
        end if
        if (allocated(self%ind)) then
            call deallocate_array(self%ind)
        end if
        self%num_elements = 0
    end subroutine destroy_adjacency

end module domain_adjacency_adjacency_element
!================================================================!