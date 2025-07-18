module domain_adjacency_adjacency_node
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use :: stdlib_sorting, only:sort
    use :: module_core, only:allocate_array, deallocate_array

    implicit none
    private

    public :: type_node_adjacency

    type :: type_node_adjacency
        integer(int32) :: num_nodes = 0
        integer(int32), allocatable :: ptr(:)
        integer(int32), allocatable :: ind(:)
    contains
        procedure, pass(self), public :: initialize => initialize_node_adjacency
        procedure, pass(self), public :: is_adjacent => check_node_adjacent
        procedure, pass(self), public :: get_degree => get_node_degree
        procedure, pass(self), public :: get_num_nodes => get_num_nodes
        ! 追加: 指定ノードの隣接ノードリストを取得
        procedure, pass(self), public :: get_neighbors => get_node_neighbors
        procedure, pass(self), public :: destroy => destroy_node_adjacency
    end type type_node_adjacency

contains

    !================================================================!
    !【コントローラー】初期化処理のメインフロー
    !================================================================!
    subroutine initialize_node_adjacency(self, num_nodes_in, num_elems, &
                                         elements_conn_data, elements_ptr)
        class(type_node_adjacency), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes_in, num_elems
        integer(int32), intent(in) :: elements_conn_data(:)
        integer(int32), intent(in) :: elements_ptr(:)

        integer(int32), allocatable :: edge_i(:), edge_j(:)
        integer(int32) :: edge_count, istat

        self%num_nodes = num_nodes_in

        ! ステップ1: 要素情報から全てのエッジ(ノードペア)を抽出
        call generate_all_edges(num_elems, elements_ptr, elements_conn_data, &
                                edge_i, edge_j, edge_count, istat)
        if (istat /= 0) then
            print *, "Error in generate_all_edges"
            return
        end if

        ! ステップ2: エッジリストからCSR形式の隣接グラフを構築
        call build_csr_from_edges(self, edge_i, edge_j, edge_count)

        call deallocate_array(edge_i)
        call deallocate_array(edge_j)
    end subroutine initialize_node_adjacency

    !================================================================!
    !【ステップ1】要素情報から全てのエッジ(ノードペア)を抽出
    !================================================================!
    subroutine generate_all_edges(num_elems, elem_ptr, elem_data, &
                                  edge_i, edge_j, edge_count, istat)
        integer(int32), intent(in) :: num_elems, elem_ptr(:), elem_data(:)
        integer(int32), allocatable, intent(out) :: edge_i(:), edge_j(:)
        integer(int32), intent(out) :: edge_count, istat

        integer(int32) :: i, j, k, n1, n2, start_idx, end_idx, est_edges

        istat = 0
        est_edges = size(elem_data) * 4 ! 推定サイズ
        call allocate_array(edge_i, est_edges)
        call allocate_array(edge_j, est_edges)
        edge_count = 0

        do i = 1, num_elems
            start_idx = elem_ptr(i)
            end_idx = elem_ptr(i + 1) - 1
            do j = start_idx, end_idx
                do k = j + 1, end_idx
                    edge_count = edge_count + 1
                    if (edge_count > est_edges) then
                        istat = -1; return ! メモリ見積もりエラー
                    end if
                    n1 = elem_data(j)
                    n2 = elem_data(k)
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
    end subroutine generate_all_edges

    !================================================================!
    !【ステップ2】エッジリストからCSR形式の隣接グラフを構築
    !================================================================!
    subroutine build_csr_from_edges(self, edge_i_in, edge_j_in, edge_count_in)
        class(type_node_adjacency), intent(inout) :: self
        integer(int32), intent(in) :: edge_i_in(:), edge_j_in(:), edge_count_in

        integer(int64), allocatable :: sort_keys(:)
        integer(int32), allocatable :: temp_deg(:), temp_pos(:)
        integer(int32) :: unique_count, i, n1, n2, total_adj

        if (edge_count_in == 0) then
            call allocate_array(self%ptr, self%num_nodes + 1_int32)
            call allocate_array(self%ind, 0_int32)
            self%ptr = 1
            return
        end if

        call allocate_array(sort_keys, int(edge_count_in, kind=int64))
        do i = 1, edge_count_in
            sort_keys(i) = int(edge_i_in(i), int64) * (self%num_nodes + 1) &
                           + int(edge_j_in(i), int64)
        end do
        call sort(sort_keys)

        unique_count = 1
        do i = 2, edge_count_in
            if (sort_keys(i) > sort_keys(i - 1)) then
                unique_count = unique_count + 1
                sort_keys(unique_count) = sort_keys(i)
            end if
        end do

        call allocate_array(temp_deg, self%num_nodes)
        temp_deg = 0

        do i = 1, unique_count
            n1 = int(sort_keys(i) / (self%num_nodes + 1))
            n2 = int(mod(sort_keys(i), int(self%num_nodes + 1, int64)))
            temp_deg(n1) = temp_deg(n1) + 1
            temp_deg(n2) = temp_deg(n2) + 1
        end do

        call allocate_array(self%ptr, self%num_nodes + 1_int32)
        self%ptr(1) = 1
        do i = 1, self%num_nodes
            self%ptr(i + 1) = self%ptr(i) + temp_deg(i)
        end do

        total_adj = self%ptr(self%num_nodes + 1) - 1
        call allocate_array(self%ind, total_adj)
        call allocate_array(temp_pos, self%num_nodes)
        temp_pos = self%ptr(1:self%num_nodes)

        do i = 1, unique_count
            n1 = int(sort_keys(i) / (self%num_nodes + 1))
            n2 = int(mod(sort_keys(i), int(self%num_nodes + 1, int64)))
            self%ind(temp_pos(n1)) = n2
            temp_pos(n1) = temp_pos(n1) + 1
            self%ind(temp_pos(n2)) = n1
            temp_pos(n2) = temp_pos(n2) + 1
        end do

        call deallocate_array(sort_keys)
        call deallocate_array(temp_deg)
        call deallocate_array(temp_pos)
    end subroutine build_csr_from_edges

    !================================================================!
    ! 照会・取得・解放用サブルーチン
    !================================================================!
    function check_node_adjacent(self, i, j) result(is_adj)
        class(type_node_adjacency), intent(in) :: self
        integer(int32), intent(in) :: i, j; logical :: is_adj
        integer(int32) :: k, startp, endp

        is_adj = .false.
        if (i < 1 .or. i > self%num_nodes .or. j < 1 .or. j > self%num_nodes) return

        startp = self%ptr(i)
        endp = self%ptr(i + 1) - 1

        do k = startp, endp
            if (self%ind(k) == j) then
                is_adj = .true.
                return
            end if
        end do
    end function

    function get_node_degree(self, i) result(deg)
        class(type_node_adjacency), intent(in) :: self
        integer(int32), intent(in) :: i; integer(int32) :: deg
        if (i < 1 .or. i > self%num_nodes) then
            deg = 0
        else
            deg = self%ptr(i + 1) - self%ptr(i)
        end if
    end function

    function get_num_nodes(self) result(n)
        class(type_node_adjacency), intent(in) :: self
        integer(int32) :: n
        n = self%num_nodes
    end function

    subroutine destroy_node_adjacency(self)
        class(type_node_adjacency), intent(inout) :: self
        if (allocated(self%ptr)) call deallocate_array(self%ptr)
        if (allocated(self%ind)) call deallocate_array(self%ind)
        self%num_nodes = 0
    end subroutine

    !================================================================!
    !【追加】指定されたノードの隣接ノードリストを取得する
    !================================================================!
    subroutine get_node_neighbors(self, node_id, neighbors)
        class(type_node_adjacency), intent(in) :: self
        integer(int32), intent(in) :: node_id
        integer(int32), allocatable, intent(out) :: neighbors(:)

        integer(int32) :: degree, start_p, end_p

        if (node_id < 1 .or. node_id > self%num_nodes) then
            ! 不正なノードIDの場合は、0サイズの配列を返す
            if (allocated(neighbors)) deallocate (neighbors)
            allocate (neighbors(0))
            return
        end if

        ! 1. 隣接ノードの数（次数）を計算
        start_p = self%ptr(node_id)
        end_p = self%ptr(node_id + 1) - 1
        degree = end_p - start_p + 1

        ! 2. 戻り値の配列を確保]
        if (allocated(neighbors)) deallocate (neighbors)
        allocate (neighbors(degree))

        ! 3. self%indから隣接ノードのリストをコピー
        neighbors = self%ind(start_p:end_p)

    end subroutine get_node_neighbors

end module domain_adjacency_adjacency_node
