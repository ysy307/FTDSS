!================================================================!
! domain_adjacency_adjacency_node (ハイブリッド形式 / 自己ループ対応)
!
! CSR行列構築のため、自己ループ(対角成分)を含む隣接関係を
! COO形式とCSR形式の両方で保持するモジュール。
!================================================================!
module domain_adjacency_adjacency_node
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use :: module_core, only:allocate_array, deallocate_array, unique
    use :: module_mesh, only:holder_sides, holder_elements

    implicit none
    private
    public :: type_node_adjacency

    !> @type type_node_adjacency
    !! @brief ノードの隣接関係をCOO形式とCSR形式の両方で保持する型。
    type :: type_node_adjacency
        integer(int32) :: num_nodes = 0
        integer(int32) :: nnz = 0

        ! COO (Coordinate list) 形式のデータ
        integer(int32), allocatable :: row(:)
        integer(int32), allocatable :: col(:)

        ! CSR (Compressed Sparse Row) 形式のデータ
        integer(int32), allocatable :: ptr(:)
        integer(int32), allocatable :: ind(:)
    contains
        procedure, pass(self), public :: initialize => initialize_hybrid_from_mesh
        procedure, pass(self), public :: get_num_nodes => get_num_nodes
        procedure, pass(self), public :: get_degree => get_degree_csr
        procedure, pass(self), public :: get_neighbors => get_neighbors_csr
        procedure, pass(self), public :: get_nnz => get_nnz
        procedure, pass(self), public :: get_coo => get_coo
        procedure, pass(self), public :: get_csr => get_csr
        procedure, pass(self), public :: destroy => destroy_hybrid
    end type type_node_adjacency

contains

    !================================================================!
    ! メインの初期化処理 (COOとCSRを両方構築)
    !================================================================!
    subroutine initialize_hybrid_from_mesh(self, num_nodes, computation_dimension, sides, elements)
        implicit none
        class(type_node_adjacency), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in) :: computation_dimension
        class(holder_sides), intent(in) :: sides(:)
        class(holder_elements), intent(in) :: elements(:)

        integer(int32) :: estimated_nnz, actual_nnz
        integer(int32), allocatable :: temp_row_indices(:), temp_col_indices(:)

        self%num_nodes = num_nodes
        if (self%num_nodes <= 0) return

        ! 1. 一時COO配列の最大サイズを見積もる
        estimated_nnz = estimate_max_coo_size(computation_dimension, sides, elements)
        if (estimated_nnz <= 0) return

        call allocate_array(temp_row_indices, estimated_nnz)
        call allocate_array(temp_col_indices, estimated_nnz)

        ! 2. メッシュから重複を含むCOOリストを生成
        call create_coo_from_mesh(computation_dimension, sides, elements, temp_row_indices, temp_col_indices, actual_nnz)

        ! 3.【COO構築】一時COOリストからユニークなCOOリストを作成
        call create_unique_coo(self, temp_row_indices(1:actual_nnz), temp_col_indices(1:actual_nnz))

        ! 4.【CSR構築】作成したユニークCOOリストからCSR形式を構築
        call build_csr_from_coo(self)

        call deallocate_array(temp_row_indices)
        call deallocate_array(temp_col_indices)
    end subroutine initialize_hybrid_from_mesh

    !================================================================!
    ! COO配列の最大サイズを見積もる
    !================================================================!
    function estimate_max_coo_size(computation_dimension, sides, elements) result(max_size)
        implicit none
        integer(int32), intent(in) :: computation_dimension
        class(holder_sides), intent(in) :: sides(:)
        class(holder_elements), intent(in) :: elements(:)
        integer(int32) :: max_size
        integer(int32) :: i

        max_size = 0

        if (computation_dimension >= 2) then
            !$omp parallel do reduction(+:max_size) private(i)
            do i = 1, size(elements)
                max_size = max_size + elements(i)%e%get_num_nodes()**2
            end do
            !$omp end parallel do
        end if

        if (computation_dimension >= 1) then
            !$omp parallel do reduction(+:max_size) private(i)
            do i = 1, size(sides)
                max_size = max_size + sides(i)%s%get_num_nodes()**2
            end do
            !$omp end parallel do
        end if
    end function estimate_max_coo_size

    !================================================================!
    ! メッシュ情報からCOOリストを生成 (並列化対応)
    !================================================================!
    subroutine create_coo_from_mesh(computation_dimension, sides, elements, row_indices, col_indices, counter)
        implicit none
        integer(int32), intent(in) :: computation_dimension
        class(holder_sides), intent(in) :: sides(:)
        class(holder_elements), intent(in) :: elements(:)
        integer(int32), intent(out) :: row_indices(:), col_indices(:)
        integer(int32), intent(out) :: counter

        integer(int32) :: num_elements, num_sides
        integer(int32), allocatable :: offsets_e(:), offsets_s(:)
        integer(int32) :: i, j, k, base, nodes_per_mesh
        integer(int32), pointer :: p_conn(:) => null()

        num_elements = size(elements)
        num_sides = size(sides)

        ! 並列書き込みのためのオフセット計算
        call allocate_array(offsets_e, num_elements + 1)
        call allocate_array(offsets_s, num_sides + 1)
        offsets_e(1) = 0
        if (computation_dimension >= 2) then
            do i = 1, num_elements
                offsets_e(i + 1) = offsets_e(i) + elements(i)%e%get_num_nodes()**2
            end do
        else
            offsets_e(2:) = 0
        end if

        offsets_s(1) = 0
        if (computation_dimension >= 1) then
            do i = 1, num_sides
                offsets_s(i + 1) = offsets_s(i) + sides(i)%s%get_num_nodes()**2
            end do
        else
            offsets_s(2:) = 0
        end if

        !$omp parallel do private(i, j, k, base, nodes_per_mesh, p_conn)
        do i = 1, num_elements
            if (computation_dimension < 2) cycle
            nodes_per_mesh = elements(i)%e%get_num_nodes()
            p_conn => elements(i)%e%get_connectivity()
            base = offsets_e(i)
            do j = 1, nodes_per_mesh
                do k = 1, nodes_per_mesh
                    row_indices(base + (j - 1) * nodes_per_mesh + k) = p_conn(j)
                    col_indices(base + (j - 1) * nodes_per_mesh + k) = p_conn(k)
                end do
            end do
        end do
        !$omp end parallel do

        base = offsets_e(num_elements + 1)
        !$omp parallel do private(i, j, k, nodes_per_mesh, p_conn)
        do i = 1, num_sides
            if (computation_dimension < 1) cycle
            nodes_per_mesh = sides(i)%s%get_num_nodes()
            p_conn => sides(i)%s%get_connectivity()
            do j = 1, nodes_per_mesh
                do k = 1, nodes_per_mesh
                    row_indices(base + offsets_s(i) + (j - 1) * nodes_per_mesh + k) = p_conn(j)
                    col_indices(base + offsets_s(i) + (j - 1) * nodes_per_mesh + k) = p_conn(k)
                end do
            end do
        end do
        !$omp end parallel do

        counter = offsets_e(num_elements + 1) + offsets_s(num_sides + 1)
        deallocate (offsets_e, offsets_s)
    end subroutine create_coo_from_mesh

    !================================================================!
    !【COO構築】一時COOリストからユニークなCOOリストを生成 (自己ループ対応)
    !================================================================!
    subroutine create_unique_coo(self, temp_row, temp_col)
        implicit none
        class(type_node_adjacency), intent(inout) :: self
        integer(int32), intent(in) :: temp_row(:), temp_col(:)

        integer(int64), allocatable :: packed_edges(:), unique_packed_edges(:)
        integer(int32) :: i, n1, n2, edge_count

        if (size(temp_row) == 0) return

        ! (i,j) と (j,i) の両方を持つ対称COOリストを作成するため、2倍のサイズを確保
        allocate (packed_edges(size(temp_row) * 2))
        edge_count = 0
        do i = 1, size(temp_row)
            n1 = temp_row(i)
            n2 = temp_col(i)

            ! 【修正点】自己ループ(i,i)の場合は、対称な(i,i)を追加する必要はない
            if (n1 == n2) then
                edge_count = edge_count + 1
                packed_edges(edge_count) = ishft(int(n1, int64), 32) + int(n2, int64)
            else
                edge_count = edge_count + 1
                packed_edges(edge_count) = ishft(int(n1, int64), 32) + int(n2, int64)

                edge_count = edge_count + 1
                packed_edges(edge_count) = ishft(int(n2, int64), 32) + int(n1, int64)
            end if
        end do

        ! ソートしてユニークなエッジのみを抽出
        call unique(packed_edges(1:edge_count), unique_packed_edges)
        deallocate (packed_edges)

        self%nnz = size(unique_packed_edges)
        call allocate_array(self%row, self%nnz)
        call allocate_array(self%col, self%nnz)

        ! 結果を自身のCOOメンバに格納
        do i = 1, self%nnz
            self%row(i) = int(ishft(unique_packed_edges(i), -32), kind=int32)
            self%col(i) = int(iand(unique_packed_edges(i), int(z'FFFFFFFF', int64)), kind=int32)
        end do
        deallocate (unique_packed_edges)
    end subroutine create_unique_coo

    !================================================================!
    !【CSR構築】自身のCOOメンバからCSR形式を構築
    !================================================================!
    subroutine build_csr_from_coo(self)
        implicit none
        class(type_node_adjacency), intent(inout) :: self
        integer(int32) :: i
        integer(int32), allocatable :: degree(:)

        if (self%nnz == 0) then
            if (self%num_nodes > 0) then
                call allocate_array(self%ptr, self%num_nodes + 1)
                self%ptr = 1
            end if
            call allocate_array(self%ind, 0)
            return
        end if

        ! 1. 各ノードの次数を計算
        call allocate_array(degree, self%num_nodes)
        degree = 0
        do i = 1, self%nnz
            degree(self%row(i)) = degree(self%row(i)) + 1
        end do

        ! 2. 次数の累積和からptr配列を構築
        call allocate_array(self%ptr, self%num_nodes + 1)
        self%ptr(1) = 1
        do i = 1, self%num_nodes
            self%ptr(i + 1) = self%ptr(i) + degree(i)
        end do
        deallocate (degree)

        ! 3. ind配列を構築 (COOのcol配列をソートして格納)
        call allocate_array(self%ind, self%nnz)
        self%ind = self%col
        do i = 1, self%num_nodes
            if (self%ptr(i + 1) > self%ptr(i)) then
                call sort(self%ind(self%ptr(i):self%ptr(i + 1) - 1))
            end if
        end do
    end subroutine build_csr_from_coo

    !================================================================!
    ! 照会・解放用サブルーチン
    !================================================================!
    pure function get_num_nodes(self) result(n_nodes)
        implicit none
        class(type_node_adjacency), intent(in) :: self
        integer(int32) :: n_nodes
        n_nodes = self%num_nodes
    end function get_num_nodes

    pure function get_degree_csr(self, node_id) result(degree)
        implicit none
        class(type_node_adjacency), intent(in) :: self
        integer(int32), intent(in) :: node_id
        integer(int32) :: degree
        if (node_id < 1 .or. node_id > self%num_nodes) then
            degree = 0; return
        end if
        degree = self%ptr(node_id + 1) - self%ptr(node_id)
    end function get_degree_csr

    subroutine get_neighbors_csr(self, node_id, neighbors)
        implicit none
        class(type_node_adjacency), intent(in) :: self
        integer(int32), intent(in) :: node_id
        integer(int32), allocatable, intent(out) :: neighbors(:)
        integer(int32) :: start_p, end_p, degree

        if (node_id < 1 .or. node_id > self%num_nodes) then
            allocate (neighbors(0))
            return
        end if

        start_p = self%ptr(node_id)
        end_p = self%ptr(node_id + 1) - 1
        degree = end_p - start_p + 1

        if (degree <= 0) then
            allocate (neighbors(0))
            return
        end if

        allocate (neighbors(degree))
        neighbors = self%ind(start_p:end_p)
    end subroutine get_neighbors_csr

    pure function get_nnz(self) result(nnz)
        implicit none
        class(type_node_adjacency), intent(in) :: self
        integer(int32) :: nnz
        nnz = self%nnz
    end function get_nnz

    subroutine get_coo(self, row_out, col_out)
        implicit none
        class(type_node_adjacency), intent(in) :: self
        integer(int32), intent(inout), allocatable :: row_out(:)
        integer(int32), intent(inout), allocatable :: col_out(:)

        if (self%nnz <= 0) return

        call allocate_array(row_out, self%nnz)
        call allocate_array(col_out, self%nnz)
        row_out(:) = self%row(:)
        col_out(:) = self%col(:)
    end subroutine get_coo

    subroutine get_csr(self, ptr_out, ind_out)
        implicit none
        class(type_node_adjacency), intent(in) :: self
        integer(int32), intent(inout), allocatable :: ptr_out(:)
        integer(int32), intent(inout), allocatable :: ind_out(:)

        if (self%nnz <= 0) return

        call allocate_array(ptr_out, self%num_nodes + 1_int32)
        call allocate_array(ind_out, self%nnz)
        ptr_out(:) = self%ptr(:)
        ind_out(:) = self%ind(:)
    end subroutine get_csr

    subroutine destroy_hybrid(self)
        implicit none
        class(type_node_adjacency), intent(inout) :: self

        call deallocate_array(self%row)
        call deallocate_array(self%col)
        call deallocate_array(self%ptr)
        call deallocate_array(self%ind)
        self%num_nodes = 0
        self%nnz = 0
    end subroutine destroy_hybrid

end module domain_adjacency_adjacency_node
