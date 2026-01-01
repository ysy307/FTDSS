!>
!> Generates and stores node adjacency information from mesh connectivity data.
!>
!> This module extends the generic `type_graph` to support initialization from
!> finite element connectivity arrays (conn_ind, conn_val).
!>
module domain_adjacency_node
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use :: module_core, only:type_graph

    implicit none
    private
    public :: type_node_adjacency

    !>
    !> Stores node adjacency information.
    !> Inherits CSR storage and graph algorithms (get_degree, get_neighbors) from type_graph.
    !>
    type, extends(type_graph) :: type_node_adjacency
    contains
        ! メッシュデータからの初期化メソッド
        procedure, public, pass(self) :: initialize => initialize_from_connectivity

        procedure, public, pass(self) :: get_nnz => get_nnz_csr
        procedure, public, pass(self) :: get_coo => reconstruct_coo
        procedure, public, pass(self) :: get_csr => export_csr
    end type type_node_adjacency

contains

    !>
    !> Initializes the graph from element connectivity data.
    !> Converts connectivity to edge pairs and delegates construction to type_graph.
    !>
    subroutine initialize_from_connectivity(self, num_all_nodes, conn_ind, conn_val)
        implicit none
        class(type_node_adjacency), intent(inout) :: self
        integer(int32), intent(in) :: num_all_nodes
        integer(int32), intent(in) :: conn_ind(:)
        integer(int32), intent(in) :: conn_val(:)

        integer(int32) :: estimated_nnz, actual_nnz
        integer(int32), allocatable :: pairs(:, :) ! (2, estimated_nnz)

        call self%destroy()

        if (num_all_nodes <= 0) return

        ! 1. Estimate size for temporary pairs array
        estimated_nnz = estimate_edges_count(conn_ind)
        if (estimated_nnz <= 0) return

        allocate (pairs(2, estimated_nnz))

        ! 2. Expand connectivity into raw edge pairs (COO-like structure)
        call expand_connectivity_to_pairs(conn_ind, conn_val, pairs, actual_nnz)

        ! 3. Build the graph using the robust logic in type_graph
        !    (Sorts, removes duplicates/self-loops, builds CSR)
        call self%build(pairs(:, 1:actual_nnz), num_all_nodes)

        deallocate (pairs)
    end subroutine initialize_from_connectivity

    !>
    !> Returns the number of non-zero entries (stored edges).
    !> Note: In CSR, this is the size of the col_ind array.
    !>
    pure function get_nnz_csr(self) result(nnz)
        class(type_node_adjacency), intent(in) :: self
        integer(int32) :: nnz
        if (allocated(self%col_ind)) then
            nnz = size(self%col_ind, kind=int32)
        else
            nnz = 0
        end if
    end function get_nnz_csr

    !>
    !> Reconstructs COO format from the internal CSR storage.
    !> (Used for compatibility with legacy code expecting get_coo)
    !>
    subroutine reconstruct_coo(self, row_out, col_out)
        class(type_node_adjacency), intent(in) :: self
        integer(int32), allocatable, intent(inout) :: row_out(:)
        integer(int32), allocatable, intent(inout) :: col_out(:)

        integer(int32) :: i, j, k, nnz, start_idx, end_idx

        nnz = self%get_nnz()
        if (nnz == 0) then
            if (allocated(row_out)) deallocate (row_out)
            if (allocated(col_out)) deallocate (col_out)
            allocate (row_out(0), col_out(0))
            return
        end if

        allocate (row_out(nnz), col_out(nnz))

        k = 0
        do i = 1, self%num_nodes
            start_idx = self%row_ptr(i)
            end_idx = self%row_ptr(i + 1) - 1
            if (start_idx <= end_idx) then
                do j = start_idx, end_idx
                    k = k + 1
                    row_out(k) = i
                    col_out(k) = self%col_ind(j)
                end do
            end if
        end do
    end subroutine reconstruct_coo

    !>
    !> Exports CSR arrays (Deep copy).
    !>
    subroutine export_csr(self, ptr_out, ind_out)
        class(type_node_adjacency), intent(in) :: self
        integer(int32), allocatable, intent(inout) :: ptr_out(:)
        integer(int32), allocatable, intent(inout) :: ind_out(:)

        if (allocated(self%row_ptr)) then
            allocate (ptr_out, source=self%row_ptr)
            allocate (ind_out, source=self%col_ind)
        else
            allocate (ptr_out(0), ind_out(0))
        end if
    end subroutine export_csr

    ! ------------------------------------------------------------------
    ! Private Helpers
    ! ------------------------------------------------------------------

    pure function estimate_edges_count(conn_ind) result(max_size)
        integer(int32), intent(in) :: conn_ind(:)
        integer(int32) :: max_size
        integer(int32) :: i, n_elem, nodes_in_elem

        max_size = 0
        n_elem = size(conn_ind) - 1
        do i = 1, n_elem
            nodes_in_elem = conn_ind(i + 1) - conn_ind(i)
            max_size = max_size + nodes_in_elem**2
        end do
    end function estimate_edges_count

    subroutine expand_connectivity_to_pairs(conn_ind, conn_val, pairs, count)
        implicit none
        integer(int32), intent(in) :: conn_ind(:)
        integer(int32), intent(in) :: conn_val(:)
        integer(int32), intent(inout) :: pairs(:, :)
        integer(int32), intent(inout) :: count

        integer(int32) :: i, j, k, n_elem, start_idx, end_idx, n_nodes

        n_elem = size(conn_ind) - 1
        count = 1

        do i = 1, n_elem
            start_idx = conn_ind(i)
            end_idx = conn_ind(i + 1) - 1
            n_nodes = end_idx - start_idx + 1
            if (n_nodes <= 0) cycle

            ! Element内の全ペアを列挙
            do j = 1, n_nodes
                do k = 1, n_nodes
                    ! 【修正】インデックスがずれないように -1 する
                    pairs(1, count) = conn_val(start_idx + j - 1)
                    pairs(2, count) = conn_val(start_idx + k - 1)
                    count = count + 1
                end do
            end do
        end do
    end subroutine expand_connectivity_to_pairs
end module domain_adjacency_node
