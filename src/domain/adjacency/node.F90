!>
!> Generates and stores node adjacency information, including self-loops, from
!> mesh connectivity data.
!> This module creates a symmetric graph where an edge (i, j) exists if nodes i
!> and j share an element. The adjacency is stored in both Coordinate (COO) and
!> Compressed Sparse Row (CSR) formats.
!>
module domain_adjacency_node
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use :: stdlib_sorting, only:sort
    use :: module_core, only:allocate_array, deallocate_array, unique

    implicit none
    private
    public :: type_node_adjacency

    !>
    !> Stores node adjacency information in both COO and CSR sparse formats.
    !>
    type :: type_node_adjacency
        !> Total number of nodes in the graph.
        integer(int32) :: num_nodes = 0
        !> Total number of non-zero entries (edges) in the adjacency matrix.
        integer(int32) :: nnz = 0

        !> Row indices for COO format (sorted).
        integer(int32), allocatable :: row(:)
        !> Column indices for COO format (sorted).
        integer(int32), allocatable :: col(:)

        !> Row pointers for CSR format (1-based). ptr(i) is the starting index in 'ind' for row i.
        integer(int32), allocatable :: ptr(:)
        !> Column indices for CSR format.
        integer(int32), allocatable :: ind(:)
    contains
        procedure, pass(self), public :: initialize => initialize_type_node_adjacency
        procedure, pass(self), public :: get_num_nodes => get_num_nodes
        procedure, pass(self), public :: get_degree => get_degree_csr
        procedure, pass(self), public :: get_neighbors => get_neighbors_csr
        procedure, pass(self), public :: get_nnz => get_nnz
        procedure, pass(self), public :: get_coo => get_coo
        procedure, pass(self), public :: get_csr => get_csr
        procedure, pass(self), public :: destroy => destroy_type_node_adjacency
    end type type_node_adjacency

contains

    !>
    !> Initializes the adjacency structure from element connectivity data.
    !> This is the main routine that orchestrates the creation of both the
    !> COO and CSR representations of the graph.
    !>
    subroutine initialize_type_node_adjacency(self, num_all_nodes, conn_ind, conn_val)
        implicit none
        !> The node adjacency object to initialize.
        class(type_node_adjacency), intent(inout) :: self
        !> The total number of nodes in the mesh.
        integer(int32), intent(in) :: num_all_nodes
        !> The CSR-style pointer array for the element connectivity.
        integer(int32), intent(in) :: conn_ind(:)
        !> The CSR-style index array for the element connectivity, containing node IDs.
        integer(int32), intent(in) :: conn_val(:)

        integer(int32) :: estimated_nnz, actual_nnz
        integer(int32), allocatable :: temp_row(:), temp_col(:)

        self%num_nodes = num_all_nodes
        if (self%num_nodes <= 0) return

        ! 1. Estimate the max size for a temporary COO array to pre-allocate memory.
        estimated_nnz = estimate_coo_size_from_connectivity(conn_ind)
        if (estimated_nnz <= 0) return

        call allocate_array(temp_row, estimated_nnz)
        call allocate_array(temp_col, estimated_nnz)

        ! 2. Generate a raw COO list with duplicate edges from the connectivity.
        call create_coo_from_connectivity(conn_ind, conn_val, temp_row, temp_col, actual_nnz)

        ! 3. Build the final, unique, and symmetric COO representation.
        call create_unique_coo(self, temp_row(1:actual_nnz), temp_col(1:actual_nnz))

        ! 4. Build the CSR representation from the final COO data.
        call build_csr_from_coo(self)

        call deallocate_array(temp_row)
        call deallocate_array(temp_col)
    end subroutine initialize_type_node_adjacency

    !>
    !> Estimates the maximum possible size of the temporary COO array.
    !> The size is the sum of n*n for each element, where n is the number
    !> of nodes in that element.
    !>
    function estimate_coo_size_from_connectivity(conn_ind) result(max_size)
        implicit none
        !> The CSR-style pointer array for the element connectivity.
        integer(int32), intent(in) :: conn_ind(:)
        !> The estimated maximum number of non-zero entries.
        integer(int32) :: max_size
        integer(int32) :: i, num_elements, nodes_in_elem

        max_size = 0
        num_elements = size(conn_ind) - 1
        if (num_elements <= 0) return

        ! Sum the number of node pairs (n*n) for each element.
        !$omp parallel do reduction(+:max_size) private(i, nodes_in_elem)
        do i = 1, num_elements
            nodes_in_elem = conn_ind(i + 1) - conn_ind(i)
            max_size = max_size + nodes_in_elem**2
        end do
        !$omp end parallel do
    end function estimate_coo_size_from_connectivity

    !>
    !> Creates a raw, duplicated list of node pairs (edges) from element connectivity.
    !> For each element, it generates an edge for every pair of nodes within that
    !> element, including self-loops (i,i).
    !>
    subroutine create_coo_from_connectivity(conn_ind, conn_val, row_out, col_out, actual_size)
        implicit none
        !> The CSR-style pointer array for the element connectivity.
        integer(int32), intent(in) :: conn_ind(:)
        !> The CSR-style index array for the element connectivity.
        integer(int32), intent(in) :: conn_val(:)
        !> The output array for row indices.
        integer(int32), intent(inout) :: row_out(:)
        !> The output array for column indices.
        integer(int32), intent(inout) :: col_out(:)
        !> The actual number of entries written to the output arrays.
        integer(int32), intent(inout) :: actual_size

        integer(int32) :: num_elements, i, j, k, start_idx, end_idx
        integer(int32) :: nodes_in_elem
        integer(int32), allocatable :: offsets(:)

        num_elements = size(conn_ind) - 1
        if (num_elements <= 0) then
            actual_size = 0
            return
        end if

        ! Calculate write offsets for parallel execution
        call allocate_array(offsets, num_elements + 1)
        offsets(1) = 0
        do i = 1, num_elements
            nodes_in_elem = conn_ind(i + 1) - conn_ind(i)
            offsets(i + 1) = offsets(i) + nodes_in_elem**2
        end do

        !$omp parallel do private(i, j, k, start_idx, end_idx, nodes_in_elem)
        do i = 1, num_elements
            start_idx = conn_ind(i)
            end_idx = conn_ind(i + 1) - 1
            nodes_in_elem = end_idx - start_idx + 1
            if (nodes_in_elem <= 0) cycle

            do j = 1, nodes_in_elem
                do k = 1, nodes_in_elem
                    row_out(offsets(i) + (j - 1) * nodes_in_elem + k) = conn_val(start_idx + j - 1)
                    col_out(offsets(i) + (j - 1) * nodes_in_elem + k) = conn_val(start_idx + k - 1)
                end do
            end do
        end do
        !$omp end parallel do

        actual_size = offsets(num_elements + 1)
        call deallocate_array(offsets)
    end subroutine create_coo_from_connectivity

    !>
    !> Creates a sorted, unique, and symmetric COO representation from a raw list of edges.
    !> It uses 64-bit integer packing to efficiently find and remove duplicate
    !> edges and to enforce symmetry (i.e., if (i,j) exists, (j,i) must also exist).
    !>
    subroutine create_unique_coo(self, temp_row, temp_col)
        implicit none
        !> The node adjacency object to populate with COO data.
        class(type_node_adjacency), intent(inout) :: self
        !> The raw row indices from the connectivity.
        integer(int32), intent(in) :: temp_row(:)
        !> The raw column indices from the connectivity.
        integer(int32), intent(in) :: temp_col(:)

        integer(int64), allocatable :: packed_edges(:), unique_packed_edges(:)
        integer(int32) :: i, n1, n2
        integer(int64) :: edge_count

        if (size(temp_row) == 0) return

        ! Allocate up to twice the size to hold symmetric pairs (i,j) and (j,i).
        allocate (packed_edges(size(temp_row, kind=int64) * 2))
        edge_count = 0
        do i = 1, size(temp_row)
            n1 = temp_row(i)
            n2 = temp_col(i)

            ! Pack two 32-bit integers into one 64-bit integer for efficient sorting and uniqueness checks.
            if (n1 == n2) then ! Self-loop
                edge_count = edge_count + 1
                packed_edges(edge_count) = ishft(int(n1, int64), 32) + int(n2, int64)
            else ! Off-diagonal entries, add both (n1,n2) and (n2,n1) to ensure symmetry.
                edge_count = edge_count + 1
                packed_edges(edge_count) = ishft(int(n1, int64), 32) + int(n2, int64)
                edge_count = edge_count + 1
                packed_edges(edge_count) = ishft(int(n2, int64), 32) + int(n1, int64)
            end if
        end do

        ! Find the unique packed edges
        call unique(packed_edges(1:edge_count), unique_packed_edges)
        deallocate (packed_edges)

        self%nnz = size(unique_packed_edges, kind=int32)
        call allocate_array(self%row, self%nnz)
        call allocate_array(self%col, self%nnz)

        ! Unpack the 64-bit integers back into 32-bit row and column indices.
        do i = 1, self%nnz
            self%row(i) = int(ishft(unique_packed_edges(i), -32), kind=int32)
            self%col(i) = int(iand(unique_packed_edges(i), int(z'FFFFFFFF', int64)), kind=int32)
        end do
        deallocate (unique_packed_edges)
    end subroutine create_unique_coo

    !>
    !> Builds the CSR representation from the internal sorted COO data.
    !>
    subroutine build_csr_from_coo(self)
        implicit none
        !> The node adjacency object containing COO data to convert to CSR.
        class(type_node_adjacency), intent(inout) :: self
        integer(int32) :: i, row_val, col_val, cumulative_sum, count
        integer(int32), allocatable :: next_pos(:)

        if (self%nnz == 0) then
            if (self%num_nodes > 0) then
                call allocate_array(self%ptr, self%num_nodes + 1)
                self%ptr = 1
            end if
            call allocate_array(self%ind, 0)
            return
        end if

        call allocate_array(self%ptr, self%num_nodes + 1)
        call allocate_array(self%ind, self%nnz)
        self%ptr = 0

        ! 1. Count non-zero elements per row (create histogram).
        do i = 1, self%nnz
            self%ptr(self%row(i)) = self%ptr(self%row(i)) + 1
        end do

        ! 2. Calculate the cumulative sum to get the row pointers.
        cumulative_sum = 1
        do i = 1, self%num_nodes
            count = self%ptr(i)
            self%ptr(i) = cumulative_sum
            cumulative_sum = cumulative_sum + count
        end do
        self%ptr(self%num_nodes + 1) = cumulative_sum

        ! 3. Place the column indices into the correct positions in the 'ind' array.
        allocate (next_pos(self%num_nodes + 1))
        next_pos = self%ptr

        do i = 1, self%nnz
            row_val = self%row(i)
            col_val = self%col(i)
            self%ind(next_pos(row_val)) = col_val
            next_pos(row_val) = next_pos(row_val) + 1
        end do
        deallocate (next_pos)

        ! 4. Sort the column indices within each row.
        !$omp parallel do private(i)
        do i = 1, self%num_nodes
            if (self%ptr(i + 1) > self%ptr(i)) then
                call sort(self%ind(self%ptr(i):self%ptr(i + 1) - 1))
            end if
        end do
        !$omp end parallel do
    end subroutine build_csr_from_coo

    !>
    !> Returns the total number of nodes in the graph.
    !>
    pure function get_num_nodes(self) result(n_nodes)
        implicit none
        !> The node adjacency object.
        class(type_node_adjacency), intent(in) :: self
        !> The total number of nodes.
        integer(int32) :: n_nodes
        n_nodes = self%num_nodes
    end function get_num_nodes

    !>
    !> Returns the degree (number of neighbors) of a specified node.
    !>
    pure function get_degree_csr(self, node_id) result(degree)
        implicit none
        !> The node adjacency object.
        class(type_node_adjacency), intent(in) :: self
        !> The 1-based ID of the node to query.
        integer(int32), intent(in) :: node_id
        !> The degree of the specified node.
        integer(int32) :: degree
        if (node_id < 1 .or. node_id > self%num_nodes) then
            degree = 0
            return
        end if
        degree = self%ptr(node_id + 1) - self%ptr(node_id)
    end function get_degree_csr

    !>
    !> Retrieves the list of neighbors for a specified node.
    !>
    subroutine get_neighbors_csr(self, node_id, neighbors)
        implicit none
        !> The node adjacency object.
        class(type_node_adjacency), intent(in) :: self
        !> The 1-based ID of the node to query.
        integer(int32), intent(in) :: node_id
        !> An allocatable array that will contain the list of neighbor node IDs.
        integer(int32), allocatable, intent(inout) :: neighbors(:)

        integer(int32) :: start_p, end_p, degree

        if (node_id < 1 .or. node_id > self%num_nodes) then
            call allocate_array(neighbors, 0)
            return
        end if
        start_p = self%ptr(node_id)
        end_p = self%ptr(node_id + 1) - 1
        degree = end_p - start_p + 1
        if (degree <= 0) then
            call allocate_array(neighbors, 0)
            return
        end if
        call allocate_array(neighbors, degree)
        neighbors = self%ind(start_p:end_p)
    end subroutine get_neighbors_csr

    !>
    !> Returns the total number of non-zero entries (edges) in the graph.
    !>
    pure function get_nnz(self) result(nnz_out)
        implicit none
        !> The node adjacency object.
        class(type_node_adjacency), intent(in) :: self
        !> The total number of non-zero entries.
        integer(int32) :: nnz_out

        nnz_out = self%nnz
    end function get_nnz

    !>
    !> Retrieves the COO representation of the adjacency graph.
    !>
    subroutine get_coo(self, row_out, col_out)
        implicit none
        !> The node adjacency object.
        class(type_node_adjacency), intent(in) :: self
        !> An allocatable array that will contain the row indices.
        integer(int32), allocatable, intent(inout) :: row_out(:)
        !> An allocatable array that will contain the column indices.
        integer(int32), allocatable, intent(inout) :: col_out(:)

        if (self%nnz > 0) then
            call allocate_array(row_out, self%nnz)
            call allocate_array(col_out, self%nnz)
            row_out = self%row
            col_out = self%col
        else
            call allocate_array(row_out, 0)
            call allocate_array(col_out, 0)
        end if
    end subroutine get_coo

    !>
    !> Retrieves the CSR representation of the adjacency graph.
    !>
    subroutine get_csr(self, ptr_out, ind_out)
        implicit none
        !> The node adjacency object.
        class(type_node_adjacency), intent(in) :: self
        !> An allocatable array that will contain the row pointers.
        integer(int32), allocatable, intent(inout) :: ptr_out(:)
        !> An allocatable array that will contain the column indices.
        integer(int32), allocatable, intent(inout) :: ind_out(:)

        if (self%num_nodes > 0 .and. self%nnz > 0) then
            call allocate_array(ptr_out, self%num_nodes + 1)
            call allocate_array(ind_out, self%nnz)
            ptr_out = self%ptr
            ind_out = self%ind
        else
            if (self%num_nodes > 0) then
                call allocate_array(ptr_out, self%num_nodes + 1)
                ptr_out = 1
            else
                call allocate_array(ptr_out, 0)
            end if
            call allocate_array(ind_out, 0)
        end if
    end subroutine get_csr

    !>
    !> Deallocates all internal arrays of the node adjacency object.
    !>
    subroutine destroy_type_node_adjacency(self)
        implicit none
        !> The node adjacency object to destroy.
        class(type_node_adjacency), intent(inout) :: self

        call deallocate_array(self%row)
        call deallocate_array(self%col)
        call deallocate_array(self%ptr)
        call deallocate_array(self%ind)
        self%num_nodes = 0
        self%nnz = 0
    end subroutine destroy_type_node_adjacency

end module domain_adjacency_node
