module core_types_graph
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: type_graph

    ! ------------------------------------------------------------------
    ! Graph type definition
    ! ------------------------------------------------------------------
    type :: type_graph
        integer(int32) :: num_nodes = 0
        integer(int32) :: num_edges = 0

        ! CSR Format
        integer(int32), allocatable :: row_ptr(:) ! Size: num_nodes + 1
        integer(int32), allocatable :: col_ind(:) ! Size: 2 * num_edges (symmetric)
    contains
        procedure, pass(self) :: build => build_from_pairs
        procedure, pass(self) :: get_degree
        procedure, pass(self) :: get_neighbors
        procedure, pass(self) :: print => print_graph_info
        procedure, pass(self) :: destroy
    end type type_graph

contains

    ! ==================================================================
    ! Builds a simple undirected graph (CSR) from a list of edge pairs.
    ! Uses 64-bit packing for robust unique/sort operations.
    ! ==================================================================
    subroutine build_from_pairs(self, pair_lists, num_nodes)
        implicit none
        class(type_graph), intent(inout) :: self
        integer(int32), intent(in) :: pair_lists(:, :)
        integer(int32), intent(in) :: num_nodes

        integer(int32) :: i, u, v, n_input, n_unique
        integer(int64), allocatable :: packed_edges(:)
        integer(int32), allocatable :: temp_row(:), temp_col(:)
        integer(int32) :: count, cumulative_sum, row_val, col_val
        integer(int32), allocatable :: next_pos(:)

        self%num_nodes = num_nodes
        n_input = size(pair_lists, 2)
        if (num_nodes <= 0) return

        ! --- Step 1: Pack edges into 64-bit integers for sorting ---
        ! We store both (u,v) and (v,u) to ensure symmetry.
        ! Max possible edges = 2 * input (minus self-loops, handled later)
        allocate (packed_edges(size(pair_lists, 2, kind=int64) * 2))

        count = 0
        do i = 1, n_input
            u = pair_lists(1, i)
            v = pair_lists(2, i)

            ! Validate indices
            if (u < 1 .or. u > num_nodes .or. v < 1 .or. v > num_nodes) cycle
            ! ! Skip self-loops
            ! if (u == v) cycle

            ! Add (u, v) - packing: (u << 32) | v
            count = count + 1
            packed_edges(count) = ishft(int(u, int64), 32) + int(v, int64)

            ! Add (v, u) - packing: (v << 32) | u
            count = count + 1
            packed_edges(count) = ishft(int(v, int64), 32) + int(u, int64)
        end do

        if (count == 0) then
            call self%destroy() ! Allocate empty structure
            allocate (self%row_ptr(num_nodes + 1)); self%row_ptr = 1
            allocate (self%col_ind(0))
            return
        end if

        ! --- Step 2: Sort and Unique (The core robust logic) ---
        ! Resize to actual count before sorting
        call sort_int64(packed_edges(1:count))

        ! In-place unique
        n_unique = 0
        if (count > 0) then
            n_unique = 1
            ! packed_edges(1) is already in place
            do i = 2, count
                if (packed_edges(i) /= packed_edges(i - 1)) then
                    n_unique = n_unique + 1
                    packed_edges(n_unique) = packed_edges(i)
                end if
            end do
        end if

        self%num_edges = n_unique / 2 ! Logic edges (undirected)

        ! --- Step 3: Unpack to temporary COO ---
        allocate (temp_row(n_unique), temp_col(n_unique))
        do i = 1, n_unique
            temp_row(i) = int(ishft(packed_edges(i), -32), kind=int32)
            temp_col(i) = int(iand(packed_edges(i), int(z'FFFFFFFF', int64)), kind=int32)
        end do
        deallocate (packed_edges)

        ! --- Step 4: Build CSR from sorted COO ---
        allocate (self%row_ptr(num_nodes + 1))
        allocate (self%col_ind(n_unique))
        self%row_ptr = 0

        ! 4-1: Histogram (Degree count)
        do i = 1, n_unique
            self%row_ptr(temp_row(i)) = self%row_ptr(temp_row(i)) + 1
        end do

        ! 4-2: Cumulative Sum
        cumulative_sum = 1
        do i = 1, num_nodes
            count = self%row_ptr(i)
            self%row_ptr(i) = cumulative_sum
            cumulative_sum = cumulative_sum + count
        end do
        self%row_ptr(num_nodes + 1) = cumulative_sum

        ! 4-3: Fill col_ind
        allocate (next_pos(num_nodes))
        next_pos = self%row_ptr(1:num_nodes)

        do i = 1, n_unique
            row_val = temp_row(i)
            col_val = temp_col(i)
            self%col_ind(next_pos(row_val)) = col_val
            next_pos(row_val) = next_pos(row_val) + 1
        end do

        ! Since the packed array was sorted, col_ind is naturally sorted per row!
        ! No need for row-wise sort here.

    end subroutine build_from_pairs

    ! ==================================================================
    ! Returns the degree of a specific node.
    ! ==================================================================
    pure elemental subroutine get_degree(self, node_id, degree)
        implicit none
        class(type_graph), intent(in) :: self
        integer(int32), intent(in) :: node_id
        integer(int32), intent(inout) :: degree

        if (node_id < 1 .or. node_id > self%num_nodes) then
            degree = 0
        else
            degree = self%row_ptr(node_id + 1) - self%row_ptr(node_id)
        end if
    end subroutine get_degree

    ! ==================================================================
    ! Returns an allocatable array of neighbors for a specific node.
    ! ==================================================================
    subroutine get_neighbors(self, node_id, neighbors)
        implicit none
        class(type_graph), intent(in), target :: self
        integer(int32), intent(in) :: node_id
        integer(int32), dimension(:), pointer, intent(inout) :: neighbors
        integer(int32) :: start_p, end_p, deg

        call self%get_degree(node_id, deg)
        if (deg <= 0) then
            neighbors => null()
            return
        end if

        start_p = self%row_ptr(node_id)
        end_p = self%row_ptr(node_id + 1) - 1

        neighbors => self%col_ind(start_p:end_p)
    end subroutine get_neighbors

    ! ==================================================================
    ! Destructor
    ! ==================================================================
    subroutine destroy(self)
        implicit none
        class(type_graph), intent(inout) :: self
        if (allocated(self%row_ptr)) deallocate (self%row_ptr)
        if (allocated(self%col_ind)) deallocate (self%col_ind)
        self%num_nodes = 0
        self%num_edges = 0
    end subroutine destroy

    ! ==================================================================
    ! Helper: Print info (Debugging)
    ! ==================================================================
    subroutine print_graph_info(self)
        implicit none
        class(type_graph), intent(in) :: self
        integer(int32) :: i, j, start_idx, end_idx

        print *, "--- Graph Info ---"
        print *, "Nodes:", self%num_nodes, ", Edges (undirected):", self%num_edges
        if (.not. allocated(self%row_ptr)) return

        do i = 1, self%num_nodes
            start_idx = self%row_ptr(i)
            end_idx = self%row_ptr(i + 1) - 1
            write (*, '(A, I0, A)', advance='no') "Node ", i, ": ["
            if (start_idx <= end_idx) then
                do j = start_idx, end_idx
                    write (*, '(I0)', advance='no') self%col_ind(j)
                    if (j < end_idx) write (*, '(A)', advance='no') ", "
                end do
            end if
            print *, "]"
        end do
    end subroutine print_graph_info

    ! ==================================================================
    ! Internal: QuickSort for int64 array
    ! (Required for the 64-bit packing logic)
    ! ==================================================================
    subroutine sort_int64(arr)
        integer(int64), intent(inout) :: arr(:)
        call quicksort(arr, 1, size(arr))
    contains
        recursive subroutine quicksort(a, first, last)
            integer(int64), intent(inout) :: a(:)
            integer(int32) :: first, last
            integer(int32) :: i, j
            integer(int64) :: x, t

            if (first < last) then
                x = a(first)
                i = first
                j = last + 1
                do
                    do
                        i = i + 1
                        if (i > last) exit
                        if (a(i) >= x) exit
                    end do
                    do
                        j = j - 1
                        if (a(j) <= x) exit
                    end do
                    if (i < j) then
                        t = a(i); a(i) = a(j); a(j) = t
                    else
                        exit
                    end if
                end do
                t = a(first); a(first) = a(j); a(j) = t
                call quicksort(a, first, j - 1)
                call quicksort(a, j + 1, last)
            end if
        end subroutine quicksort
    end subroutine sort_int64

end module core_types_graph
