!>
!> Manages a reverse map from nodes to the elements they belong to.
!>
!> This module transforms the Element->Node connectivity (input) into a
!> Node->Element map (output) using a CSR (Compressed Sparse Row) format.
!> This is equivalent to transposing the connectivity matrix.
!>
module domain_adjacency_node_element
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private

    public :: type_map_node_to_element

    !>
    !> Encapsulates the mapping from each node to a list of its parent elements.
    !> Uses CSR format for high performance and low memory overhead.
    !>
    type :: type_map_node_to_element
        integer(int32) :: num_nodes = 0
        integer(int32) :: num_entries = 0 ! Total number of node-element links

        !> Row pointers (Size: num_nodes + 1)
        integer(int32), allocatable :: ptr(:)
        !> Column indices (Size: num_entries) - Stores Element IDs
        integer(int32), allocatable :: ind(:)
    contains
        procedure, pass(self), public :: initialize
        procedure, pass(self), public :: get_list => get_element_list
        procedure, pass(self), public :: destroy
    end type type_map_node_to_element

contains

    !>
    !> Builds the node-to-element map by transposing the element connectivity.
    !>
    subroutine initialize(self, num_nodes, num_elements, conn_ind, conn_val)
        implicit none
        class(type_map_node_to_element), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in) :: num_elements
        integer(int32), intent(in) :: conn_ind(:) ! Element -> Node pointers
        integer(int32), intent(in) :: conn_val(:) ! Element -> Node indices

        integer(int32) :: i, j, start_idx, end_idx, node_id, cum_sum
        integer(int32), allocatable :: counts(:)
        integer(int32), allocatable :: next_pos(:)

        call self%destroy()
        if (num_nodes <= 0 .or. num_elements <= 0) return

        self%num_nodes = num_nodes

        ! --- Pass 1: Count elements per node (Degree calculation) ---
        allocate (counts(num_nodes))
        counts = 0

        do i = 1, num_elements
            start_idx = conn_ind(i)
            end_idx = conn_ind(i + 1) - 1
            do j = start_idx, end_idx
                node_id = conn_val(j)
                if (node_id >= 1 .and. node_id <= num_nodes) then
                    counts(node_id) = counts(node_id) + 1
                end if
            end do
        end do

        ! --- Pass 2: Build CSR Row Pointers (Prefix Sum) ---
        allocate (self%ptr(num_nodes + 1))
        self%ptr(1) = 1
        cum_sum = 1

        do i = 1, num_nodes
            cum_sum = cum_sum + counts(i)
            self%ptr(i + 1) = cum_sum
        end do

        self%num_entries = cum_sum - 1

        ! --- Pass 3: Fill Element IDs (Transpose) ---
        allocate (self%ind(self%num_entries))

        ! Use 'counts' array to keep track of current insert position for each node
        ! Re-initialize next_pos using ptr
        allocate (next_pos(num_nodes))
        next_pos = self%ptr(1:num_nodes)

        do i = 1, num_elements
            start_idx = conn_ind(i)
            end_idx = conn_ind(i + 1) - 1
            do j = start_idx, end_idx
                node_id = conn_val(j)
                if (node_id >= 1 .and. node_id <= num_nodes) then
                    self%ind(next_pos(node_id)) = i ! Store Element ID
                    next_pos(node_id) = next_pos(node_id) + 1
                end if
            end do
        end do

        deallocate (counts)
        deallocate (next_pos)

    end subroutine initialize

    !>
    !> Returns the list of element IDs for a specific node.
    !>
    subroutine get_element_list(self, node_id, element_list)
        implicit none
        class(type_map_node_to_element), intent(in), target :: self
        integer(int32), intent(in) :: node_id
        integer(int32), intent(inout), pointer, contiguous, dimension(:) :: element_list
        integer(int32) :: start_idx, end_idx, n

        if (node_id < 1 .or. node_id > self%num_nodes) then
            element_list => null()
            return
        end if

        start_idx = self%ptr(node_id)
        end_idx = self%ptr(node_id + 1) - 1
        n = end_idx - start_idx + 1

        if (n > 0) then
            element_list => self%ind(start_idx:end_idx)
        else
            element_list => null()
        end if
    end subroutine get_element_list

    !>
    !> Deallocates memory.
    !>
    subroutine destroy(self)
        implicit none
        class(type_map_node_to_element), intent(inout) :: self
        if (allocated(self%ptr)) deallocate (self%ptr)
        if (allocated(self%ind)) deallocate (self%ind)
        self%num_nodes = 0
        self%num_entries = 0
    end subroutine destroy

end module domain_adjacency_node_element
