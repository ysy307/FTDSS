!>
!> Manages a reverse map from nodes to the elements they belong to.
!> This module provides a data structure for efficiently querying the list of
!> elements that contain a specific node, built from a CSR-formatted element
!> connectivity array.
!>
module domain_adjacency_adjacency_node_element
    use, intrinsic :: iso_fortran_env, only: int32

    implicit none
    private

    public :: type_map_node_to_element

    !>
    !> A private type to hold a list of element IDs for a single node.
    !>
    type, private :: type_element_list
        !> An array of 1-based element IDs.
        integer(int32), allocatable :: ids(:)
    end type type_element_list

    !>
    !> Encapsulates the mapping from each node to a list of its parent elements.
    !>
    type :: type_map_node_to_element
        private
        !> The core data structure, an array where each index corresponds to a
        !> node ID and holds a list of element IDs.
        type(type_element_list), allocatable :: map_data(:)
    contains
        procedure, pass(self), public :: initialize
        procedure, pass(self), public :: get_list => get_element_list
        procedure, pass(self), public :: destroy
    end type type_map_node_to_element

contains

    !>
    !> Builds the node-to-element map from element connectivity data.
    !> This routine uses an efficient multi-pass algorithm to first count the
    !> number of elements per node, then allocate memory, and finally populate
    !> the map.
    !>
    subroutine initialize(self, num_nodes, num_elements, conn_ind, conn_val)
        implicit none
        !> The map object to be initialized.
        class(type_map_node_to_element), intent(inout) :: self
        !> The total number of nodes in the mesh.
        integer(int32), intent(in) :: num_nodes
        !> The total number of elements in the mesh.
        integer(int32), intent(in) :: num_elements
        !> The CSR-style pointer array for the element connectivity (size num_elements + 1).
        integer(int32), intent(in) :: conn_ind(:)
        !> The CSR-style index array for the element connectivity, containing node IDs.
        integer(int32), intent(in) :: conn_val(:)

        integer(int32) :: ielem, idx, node_id, start_idx, end_idx
        integer(int32), allocatable :: node_element_counts(:)
        integer(int32), allocatable :: current_indices(:)

        call self%destroy()
        if (num_nodes <= 0 .or. num_elements <= 0) return

        allocate (self%map_data(num_nodes))

        ! ==========================================================
        ! Pass 1: Count how many elements each node belongs to.
        ! ==========================================================
        allocate (node_element_counts(num_nodes))
        node_element_counts = 0
        do ielem = 1, num_elements
            start_idx = conn_ind(ielem)
            end_idx = conn_ind(ielem + 1) - 1
            do idx = start_idx, end_idx
                node_id = conn_val(idx)
                if (node_id > 0 .and. node_id <= num_nodes) then
                    node_element_counts(node_id) = node_element_counts(node_id) + 1
                end if
            end do
        end do

        ! ==========================================================
        ! Pass 2: Allocate the final arrays based on the counts.
        ! ==========================================================
        do node_id = 1, num_nodes
            if (node_element_counts(node_id) > 0) then
                allocate (self%map_data(node_id)%ids(node_element_counts(node_id)))
            end if
        end do
        deallocate (node_element_counts)

        ! ==========================================================
        ! Pass 3: Fill the arrays with the element IDs.
        ! ==========================================================
        allocate (current_indices(num_nodes))
        current_indices = 1
        do ielem = 1, num_elements
            start_idx = conn_ind(ielem)
            end_idx = conn_ind(ielem + 1) - 1
            do idx = start_idx, end_idx
                node_id = conn_val(idx)
                if (node_id > 0 .and. node_id <= num_nodes) then
                    self%map_data(node_id)%ids(current_indices(node_id)) = ielem
                    current_indices(node_id) = current_indices(node_id) + 1
                end if
            end do
        end do
        deallocate (current_indices)
    end subroutine initialize

    !>
    !> Returns a pointer to the list of element IDs for a specific node.
    !>
    function get_element_list(self, node_id) result(id_list)
        !> The map object.
        class(type_map_node_to_element), intent(in), target :: self
        !> The 1-based ID of the node to query.
        integer(int32), intent(in) :: node_id
        !> A pointer to the internal array of element IDs. This pointer should not
        !> be deallocated by the caller. It will be null if the node ID is
        !> invalid or the node belongs to no elements.
        integer(int32), pointer :: id_list(:)

        nullify (id_list)
        if (node_id < 1 .or. node_id > size(self%map_data)) return

        if (allocated(self%map_data(node_id)%ids)) then
            id_list => self%map_data(node_id)%ids
        end if
    end function get_element_list

    !>
    !> Deallocates all memory associated with the map.
    !>
    subroutine destroy(self)
        implicit none
        !> The map object to destroy.
        class(type_map_node_to_element), intent(inout) :: self
        integer(int32) :: i
        if (.not. allocated(self%map_data)) return
        do i = 1, size(self%map_data)
            if (allocated(self%map_data(i)%ids)) then
                deallocate (self%map_data(i)%ids)
            end if
        end do
        deallocate (self%map_data)
    end subroutine destroy

end module domain_adjacency_adjacency_node_element
