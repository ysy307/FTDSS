module types_topology_connectivity
    use :: iso_fortran_env, only:int32, int64
    implicit none
    private

    public :: type_csr_index
    
    !>
    !> Stores element connectivity in Compressed Sparse Row (CSR) format.
    !> This structure is used to represent the connectivity of nodes in a mesh, where each element's nodes are stored in a compact format for efficient access.
    !> The `row_ptr` array indicates the starting index of each element's nodes in the `col_ind` array, which contains the actual node IDs for all elements concatenated together.
    !>
    type :: type_csr_index
        !> Index array for CSR format. Stores the starting position of each element's nodes.
        integer(int32), allocatable :: row_ptr(:)
        !> Value array for CSR format. Stores the concatenated node IDs for all elements.
        integer(int32), allocatable :: col_ind(:)
    end type type_csr_index

contains

end module types_topology_connectivity
