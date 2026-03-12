module core_types_topology_connectivity
    use :: iso_fortran_env
    use :: stdlib_strings, only:strip
    use :: stdlib_optval, only:optval
    use :: core_memory, only:allocate_array, deallocate_array
    implicit none
    private

    public :: type_csr_index

    !>
    !> Stores element connectivity in Compressed Sparse Row (CSR) format.
    !> This structure is used to represent the connectivity of nodes in a mesh, where each element's nodes are stored
    !> in a compact format for efficient access. The `row_ptr` array indicates the starting index of each element's nodes 
    !> in the `col_ind` array, which contains the actual node IDs for all elements concatenated together.
    !>
    type :: type_csr_index
        !> Index array for CSR format. Stores the starting position of each element's nodes.
        integer(int32), allocatable :: row_ptr(:)
        !> Value array for CSR format. Stores the concatenated node IDs for all elements.
        integer(int32), allocatable :: col_ind(:)
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_csr_index
        procedure, public, pass(self) :: destroy => destroy_csr_index
        procedure, public, pass(self) :: reset => reset_csr_index
        procedure, public, pass(self) :: copy => copy_csr_index
        ! ---- Inquiry ----

        ! ---- Getter ----
        ! ---- Meta / Utility ----
        procedure, public, pass(self) :: display => display_csr_index
    end type type_csr_index

contains

    subroutine initialize_csr_index(self, num_row_ptr, num_col_ind)
        implicit none
        class(type_csr_index), intent(inout) :: self
        integer(int32), intent(in) :: num_row_ptr
        integer(int32), intent(in) :: num_col_ind

        call allocate_array(self%row_ptr, num_row_ptr)
        call allocate_array(self%col_ind, num_col_ind)

        call self%reset()
    end subroutine initialize_csr_index

    subroutine destroy_csr_index(self)
        implicit none
        class(type_csr_index), intent(inout) :: self

        call deallocate_array(self%row_ptr)
        call deallocate_array(self%col_ind)
    end subroutine destroy_csr_index

    subroutine reset_csr_index(self)
        implicit none
        class(type_csr_index), intent(inout) :: self

        self%row_ptr(:) = 0
        self%col_ind(:) = 0
    end subroutine reset_csr_index

    subroutine copy_csr_index(self, source)
        implicit none
        class(type_csr_index), intent(inout) :: self
        class(type_csr_index), intent(in) :: source

        call self%initialize(size(source%row_ptr), size(source%col_ind))
        self%row_ptr(:) = source%row_ptr(:)
        self%col_ind(:) = source%col_ind(:)
    end subroutine copy_csr_index

    subroutine display_csr_index(self, unit_in, title)
        implicit none
        class(type_csr_index), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in
        character(len=*), intent(in), optional :: title

        integer(int32) :: unit
        unit = optval(x=unit_in, default=output_unit)

        write (unit, '(A,A,A)') '        - **Connectivity (', strip(optval(title, 'unnamed')), ')**:'
        if (allocated(self%col_ind)) then
            write (unit, '(A, I0, A, I0, A)') '          - Ind Size: ', size(self%row_ptr), ', Val Size: ', size(self%col_ind)
        else
            write (unit, '(A)') '          - Not allocated'
        end if
    end subroutine display_csr_index

end module core_types_topology_connectivity
