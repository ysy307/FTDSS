!>
!> Lightweight, domain-independent carrier of the topology and sizing data that
!> the numerical system layer (global Jacobian matrix / residual vector) needs
!> in order to be constructed.
!>
!> This type is the dependency-injection seam that decouples the system layer
!> from the heavyweight \c type_domain "God object": the system modules consume
!> only this carrier, while \c type_domain knows how to populate it through its
!> \c export_topology type-bound procedure.
!>
!> Stored quantities:
!>   - scalar sizes: number of nodes, total DOFs, DOFs per node, number of FEs,
!>   - per-physics DOF counts (indexed by physics id),
!>   - the CSR node-adjacency sparsity pattern (node graph),
!>   - the element connectivity packed in CSR layout.
!>
module core_types_topology_system_topology
    use, intrinsic :: iso_fortran_env
    use :: core_constants, only:PHYSICS_TYPES, MATRIX_TYPES, type_constant_id
    use :: core_memory, only:deallocate_array
    implicit none
    private

    public :: type_system_topology

    type :: type_system_topology
        private
        integer(int32) :: num_nodes = 0
        integer(int32) :: total_dofs = 0
        integer(int32) :: num_dof_per_node = 0
        integer(int32) :: num_fe = 0
        integer(int32) :: num_dofs_of_physics(PHYSICS_TYPES%NUM_ID) = 0

        !> CSR node-adjacency sparsity pattern (node graph).
        integer(int32), allocatable :: adj_row(:)
        integer(int32), allocatable :: adj_col(:)

        !> Packed (CSR-style) element connectivity: the nodes of element \c e are
        !> \c conn_idx(conn_ptr(e) : conn_ptr(e+1)-1).
        integer(int32), allocatable :: conn_ptr(:)
        integer(int32), allocatable :: conn_idx(:)
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_system_topology
        procedure, public, pass(self) :: destroy => destroy_system_topology

        ! ---- Getter ----
        procedure, public, pass(self) :: get_total_dofs => get_total_dofs_topology
        procedure, public, pass(self) :: get_num_nodes => get_num_nodes_topology
        procedure, public, pass(self) :: get_num_dof_per_node => get_num_dof_per_node_topology
        procedure, public, pass(self) :: get_target_dof => get_target_dof_topology
        procedure, public, pass(self) :: get_node_adjacency => get_node_adjacency_topology
        procedure, public, pass(self) :: get_num_fe => get_num_fe_topology
        procedure, public, pass(self) :: get_fe_connectivity => get_fe_connectivity_topology
    end type type_system_topology

contains

    !> Deep-copies all sizing/topology arrays into the carrier. Pre-existing
    !> contents are released first, so the call is idempotent.
    subroutine initialize_system_topology(self, num_nodes, total_dofs, num_dof_per_node, &
                                          num_dofs_of_physics, adj_row, adj_col, &
                                          num_fe, conn_ptr, conn_idx)
        implicit none
        class(type_system_topology), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in) :: total_dofs
        integer(int32), intent(in) :: num_dof_per_node
        integer(int32), intent(in) :: num_dofs_of_physics(:)
        integer(int32), intent(in) :: adj_row(:)
        integer(int32), intent(in) :: adj_col(:)
        integer(int32), intent(in) :: num_fe
        integer(int32), intent(in) :: conn_ptr(:)
        integer(int32), intent(in) :: conn_idx(:)

        integer(int32) :: ncopy

        call self%destroy()

        self%num_nodes = num_nodes
        self%total_dofs = total_dofs
        self%num_dof_per_node = num_dof_per_node
        self%num_fe = num_fe

        ncopy = min(size(self%num_dofs_of_physics), size(num_dofs_of_physics))
        self%num_dofs_of_physics(1:ncopy) = num_dofs_of_physics(1:ncopy)

        allocate (self%adj_row, source=adj_row)
        allocate (self%adj_col, source=adj_col)
        allocate (self%conn_ptr, source=conn_ptr)
        allocate (self%conn_idx, source=conn_idx)
    end subroutine initialize_system_topology

    subroutine destroy_system_topology(self)
        implicit none
        class(type_system_topology), intent(inout) :: self

        self%num_nodes = 0
        self%total_dofs = 0
        self%num_dof_per_node = 0
        self%num_fe = 0
        self%num_dofs_of_physics(:) = 0

        call deallocate_array(self%adj_row)
        call deallocate_array(self%adj_col)
        call deallocate_array(self%conn_ptr)
        call deallocate_array(self%conn_idx)
    end subroutine destroy_system_topology

    subroutine get_total_dofs_topology(self, total_dofs)
        implicit none
        class(type_system_topology), intent(in) :: self
        integer(int32), intent(inout) :: total_dofs

        total_dofs = self%total_dofs
    end subroutine get_total_dofs_topology

    subroutine get_num_nodes_topology(self, num_nodes)
        implicit none
        class(type_system_topology), intent(in) :: self
        integer(int32), intent(inout) :: num_nodes

        num_nodes = self%num_nodes
    end subroutine get_num_nodes_topology

    subroutine get_num_dof_per_node_topology(self, num_dofs_per_node)
        implicit none
        class(type_system_topology), intent(in) :: self
        integer(int32), intent(inout) :: num_dofs_per_node

        num_dofs_per_node = self%num_dof_per_node
    end subroutine get_num_dof_per_node_topology

    !> Returns the number of DOFs carried by \c physics_type, or 0 when the
    !> physics id is out of range or inactive.
    subroutine get_target_dof_topology(self, physics_type, target_dof)
        implicit none
        class(type_system_topology), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        integer(int32), intent(inout) :: target_dof

        target_dof = 0
        if (physics_type%ID >= 1 .and. physics_type%ID <= size(self%num_dofs_of_physics)) then
            target_dof = self%num_dofs_of_physics(physics_type%ID)
        end if
    end subroutine get_target_dof_topology

    !> Returns a fresh copy of the stored CSR node-adjacency pattern. Only the
    !> CSR layout is retained; an invalid \c matrix_type leaves the outputs
    !> untouched, mirroring \c type_domain behaviour.
    subroutine get_node_adjacency_topology(self, matrix_type, row, col)
        implicit none
        class(type_system_topology), intent(in) :: self
        type(type_constant_id), intent(in) :: matrix_type
        integer(int32), allocatable, intent(inout) :: row(:)
        integer(int32), allocatable, intent(inout) :: col(:)

        if (.not. MATRIX_TYPES%is_valid(matrix_type)) return

        call deallocate_array(row)
        call deallocate_array(col)
        allocate (row, source=self%adj_row)
        allocate (col, source=self%adj_col)
    end subroutine get_node_adjacency_topology

    subroutine get_num_fe_topology(self, num_fe)
        implicit none
        class(type_system_topology), intent(in) :: self
        integer(int32), intent(inout) :: num_fe

        num_fe = self%num_fe
    end subroutine get_num_fe_topology

    !> Associates \c connectivity with the (contiguous) node list of element
    !> \c element_id. The pointer is nullified for out-of-range ids.
    subroutine get_fe_connectivity_topology(self, element_id, connectivity)
        implicit none
        class(type_system_topology), intent(in), target :: self
        integer(int32), intent(in) :: element_id
        integer(int32), intent(inout), pointer, contiguous, dimension(:) :: connectivity

        integer(int32) :: first, last

        nullify (connectivity)
        if (element_id < 1 .or. element_id > self%num_fe) return

        first = self%conn_ptr(element_id)
        last = self%conn_ptr(element_id + 1) - 1
        connectivity => self%conn_idx(first:last)
    end subroutine get_fe_connectivity_topology

end module core_types_topology_system_topology
