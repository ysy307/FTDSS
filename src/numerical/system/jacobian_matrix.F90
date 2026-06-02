module numerical_system_jacobian_matrix
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    use :: stdlib_optval, only:optval
    use :: module_core
    use :: module_linalg
    implicit none
    private

    public :: type_jacobian_matrix

    type :: type_jacobian_matrix
        private
        integer(int32) :: num_nodes = 0
        integer(int32) :: num_dofs_per_node = 0
        integer(int32) :: num_dofs_of_physics(PHYSICS_TYPES%NUM_ID) = 0
        integer(int32) :: size = 0

        type(type_constant_id) :: coupling_mode

        type(type_matrix_bsr), allocatable :: matrix(:)
        type(type_scatter_map) :: scatter_map

        integer(int32) :: num_system = 0

        !> physics i -> system index (0 if inactive)
        integer(int32) :: physics_to_system(PHYSICS_TYPES%NUM_ID) = 0

        !> per system total dofs
        integer(int32), allocatable :: system_size(:)
    contains
        procedure, public, pass(self) :: initialize => initialize_jacobian_matrix
        procedure, public, pass(self) :: destroy => destroy_jacobian_matrix
        procedure, public, pass(self) :: build_scatter_map => build_scatter_map_jacobian

        procedure, public, pass(self) :: get_size => get_size_jacobian_matrix
        procedure, public, pass(self) :: get_num_dofs_per_node => get_num_dofs_per_node
        procedure, public, pass(self) :: get_matrix => get_underlying_matrix

        procedure, private, pass(self) :: resolve_block_target
        procedure, private, pass(self) :: set_value_local => set_value_jacobian_matrix
        procedure, private, pass(self) :: add_value_local => add_value_jacobian_matrix
        procedure, private, pass(self) :: add_local_matrix => add_local_jacobian_matrix
        generic, public :: set => set_value_local
        generic, public :: add => add_value_local, add_local_matrix

        procedure, private, pass(self) :: zero_all => zero_all_jacobian_matrix
        procedure, private, pass(self) :: zero_row => zero_row_jacobian_matrix
        generic, public :: zero => zero_all, zero_row
        procedure, public, pass(self) :: display => display_jacobian_matrix
    end type type_jacobian_matrix

contains

    subroutine initialize_jacobian_matrix(self, topology, coupling_mode)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        type(type_system_topology), intent(in) :: topology
        type(type_constant_id), intent(in) :: coupling_mode

        integer(int32), allocatable :: row(:), col(:)
        integer(int32) :: i, j

        call self%destroy()

        self%coupling_mode = coupling_mode
        self%physics_to_system(:) = 0
        self%num_dofs_of_physics(:) = 0

        call topology%get_total_dofs(self%size)
        call topology%get_num_nodes(self%num_nodes)
        call topology%get_node_adjacency(MATRIX_TYPES%CSR, row, col)

        select case (coupling_mode%ID)

        case (COUPLING_MODES%MONOLITHIC%ID)

            call topology%get_num_dof_per_node(self%num_dofs_per_node)
            self%num_system = 1

            allocate (self%system_size(1))
            self%system_size(1) = self%size

        case (COUPLING_MODES%STAGGERED%ID)
            do i = 1, PHYSICS_TYPES%NUM_ID
                call topology%get_target_dof(PHYSICS_TYPES%to_object(i), self%num_dofs_of_physics(i))
            end do

            self%num_system = count(self%num_dofs_of_physics > 0)

            allocate (self%system_size(self%num_system))

            self%num_dofs_per_node = 0

        end select

        allocate (self%matrix(self%num_system))

        select case (coupling_mode%ID)
        case (COUPLING_MODES%MONOLITHIC%ID)
            call self%matrix(1)%initialize(self%num_nodes, row, col, self%num_dofs_per_node, self%num_dofs_per_node)
        case (COUPLING_MODES%STAGGERED%ID)
            j = 0
            do i = 1, PHYSICS_TYPES%NUM_ID
                if (self%num_dofs_of_physics(i) == 0) cycle
                j = j + 1
                self%physics_to_system(i) = j
                self%system_size(j) = self%num_nodes * self%num_dofs_of_physics(i)
                call self%matrix(j)%initialize( &
                    self%num_nodes, row, col, self%num_dofs_of_physics(i), self%num_dofs_of_physics(i) &
                    )
            end do

        end select

        call self%build_scatter_map(topology)

        deallocate (row, col)
    end subroutine initialize_jacobian_matrix

    subroutine build_scatter_map_jacobian(self, topology)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        type(type_system_topology), intent(in), target :: topology

        integer(int32) :: num_fe, elem_id
        integer(int32) :: i, j, n_local
        integer(int32), allocatable :: shape(:, :)

        integer(int32), pointer, contiguous :: connectivity(:)
        integer(int32), pointer :: ptr(:), ind(:)

        call topology%get_num_fe(num_fe)
        if (num_fe == 0) return

        allocate (shape(2, num_fe))

        do elem_id = 1, num_fe
            call topology%get_fe_connectivity(elem_id, connectivity)
            shape(1, elem_id) = size(connectivity)
            shape(2, elem_id) = size(connectivity)
        end do

        call self%scatter_map%initialize(num_fe, 2, shape)

        ptr => self%matrix(1)%get_ptr()
        ind => self%matrix(1)%get_ind()

        do elem_id = 1, num_fe
            call topology%get_fe_connectivity(elem_id, connectivity)
            n_local = size(connectivity)

            do i = 1, n_local
                do j = 1, n_local
                    call self%scatter_map%set(elem_id, [i, j], &
                                              binary_find(connectivity(j), ind, &
                                                          ptr(connectivity(i)), ptr(connectivity(i) + 1) - 1))
                end do
            end do
        end do

    end subroutine build_scatter_map_jacobian

    subroutine destroy_jacobian_matrix(self)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32) :: i

        if (allocated(self%matrix)) then
            do i = 1, size(self%matrix)
                call self%matrix(i)%destroy()
            end do
            deallocate (self%matrix)
        end if

        call deallocate_array(self%system_size)

        call self%scatter_map%destroy()
        self%size = 0
        self%num_nodes = 0
        self%num_dofs_per_node = 0
        self%num_system = 0
        self%physics_to_system(:) = 0
        self%num_dofs_of_physics(:) = 0
    end subroutine destroy_jacobian_matrix

    subroutine get_size_jacobian_matrix(self, size)
        implicit none
        class(type_jacobian_matrix), intent(in) :: self
        integer(int32), intent(out) :: size

        size = self%size
    end subroutine get_size_jacobian_matrix

    subroutine get_num_dofs_per_node(self, num_dofs)
        implicit none
        class(type_jacobian_matrix), intent(in) :: self
        integer(int32), intent(out) :: num_dofs

        num_dofs = self%num_dofs_per_node
    end subroutine get_num_dofs_per_node

    function get_underlying_matrix(self, physics_id_in) result(matrix)
        implicit none
        class(type_jacobian_matrix), intent(in), target :: self
        type(type_constant_id), intent(in), optional :: physics_id_in
        class(abst_matrix), pointer :: matrix

        integer(int32) :: sys_id, physics_id

        select case (self%coupling_mode%ID)
        case (COUPLING_MODES%MONOLITHIC%ID)
            physics_id = 1
            if (present(physics_id_in)) physics_id = physics_id_in%ID
            if (physics_id /= 1) then
                nullify (matrix)
                return
            end if
            matrix => self%matrix(1)
        case (COUPLING_MODES%STAGGERED%ID)
            if (.not. present(physics_id_in)) then
                if (allocated(self%matrix) .and. size(self%matrix) > 0) then
                    matrix => self%matrix(1)
                else
                    nullify (matrix)
                end if
                return
            end if

            physics_id = physics_id_in%ID
            if (physics_id < 1 .or. physics_id > PHYSICS_TYPES%NUM_ID) then
                nullify (matrix)
                return
            end if
            sys_id = self%physics_to_system(physics_id)
            if (sys_id <= 0 .or. sys_id > size(self%matrix)) then
                nullify (matrix)
                return
            end if
            matrix => self%matrix(sys_id)
        end select

    end function

    !> Resolve the target matrix pointer and the single-entry block coordinates
    !> for a given (row, col) physics pair, encapsulating the coupling-mode
    !> logic in one place. Returns a nullified pointer when the entry is not
    !> storable (off-diagonal in STAGGERED, or out-of-range physics).
    subroutine resolve_block_target(self, row_physics_id, col_physics_id, mat, row_blk, col_blk)
        implicit none
        class(type_jacobian_matrix), intent(in), target :: self
        integer(int32), intent(in) :: row_physics_id, col_physics_id
        class(abst_matrix), pointer, intent(inout) :: mat
        integer(int32), intent(inout) :: row_blk, col_blk

        integer(int32) :: sys

        nullify (mat)
        if (.not. allocated(self%matrix)) return

        select case (self%coupling_mode%ID)
        case (COUPLING_MODES%MONOLITHIC%ID)
            mat => self%matrix(1)
            row_blk = row_physics_id
            col_blk = col_physics_id
        case (COUPLING_MODES%STAGGERED%ID)
            if (row_physics_id /= col_physics_id) return
            if (row_physics_id < 1 .or. row_physics_id > PHYSICS_TYPES%NUM_ID) return
            sys = self%physics_to_system(row_physics_id)
            if (sys <= 0 .or. sys > size(self%matrix)) return
            mat => self%matrix(sys)
            row_blk = 1
            col_blk = 1
        end select
    end subroutine resolve_block_target

    subroutine set_value_jacobian_matrix(self, row_physics_id, col_physics_id, row_node, col_node, value)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_physics_id
        integer(int32), intent(in) :: col_physics_id
        integer(int32), intent(in) :: row_node
        integer(int32), intent(in) :: col_node
        real(real64), intent(in) :: value

        class(abst_matrix), pointer :: mat
        integer(int32) :: row_blk, col_blk

        call self%resolve_block_target(row_physics_id, col_physics_id, mat, row_blk, col_blk)
        if (.not. associated(mat)) return
        call bsr_set_node_entry(mat, MATRIX_OPS%INS, row_node, col_node, row_blk, col_blk, value)
    end subroutine set_value_jacobian_matrix

    subroutine add_value_jacobian_matrix(self, row_physics_id, col_physics_id, row_node, col_node, value)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_physics_id
        integer(int32), intent(in) :: col_physics_id
        integer(int32), intent(in) :: row_node
        integer(int32), intent(in) :: col_node
        real(real64), intent(in) :: value

        class(abst_matrix), pointer :: mat
        integer(int32) :: row_blk, col_blk

        call self%resolve_block_target(row_physics_id, col_physics_id, mat, row_blk, col_blk)
        if (.not. associated(mat)) return
        call bsr_set_node_entry(mat, MATRIX_OPS%ADD, row_node, col_node, row_blk, col_blk, value)
    end subroutine add_value_jacobian_matrix

    subroutine bsr_set_node_entry(mat, op, row_node, col_node, row_sys, col_sys, value)
        implicit none
        class(abst_matrix), intent(inout) :: mat
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: row_node, col_node, row_sys, col_sys
        real(real64), intent(in) :: value

        integer(int32) :: idx_arr(1, 1)
        real(real64) :: val_arr(1, 1)
        integer(int32), pointer :: ptr_p(:), ind_p(:)

        select type (m => mat)
        type is (type_matrix_bsr)
            ptr_p => m%get_ptr()
            ind_p => m%get_ind()
            idx_arr(1, 1) = binary_find(col_node, ind_p, ptr_p(row_node), ptr_p(row_node + 1) - 1)
            val_arr(1, 1) = value
            call m%set(op, 1, idx_arr, row_sys, col_sys, val_arr)
        end select
    end subroutine bsr_set_node_entry

    subroutine add_local_jacobian_matrix(self, row_physics_id, col_physics_id, element_id, n_local, local_data)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_physics_id
        integer(int32), intent(in) :: col_physics_id
        integer(int32), intent(in) :: element_id
        integer(int32), intent(in) :: n_local
        type(type_matrix_dense), intent(in) :: local_data

        integer(int32) :: i, j, row_blk, col_blk
        integer(int32), allocatable :: indices(:, :)
        real(real64), pointer, dimension(:, :) :: dense_val
        class(abst_matrix), pointer :: mat

        if (.not. allocated(self%matrix)) return

        dense_val => local_data%get_val()
        if (any(.not. ieee_is_finite(dense_val))) then
            write (*, '(A,I0)') '[ERR-MATRIX] dense_val contains non-finite values for element_id=', element_id
            return
        end if

        ! Build global BSR block index array from scatter_map (O(1) per entry)
        allocate (indices(n_local, n_local))
        do j = 1, n_local
            do i = 1, n_local
                call self%scatter_map%get_index(element_id, [i, j], indices(i, j))
            end do
        end do

        ! The dense block is already laid out via indices, so the DOF offsets
        ! are always (1, 1) regardless of coupling mode; only the target matrix
        ! differs, which the helper resolves.
        call self%resolve_block_target(row_physics_id, col_physics_id, mat, row_blk, col_blk)
        if (associated(mat)) call mat%set(MATRIX_OPS%ADD, n_local, indices, 1, 1, dense_val)

        deallocate (indices)
    end subroutine add_local_jacobian_matrix

    subroutine zero_all_jacobian_matrix(self)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32) :: i

        if (allocated(self%matrix)) then
            do i = 1, size(self%matrix)
                call self%matrix(i)%zero()
            end do
        end if
    end subroutine zero_all_jacobian_matrix

    subroutine zero_row_jacobian_matrix(self, row_node, row_physics_id)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_node
        integer(int32), intent(in), optional :: row_physics_id
        class(abst_matrix), pointer :: mat
        integer(int32) :: i, row_blk, col_blk

        ! Without a physics id, zero the node-row across every system block.
        if (.not. present(row_physics_id)) then
            if (allocated(self%matrix)) then
                do i = 1, size(self%matrix)
                    call self%matrix(i)%zero(row_node)
                end do
            end if
            return
        end if

        ! With a physics id, target the resolved block (passing the id twice
        ! restricts STAGGERED to its diagonal system, as before).
        call self%resolve_block_target(row_physics_id, row_physics_id, mat, row_blk, col_blk)
        if (associated(mat)) call mat%zero(row_node, row_blk)
    end subroutine zero_row_jacobian_matrix

    subroutine display_jacobian_matrix(self, unit_in)
        implicit none
        class(type_jacobian_matrix), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit
        integer(int32) :: i

        unit = optval(unit_in, output_unit)

        write (unit, '(A)') '--- Jacobian Matrix (BSR based) ---'
        write (unit, '(A, I0)') 'Num DOFs per Node: ', self%num_dofs_per_node
        if (allocated(self%matrix)) then
            do i = 1, size(self%matrix)
                call self%matrix(i)%display(unit)
            end do
        else
            write (unit, '(A)') 'Matrix not allocated.'
        end if
        write (unit, '(A)') '-----------------------------------'
    end subroutine display_jacobian_matrix

end module numerical_system_jacobian_matrix
