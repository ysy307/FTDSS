module numerical_system_jacobian_matrix
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: module_core
    use :: module_domain, only:type_domain
    use :: module_linalg
    ! use :: core_types_matrix_factory
    implicit none
    private

    public :: type_jacobian_matrix

    !>
    !> Container class for the Jacobian matrix.
    !> Holds a single BSR matrix (abst_matrix) internally and maps
    !> (Node, DOF) index operations to BSR block operations.
    !>
    type :: type_jacobian_matrix
        private
        integer(int32) :: matrix_type = -1
        integer(int32) :: num_nodes = 0
        integer(int32) :: num_dofs_per_node = 0
        integer(int32) :: size = 0

        ! Single matrix object (replaces the former holder_matrices array)
        class(abst_matrix), allocatable :: matrix
    contains
        procedure, public, pass(self) :: initialize => initialize_jacobian_matrix
        procedure, public, pass(self) :: destroy => destroy_jacobian_matrix

        procedure, public, pass(self) :: get_size => get_size_jacobian_matrix
        procedure, public, pass(self) :: get_num_dofs_per_node => get_num_dofs_per_node

        ! Accessor to expose the matrix object for solvers
        procedure, public, pass(self) :: get_matrix => get_underlying_matrix

        ! Value set/add operations
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

    ! -------------------------------------------------------------------
    !  Initialize / Destroy
    ! -------------------------------------------------------------------
    subroutine initialize_jacobian_matrix(self, domain)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        class(type_domain), intent(in) :: domain
        integer(int32) :: num_nodes, num_dofs
        integer(int32), allocatable :: row(:), col(:)
        integer(int32) :: target_matrix_type

        if (allocated(self%matrix)) call self%destroy()

        call domain%get_total_dofs(self%size)
        call domain%get_num_nodes(self%num_nodes)
        call domain%get_num_dof_per_node(self%num_dofs_per_node)
        call domain%get_node_adjacency(MATRIX_TYPES%CSR, row, col)

        self%matrix_type = MATRIX_TYPES%BSR%ID

        ! Create BSR matrix using the matrix factory
        self%matrix = create_matrix(MATRIX_TYPES%BSR, self%num_nodes, row, col, self%num_dofs_per_node)

        deallocate (row, col)
    end subroutine initialize_jacobian_matrix

    subroutine destroy_jacobian_matrix(self)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self

        if (allocated(self%matrix)) then
            call self%matrix%destroy()
            deallocate (self%matrix)
        end if
        self%size = 0
        self%matrix_type = -1
        self%num_dofs_per_node = 0
    end subroutine destroy_jacobian_matrix

    ! -------------------------------------------------------------------
    !  Getters
    ! -------------------------------------------------------------------
    pure function get_size_jacobian_matrix(self) result(size)
        implicit none
        class(type_jacobian_matrix), intent(in) :: self
        integer(int32) :: size
        size = self%size
    end function

    pure function get_num_dofs_per_node(self) result(num_dofs)
        implicit none
        class(type_jacobian_matrix), intent(in) :: self
        integer(int32) :: num_dofs
        num_dofs = self%num_dofs_per_node
    end function

    !> Accessor to pass the matrix object to solvers
    function get_underlying_matrix(self) result(matrix)
        implicit none
        class(type_jacobian_matrix), intent(in), target :: self
        class(abst_matrix), pointer :: matrix
        matrix => self%matrix
    end function

    ! -------------------------------------------------------------------
    !  Setters / Adders (local index API)
    ! -------------------------------------------------------------------

    subroutine set_value_jacobian_matrix(self, row_dof, col_dof, row_node, col_node, value)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_dof ! Block-local row index (1 to num_dofs)
        integer(int32), intent(in) :: col_dof ! Block-local column index (1 to num_dofs)
        integer(int32), intent(in) :: row_node ! Block row index (Node ID)
        integer(int32), intent(in) :: col_node ! Block column index (Node ID)
        real(real64), intent(in) :: value

        ! Uses abst_matrix (BSR) set_value_block
        ! Arg order: op, row(node), col(node), row_block(dof), col_block(dof), value
        if (allocated(self%matrix)) then
            call self%matrix%set(MATRIX_OPS%INS, row_node, col_node, row_dof, col_dof, value)
        end if
    end subroutine set_value_jacobian_matrix

    subroutine add_value_jacobian_matrix(self, row_dof, col_dof, row_node, col_node, value)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: col_dof
        integer(int32), intent(in) :: row_node
        integer(int32), intent(in) :: col_node
        real(real64), intent(in) :: value

        if (allocated(self%matrix)) then
            call self%matrix%set(MATRIX_OPS%ADD, row_node, col_node, row_dof, col_dof, value)
        end if
    end subroutine add_value_jacobian_matrix

    subroutine add_local_jacobian_matrix(self, row_dof, col_dof, global_connectivity, local_data)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: col_dof
        integer(int32), intent(in) :: global_connectivity(:)
        type(type_matrix_dense), intent(in) :: local_data

        integer(int32) :: i, j, num_local_nodes
        integer(int32) :: row_node, ptr_start, ptr_end, block_index
        integer(int32), pointer :: ptr(:), ind(:)
        real(real64), pointer :: val_bsr(:, :, :)
        real(real64), pointer, dimension(:, :) :: dense_val

        if (.not. allocated(self%matrix)) return

        num_local_nodes = size(global_connectivity)
        dense_val => local_data%get_val()

        select type (matrix => self%matrix)
        type is (type_matrix_bsr)
            ptr => matrix%get_ptr()
            ind => matrix%get_ind()
            val_bsr => matrix%get_val()

            do i = 1, num_local_nodes
                row_node = global_connectivity(i)
                ptr_start = ptr(row_node)
                ptr_end = ptr(row_node + 1) - 1

                do j = 1, num_local_nodes
                    block_index = binary_find(global_connectivity(j), ind, ptr_start, ptr_end)
                    if (block_index > 0) then
                        val_bsr(row_dof, col_dof, block_index) = &
                            val_bsr(row_dof, col_dof, block_index) + dense_val(i, j)
                    end if
                end do
            end do
        class default
            do i = 1, num_local_nodes
                do j = 1, num_local_nodes
                    call self%matrix%set(MATRIX_OPS%ADD, &
                                         global_connectivity(i), global_connectivity(j), &
                                         row_dof, col_dof, dense_val(i, j))
                end do
            end do
        end select
    end subroutine add_local_jacobian_matrix

    ! -------------------------------------------------------------------
    !  Operations
    ! -------------------------------------------------------------------
    subroutine zero_all_jacobian_matrix(self)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self

        if (allocated(self%matrix)) call self%matrix%zero()
    end subroutine zero_all_jacobian_matrix

    subroutine zero_row_jacobian_matrix(self, row_node, row_block)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_node
        integer(int32), intent(in), optional :: row_block

        if (allocated(self%matrix)) then
            if (present(row_block)) then
                call self%matrix%zero(row_node, row_block)
            else
                call self%matrix%zero(row_node)
            end if
        end if
    end subroutine zero_row_jacobian_matrix

    subroutine display_jacobian_matrix(self, unit_in)
        implicit none
        class(type_jacobian_matrix), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit

        unit = optval(unit_in, output_unit)

        write (unit, '(A)') '--- Jacobian Matrix (BSR based) ---'
        write (unit, '(A, I0)') 'Num DOFs per Node: ', self%num_dofs_per_node
        if (allocated(self%matrix)) then
            call self%matrix%display(unit)
        else
            write (unit, '(A)') 'Matrix not allocated.'
        end if
        write (unit, '(A)') '-----------------------------------'
    end subroutine display_jacobian_matrix

end module numerical_system_jacobian_matrix
