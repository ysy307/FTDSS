module numerical_system_jacobian_matrix
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: module_core
    use :: module_domain, only:type_domain
    use :: module_linalg
    implicit none
    private

    public :: type_jacobian_matrix

    type :: type_jacobian_matrix
        private
        integer(int32) :: matrix_type = -1
        integer(int32) :: num_nodes = 0
        integer(int32) :: num_dofs_per_node = 0
        integer(int32) :: size = 0

        class(abst_matrix), allocatable :: matrix

        integer(int32), allocatable :: scatter_data(:)
        integer(int32), allocatable :: scatter_ptr(:)
        integer(int32) :: scatter_max_nodes = 0
        logical :: scatter_ready = .false.
    contains
        procedure, public, pass(self) :: initialize => initialize_jacobian_matrix
        procedure, public, pass(self) :: destroy => destroy_jacobian_matrix
        procedure, public, pass(self) :: build_scatter_map => build_scatter_map_jacobian

        procedure, public, pass(self) :: get_size => get_size_jacobian_matrix
        procedure, public, pass(self) :: get_num_dofs_per_node => get_num_dofs_per_node

        procedure, public, pass(self) :: get_matrix => get_underlying_matrix
        procedure, public, pass(self) :: is_scatter_ready => is_scatter_ready_jacobian

        procedure, private, pass(self) :: set_value_local => set_value_jacobian_matrix
        procedure, private, pass(self) :: add_value_local => add_value_jacobian_matrix
        procedure, private, pass(self) :: add_local_matrix => add_local_jacobian_matrix
        procedure, private, pass(self) :: add_local_matrix_fast => add_local_fast_jacobian_matrix
        generic, public :: set => set_value_local
        generic, public :: add => add_value_local, add_local_matrix, add_local_matrix_fast

        procedure, private, pass(self) :: zero_all => zero_all_jacobian_matrix
        procedure, private, pass(self) :: zero_row => zero_row_jacobian_matrix
        generic, public :: zero => zero_all, zero_row
        procedure, public, pass(self) :: display => display_jacobian_matrix
    end type type_jacobian_matrix

contains

    subroutine initialize_jacobian_matrix(self, domain)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        class(type_domain), intent(in) :: domain
        integer(int32), allocatable :: row(:), col(:)

        if (allocated(self%matrix)) call self%destroy()

        call domain%get_total_dofs(self%size)
        call domain%get_num_nodes(self%num_nodes)
        call domain%get_num_dof_per_node(self%num_dofs_per_node)
        call domain%get_node_adjacency(MATRIX_TYPES%CSR, row, col)

        self%matrix_type = MATRIX_TYPES%BSR%ID

        self%matrix = create_matrix(MATRIX_TYPES%BSR, self%num_nodes, row, col, self%num_dofs_per_node)

        deallocate (row, col)
    end subroutine initialize_jacobian_matrix

    subroutine build_scatter_map_jacobian(self, domain)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        class(type_domain), intent(in) :: domain

        integer(int32) :: num_fe, elem_id, i, j, n_local
        integer(int32) :: row_node, ptr_start, ptr_end, block_index
        integer(int32) :: total_entries, offset
        integer(int32), pointer, contiguous :: connectivity(:)
        integer(int32), pointer :: bsr_ptr(:), bsr_ind(:)

        if (.not. allocated(self%matrix)) return

        call domain%get_num_fe(num_fe)
        if (num_fe == 0) return

        select type (matrix => self%matrix)
        type is (type_matrix_bsr)
            bsr_ptr => matrix%get_ptr()
            bsr_ind => matrix%get_ind()
        class default
            return
        end select

        if (allocated(self%scatter_ptr)) deallocate (self%scatter_ptr)
        allocate (self%scatter_ptr(num_fe + 1))

        total_entries = 0
        self%scatter_max_nodes = 0
        do elem_id = 1, num_fe
            nullify (connectivity)
            call domain%get_fe_connectivity(elem_id, connectivity)
            n_local = size(connectivity)
            self%scatter_ptr(elem_id) = total_entries + 1
            total_entries = total_entries + n_local * n_local
            if (n_local > self%scatter_max_nodes) self%scatter_max_nodes = n_local
        end do
        self%scatter_ptr(num_fe + 1) = total_entries + 1

        if (allocated(self%scatter_data)) deallocate (self%scatter_data)
        allocate (self%scatter_data(total_entries))

        do elem_id = 1, num_fe
            nullify (connectivity)
            call domain%get_fe_connectivity(elem_id, connectivity)
            n_local = size(connectivity)
            offset = self%scatter_ptr(elem_id) - 1

            do i = 1, n_local
                row_node = connectivity(i)
                ptr_start = bsr_ptr(row_node)
                ptr_end = bsr_ptr(row_node + 1) - 1

                do j = 1, n_local
                    block_index = binary_find(connectivity(j), bsr_ind, ptr_start, ptr_end)
                    self%scatter_data(offset + (i - 1) * n_local + j) = block_index
                end do
            end do
        end do

        self%scatter_ready = .true.

    end subroutine build_scatter_map_jacobian

    subroutine destroy_jacobian_matrix(self)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self

        if (allocated(self%matrix)) then
            call self%matrix%destroy()
            deallocate (self%matrix)
        end if
        if (allocated(self%scatter_data)) deallocate (self%scatter_data)
        if (allocated(self%scatter_ptr)) deallocate (self%scatter_ptr)
        self%scatter_ready = .false.
        self%scatter_max_nodes = 0
        self%size = 0
        self%matrix_type = -1
        self%num_dofs_per_node = 0
    end subroutine destroy_jacobian_matrix

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

    pure function is_scatter_ready_jacobian(self) result(ready)
        implicit none
        class(type_jacobian_matrix), intent(in) :: self
        logical :: ready
        ready = self%scatter_ready
    end function

    function get_underlying_matrix(self) result(matrix)
        implicit none
        class(type_jacobian_matrix), intent(in), target :: self
        class(abst_matrix), pointer :: matrix
        matrix => self%matrix
    end function

    subroutine set_value_jacobian_matrix(self, row_dof, col_dof, row_node, col_node, value)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: col_dof
        integer(int32), intent(in) :: row_node
        integer(int32), intent(in) :: col_node
        real(real64), intent(in) :: value

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

    subroutine add_local_fast_jacobian_matrix(self, row_dof, col_dof, element_id, n_local, local_data)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: col_dof
        integer(int32), intent(in) :: element_id
        integer(int32), intent(in) :: n_local
        type(type_matrix_dense), intent(in) :: local_data

        integer(int32) :: i, j, offset, block_index
        real(real64), pointer :: val_bsr(:, :, :)
        real(real64), pointer, dimension(:, :) :: dense_val

        if (.not. allocated(self%matrix)) return

        dense_val => local_data%get_val()
        offset = self%scatter_ptr(element_id) - 1

        select type (matrix => self%matrix)
        type is (type_matrix_bsr)
            val_bsr => matrix%get_val()

            do i = 1, n_local
                do j = 1, n_local
                    block_index = self%scatter_data(offset + (i - 1) * n_local + j)
                    val_bsr(row_dof, col_dof, block_index) = &
                        val_bsr(row_dof, col_dof, block_index) + dense_val(i, j)
                end do
            end do
        end select
    end subroutine add_local_fast_jacobian_matrix

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
