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

    subroutine initialize_jacobian_matrix(self, domain, coupling_mode)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        class(type_domain), intent(in) :: domain
        type(type_constant_id), intent(in) :: coupling_mode

        integer(int32), allocatable :: row(:), col(:)
        integer(int32) :: i, j

        if (allocated(self%matrix)) call self%destroy()

        self%coupling_mode = coupling_mode
        self%physics_to_system(:) = 0
        self%num_dofs_of_physics(:) = 0

        call domain%get_total_dofs(self%size)
        call domain%get_num_nodes(self%num_nodes)
        call domain%get_node_adjacency(MATRIX_TYPES%CSR, row, col)

        select case (coupling_mode%ID)

        case (COUPLING_MODES%MONOLITHIC%ID)

            call domain%get_num_dof_per_node(self%num_dofs_per_node)
            self%num_system = 1

            allocate (self%system_size(1))
            self%system_size(1) = self%size

        case (COUPLING_MODES%STAGGERED%ID)
            do i = 1, PHYSICS_TYPES%NUM_ID
                call domain%get_target_dof(PHYSICS_TYPES%to_object(i), self%num_dofs_of_physics(i))
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

        call self%build_scatter_map(domain)

        deallocate (row, col)
    end subroutine initialize_jacobian_matrix

    subroutine build_scatter_map_jacobian(self, domain)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        class(type_domain), intent(in) :: domain

        integer(int32) :: num_fe, elem_id
        integer(int32) :: i, j, n_local
        integer(int32), allocatable :: shape(:, :)

        integer(int32), pointer, contiguous :: connectivity(:)
        integer(int32), pointer :: ptr(:), ind(:)

        call domain%get_num_fe(num_fe)
        if (num_fe == 0) return

        allocate (shape(2, num_fe))

        do elem_id = 1, num_fe
            call domain%get_fe_connectivity(elem_id, connectivity)
            shape(1, elem_id) = size(connectivity)
            shape(2, elem_id) = size(connectivity)
        end do

        call self%scatter_map%initialize(num_fe, 2, shape)

        ptr => self%matrix(1)%get_ptr()
        ind => self%matrix(1)%get_ind()

        do elem_id = 1, num_fe
            call domain%get_fe_connectivity(elem_id, connectivity)
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
        call self%scatter_map%destroy()
        self%size = 0
        self%num_dofs_per_node = 0
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

    function get_underlying_matrix(self, physics_id) result(matrix)
        implicit none
        class(type_jacobian_matrix), intent(in), target :: self
        type(type_constant_id), intent(in), optional :: physics_id
        class(abst_matrix), pointer :: matrix

        integer(int32) :: sys_id

        if (present(physics_id)) then
            sys_id = physics_id%ID
        else
            sys_id = 1
        end if

        select case (self%coupling_mode%ID)
        case (COUPLING_MODES%MONOLITHIC%ID)
            if (sys_id /= 1) then
                nullify (matrix)
                return
            end if
        case (COUPLING_MODES%STAGGERED%ID)
            if (sys_id < 1 .or. sys_id > PHYSICS_TYPES%NUM_ID) then
                nullify (matrix)
                return
            end if
            sys_id = self%physics_to_system(sys_id)
            if (sys_id == 0) then
                nullify (matrix)
                return
            end if
        end select

        matrix => self%matrix(sys_id)
    end function

    subroutine set_value_jacobian_matrix(self, row_dof, col_dof, row_node, col_node, value)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: col_dof
        integer(int32), intent(in) :: row_node
        integer(int32), intent(in) :: col_node
        real(real64), intent(in) :: value

        integer(int32) :: sys

        if (.not. allocated(self%matrix)) return

        select case (self%coupling_mode%ID)
        case (COUPLING_MODES%MONOLITHIC%ID)
            call self%matrix(1)%set(MATRIX_OPS%INS, row_node, col_node, row_dof, col_dof, value)
        case (COUPLING_MODES%STAGGERED%ID)
            if (row_dof /= col_dof) return
            sys = self%physics_to_system(row_dof)
            if (sys == 0) return
            call self%matrix(sys)%set(MATRIX_OPS%INS, row_node, col_node, 1, 1, value)
        end select
    end subroutine set_value_jacobian_matrix

    subroutine add_value_jacobian_matrix(self, row_dof, col_dof, row_node, col_node, value)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: col_dof
        integer(int32), intent(in) :: row_node
        integer(int32), intent(in) :: col_node
        real(real64), intent(in) :: value

        integer(int32) :: sys

        if (.not. allocated(self%matrix)) return

        select case (self%coupling_mode%ID)
        case (COUPLING_MODES%MONOLITHIC%ID)
            call self%matrix(1)%set(MATRIX_OPS%ADD, row_node, col_node, row_dof, col_dof, value)
        case (COUPLING_MODES%STAGGERED%ID)
            if (row_dof /= col_dof) return
            sys = self%physics_to_system(row_dof)
            if (sys == 0) return
            call self%matrix(sys)%set(MATRIX_OPS%ADD, row_node, col_node, 1, 1, value)
        end select
    end subroutine add_value_jacobian_matrix

    subroutine add_local_jacobian_matrix(self, row_dof, col_dof, element_id, n_local, local_data)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: col_dof
        integer(int32), intent(in) :: element_id
        integer(int32), intent(in) :: n_local
        type(type_matrix_dense), intent(in) :: local_data

        integer(int32) :: i, j, idx
        integer(int32) :: sys
        real(real64), pointer, dimension(:, :) :: dense_val

        if (.not. allocated(self%matrix)) return

        dense_val => local_data%get_val()

        select case (self%coupling_mode%ID)

        case (COUPLING_MODES%MONOLITHIC%ID)

            do i = 1, n_local
                do j = 1, n_local
                    call self%scatter_map%get_index(element_id, [i, j], idx)
                    call self%matrix(1)%set(MATRIX_OPS%ADD, idx, row_dof, col_dof, dense_val(i, j))
                end do
            end do

        case (COUPLING_MODES%STAGGERED%ID)
            if (row_dof /= col_dof) return

            sys = self%physics_to_system(row_dof)
            if (sys == 0) return

            do i = 1, n_local
                do j = 1, n_local
                    call self%scatter_map%get_index(element_id, [i, j], idx)
                    call self%matrix(sys)%set(MATRIX_OPS%ADD, idx, 1, 1, dense_val(i, j))
                end do
            end do

        end select
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

    subroutine zero_row_jacobian_matrix(self, row_node, row_block)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_node
        integer(int32), intent(in), optional :: row_block
        integer(int32) :: i

        if (allocated(self%matrix)) then
            do i = 1, size(self%matrix)
                if (present(row_block)) then
                    call self%matrix(i)%zero(row_node, row_block)
                else
                    call self%matrix(i)%zero(row_node)
                end if
            end do
        end if
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
