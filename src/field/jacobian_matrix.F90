!>
!> ヤコビ行列を管理する高レベルなコンテナ．
!> MonolithicとStaggeredの2つのカップリングモードを透過的にサポートする．
!> APIは(ノード, DOF)のローカルインデックスを基本とする．
!>
module field_jacobian_matrix
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_domain, only:type_domain
    use :: module_linalg
    implicit none
    private

    public :: type_jacobian_matrix

    type :: type_jacobian_matrix
        private
        integer(int32) :: matrix_type = -1
        integer(int32) :: coupling_mode = -1
        integer(int32) :: num_dofs_per_node = 0
        integer(int32) :: size = 0
        type(holder_matrices), allocatable :: data(:, :) ! coupling_modeに応じてサイズが変わる
    contains
        procedure, public, pass(self) :: initialize => initialize_jacobian_matrix
        procedure, public, pass(self) :: destroy => destroy_jacobian_matrix

        procedure, public, pass(self) :: get_size => get_size_jacobian_matrix
        procedure, public, pass(self) :: get_matrix_type => get_matrix_type_jacobian_matrix
        procedure, public, pass(self) :: get_coupling_mode => get_coupling_mode_jacobian_matrix
        procedure, public, pass(self) :: get_num_dofs_per_node => get_num_dofs_per_node

        procedure, private, pass(self) :: set_value_local => set_value_jacobian_matrix
        procedure, private, pass(self) :: add_value_local => add_value_jacobian_matrix
        procedure, private, pass(self) :: add_local_matrix => add_local_jacobian_matrix
        generic, public :: set => set_value_local
        generic, public :: add => add_value_local, add_local_matrix

        procedure, public, pass(self) :: display => display_jacobian_matrix

        procedure, public, pass(self) :: zero => zero_jacobian_matrix
        procedure, public, pass(self) :: get_matrix_block => get_matrix_jacobian_block_matrix
    end type type_jacobian_matrix

contains

    ! -------------------------------------------------------------------
    !  Initialize / Destroy
    ! -------------------------------------------------------------------
    subroutine initialize_jacobian_matrix(self, domain, matrix_type)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        class(type_domain), intent(in) :: domain
        integer(int32), intent(in) :: matrix_type
        integer(int32) :: i, j, num_dofs, num_nodes
        integer(int32), pointer :: row_ptr(:) => null(), col_ptr(:) => null()

        if (allocated(self%data)) call self%destroy()

        self%matrix_type = matrix_type
        self%coupling_mode = domain%get_coupling_mode()
        self%size = domain%get_total_dofs()
        num_nodes = domain%get_num_nodes()
        num_dofs = domain%get_num_dofs_per_node()
        self%num_dofs_per_node = num_dofs

        if (matrix_type == MATRIX_CRS .or. matrix_type == MATRIX_COO) then
            call domain%get_node_adjacency(matrix_type, row_ptr, col_ptr)
        end if

        select case (self%coupling_mode)
        case (COUPLING_MODE_STAGGERED)
            allocate (self%data(1, num_dofs))
            do i = 1, num_dofs
                ! 各行列ブロックはノード間の関係のみを扱う(DOF=1)
                if (associated(row_ptr) .and. associated(col_ptr)) then
                    self%data(1, i)%m = create_matrix(matrix_type, num_nodes, row_ptr, col_ptr)
                else
                    self%data(1, i)%m = create_matrix(matrix_type, num_nodes)
                end if
            end do
        case (COUPLING_MODE_MONOLITHIC)
            ! <<< 修正: (num_dofs x num_dofs)のブロック行列として確保
            allocate (self%data(num_dofs, num_dofs))
            do i = 1, num_dofs
                do j = 1, num_dofs
                    ! 各行列ブロックはノード間の関係のみを扱う(DOF=1)
                    if (associated(row_ptr) .and. associated(col_ptr)) then
                        self%data(i, j)%m = create_matrix(matrix_type, num_nodes, row_ptr, col_ptr)
                    else
                        self%data(i, j)%m = create_matrix(matrix_type, num_nodes)
                    end if
                end do
            end do
        end select

        nullify (row_ptr, col_ptr)
    end subroutine initialize_jacobian_matrix

    subroutine destroy_jacobian_matrix(self)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self

        integer(int32) :: i, j

        if (allocated(self%data)) then
            do i = 1, size(self%data, 1)
                do j = 1, size(self%data, 2)
                    if (allocated(self%data(i, j)%m)) then
                        call self%data(i, j)%m%destroy()
                    end if
                end do
            end do
            deallocate (self%data)
        end if
        self%size = 0
        self%matrix_type = -1
        self%coupling_mode = -1
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

    pure function get_matrix_type_jacobian_matrix(self) result(matrix_type)
        implicit none
        class(type_jacobian_matrix), intent(in) :: self
        integer(int32) :: matrix_type

        matrix_type = self%matrix_type
    end function

    pure function get_coupling_mode_jacobian_matrix(self) result(coupling_mode)
        implicit none
        class(type_jacobian_matrix), intent(in) :: self
        integer(int32) :: coupling_mode

        coupling_mode = self%coupling_mode
    end function

    pure function get_num_dofs_per_node(self) result(num_dofs)
        implicit none
        class(type_jacobian_matrix), intent(in) :: self
        integer(int32) :: num_dofs

        num_dofs = self%num_dofs_per_node
    end function
    ! -------------------------------------------------------------------
    !  Setters / Adders (ローカルインデックスAPI)
    ! -------------------------------------------------------------------

    subroutine set_value_jacobian_matrix(self, row_dof, col_dof, row_node, col_node, value)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: col_dof
        integer(int32), intent(in) :: row_node
        integer(int32), intent(in) :: col_node
        real(real64), intent(in) :: value
        class(abst_matrix), pointer :: m

        if (self%coupling_mode == COUPLING_MODE_STAGGERED) then
            if (row_dof /= col_dof) error stop 'In Staggered mode, only diagonal blocks can be accessed.'
        end if

        m => self%get_matrix_block(row_dof, col_dof)
        if (associated(m)) call m%set(row_node, col_node, value)
    end subroutine

    subroutine add_value_jacobian_matrix(self, row_dof, col_dof, row_node, col_node, value)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: col_dof
        integer(int32), intent(in) :: row_node
        integer(int32), intent(in) :: col_node
        real(real64), intent(in) :: value
        class(abst_matrix), pointer :: m

        if (self%coupling_mode == COUPLING_MODE_STAGGERED) then
            if (row_dof /= col_dof) error stop 'In Staggered mode, only diagonal blocks can be accessed.'
        end if

        m => self%get_matrix_block(row_dof, col_dof)
        if (associated(m)) call m%add(row_node, col_node, value)
    end subroutine add_value_jacobian_matrix

    subroutine add_local_jacobian_matrix(self, row_dof, col_dof, global_connectivity, local_data)
        implicit none
        class(type_jacobian_matrix), intent(inout), target :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: col_dof
        integer(int32), intent(in) :: global_connectivity(:)
        type(type_dense), intent(in) :: local_data

        class(abst_matrix), pointer :: m
        integer(int32) :: num_nodes_local, i, j

        num_nodes_local = size(global_connectivity)

        m => self%get_matrix_block(row_dof, col_dof)

        if (associated(m)) call m%add(global_connectivity, local_data)
    end subroutine add_local_jacobian_matrix

    ! -------------------------------------------------------------------
    !  Operations
    ! -------------------------------------------------------------------
    subroutine zero_jacobian_matrix(self)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32) :: i, j
        if (.not. allocated(self%data)) return
        do i = 1, size(self%data, 1)
            do j = 1, size(self%data, 2)
                if (allocated(self%data(i, j)%m)) call self%data(i, j)%m%zero()
            end do
        end do
    end subroutine zero_jacobian_matrix
    ! -------------------------------------------------------------------
    !  Private Helper Function
    ! -------------------------------------------------------------------
    function get_matrix_jacobian_block_matrix(self, row_dof, col_dof) result(matrix)
        implicit none
        class(type_jacobian_matrix), intent(in), target :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: col_dof
        class(abst_matrix), pointer :: matrix

        matrix => null()
        if (.not. allocated(self%data)) return

        select case (self%coupling_mode)
        case (COUPLING_MODE_MONOLITHIC)
            if (row_dof >= 1 .and. row_dof <= size(self%data, 1) .and. &
                col_dof >= 1 .and. col_dof <= size(self%data, 2)) then
                matrix => self%data(row_dof, col_dof)%m
            end if
        case (COUPLING_MODE_STAGGERED)
            if (row_dof /= col_dof) return ! Staggeredは対角ブロックのみ
            if (row_dof >= 1 .and. row_dof <= size(self%data, 2)) then
                matrix => self%data(1, row_dof)%m
            end if
        end select
    end function get_matrix_jacobian_block_matrix

    subroutine display_jacobian_matrix(self)
        implicit none
        class(type_jacobian_matrix), intent(in) :: self
        integer(int32) :: i, j

        if (.not. allocated(self%data)) then
            write (*, '(A)') 'Jacobian Matrix is not allocated.'
            return
        end if

        write (*, '(A)') '--- Jacobian Matrix ---'
        write (*, '(A, I0)') 'Matrix Type: ', self%matrix_type
        write (*, '(A, I0)') 'Coupling Mode: ', self%coupling_mode
        write (*, '(A, I0)') 'Size (total DOFs): ', self%size
        write (*, '(A, I0)') 'Number of DOFs per Node: ', self%num_dofs_per_node
        write (*, '(A)') '-----------------------'

        do i = 1, size(self%data, 1)
            do j = 1, size(self%data, 2)
                write (*, '(A, I0, A, I0)') 'Block (', i, ',', j, '):'
                if (allocated(self%data(i, j)%m)) then
                    call self%data(i, j)%m%display()
                else
                    write (*, '(A)') '  [Not allocated]'
                end if
            end do
        end do
        write (*, '(A)') '-----------------------'
    end subroutine display_jacobian_matrix

end module field_jacobian_matrix
