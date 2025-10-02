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
        procedure, public, pass(self) :: get_matrix => get_matrix_jacobian_matrix

        ! --- 修正: ローカルインデックスAPIに統一 ---
        procedure, private, pass(self) :: set_value_local => set_value_jacobian_matrix
        procedure, private, pass(self) :: add_value_local => add_value_jacobian_matrix
        procedure, private, pass(self) :: add_local_matrix => add_local_jacobian_matrix
        generic, public :: set => set_value_local
        generic, public :: add => add_value_local, add_local_matrix

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

    ! <<< 修正: 引数を2つにして、どのブロックを取得するかを明示
    function get_matrix_jacobian_matrix(self, row_dof, col_dof) result(matrix)
        class(type_jacobian_matrix), intent(in) :: self
        integer(int32), intent(in), optional :: row_dof, col_dof
        class(abst_matrix), pointer :: matrix

        matrix => self%get_matrix_block(row_dof, col_dof)
    end function get_matrix_jacobian_matrix
    ! -------------------------------------------------------------------
    !  Setters / Adders (ローカルインデックスAPI)
    ! -------------------------------------------------------------------

    subroutine set_value_jacobian_matrix(self, row_node, col_node, value, row_dof, col_dof)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_node, col_node
        real(real64), intent(in) :: value
        integer(int32), intent(in), optional :: row_dof, col_dof
        class(abst_matrix), pointer :: m
        integer(int32) :: rdof, cdof

        rdof = 1
        if (present(row_dof)) rdof = row_dof
        cdof = 1
        if (present(col_dof)) cdof = col_dof

        if (self%coupling_mode == COUPLING_MODE_MONOLITHIC) then
            m => self%get_matrix_block()
            if (associated(m)) call m%set(row_node, col_node, value)
        else ! COUPLING_MODE_STAGGERED
            if (rdof /= cdof) return ! 対角ブロック外の操作は不可
            m => self%get_matrix_block(rdof)
            ! Staggeredの各行列はDOF=1なので，(1,1)を渡す
            if (associated(m)) call m%set(row_node, col_node, value)
        end if
    end subroutine

    subroutine add_value_jacobian_matrix(self, row_dof, col_dof, row_node, col_node, value)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in), optional :: row_dof, col_dof
        integer(int32), intent(in) :: row_node, col_node
        real(real64), intent(in) :: value
        class(abst_matrix), pointer :: m
        integer(int32) :: rdof, cdof

        rdof = 1
        if (present(row_dof)) rdof = row_dof
        cdof = 1
        if (present(col_dof)) cdof = col_dof

        if (self%coupling_mode == COUPLING_MODE_MONOLITHIC) then
            m => self%get_matrix_block()
            if (associated(m)) call m%add(row_node, col_node, value)
        else ! COUPLING_MODE_STAGGERED
            if (rdof /= cdof) return ! 対角ブロック外の操作は不可
            m => self%get_matrix_block(rdof)
            ! Staggeredの各行列はDOF=1なので，(1,1)を渡す
            if (associated(m)) call m%add(row_node, col_node, value)
        end if
    end subroutine

    subroutine add_local_jacobian_matrix(self, row_dof, col_dof, global_connectivity, local_data)
        implicit none
        class(type_jacobian_matrix), intent(inout), target :: self
        integer(int32), intent(in) :: global_connectivity(:)
        integer(int32), intent(in) :: row_dof, col_dof
        type(type_dense), intent(in) :: local_data
        class(abst_matrix), pointer :: target_block
        integer(int32) :: num_nodes_local, i, j

        num_nodes_local = size(global_connectivity)

        ! (row_dof, col_dof)に対応する全体行列のブロックを取得
        target_block => self%get_matrix_block(row_dof, col_dof)
        if (.not. associated(target_block)) return

        call target_block%add(global_connectivity, local_data)
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
        integer(int32), intent(in), optional :: row_dof, col_dof
        class(abst_matrix), pointer :: matrix

        integer(int32) :: rdof, cdof

        matrix => null()
        if (.not. allocated(self%data)) return

        rdof = 1
        if (present(row_dof)) rdof = row_dof
        cdof = 1
        if (present(col_dof)) cdof = col_dof

        select case (self%coupling_mode)
        case (COUPLING_MODE_MONOLITHIC)
            if (rdof >= 1 .and. rdof <= size(self%data, 1) .and. &
                cdof >= 1 .and. cdof <= size(self%data, 2)) then
                matrix => self%data(rdof, cdof)%m
            end if
        case (COUPLING_MODE_STAGGERED)
            if (rdof /= cdof) return ! Staggeredは対角ブロックのみ
            if (rdof >= 1 .and. rdof <= size(self%data, 2)) then
                matrix => self%data(1, rdof)%m
            end if
        end select
    end function get_matrix_jacobian_block_matrix

end module field_jacobian_matrix
