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
        integer(int32) :: size = 0
        type(holder_matrices), allocatable :: data(:)
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
        procedure, private, pass(self) :: p_get_target_matrix
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
        integer(int32) :: i, num_dofs, num_nodes
        integer(int32), pointer :: row_ptr(:) => null(), col_ptr(:) => null()

        self%matrix_type = matrix_type
        self%coupling_mode = domain%get_coupling_mode()
        self%size = domain%get_total_dofs()
        num_nodes = domain%get_num_nodes()
        num_dofs = domain%get_num_dofs_per_node()

        if (allocated(self%data)) deallocate (self%data)

        if (matrix_type == MATRIX_CRS .or. matrix_type == MATRIX_COO) then
            row_ptr => domain%get_node_adjacency_ptr_row()
            col_ptr => domain%get_node_adjacency_ptr_col()
        end if

        select case (self%coupling_mode)
        case (COUPLING_MODE_STAGGERED)
            allocate (self%data(num_dofs))
            do i = 1, num_dofs
                self%data(i)%m = create_matrix(matrix_type, num_nodes, 1, row_ptr, col_ptr)
            end do
        case (COUPLING_MODE_MONOLITHIC)
            allocate (self%data(1))
            self%data(1)%m = create_matrix(matrix_type, num_nodes, num_dofs, row_ptr, col_ptr)
        end select

        nullify (row_ptr, col_ptr)
    end subroutine initialize_jacobian_matrix

    subroutine destroy_jacobian_matrix(self)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        if (allocated(self%data)) deallocate (self%data)
        self%size = 0
        self%matrix_type = -1
        self%coupling_mode = -1
    end subroutine destroy_jacobian_matrix

    ! -------------------------------------------------------------------
    !  Getters
    ! -------------------------------------------------------------------
    pure function get_size_jacobian_matrix(self) result(sz)
        class(type_jacobian_matrix), intent(in) :: self; integer(int32) :: sz; sz = self%size
    end function
    pure function get_matrix_type_jacobian_matrix(self) result(mt)
        class(type_jacobian_matrix), intent(in) :: self; integer(int32) :: mt; mt = self%matrix_type
    end function

    function get_matrix_jacobian_matrix(self, dof) result(matrix)
        class(type_jacobian_matrix), intent(in) :: self
        integer(int32), intent(in), optional :: dof
        class(abst_matrix), pointer :: matrix
        matrix => self%p_get_target_matrix(dof)
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

        rdof = 1; if (present(row_dof)) rdof = row_dof
        cdof = 1; if (present(col_dof)) cdof = col_dof

        if (self%coupling_mode == COUPLING_MODE_MONOLITHIC) then
            m => self%p_get_target_matrix()
            if (associated(m)) call m%set(rdof, cdof, row_node, col_node, value)
        else ! COUPLING_MODE_STAGGERED
            if (rdof /= cdof) return ! 対角ブロック外の操作は不可
            m => self%p_get_target_matrix(rdof)
            ! Staggeredの各行列はDOF=1なので，(1,1)を渡す
            if (associated(m)) call m%set(1, 1, row_node, col_node, value)
        end if
    end subroutine

    subroutine add_value_jacobian_matrix(self, row_node, col_node, value, row_dof, col_dof)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_node, col_node
        real(real64), intent(in) :: value
        integer(int32), intent(in), optional :: row_dof, col_dof
        class(abst_matrix), pointer :: m
        integer(int32) :: rdof, cdof

        rdof = 1; if (present(row_dof)) rdof = row_dof
        cdof = 1; if (present(col_dof)) cdof = col_dof

        if (self%coupling_mode == COUPLING_MODE_MONOLITHIC) then
            m => self%p_get_target_matrix()
            if (associated(m)) call m%add(rdof, cdof, row_node, col_node, value)
        else ! COUPLING_MODE_STAGGERED
            if (rdof /= cdof) return ! 対角ブロック外の操作は不可
            m => self%p_get_target_matrix(rdof)
            ! Staggeredの各行列はDOF=1なので，(1,1)を渡す
            if (associated(m)) call m%add(1, 1, row_node, col_node, value)
        end if
    end subroutine

    subroutine add_local_jacobian_matrix(self, connectivity, local_data)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: connectivity(:)
        type(type_dense), intent(in) :: local_data
        class(abst_matrix), pointer :: target_matrix
        integer(int32) :: num_nodes_local, num_dofs_local, i, j, idof, jdof, irow, icol

        ! NOTE: add_localはMonolithicモードでのみ意味を持つ
        if (self%coupling_mode /= COUPLING_MODE_MONOLITHIC) return
        target_matrix => self%p_get_target_matrix()
        if (.not. associated(target_matrix)) return

        num_nodes_local = size(connectivity)
        num_dofs_local = target_matrix%num_dofs
        if (size(local_data%val, 1) /= num_nodes_local * num_dofs_local) return

        do i = 1, num_nodes_local
            do idof = 1, num_dofs_local
                irow = (i - 1) * num_dofs_local + idof
                do j = 1, num_nodes_local
                    do jdof = 1, num_dofs_local
                        icol = (j - 1) * num_dofs_local + jdof
                        call target_matrix%add(idof, jdof, connectivity(i), connectivity(j), local_data%val(irow, icol))
                    end do
                end do
            end do
        end do
    end subroutine add_local_jacobian_matrix

    ! -------------------------------------------------------------------
    !  Operations
    ! -------------------------------------------------------------------
    subroutine zero_jacobian_matrix(self)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32) :: i
        if (.not. allocated(self%data)) return
        do i = 1, size(self%data)
            if (allocated(self%data(i)%m)) call self%data(i)%m%zero()
        end do
    end subroutine zero_jacobian_matrix

    ! -------------------------------------------------------------------
    !  Private Helper Function
    ! -------------------------------------------------------------------
    function p_get_target_matrix(self, dof) result(matrix)
        class(type_jacobian_matrix), intent(in) :: self
        integer(int32), intent(in), optional :: dof
        class(abst_matrix), pointer :: matrix
        integer(int32) :: idx

        matrix => null()
        if (.not. allocated(self%data)) return

        if (self%coupling_mode == COUPLING_MODE_MONOLITHIC) then
            idx = 1
        else ! COUPLING_MODE_STAGGERED
            idx = 1; if (present(dof)) idx = dof
        end if

        if (idx < 1 .or. idx > size(self%data)) return
        if (allocated(self%data(idx)%m)) matrix => self%data(idx)%m
    end function p_get_target_matrix

end module field_jacobian_matrix
