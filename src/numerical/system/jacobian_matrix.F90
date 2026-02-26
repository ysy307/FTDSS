module system_jacobian_matrix
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
    !> ヤコビ行列を管理するコンテナクラス
    !> 内部で単一のBSR行列(abst_matrix)を保持し、(Node, DOF)のインデックス操作を
    !> BSRのブロック操作にマッピングする。
    !>
    type :: type_jacobian_matrix
        private
        integer(int32) :: matrix_type = -1
        integer(int32) :: num_nodes = 0
        integer(int32) :: num_dofs_per_node = 0
        integer(int32) :: size = 0

        ! 以前の holder_matrices 配列を廃止し、単一の行列オブジェクトで管理
        class(abst_matrix), allocatable :: matrix
    contains
        procedure, public, pass(self) :: initialize => initialize_jacobian_matrix
        procedure, public, pass(self) :: destroy => destroy_jacobian_matrix

        procedure, public, pass(self) :: get_size => get_size_jacobian_matrix
        procedure, public, pass(self) :: get_num_dofs_per_node => get_num_dofs_per_node

        ! ソルバー向けに行列オブジェクトそのものを公開するアクセサ
        procedure, public, pass(self) :: get_matrix => get_underlying_matrix

        ! 値設定・加算系
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

        ! 既に割り当てられていれば破棄
        if (allocated(self%matrix)) call self%destroy()

        call domain%get_total_dofs(self%size)
        call domain%get_num_nodes(self%num_nodes)
        call domain%get_num_dofs_per_node(self%num_dofs_per_node)
        call domain%get_node_adjacency(MATRIX_TYPES%CSR, row, col)

        self%matrix_type = MATRIX_TYPES%BSR%ID

        ! 行列ファクトリを使用してBSR行列を生成
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

    !> ソルバー等に行列の実体を渡すためのアクセサ
    function get_underlying_matrix(self) result(matrix)
        implicit none
        class(type_jacobian_matrix), intent(in), target :: self
        class(abst_matrix), pointer :: matrix
        matrix => self%matrix
    end function

    ! -------------------------------------------------------------------
    !  Setters / Adders (ローカルインデックスAPI)
    ! -------------------------------------------------------------------

    subroutine set_value_jacobian_matrix(self, row_dof, col_dof, row_node, col_node, value)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row_dof ! ブロック内行インデックス (1 to num_dofs)
        integer(int32), intent(in) :: col_dof ! ブロック内列インデックス (1 to num_dofs)
        integer(int32), intent(in) :: row_node ! ブロック行インデックス (Node ID)
        integer(int32), intent(in) :: col_node ! ブロック列インデックス (Node ID)
        real(real64), intent(in) :: value

        ! abst_matrix (BSR) の set_value_block を使用
        ! 引数順序: op, row(node), col(node), row_block(dof), col_block(dof), value
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
        real(real64) :: val
        real(real64), pointer, dimension(:, :) :: dense_val

        if (.not. allocated(self%matrix)) return

        num_local_nodes = size(global_connectivity)
        dense_val => local_data%get_val()

        ! ローカル密行列の各成分をBSR行列に加算
        ! Note: BSRへの値セットはブロック単位で行われるため、
        ! ここでは各ノードペアに対して set_value_block を呼び出す。
        do i = 1, num_local_nodes
            do j = 1, num_local_nodes
                call self%matrix%set(MATRIX_OPS%ADD, &
                                     global_connectivity(i), global_connectivity(j), &
                                     row_dof, col_dof, dense_val(i, j))
            end do
        end do
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

end module system_jacobian_matrix
