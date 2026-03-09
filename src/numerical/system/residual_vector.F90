!>
!> 残差ベクトルを管理する高レベルなコンテナ．
!> 内部で単一のブロック付きベクトル(type_vector_dp)を保持し、
!> (ノード, DOF)の操作をベクトルのブロック操作にマッピングする．
!>
module system_residual_vector
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: module_core
    use :: module_domain, only:type_domain
    use :: module_linalg
    ! use :: core_types_vector
    implicit none
    private

    public :: type_residual_vector

    type :: type_residual_vector
        private
        integer(int32) :: num_nodes = 0
        integer(int32) :: num_dofs_per_node = 0
        integer(int32) :: size = 0

        ! 内部データとして type_vector_dp を保持
        type(type_vector_dp) :: data
    contains
        ! --- initialize/destroy ---
        procedure, pass(self), private :: initialize_residual_vector_from_domain
        procedure, pass(self), private :: initialize_residual_vector_from_values
        generic, public :: initialize => initialize_residual_vector_from_domain, &
            initialize_residual_vector_from_values
        procedure, pass(self), public :: destroy => destroy_residual_vector

        ! --- getter ---
        procedure, pass(self), public :: get_size => get_size_residual_vector
        procedure, pass(self), public :: get_num_dofs_per_node => get_num_dofs_per_node_residual_vector

        ! 内部ベクトルへのアクセサ
        procedure, public, pass(self) :: get_vector => get_underlying_vector
        procedure, public, pass(self) :: get_data => get_data_vector

        ! --- setter ---
        procedure, pass(self), private :: set_scalar_residual_vector
        procedure, pass(self), private :: set_array_residual_vector
        procedure, pass(self), private :: set_value_at_index_residual_vector
        procedure, pass(self), private :: set_values_at_indices_residual_vector
        generic, public :: set => set_scalar_residual_vector, set_array_residual_vector, &
            set_value_at_index_residual_vector, set_values_at_indices_residual_vector

        ! --- operation ---
        procedure, pass(self), private :: add_value_residual_vector
        procedure, pass(self), private :: add_array_residual_vector
        procedure, pass(self), private :: add_value_at_index_residual_vector
        procedure, pass(self), private :: add_values_at_indices_residual_vector
        procedure, pass(self), private :: add_values_from_vector_residual_vector
        generic, public :: add => add_value_residual_vector, add_array_residual_vector, &
            add_value_at_index_residual_vector, add_values_at_indices_residual_vector, &
            add_values_from_vector_residual_vector

        procedure, pass(self), public :: zero => zero_residual_vector
        procedure, public, pass(self) :: copy => copy_residual_vector
        procedure, public, pass(self) :: scale => scale_residual_vector
        procedure, public, pass(self) :: display => display_residual_vector
    end type type_residual_vector

contains

    subroutine initialize_residual_vector_from_domain(self, domain)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        type(type_domain), intent(in) :: domain

        call domain%get_total_dofs(self%size)
        call domain%get_num_dof_per_node(self%num_dofs_per_node)
        call domain%get_num_nodes(self%num_nodes)

        ! num_blocks = num_dofs_per_node として初期化
        call self%data%initialize(self%num_nodes, num_blocks=self%num_dofs_per_node)

    end subroutine initialize_residual_vector_from_domain

    subroutine initialize_residual_vector_from_values(self, num_nodes, num_dofs_per_node)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in) :: num_dofs_per_node

        self%size = num_nodes * num_dofs_per_node
        self%num_dofs_per_node = num_dofs_per_node

        call self%data%initialize(num_nodes, num_blocks=num_dofs_per_node)

    end subroutine initialize_residual_vector_from_values

    pure function get_size_residual_vector(self) result(size)
        implicit none
        class(type_residual_vector), intent(in) :: self
        integer(int32) :: size
        size = self%size
    end function get_size_residual_vector

    pure function get_num_dofs_per_node_residual_vector(self) result(num_dofs_per_node)
        implicit none
        class(type_residual_vector), intent(in) :: self
        integer(int32) :: num_dofs_per_node
        num_dofs_per_node = self%num_dofs_per_node
    end function get_num_dofs_per_node_residual_vector

    function get_underlying_vector(self) result(vec)
        implicit none
        class(type_residual_vector), intent(in), target :: self
        class(type_vector_dp), pointer :: vec

        vec => self%data
    end function get_underlying_vector

    function get_data_vector(self) result(data_ptr)
        implicit none
        class(type_residual_vector), intent(in), target :: self
        real(real64), pointer, dimension(:) :: data_ptr

        data_ptr => self%data%get_data()
    end function get_data_vector

    ! -------------------------------------------------------------------
    !  Setters (Mapping row_dof to row_block)
    ! -------------------------------------------------------------------

    subroutine set_scalar_residual_vector(self, row_dof, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        real(real64), intent(in) :: value

        call self%data%set(VECTOR_OPS%INS, value, row_block=row_dof)
    end subroutine set_scalar_residual_vector

    subroutine set_array_residual_vector(self, row_dof, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        real(real64), intent(in) :: values(:)

        call self%data%set(VECTOR_OPS%INS, values, row_block=row_dof)
    end subroutine set_array_residual_vector

    subroutine set_value_at_index_residual_vector(self, row_dof, global_index, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: global_index
        real(real64), intent(in) :: value

        call self%data%set(VECTOR_OPS%INS, global_index, value, row_block=row_dof)
    end subroutine set_value_at_index_residual_vector

    subroutine set_values_at_indices_residual_vector(self, row_dof, global_indices, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: global_indices(:)
        real(real64), intent(in) :: values(:)

        call self%data%set(VECTOR_OPS%INS, global_indices, values, row_block=row_dof)
    end subroutine set_values_at_indices_residual_vector

    ! -------------------------------------------------------------------
    !  Adders (Mapping row_dof to row_block with VECTOR_OPS%ADD)
    ! -------------------------------------------------------------------

    subroutine add_value_residual_vector(self, row_dof, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        real(real64), intent(in) :: value

        call self%data%set(VECTOR_OPS%ADD, value, row_block=row_dof)
    end subroutine add_value_residual_vector

    subroutine add_array_residual_vector(self, row_dof, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        real(real64), intent(in) :: values(:)

        call self%data%set(VECTOR_OPS%ADD, values, row_block=row_dof)
    end subroutine add_array_residual_vector

    subroutine add_value_at_index_residual_vector(self, row_dof, global_index, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: global_index
        real(real64), intent(in) :: value

        call self%data%set(VECTOR_OPS%ADD, global_index, value, row_block=row_dof)
    end subroutine add_value_at_index_residual_vector

    subroutine add_values_at_indices_residual_vector(self, row_dof, global_indices, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: global_indices(:)
        real(real64), intent(in) :: values(:)

        call self%data%set(VECTOR_OPS%ADD, global_indices, values, row_block=row_dof)
    end subroutine add_values_at_indices_residual_vector

    subroutine add_values_from_vector_residual_vector(self, row_dof, global_indices, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: global_indices(:)
        type(type_vector_dp), intent(in) :: values

        real(real64), pointer, dimension(:) :: vals_data

        vals_data => values%get_data()

        if (.not. associated(vals_data)) return

        call self%data%set(VECTOR_OPS%ADD, global_indices, vals_data, row_block=row_dof)
    end subroutine add_values_from_vector_residual_vector

    ! -------------------------------------------------------------------
    !  Operations
    ! -------------------------------------------------------------------

    subroutine scale_residual_vector(self, alpha)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        real(real64), intent(in) :: alpha

        real(real64), pointer :: raw_data(:)

        if (.not. self%data%is_initialized()) return

        ! type_vector_dp にスカラー倍のインターフェースがないため、
        ! 生データポインタ経由で計算を行う
        raw_data => self%data%get_data()
        raw_data = raw_data * alpha
    end subroutine scale_residual_vector

    subroutine copy_residual_vector(self, source_vector)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        class(type_residual_vector), intent(in) :: source_vector

        if (.not. source_vector%data%is_initialized()) return
        call self%data%copy(source_vector%data)
    end subroutine copy_residual_vector

    subroutine zero_residual_vector(self)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        call self%data%zero()
    end subroutine zero_residual_vector

    subroutine destroy_residual_vector(self)
        implicit none
        class(type_residual_vector), intent(inout) :: self

        call self%data%destroy()
        self%size = 0
        self%num_dofs_per_node = 0
    end subroutine destroy_residual_vector

    subroutine display_residual_vector(self, unit_in)
        implicit none
        class(type_residual_vector), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in
        integer(int32) :: unit

        unit = optval(unit_in, output_unit)

        write (unit, '(A)') '--- Residual Vector (Blocked) ---'
        write (unit, '(A, I0)') 'Size (total DOFs): ', self%size
        write (unit, '(A, I0)') 'Number of DOFs per Node: ', self%num_dofs_per_node

        if (self%data%is_initialized()) then
            call self%data%display(unit)
        else
            write (unit, '(A)') '  [Not initialized]'
        end if
        write (unit, '(A)') '---------------------------------'
    end subroutine display_residual_vector

end module system_residual_vector
