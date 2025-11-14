!>
!> 残差ベクトルを管理する高レベルなコンテナ．
!> APIは(ノード, DOF)のローカルインデックスを基本とする．
!>
module field_residual_vector
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_domain, only:type_domain
    use :: module_linalg
    implicit none
    private

    public :: type_residual_vector

    type :: type_residual_vector
        private
        ! integer(int32) :: coupling_mode = -1 ! coupling_modeは使用されないため削除
        integer(int32) :: num_dofs_per_node = 0
        integer(int32) :: size = 0
        type(type_vector_dp), allocatable :: data(:)
    contains
        ! --- initialize/destroy ---
        procedure, pass(self), public :: initialize => initialize_residual_vector
        procedure, pass(self), public :: destroy => destroy_residual_vector

        ! --- getter ---
        procedure, pass(self), public :: get_size => get_size_residual_vector
        procedure, pass(self), public :: get_data => get_data_residual_vector

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
        generic, public :: add => add_value_residual_vector, add_array_residual_vector, &
            add_value_at_index_residual_vector, add_values_at_indices_residual_vector
        procedure, pass(self), public :: zero => zero_residual_vector

        procedure, public, pass(self) :: scale => scale_residual_vector

        procedure, public, pass(self) :: display => display_residual_vector
    end type type_residual_vector

contains

    subroutine initialize_residual_vector(self, domain)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        type(type_domain), intent(in) :: domain

        integer(int32) :: i, num_dofs

        ! self%coupling_mode = domain%get_coupling_mode() ! coupling_modeは使用されないため削除
        self%size = domain%get_num_nodes() * domain%get_num_dofs_per_node()
        self%num_dofs_per_node = domain%get_num_dofs_per_node()

        allocate (self%data(self%num_dofs_per_node))
        do i = 1, self%num_dofs_per_node
            call self%data(i)%initialize(domain%get_num_nodes())
        end do

    end subroutine initialize_residual_vector

    pure function get_size_residual_vector(self) result(size)
        implicit none
        class(type_residual_vector), intent(in) :: self
        integer(int32) :: size

        size = self%size

    end function get_size_residual_vector

    function get_data_residual_vector(self, row_dof) result(data)
        implicit none
        class(type_residual_vector), intent(in), target :: self
        integer(int32), intent(in) :: row_dof
        type(type_vector_dp), pointer :: data

        ! 配列の境界チェックを追加 (念のため)
        if (row_dof >= 1 .and. row_dof <= self%num_dofs_per_node) then
            data => self%data(row_dof)
        else
            data => null()
            ! エラー処理や警告をここに追加しても良い
            ! print *, "Error: row_dof out of bounds in get_data_residual_vector"
        end if

    end function get_data_residual_vector

    subroutine set_scalar_residual_vector(self, row_dof, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        real(real64), intent(in) :: value

        ! row_dofの境界チェック
        if (row_dof < 1 .or. row_dof > self%num_dofs_per_node) then
            error stop 'Error: row_dof out of bounds in set_scalar_residual_vector'
        end if
        call self%data(row_dof)%set(value)
    end subroutine set_scalar_residual_vector

    subroutine set_array_residual_vector(self, row_dof, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        real(real64), intent(in) :: values(:)

        ! row_dofの境界チェック
        if (row_dof < 1 .or. row_dof > self%num_dofs_per_node) then
            error stop 'Error: row_dof out of bounds in set_array_residual_vector'
        end if
        call self%data(row_dof)%set(values)
    end subroutine set_array_residual_vector

    subroutine set_value_at_index_residual_vector(self, row_dof, global_index, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: global_index
        real(real64), intent(in) :: value

        ! row_dofの境界チェック
        if (row_dof < 1 .or. row_dof > self%num_dofs_per_node) then
            error stop 'Error: row_dof out of bounds in set_value_at_index_residual_vector'
        end if
        call self%data(row_dof)%set(global_index, value)
    end subroutine set_value_at_index_residual_vector

    subroutine set_values_at_indices_residual_vector(self, row_dof, global_indices, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: global_indices(:)
        real(real64), intent(in) :: values(:)

        ! row_dofの境界チェック
        if (row_dof < 1 .or. row_dof > self%num_dofs_per_node) then
            error stop 'Error: row_dof out of bounds in set_values_at_indices_residual_vector'
        end if
        call self%data(row_dof)%set(global_indices, values)
    end subroutine set_values_at_indices_residual_vector

    subroutine add_value_residual_vector(self, row_dof, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        real(real64), intent(in) :: value

        ! row_dofの境界チェック
        if (row_dof < 1 .or. row_dof > self%num_dofs_per_node) then
            error stop 'Error: row_dof out of bounds in add_value_residual_vector'
        end if
        call self%data(row_dof)%add(value)

    end subroutine add_value_residual_vector

    subroutine add_array_residual_vector(self, row_dof, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        real(real64), intent(in) :: values(:)

        ! row_dofの境界チェック
        if (row_dof < 1 .or. row_dof > self%num_dofs_per_node) then
            error stop 'Error: row_dof out of bounds in add_array_residual_vector'
        end if
        call self%data(row_dof)%add(values)
    end subroutine add_array_residual_vector

    subroutine add_value_at_index_residual_vector(self, row_dof, global_index, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: global_index
        real(real64), intent(in) :: value

        ! row_dofの境界チェック
        if (row_dof < 1 .or. row_dof > self%num_dofs_per_node) then
            error stop 'Error: row_dof out of bounds in add_value_at_index_residual_vector'
        end if
        call self%data(row_dof)%add(global_index, value)

    end subroutine add_value_at_index_residual_vector

    subroutine add_values_at_indices_residual_vector(self, row_dof, global_indices, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: row_dof
        integer(int32), intent(in) :: global_indices(:)
        real(real64), intent(in) :: values(:)

        ! row_dofの境界チェック
        if (row_dof < 1 .or. row_dof > self%num_dofs_per_node) then
            error stop 'Error: row_dof out of bounds in add_values_at_indices_residual_vector'
        end if
        call self%data(row_dof)%add(global_indices, values)
    end subroutine add_values_at_indices_residual_vector

    subroutine scale_residual_vector(self, alpha)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        real(real64), intent(in) :: alpha

        integer(int32) :: i

        ! allocateされていない場合のチェックを追加
        if (.not. allocated(self%data)) return

        do i = 1, self%num_dofs_per_node
            call self%data(i)%scale(alpha)
        end do
    end subroutine scale_residual_vector

    subroutine zero_residual_vector(self)
        implicit none
        class(type_residual_vector), intent(inout) :: self

        integer(int32) :: i

        ! allocateされていない場合のチェックを追加
        if (.not. allocated(self%data)) return

        do i = 1, self%num_dofs_per_node
            call self%data(i)%zero()
        end do
    end subroutine zero_residual_vector

    subroutine destroy_residual_vector(self)
        implicit none
        class(type_residual_vector), intent(inout) :: self

        integer(int32) :: i

        if (allocated(self%data)) then
            do i = 1, self%num_dofs_per_node
                call self%data(i)%destroy()
            end do
            deallocate (self%data)
        end if

        self%size = 0
        self%num_dofs_per_node = 0

    end subroutine destroy_residual_vector

    subroutine display_residual_vector(self)
        implicit none
        class(type_residual_vector), intent(in) :: self

        integer(int32) :: i

        write (*, '(A)') '--- Residual Vector ---'
        write (*, '(A, I0)') 'Size (total DOFs): ', self%size
        write (*, '(A, I0)') 'Number of DOFs per Node: ', self%num_dofs_per_node
        write (*, '(A)') '-----------------------'

        if (.not. allocated(self%data)) then
            write (*, '(A)') '  [Not allocated]'
            write (*, '(A)') '-----------------------'
            return
        end if

        do i = 1, self%num_dofs_per_node
            write (*, '(A, I0, A)') 'Block (DOF ', i, '):'
            if (self%data(i)%is_initialized()) then
                call self%data(i)%display()
            else
                write (*, '(A)') '  [Not allocated]'
            end if
        end do
        write (*, '(A)') '-----------------------'
    end subroutine display_residual_vector

end module field_residual_vector
