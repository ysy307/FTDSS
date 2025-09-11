module field_jacobian_matrix
    use, intrinsic :: iso_fortran_env
    use :: module_core
    implicit none
    private

    public :: type_jacobian_matrix

    type :: type_jacobian_matrix
        private
        integer(int32) :: size = 0
        integer(int32) :: matrix_type = -1
        class(abst_matrix), allocatable :: data
    contains
        ! --- initialize/destroy ---
        procedure, public, pass(self) :: initialize => initialize_jacobian_matrix
        procedure, public, pass(self) :: destroy => destroy_jacobian_matrix

        ! --- getter ---
        procedure, public, pass(self) :: get_size => get_size_jacobian_matrix
        procedure, public, pass(self) :: get_matrix_type => get_matrix_type_jacobian_matrix
        procedure, public, pass(self) :: get_val_rank1 => get_val_rank1_jacobian_matrix
        procedure, public, pass(self) :: get_val_rank2 => get_val_rank2_jacobian_matrix
        procedure, public, pass(self) :: get_matrix => get_matrix_jacobian_matrix

        ! --- setter ---
        procedure, private, pass(self) :: set_value => set_value_jacobian_matrix
        procedure, private, pass(self) :: set_row => set_row_jacobian_matrix
        procedure, private, pass(self) :: set_local => set_local_jacobian_matrix
        generic, public :: set => set_value, set_row, set_local

        ! --- operation ---
        procedure, private, pass(self) :: add_value => add_value_jacobian_matrix
        procedure, private, pass(self) :: add_local => add_local_jacobian_matrix
        generic, public :: add => add_value, add_local
        procedure, public, pass(self) :: zero => zero_jacobian_matrix
    end type type_jacobian_matrix

contains

    subroutine initialize_jacobian_matrix(self, matrix_type, num_dof, row, col)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: matrix_type
        integer(int32), intent(in) :: num_dof
        integer(int32), intent(in) :: row(:)
        integer(int32), intent(in) :: col(:)

        self%size = num_dof
        select case (matrix_type)
        case (matrix_dense)
            self%matrix_type = matrix_dense
            allocate (type_dense :: self%data)
        case (matrix_crs)
            self%matrix_type = matrix_crs
            allocate (type_crs :: self%data)
        case (matrix_coo)
            self%matrix_type = matrix_coo
            allocate (type_coo :: self%data)
        end select
        call self%data%initialize(num_dof, row, col)

    end subroutine initialize_jacobian_matrix

    pure function get_size_jacobian_matrix(self) result(size)
        implicit none
        class(type_jacobian_matrix), intent(in) :: self
        integer(int32) :: size

        size = self%size

    end function get_size_jacobian_matrix

    pure function get_matrix_type_jacobian_matrix(self) result(matrix_type)
        implicit none
        class(type_jacobian_matrix), intent(in) :: self
        integer(int32) :: matrix_type

        matrix_type = self%matrix_type

    end function get_matrix_type_jacobian_matrix

    function get_row_jacobian_matrix(self) result(row)
        implicit none
        class(type_jacobian_matrix), intent(in), target :: self
        integer(int32), dimension(:), pointer :: row

        select type (matrix => self%data)
        type is (type_crs)
            row => matrix%get_ptr()
        type is (type_coo)
            row => matrix%get_row()
        end select

    end function get_row_jacobian_matrix

    function get_col_jacobian_matrix(self) result(col)
        implicit none
        class(type_jacobian_matrix), intent(in), target :: self
        integer(int32), dimension(:), pointer :: col

        select type (matrix => self%data)
        type is (type_crs)
            col => matrix%get_ind()
        type is (type_coo)
            col => matrix%get_col()
        end select

    end function get_col_jacobian_matrix

    function get_val_rank1_jacobian_matrix(self) result(val)
        implicit none
        class(type_jacobian_matrix), intent(in), target :: self
        real(real64), dimension(:), pointer :: val

        select type (matrix => self%data)
        type is (type_crs)
            val => matrix%get_val()
        type is (type_coo)
            val => matrix%get_val()
        end select

    end function get_val_rank1_jacobian_matrix

    function get_val_rank2_jacobian_matrix(self) result(val)
        implicit none
        class(type_jacobian_matrix), intent(in), target :: self
        real(real64), dimension(:, :), pointer :: val

        select type (matrix => self%data)
        type is (type_dense)
            val => matrix%get_val()
        end select

    end function get_val_rank2_jacobian_matrix

    function get_matrix_jacobian_matrix(self) result(matrix)
        implicit none
        class(type_jacobian_matrix), intent(in), target :: self
        class(abst_matrix), pointer :: matrix

        matrix => self%data
    end function get_matrix_jacobian_matrix

    subroutine set_value_jacobian_matrix(self, row, col, value)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row
        integer(int32), intent(in) :: col
        real(real64), intent(in) :: value

        call self%data%set(row, col, value)

    end subroutine set_value_jacobian_matrix

    subroutine set_row_jacobian_matrix(self, row, value)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row
        real(real64), intent(in) :: value

        call self%data%set(row, value)

    end subroutine set_row_jacobian_matrix

    subroutine set_local_jacobian_matrix(self, connectivity, local_data)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: connectivity(:)
        type(type_dense), intent(in) :: local_data

        integer(int32) :: i, j
        integer(int32) :: num_nodes

        num_nodes = size(connectivity)

        do i = 1, num_nodes
            do j = 1, num_nodes
                call self%data%set(connectivity(i), connectivity(j), local_data%val(i, j))
            end do
        end do

    end subroutine set_local_jacobian_matrix

    subroutine add_value_jacobian_matrix(self, row, col, value)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: row
        integer(int32), intent(in) :: col
        real(real64), intent(in) :: value

        call self%data%add(row, col, value)

    end subroutine add_value_jacobian_matrix

    subroutine add_local_jacobian_matrix(self, connectivity, local_data)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self
        integer(int32), intent(in) :: connectivity(:)
        type(type_dense), intent(in) :: local_data

        integer(int32) :: i, j
        integer(int32) :: num_nodes

        num_nodes = size(connectivity)

        do i = 1, num_nodes
            do j = 1, num_nodes
                call self%data%add(connectivity(i), connectivity(j), local_data%val(i, j))
            end do
        end do

    end subroutine add_local_jacobian_matrix

    subroutine zero_jacobian_matrix(self)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self

        call self%data%zero()
    end subroutine zero_jacobian_matrix

    subroutine destroy_jacobian_matrix(self)
        implicit none
        class(type_jacobian_matrix), intent(inout) :: self

        call self%data%destroy()
        self%size = 0
        self%matrix_type = -1
    end subroutine destroy_jacobian_matrix

end module field_jacobian_matrix
