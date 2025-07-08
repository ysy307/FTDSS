module matrix_adjacency
    use, intrinsic :: iso_fortran_env, only: int32, logical32
    use :: Domain_Module, only:Domain_t
    use :: core_core, only:allocate_array
    implicit none
    private
    public :: type_adjacency

    type :: type_adjacency
        private
        logical, allocatable :: matrix(:, :)
        integer(int32), allocatable :: degree(:)
        integer(int32) :: num_elements
    contains
        procedure, pass(self), public :: initialize => adjacency_initialize
        procedure, pass(self), public :: check => check_adjacent
        procedure, pass(self), public :: get => get_degree
        procedure, pass(self), public :: get_num_elements => get_num_elements
        procedure, pass(self), public :: destroy => adjacency_destroy
    end type type_adjacency

contains

    !================================================================!
    ! 修正版: build_adjacency
    ! 隣接判定と次数計算を1つのループに統合
    !================================================================!
    subroutine adjacency_initialize(self, Domain)
        implicit none
        class(type_adjacency), intent(inout) :: self
        type(Domain_t), intent(in) :: Domain
        integer(int32) :: i, j

        self%num_elements = Domain%get_numElement()

        call allocate_array(self%matrix, self%num_elements, self%num_elements)
        call allocate_array(self%degree, self%num_elements)

        self%matrix(:, :) = .false.
        self%degree(:) = 0

        do i = 1, self%num_elements
            do j = i + 1, self%num_elements
                if (share_node(Domain, i, j)) then
                    self%matrix(i, j) = .true.
                    self%matrix(j, i) = .true.

                    self%degree(i) = self%degree(i) + 1
                    self%degree(j) = self%degree(j) + 1
                end if
            end do
        end do

    end subroutine adjacency_initialize

    !================================================================!
    ! ヘルパー関数 (変更なし)
    !================================================================!
    function share_node(Domain, a, b) result(shared)
        type(Domain_t), intent(in) :: Domain
        integer(int32), intent(in) :: a, b
        logical :: shared

        integer(int32) :: ie
        integer(int32) :: nsize

        shared = .false.

        nsize = Domain%Elements(a)%e%get_size()
        do ie = 1, nsize
            if (any(Domain%Elements(b)%e%conn(:) == Domain%Elements(a)%e%conn(ie))) then
                shared = .true.
                return
            end if
        end do

    end function share_node

    !================================================================!
    ! 照会用の関数 (変更なし)
    !================================================================!
    function check_adjacent(self, i, j) result(is_adjacent)
        implicit none
        class(type_adjacency), intent(in) :: self
        integer(int32), intent(in) :: i, j
        logical :: is_adjacent

        if (i < 1 .or. i > self%num_elements .or. j < 1 .or. j > self%num_elements) then
            is_adjacent = .false.
        else
            is_adjacent = self%matrix(i, j)
        end if
    end function check_adjacent

    function get_degree(self, i) result(degree)
        implicit none
        class(type_adjacency), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32) :: degree
        if (i < 1 .or. i > self%num_elements) then
            degree = 0
        else
            degree = self%degree(i)
        end if
    end function get_degree

    function get_num_elements(self) result(num_elements)
        implicit none
        class(type_adjacency), intent(in) :: self
        integer(int32) :: num_elements

        num_elements = self%num_elements
    end function get_num_elements

    subroutine adjacency_destroy(self)
        implicit none
        class(type_adjacency), intent(inout) :: self

        if (allocated(self%matrix)) deallocate (self%matrix)
        if (allocated(self%degree)) deallocate (self%degree)

        self%num_elements = 0
    end subroutine adjacency_destroy

end module matrix_adjacency
