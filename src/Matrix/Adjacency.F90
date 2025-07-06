module Matrix_Adjacency
    use, intrinsic :: iso_fortran_env, only: int32, logical32
    use :: Domain_Module, only:Domain_t
    use :: Core_Allocate, only:Allocate_Array
    implicit none
    private
    ! public :: Adjacency_t, is_adjacent, get_degree
    public :: Adjacency_t

    type :: Adjacency_t
        private
        logical(logical32), allocatable :: Matrix(:, :)
        integer(int32), allocatable :: degree(:)
        integer(int32) :: numElements
    contains
        procedure, pass(self), public :: initialize => adjacency_initialize
        procedure, pass(self), public :: check => check_adjacent
        procedure, pass(self), public :: get => get_degree
        procedure, pass(self), public :: get_numElements => get_numElements
        procedure, pass(self), public :: destroy => adjacency_destroy
    end type Adjacency_t

contains

    !================================================================!
    ! 修正版: build_adjacency
    ! 隣接判定と次数計算を1つのループに統合
    !================================================================!
    subroutine adjacency_initialize(self, Domain)
        implicit none
        class(Adjacency_t), intent(inout) :: self
        type(Domain_t), intent(in) :: Domain
        integer(int32) :: i, j

        self%numElements = Domain%get_numElement()

        call Allocate_Array(self%Matrix, self%numElements, self%numElements)
        call Allocate_Array(self%degree, self%numElements)

        self%Matrix(:, :) = .false.
        self%degree(:) = 0

        do i = 1, self%numElements
            do j = i + 1, self%numElements
                if (share_node(Domain, i, j)) then
                    self%Matrix(i, j) = .true.
                    self%Matrix(j, i) = .true.

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
        logical(logical32) :: shared

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
        class(Adjacency_t), intent(in) :: self
        integer(int32), intent(in) :: i, j
        logical(logical32) :: is_adjacent

        if (i < 1 .or. i > self%numElements .or. j < 1 .or. j > self%numElements) then
            is_adjacent = .false.
        else
            is_adjacent = self%Matrix(i, j)
        end if
    end function check_adjacent

    function get_degree(self, i) result(degree)
        implicit none
        class(Adjacency_t), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32) :: degree
        if (i < 1 .or. i > self%numElements) then
            degree = 0
        else
            degree = self%degree(i)
        end if
    end function get_degree

    function get_numElements(self) result(numElements)
        implicit none
        class(Adjacency_t), intent(in) :: self
        integer(int32) :: numElements

        numElements = self%numElements
    end function get_numElements

    subroutine adjacency_destroy(self)
        implicit none
        class(Adjacency_t), intent(inout) :: self

        if (allocated(self%Matrix)) deallocate (self%Matrix)
        if (allocated(self%degree)) deallocate (self%degree)

        self%numElements = 0
    end subroutine adjacency_destroy

end module Matrix_Adjacency
