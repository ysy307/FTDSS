module domain_multicoloring
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_sorting, only:sort_index
    use :: module_core, only:allocate_array, deallocate_array
    use :: domain_adjacency_adjacency_element, only:type_crs_adjacency_element

    implicit none
    private

    public :: type_coloring
    public :: type_colored_info

    ! 各色に属する要素の情報を格納する型
    type :: type_colored_info
        integer(int32) :: num_elements = 0
        integer(int32), allocatable :: elements(:)
    end type type_colored_info

    ! 着色結果全体を管理する型
    type :: type_coloring
        character(:), allocatable :: algorithm_name ! 使用するアルゴリズム名
        integer(int32) :: num_colors = 0
        integer(int32), allocatable :: color(:) ! 各要素の色
        type(type_colored_info), allocatable :: colored(:) ! 色ごとの要素リスト
    contains
        procedure, pass(self) :: initialize => initialize_type_coloring
        procedure, pass(self) :: coloring_welsh_powell
        procedure, pass(self) :: coloring_dsatur
        procedure, pass(self) :: coloring_lfo
        procedure, pass(self) :: populate => populate_coloring_result
    end type type_coloring

    interface
        module subroutine coloring_welsh_powell(self, graph)
            implicit none
            class(type_coloring), intent(inout) :: self
            class(type_crs_adjacency_element), intent(in) :: graph
        end subroutine coloring_welsh_powell

        module subroutine coloring_dsatur(self, graph)
            implicit none
            class(type_coloring), intent(inout) :: self
            type(type_crs_adjacency_element), intent(in) :: graph
        end subroutine coloring_dsatur

        module subroutine coloring_lfo(self, graph)
            implicit none
            class(type_coloring), intent(inout) :: self
            type(type_crs_adjacency_element), intent(in) :: graph
        end subroutine coloring_lfo
    end interface

contains

    !================================================================!
    !【初期化メソッド】アルゴリズム名に応じて処理を分岐
    !================================================================!
    subroutine initialize_type_coloring(self, algorithm_name, adjacency)
        implicit none
        class(type_coloring), intent(inout) :: self
        class(type_crs_adjacency_element), intent(in) :: adjacency
        character(*), intent(in) :: algorithm_name

        select case (trim(adjustl(algorithm_name)))
        case ("welsh-powell")
            self%algorithm_name = "Welsh-Powell"
            call self%coloring_welsh_powell(adjacency)
        case ("dsatur") ! DSATURのケースを追加
            self%algorithm_name = "DSATUR"
            call self%coloring_dsatur(adjacency)
        case ("lfo")
            self%algorithm_name = "Largest First Order"
            call self%coloring_lfo(adjacency) ! LFOアルゴリズムの実行
        end select
    end subroutine initialize_type_coloring

    subroutine populate_coloring_result(self)
        implicit none
        class(type_coloring), intent(inout) :: self
        integer(int32) :: i, j, counts, num_nodes

        num_nodes = size(self%color)
        if (num_nodes == 0) then
            self%num_colors = 0
            return
        end if

        self%num_colors = maxval(self%color)
        if (self%num_colors == 0) return

        if (allocated(self%colored)) deallocate (self%colored)
        allocate (self%colored(self%num_colors))

        do i = 1, self%num_colors
            counts = count(self%color == i)
            self%colored(i)%num_elements = counts
            if (counts > 0) then
                allocate (self%colored(i)%elements(counts))
                self%colored(i)%elements = pack([(j, j=1, num_nodes)], self%color == i)
            end if
        end do
    end subroutine populate_coloring_result

end module domain_multicoloring
