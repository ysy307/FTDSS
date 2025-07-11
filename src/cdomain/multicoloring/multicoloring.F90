! file: domain_multicoloring.f90
module domain_multicoloring
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_sorting, only:sort_index
    use :: module_core, only:allocate_array, deallocate_array
    ! 最終版の隣接行列モジュールをuseする
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
        integer(int32) :: num_colors = 0
        integer(int32), allocatable :: color(:) ! 各要素の色
        type(type_colored_info), allocatable :: colored(:) ! 色ごとの要素リスト
    contains
        procedure, pass(self) :: initialize => initialize_coloring
    end type type_coloring

contains

    !================================================================!
    !【初期化メソッド】アルゴリズム名に応じて処理を分岐
    !================================================================!
    subroutine initialize_coloring(self, adjacency, algorithm_name)
        implicit none
        class(type_coloring), intent(inout) :: self
        class(type_crs_adjacency_element), intent(in) :: adjacency
        character(len=*), intent(in), optional :: algorithm_name

        character(len=30) :: selected_algorithm

        if (present(algorithm_name)) then
            selected_algorithm = trim(adjustl(algorithm_name))
        else
            selected_algorithm = "welsh-powell" ! 指定がない場合のデフォルト
        end if

        select case (selected_algorithm)
        case ("welsh-powell")
            call execute_welsh_powell(self, adjacency)
            ! case ("lfo")
            !     ! call execute_lfo(self, adjacency) ! 将来の拡張用
        case default
            print *, "Error: Unknown coloring algorithm specified:", selected_algorithm
            error stop 1
        end select
    end subroutine initialize_coloring

    !================================================================!
    !【アルゴリズム実装】Welsh-Powell法 (プライベート)
    !================================================================!
    subroutine execute_welsh_powell(self, adjacency)
        implicit none
        class(type_coloring), intent(inout) :: self
        class(type_crs_adjacency_element), intent(in) :: adjacency

        integer(int32) :: num_elements, i, v
        integer(int32), allocatable :: perm(:)

        num_elements = adjacency%get_num_elements()

        if (allocated(self%color)) call deallocate_array(self%color)
        call allocate_array(self%color, length=num_elements)
        self%color = 0

        ! ステップ1: 次数降順の処理順序(perm)を取得
        call get_welsh_powell_order(adjacency, perm)

        ! ステップ2: ソートされた順序で要素を着色
        do i = 1, num_elements
            v = perm(i)
            self%color(v) = find_smallest_available_color(v, adjacency, self%color)
        end do

        ! ステップ3: 結果を構造体に整理
        call populate_coloring_result(self)

        call deallocate_array(perm)
    end subroutine execute_welsh_powell

    !================================================================!
    !【ヘルパー関数群】(プライベート)
    !================================================================!

    ! 次数降順の処理順序(perm)を取得する
    subroutine get_welsh_powell_order(adjacency, perm)
        implicit none
        class(type_crs_adjacency_element), intent(in) :: adjacency
        integer(int32), allocatable, intent(inout) :: perm(:)

        integer(int32) :: num_elements, i, temp
        integer(int32), allocatable :: degrees(:)
        integer(int32), allocatable :: local_perm(:)

        num_elements = adjacency%get_num_elements()
        call allocate_array(degrees, length=num_elements)
        call allocate_array(local_perm, length=num_elements)

        do i = 1, num_elements
            degrees(i) = adjacency%get_degree(i)
        end do

        ! stdlibのsort_indexで昇順ソートのインデックスを取得
        call sort_index(array=degrees, index=local_perm)

        ! 得られたインデックス配列を手動で逆順にし、降順のインデックスを得る
        do i = 1, num_elements / 2
            temp = local_perm(i)
            local_perm(i) = local_perm(num_elements - i + 1)
            local_perm(num_elements - i + 1) = temp
        end do

        perm = local_perm

        call deallocate_array(degrees)
        call deallocate_array(local_perm)

    end subroutine get_welsh_powell_order

    ! 指定ノードに割り当て可能な最小の色を見つける (最適化版)
    function find_smallest_available_color(v, adjacency, colors) result(color_id)
        implicit none
        integer(int32) :: color_id
        integer(int32), intent(in) :: v
        class(type_crs_adjacency_element), intent(in) :: adjacency
        integer(int32), intent(in) :: colors(:)

        logical, allocatable :: forbidden_colors(:)
        integer(int32) :: neighbor_color, max_possible_colors, i
        integer(int32), allocatable :: neighbors(:)
        integer(int32) :: neighbor_id

        max_possible_colors = adjacency%get_degree(v) + 1

        call allocate_array(forbidden_colors, length=max_possible_colors)
        forbidden_colors = .false.

        ! ゲッターで隣接要素リストを取得し、使用済み色をマーク
        neighbors = adjacency%get_neighbors(v)
        do i = 1, size(neighbors)
            neighbor_id = neighbors(i)
            neighbor_color = colors(neighbor_id)
            if (neighbor_color > 0 .and. neighbor_color <= max_possible_colors) then
                forbidden_colors(neighbor_color) = .true.
            end if
        end do
        call deallocate_array(neighbors)

        ! マークされていない最小の色を見つける
        color_id = 1
        do while (color_id <= max_possible_colors)
            if (.not. forbidden_colors(color_id)) then
                exit
            end if
            color_id = color_id + 1
        end do

        call deallocate_array(forbidden_colors)
    end function find_smallest_available_color

    ! 計算結果をtype_coloring構造体に整理して格納する
    subroutine populate_coloring_result(self)
        implicit none
        class(type_coloring), intent(inout) :: self
        integer(int32) :: i, j, counts, num_elements

        num_elements = size(self%color)
        if (num_elements > 0) then
            self%num_colors = maxval(self%color)
        else
            self%num_colors = 0
            return
        end if

        if (self%num_colors == 0) return

        if (allocated(self%colored)) deallocate (self%colored)
        allocate (self%colored(self%num_colors))

        do i = 1, self%num_colors
            counts = count(self%color == i)
            call allocate_array(self%colored(i)%elements, length=counts)
            self%colored(i)%num_elements = counts
            if (counts > 0) then
                self%colored(i)%elements = pack([(j, j=1, num_elements)], self%color == i)
            end if
        end do
    end subroutine populate_coloring_result

end module domain_multicoloring
