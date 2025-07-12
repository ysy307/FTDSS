submodule(domain_multicoloring) multicoloring_welch_powell

contains
    !================================================================!
    !【アルゴリズム実装】Welsh-Powell法 (プライベート)
    !================================================================!
    module subroutine coloring_welsh_powell(self, graph)
        implicit none
        class(type_coloring), intent(inout) :: self
        class(type_crs_adjacency_element), intent(in) :: graph

        integer(int32) :: num_elements, i, v
        integer(int32), allocatable :: perm(:)

        num_elements = graph%get_num_elements()

        if (allocated(self%color)) call deallocate_array(self%color)
        call allocate_array(self%color, length=num_elements)
        self%color = 0

        ! ステップ1: 次数降順の処理順序(perm)を取得
        call get_welsh_powell_order(graph, perm)

        ! ステップ2: ソートされた順序で要素を着色
        do i = 1, num_elements
            v = perm(i)
            self%color(v) = find_smallest_available_color(v, graph, self%color)
        end do

        ! ステップ3: 結果を構造体に整理
        call self%populate()

        call deallocate_array(perm)
    end subroutine coloring_welsh_powell

    !================================================================!
    !【ヘルパー関数群】(プライベート)
    !================================================================!

    ! 次数降順の処理順序(perm)を取得する
    subroutine get_welsh_powell_order(graph, perm)
        implicit none
        class(type_crs_adjacency_element), intent(in) :: graph
        integer(int32), allocatable, intent(inout) :: perm(:)

        integer(int32) :: num_elements, i, temp
        integer(int32), allocatable :: degrees(:)
        integer(int32), allocatable :: local_perm(:)

        num_elements = graph%get_num_elements()
        call allocate_array(degrees, length=num_elements)
        call allocate_array(local_perm, length=num_elements)

        do i = 1, num_elements
            degrees(i) = graph%get_degree(i)
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
    function find_smallest_available_color(v, graph, colors) result(color_id)
        implicit none
        integer(int32) :: color_id
        integer(int32), intent(in) :: v
        class(type_crs_adjacency_element), intent(in) :: graph
        integer(int32), intent(in) :: colors(:)

        logical, allocatable :: forbidden_colors(:)
        integer(int32) :: neighbor_color, max_possible_colors, i
        integer(int32), allocatable :: neighbors(:)
        integer(int32) :: neighbor_id

        max_possible_colors = graph%get_degree(v) + 1

        call allocate_array(forbidden_colors, length=max_possible_colors)
        forbidden_colors = .false.

        ! ゲッターで隣接要素リストを取得し、使用済み色をマーク
        neighbors = graph%get_neighbors(v)
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

end submodule multicoloring_welch_powell

