submodule(domain_multicoloring) multicoloring_dsatur
contains

! ==============================================================================
! DSATURアルゴリズムを用いてグラフを彩色するメインサブルーチン
! (修正・最適化版)
! ==============================================================================
    module subroutine coloring_dsatur(self, graph)
        implicit none
        class(type_coloring), intent(inout) :: self
        type(type_crs_adjacency_element), intent(in) :: graph

        integer(int32) :: num_elements
        integer(int32) :: i, j, k, next_node, current_color, max_saturation, max_degree
        integer(int32), allocatable :: degrees(:), saturation(:), neighbor_colors(:), neighbors(:)
        logical, allocatable :: is_colored(:)

        num_elements = graph%get_num_elements()

        if (allocated(self%color)) call deallocate_array(self%color)
        call allocate_array(self%color, length=num_elements)
        self%color = 0
        call allocate_array(degrees, length=num_elements)
        call allocate_array(saturation, length=num_elements)
        call allocate_array(is_colored, length=num_elements)
        is_colored(:) = .false.
        saturation(:) = 0

        do i = 1, num_elements
            degrees(i) = graph%get_degree(i)
        end do

        ! --- 彩色ループ (全頂点を彩色するまで) ---
        do i = 1, num_elements
            ! --- 次に彩色する頂点を選択 ---
            max_saturation = -1
            max_degree = -1
            next_node = -1
            do j = 1, num_elements
                if (.not. is_colored(j)) then
                    ! 修正点2：条件判定をより効率的に
                    if (saturation(j) > max_saturation) then
                        max_saturation = saturation(j)
                        max_degree = degrees(j)
                        next_node = j
                    else if (saturation(j) == max_saturation .and. degrees(j) > max_degree) then
                        max_degree = degrees(j)
                        next_node = j
                    end if
                end if
            end do

            ! --- 選択した頂点に割り当てる色を決定 ---
            neighbors = graph%get_neighbors(next_node)
            if (size(neighbors) > 0) then
                allocate (neighbor_colors(size(neighbors)))
                k = 0
                do j = 1, size(neighbors)
                    if (self%color(neighbors(j)) /= 0) then
                        k = k + 1
                        neighbor_colors(k) = self%color(neighbors(j))
                    end if
                end do
            else
                k = 0 ! 隣接なしの場合
            end if

            current_color = 1
            if (k > 0) then
                do
                    if (count(neighbor_colors(1:k) == current_color) == 0) then
                        exit ! この色に決定
                    end if
                    current_color = current_color + 1
                end do
            end if
            if (allocated(neighbor_colors)) call deallocate_array(neighbor_colors)

            ! --- 彩色と状態の更新 ---
            self%color(next_node) = current_color
            is_colored(next_node) = .true.

            do j = 1, size(neighbors)
                call update_saturation(saturation(neighbors(j)), neighbors(j), graph, self)
            end do

            call deallocate_array(neighbors)
        end do

        call self%populate()

        call deallocate_array(is_colored)
        call deallocate_array(saturation)
        call deallocate_array(is_colored)

    end subroutine coloring_dsatur

! ==============================================================================
! 特定の頂点の飽和度を再計算するヘルパーサブルーチン
! ==============================================================================
    subroutine update_saturation(current_saturation, node_id, graph, coloring)
        integer(int32), intent(out) :: current_saturation
        integer(int32), intent(in) :: node_id
        type(type_crs_adjacency_element), intent(in) :: graph
        type(type_coloring), intent(in) :: coloring

        integer(int32), allocatable :: neighbors(:)
        integer(int32), allocatable :: distinct_colors(:)
        integer(int32) :: i, j, num_distinct
        logical :: found

        neighbors = graph%get_neighbors(node_id)
        if (size(neighbors) == 0) then
            current_saturation = 0
            call deallocate_array(neighbors)
            return
        end if

        allocate (distinct_colors(size(neighbors)))
        num_distinct = 0

        do i = 1, size(neighbors)
            if (coloring%color(neighbors(i)) /= 0) then
                found = .false.
                do j = 1, num_distinct
                    if (coloring%color(neighbors(i)) == distinct_colors(j)) then
                        found = .true.
                        exit
                    end if
                end do
                if (.not. found) then
                    num_distinct = num_distinct + 1
                    distinct_colors(num_distinct) = coloring%color(neighbors(i))
                end if
            end if
        end do

        current_saturation = num_distinct
        call deallocate_array(neighbors)
        call deallocate_array(distinct_colors)

    end subroutine update_saturation
end submodule multicoloring_dsatur
