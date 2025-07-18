submodule(domain_multicoloring) multicoloring_lfo
contains
! ==============================================================================
! LFO (Largest First Ordering) アルゴリズムを実行し、彩色結果をselfに格納する
! ==============================================================================
    module subroutine coloring_lfo(self, graph)
        implicit none
        class(type_coloring), intent(inout) :: self
        type(type_crs_adjacency_element), intent(in) :: graph

        integer(int32) :: num_elements
        integer(int32) :: i, j, k, current_node, current_color
        integer(int32), allocatable :: degrees(:), sorted_indices(:)
        integer(int32), allocatable :: neighbor_colors(:), neighbors(:)

        ! --- 1. 初期化 ---
        num_elements = graph%get_num_elements()
        if (allocated(self%color)) call deallocate_array(self%color)
        call allocate_array(self%color, length=num_elements)
        self%color = 0

        call allocate_array(degrees, length=num_elements)
        call allocate_array(sorted_indices, length=num_elements)

        ! --- 2. 次数の計算と順序付け ---
        do i = 1, num_elements
            degrees(i) = graph%get_degree(i)
            sorted_indices(i) = i ! 初期インデックスを格納
        end do

        ! 次数に基づいてインデックスを降順ソート (単純なバブルソートで実装)
        do i = 1, num_elements - 1
            do j = i + 1, num_elements
                if (degrees(sorted_indices(i)) < degrees(sorted_indices(j))) then
                    k = sorted_indices(i)
                    sorted_indices(i) = sorted_indices(j)
                    sorted_indices(j) = k
                end if
            end do
        end do

        deallocate (degrees)

        ! --- 3. ソートされた順序で彩色 ---
        do i = 1, num_elements
            current_node = sorted_indices(i)

            ! 利用可能な最小の色を決定
            neighbors = graph%get_neighbors(current_node)
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
                k = 0
            end if

            current_color = 1
            if (k > 0) then
                do
                    if (count(neighbor_colors(1:k) == current_color) == 0) exit
                    current_color = current_color + 1
                end do
            end if
            if (allocated(neighbor_colors)) deallocate (neighbor_colors)

            deallocate (neighbors)

            ! 彩色
            self%color(current_node) = current_color
        end do

        call self%populate()

        deallocate (sorted_indices)

        ! --- 4. 結果の集計 ---

    end subroutine coloring_lfo

end submodule multicoloring_lfo
