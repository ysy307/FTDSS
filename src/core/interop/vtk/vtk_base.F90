submodule(core_vtk) core_vtk_base
    implicit none

contains
    module function type_vtk_cell_get_dimension(self) result(dimension)
        !> Get the dimension of the cell
        implicit none
        class(type_vtk_cell), intent(in) :: self !! VTK cell data
        integer(int32) :: dimension
        dimension = self%cell_dimension
    end function type_vtk_cell_get_dimension

    module function type_vtk_cell_get_order(self) result(order)
        !> Get the order of the cell
        implicit none
        class(type_vtk_cell), intent(in) :: self !! VTK cell data
        integer(int32) :: order
        order = self%cell_order
    end function type_vtk_cell_get_order

    module function type_vtk_cell_get_size_connectivity(self) result(size)
        implicit none
        class(type_vtk_cell), intent(in) :: self
        integer(int32) :: size

        size = self%num_nodes_in_cell
    end function type_vtk_cell_get_size_connectivity

    module subroutine type_vtk_cell_set(self, num_nodes_in_cell)
        implicit none
        class(type_vtk_cell), intent(inout) :: self !! VTK cells data
        integer(int32), intent(in) :: num_nodes_in_cell !! セルのノード数

        call vtk_constants%get_cell_info_from_cell_type( &
            self%cell_type, self%cell_type_name, self%num_nodes_in_cell, &
            self%cell_dimension, self%cell_order)

        if (self%num_nodes_in_cell < 0) then
            self%num_nodes_in_cell = num_nodes_in_cell
        end if

    end subroutine type_vtk_cell_set

    module subroutine get_active_region_info(self, unique_ids, target_dim)
        implicit none
        class(Type_VTK), intent(in) :: self !! VTK data
        integer(int32), allocatable, intent(inout) :: unique_ids(:)
        integer(int32), intent(in), optional :: target_dim

        ! --- ローカル変数 ---
        integer(int32) :: i_cell, count, dim
        integer(int32), allocatable :: collected_ids(:)
        integer(int32) :: max_dim_local

        max_dim_local = 0
        count = 0

        ! --- ステップ1: 収集する次元を決定 ---
        if (present(target_dim)) then
            max_dim_local = target_dim
        else
            ! 指定がなければ自プロセス内で最大次元を判定
            do i_cell = 1, self%num_total_cells
                max_dim_local = max(max_dim_local, self%CELLS(i_cell)%get_dimension())
            end do
            if (max_dim_local <= 0) then
                allocate (unique_ids(0))
                return
            end if
        end if

        ! --- ステップ2: 指定次元のCellEntityIdを収集 ---
        allocate (collected_ids(self%num_total_cells))
        do i_cell = 1, self%num_total_cells
            dim = self%CELLS(i_cell)%get_dimension()
            if (dim == max_dim_local) then
                count = count + 1
                collected_ids(count) = self%CELLS(i_cell)%cell_entity_id
            end if
        end do

        ! --- ステップ3: ユニークなIDのみ抽出 ---
        if (count > 0) then
            call unique(collected_ids(1:count), unique_ids)
        else
            allocate (unique_ids(0))
        end if

        deallocate (collected_ids)
    end subroutine get_active_region_info

end submodule core_vtk_base

