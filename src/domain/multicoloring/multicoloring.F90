module domain_multicoloring
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_sorting, only:sort_index
    use :: module_core, only:allocate_array, deallocate_array
    use :: module_input, only:type_input

    implicit none
    private

    public :: type_coloring

    type :: type_colored_info
        integer(int32) :: num_elements = 0
        integer(int32), allocatable :: elements(:)
    end type type_colored_info

    type :: type_coloring
        integer(int32) :: num_colors = 0
        type(type_colored_info), allocatable :: colored(:) ! 色ごとの要素リスト
    contains
        procedure, pass(self) :: initialize => initialize_type_coloring
        procedure, pass(self) :: destroy => destroy_type_coloring
    end type type_coloring

contains

    subroutine initialize_type_coloring(self, input)
        implicit none
        class(type_coloring), intent(inout) :: self
        class(type_input), intent(in) :: input

        integer(int32) :: i, c
        integer(int32) :: cell_color
        integer(int32) :: domain_element_id
        integer(int32) :: comp_dim

        integer(int32), allocatable :: counts_per_color(:) ! 各色の要素数を数える一時配列
        integer(int32), allocatable :: current_indices(:) ! 各色のリストに次に格納する場所を指すカウンタ

        comp_dim = input%basic%simulation_settings%calculate_dimension

        ! ==========================================================
        ! パス1：計測 (Sizing Pass)
        ! ==========================================================

        ! 1a. 色の最大値と、各色の要素数を同時に数える
        self%num_colors = 0
        do i = 1, input%geometry%vtk%num_total_cells
            if (input%geometry%vtk%cells(i)%get_dimension() == comp_dim) then
                self%num_colors = max(self%num_colors, input%geometry%vtk%cells(i)%color)
            end if
        end do
        if (self%num_colors == 0) return

        ! 1b. 各色の要素数を数える
        allocate (counts_per_color(self%num_colors))
        counts_per_color = 0
        do i = 1, input%geometry%vtk%num_total_cells
            if (input%geometry%vtk%cells(i)%get_dimension() == comp_dim) then
                cell_color = input%geometry%vtk%cells(i)%color
                if (cell_color > 0) then
                    counts_per_color(cell_color) = counts_per_color(cell_color) + 1
                end if
            end if
        end do

        ! ==========================================================
        ! メモリ確保 (Allocation)
        ! ==========================================================
        allocate (self%colored(self%num_colors))
        do c = 1, self%num_colors
            self%colored(c)%num_elements = counts_per_color(c)
            if (self%colored(c)%num_elements > 0) then
                allocate (self%colored(c)%elements(self%colored(c)%num_elements))
            end if
        end do
        deallocate (counts_per_color)

        ! ==========================================================
        ! パス2：格納 (Filling Pass)
        ! ==========================================================
        allocate (current_indices(self%num_colors))
        current_indices = 0
        domain_element_id = 0
        do i = 1, input%geometry%vtk%num_total_cells
            if (input%geometry%vtk%cells(i)%get_dimension() == comp_dim) then
                domain_element_id = domain_element_id + 1
                cell_color = input%geometry%vtk%cells(i)%color
                if (cell_color > 0) then
                    current_indices(cell_color) = current_indices(cell_color) + 1
                    self%colored(cell_color)%elements(current_indices(cell_color)) = domain_element_id
                end if
            end if
        end do
        deallocate (current_indices)

    end subroutine initialize_type_coloring

    subroutine destroy_type_coloring(self)
        implicit none
        class(type_coloring), intent(inout) :: self

        integer(int32) :: i

        if (allocated(self%colored)) then
            do i = 1, self%num_colors
                if (allocated(self%colored(i)%elements)) then
                    deallocate (self%colored(i)%elements)
                end if
            end do
            deallocate (self%colored)
        end if

        self%num_colors = 0

    end subroutine destroy_type_coloring

end module domain_multicoloring
