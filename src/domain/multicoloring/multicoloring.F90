!> Module for handling multicoloring of domain elements for parallel processing.
module domain_multicoloring
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only: allocate_array, deallocate_array
    use :: module_input, only: type_input
    implicit none
    private

    public :: type_coloring

    !> Contains information for a single color group.
    type :: type_colored_info
        !> Number of elements belonging to this color.
        integer(int32) :: num_elements = 0
        !> List of 1-based domain element indices in this color group.
        integer(int32), allocatable :: elements(:)
    end type type_colored_info

    !> Stores the grouping of all domain elements by color.
    type :: type_coloring
        !> Total number of colors used in the domain.
        integer(int32), private :: num_colors = 0
        !> An array holding the data for each color group.
        type(type_colored_info), allocatable :: colored(:)
    contains
        procedure, public, pass(self) :: initialize => initialize_type_coloring
        procedure, public, pass(self) :: destroy => destroy_type_coloring
        procedure, public, pass(self) :: get_num_colors => get_num_colors_coloring
        procedure, public, pass(self) :: get_colored_elements => get_colored_elements_coloring
    end type type_coloring

contains

    !> Initializes the coloring structure from the input data.
    subroutine initialize_type_coloring(self, input)
        implicit none
        class(type_coloring), intent(inout) :: self
        class(type_input), intent(in) :: input

        integer(int32) :: i, c
        integer(int32) :: cell_color
        integer(int32) :: domain_element_id
        integer(int32) :: comp_dim
        integer(int32), allocatable :: counts_per_color(:)
        integer(int32), allocatable :: current_indices(:)

        ! 計算次元の取得
        comp_dim = input%basic%simulation_settings%calculate_dimension

        ! ==========================================================
        ! Pass 1: 最大カラー数の特定 (num_colors)
        ! ==========================================================
        self%num_colors = 0
        do i = 1, input%geometry%vtk%num_total_cells
            if (input%geometry%vtk%cells(i)%get_dimension() == comp_dim) then
                self%num_colors = max(self%num_colors, input%geometry%vtk%cells(i)%color)
            end if
        end do

        if (self%num_colors == 0) return

        ! ==========================================================
        ! Pass 2: 各カラーごとの要素数カウント
        ! ==========================================================
        call allocate_array(counts_per_color, self%num_colors)
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
        ! Pass 3: メモリ確保
        ! ==========================================================
        if (allocated(self%colored)) deallocate(self%colored)
        allocate(self%colored(self%num_colors))

        do c = 1, self%num_colors
            self%colored(c)%num_elements = counts_per_color(c)
            ! 要素がある場合のみ allocate する (安全策)
            if (self%colored(c)%num_elements > 0) then
                allocate(self%colored(c)%elements(self%colored(c)%num_elements))
            end if
        end do
        
        call deallocate_array(counts_per_color)

        ! ==========================================================
        ! Pass 4: 要素IDの格納
        ! ==========================================================
        call allocate_array(current_indices, self%num_colors)
        current_indices = 0
        domain_element_id = 0

        do i = 1, input%geometry%vtk%num_total_cells
            ! この条件分岐は domain_element_manager のID付与ロジックと完全に一致している必要がある
            if (input%geometry%vtk%cells(i)%get_dimension() == comp_dim) then
                
                ! ドメイン要素IDは、カラーに関わらずインクリメントされる
                domain_element_id = domain_element_id + 1
                
                cell_color = input%geometry%vtk%cells(i)%color
                if (cell_color > 0) then
                    current_indices(cell_color) = current_indices(cell_color) + 1
                    self%colored(cell_color)%elements(current_indices(cell_color)) = domain_element_id
                end if
            end if
        end do
        
        call deallocate_array(current_indices)

    end subroutine initialize_type_coloring

    !> Deallocates all memory associated with the coloring object.
    subroutine destroy_type_coloring(self)
        implicit none
        class(type_coloring), intent(inout) :: self
        integer(int32) :: i

        if (allocated(self%colored)) then
            do i = 1, self%num_colors
                if (allocated(self%colored(i)%elements)) then
                    deallocate(self%colored(i)%elements)
                end if
            end do
            deallocate(self%colored)
        end if
        self%num_colors = 0
    end subroutine destroy_type_coloring

    !> Returns the total number of colors used in the domain.
    pure subroutine get_num_colors_coloring(self, num_colors)
        implicit none
        class(type_coloring), intent(in) :: self
        integer(int32), intent(inout) :: num_colors
        num_colors = self%num_colors
    end subroutine get_num_colors_coloring

    !> Retrieves the list of domain element indices for a specified color.
    subroutine get_colored_elements_coloring(self, color_id, num_elements, elements)
        implicit none
        class(type_coloring), intent(in), target :: self
        integer(int32), intent(in) :: color_id
        
        ! 出力引数: 要素数
        integer(int32), intent(inout) :: num_elements
        
        ! 出力引数: 要素リストへのポインタ
        ! contiguous を明示して最適化を促進
        integer(int32), pointer, contiguous, intent(inout) :: elements(:)

        ! 範囲チェック
        if (color_id < 1 .or. color_id > self%num_colors) then
            num_elements = 0
            nullify(elements)
            return
        end if

        num_elements = self%colored(color_id)%num_elements
        
        if (num_elements > 0) then
            elements => self%colored(color_id)%elements
        else
            nullify(elements)
        end if
        
    end subroutine get_colored_elements_coloring

end module domain_multicoloring