!>
!> @brief Module for handling multicoloring of domain elements
!>
module domain_multicoloring
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_sorting, only:sort_index
    use :: module_core, only:allocate_array, deallocate_array
    use :: module_input, only:type_input
    implicit none
    private

    public :: type_coloring

    !>
    !> @brief Information for a single color group
    !>
    type :: type_colored_info
        !>
        !> @brief Number of elements in this color
        !>
        integer(int32) :: num_elements = 0
        !>
        !> @brief List of element indices in this color
        !>
        integer(int32), allocatable :: elements(:)
    end type type_colored_info

    !>
    !> @brief Stores element grouping by colors
    !>
    type :: type_coloring
        !>
        !> @brief Total number of colors
        !>
        integer(int32) :: num_colors = 0
        !>
        !> @brief Array of colored info, one per color
        !>
        type(type_colored_info), allocatable :: colored(:)
    contains
        procedure, pass(self) :: initialize => initialize_type_coloring
        procedure, pass(self) :: destroy => destroy_type_coloring
    end type type_coloring

contains

    !>
    !> @brief Initialize the coloring object
    !> @param[inout] self  Coloring object to initialize
    !> @param[in] input    Input data containing domain geometry and coloring
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

        comp_dim = input%basic%simulation_settings%calculate_dimension

        ! Pass 1: Determine number of colors and count elements
        self%num_colors = 0
        do i = 1, input%geometry%vtk%num_total_cells
            if (input%geometry%vtk%cells(i)%get_dimension() == comp_dim) then
                self%num_colors = max(self%num_colors, input%geometry%vtk%cells(i)%color)
            end if
        end do
        if (self%num_colors == 0) return

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

        ! Allocation of arrays per color
        allocate (self%colored(self%num_colors))
        do c = 1, self%num_colors
            self%colored(c)%num_elements = counts_per_color(c)
            if (self%colored(c)%num_elements > 0) then
                allocate (self%colored(c)%elements(self%colored(c)%num_elements))
            end if
        end do
        deallocate (counts_per_color)

        ! Pass 2: Fill element indices for each color
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

    !>
    !> @brief Destroy the coloring object and deallocate arrays
    !> @param[inout] self  Coloring object to destroy
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
