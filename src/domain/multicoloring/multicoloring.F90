!> Module for handling multicoloring of domain elements for parallel processing.
module domain_multicoloring
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_sorting, only:sort_index
    use :: module_core, only:allocate_array, deallocate_array
    use :: module_input, only:type_input
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
        integer(int32) :: num_colors = 0
        !> An array holding the data for each color group.
        type(type_colored_info), allocatable :: colored(:)
    contains
        procedure, pass(self) :: initialize => initialize_type_coloring
        procedure, pass(self) :: destroy => destroy_type_coloring
    end type type_coloring

contains

    !> Initializes the coloring structure from the input data.
    !> This routine determines the number of colors, counts elements per color,
    !> allocates memory, and populates the element lists for each color.
    subroutine initialize_type_coloring(self, input)
        implicit none
        !> The coloring object to be initialized.
        class(type_coloring), intent(inout) :: self
        !> The main input data structure containing geometry and coloring info.
        class(type_input), intent(in) :: input

        integer(int32) :: i, c
        integer(int32) :: cell_color
        integer(int32) :: domain_element_id
        integer(int32) :: comp_dim

        integer(int32), allocatable :: counts_per_color(:)
        integer(int32), allocatable :: current_indices(:)

        comp_dim = input%basic%simulation_settings%calculate_dimension

        ! ==========================================================
        ! Pass 1: Determine number of colors and count elements per color
        ! ==========================================================
        self%num_colors = 0
        do i = 1, input%geometry%vtk%num_total_cells
            if (input%geometry%vtk%cells(i)%get_dimension() == comp_dim) then
                self%num_colors = max(self%num_colors, input%geometry%vtk%cells(i)%color)
            end if
        end do
        if (self%num_colors == 0) return

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
        ! Allocation of arrays for each color group
        ! ==========================================================
        allocate (self%colored(self%num_colors))
        do c = 1, self%num_colors
            self%colored(c)%num_elements = counts_per_color(c)
            if (self%colored(c)%num_elements > 0) then
                allocate (self%colored(c)%elements(self%colored(c)%num_elements))
            end if
        end do
        call deallocate_array(counts_per_color)

        ! ==========================================================
        ! Pass 2: Fill element indices into their respective color groups
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
        call deallocate_array(current_indices)

    end subroutine initialize_type_coloring

    !> Deallocates all memory associated with the coloring object.
    !> This routine safely releases the element arrays for each color and the
    !> main array of color groups.
    subroutine destroy_type_coloring(self)
        implicit none
        !> The coloring object to be destroyed.
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
