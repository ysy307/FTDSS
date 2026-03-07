!> Module for handling multicoloring of domain elements for parallel processing.
module domain_multicoloring
    use, intrinsic :: iso_fortran_env
    use :: module_core
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
    subroutine initialize_type_coloring(self, config)
        implicit none
        class(type_coloring), intent(inout) :: self
        type(type_config_multicoloring), intent(in) :: config

        integer(int32) :: c

        self%num_colors = config%num_colors

        if (self%num_colors > 0) then
            allocate (self%colored(self%num_colors))
            do c = 1, self%num_colors
                self%colored(c)%num_elements = config%colored(c)%num_elements
                if (self%colored(c)%num_elements > 0) then
                    allocate (self%colored(c)%elements(self%colored(c)%num_elements))
                    self%colored(c)%elements = config%colored(c)%elements
                end if
            end do
        end if

    end subroutine initialize_type_coloring

    !> Deallocates all memory associated with the coloring object.
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

    !> Returns the total number of colors used in the domain.
    subroutine get_num_colors_coloring(self, num_colors)
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
        integer(int32), intent(inout) :: num_elements
        integer(int32), pointer, contiguous, intent(inout) :: elements(:)

        ! 範囲チェック
        if (.not. value_in_range(color_id, 1, self%num_colors)) then
            num_elements = 0
            nullify (elements)
            return
        end if

        num_elements = self%colored(color_id)%num_elements

        if (num_elements > 0) then
            elements => self%colored(color_id)%elements
        else
            nullify (elements)
        end if

    end subroutine get_colored_elements_coloring

end module domain_multicoloring
