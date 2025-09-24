module module_fe_element
    use :: domain_fe_element
    implicit none
    private

    !-------------------------------------------------------------------------------------------------------------------------------
    ! element types
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: type_triangle_first
    public :: type_triangle_second
    public :: type_square_first
    public :: type_square_second

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Constructors
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: construct_triangle_first
    public :: construct_square_first
    public :: construct_triangle_second
    public :: construct_square_second

end module module_fe_element
