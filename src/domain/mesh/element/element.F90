module module_element
    use :: domain_element, only:abst_element, type_triangle_first, type_triangle_second, type_square_first, type_square_second, & !&
                                holder_elements
    use :: domain_element_factory, only:create_element
    implicit none
    private

    !-------------------------------------------------------------------------------------------------------------------------------
    ! element types
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: abst_element
    public :: type_triangle_first
    public :: type_triangle_second
    public :: type_square_first
    public :: type_square_second
    public :: holder_elements
    !-------------------------------------------------------------------------------------------------------------------------------
    ! element operation procedures
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: create_element
    !-------------------------------------------------------------------------------------------------------------------------------

end module module_element
