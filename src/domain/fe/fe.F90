module module_fe
    use :: domain_fe, only:abst_fe
    use :: module_fe_side, only:type_side_first, type_side_second
    use :: module_fe_element, only:type_triangle_first, type_triangle_second, &
        type_square_first, type_square_second
    use :: domain_fe_factory, only:create_fe
    implicit none
    private

    !-------------------------------------------------------------------------------------------------------------------------------
    ! derived types
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: abst_fe
    public :: type_side_first
    public :: type_side_second
    public :: type_triangle_first
    public :: type_triangle_second
    public :: type_square_first
    public :: type_square_second

    !-------------------------------------------------------------------------------------------------------------------------------
    ! operation procedures
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: create_fe
    !-------------------------------------------------------------------------------------------------------------------------------

end module module_fe
