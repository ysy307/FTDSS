!>
!> Module for finite element definitions and management
!>
module module_fe
    use :: domain_base_fe, only:abst_fe, holder_fes
    use :: domain_fe_side, only:type_side_first, type_side_second
    use :: domain_fe_element, only:type_triangle_first, type_triangle_second, &
        type_triangle_third, type_square_first, type_square_second, type_square_second_lagrange, &
        type_square_third, type_square_third_serendipity
    use :: domain_fe_volume, only:type_tetra_first, type_tetra_second, type_tetra_third, &
        type_hexa_first, type_hexa_second, type_hexa_second_serendipity, &
        type_hexa_third, type_hexa_third_serendipity
    use :: domain_fe_factory, only:create_fe
    use :: domain_fe_manager, only:type_fe_manager
    implicit none
    private

    !-------------------------------------------------------------------------------------------------------------------------------
    ! derived types
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: abst_fe
    public :: holder_fes
    public :: type_side_first
    public :: type_side_second
    public :: type_triangle_first
    public :: type_triangle_second
    public :: type_triangle_third
    public :: type_square_first
    public :: type_square_second
    public :: type_square_second_lagrange
    public :: type_square_third
    public :: type_square_third_serendipity
    public :: type_tetra_first
    public :: type_tetra_second
    public :: type_tetra_third
    public :: type_hexa_first
    public :: type_hexa_second
    public :: type_hexa_second_serendipity
    public :: type_hexa_third
    public :: type_hexa_third_serendipity

    !-------------------------------------------------------------------------------------------------------------------------------
    ! operation procedures
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: create_fe

    !-------------------------------------------------------------------------------------------------------------------------------
    ! manager for finite elements
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: type_fe_manager

end module module_fe
