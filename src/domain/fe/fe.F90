!>
!> Module for finite element definitions and management
!>
module module_fe
    use :: domain_base_fe, only:abst_fe, holder_fes
    use :: domain_fe_side, only:type_side_first, type_side_second
    use :: domain_fe_element, only:type_triangle_first, type_triangle_second, &
        type_square_first, type_square_second
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
    public :: type_square_first
    public :: type_square_second

    !-------------------------------------------------------------------------------------------------------------------------------
    ! operation procedures
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: create_fe

    !-------------------------------------------------------------------------------------------------------------------------------
    ! manager for finite elements
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: type_fe_manager

end module module_fe
