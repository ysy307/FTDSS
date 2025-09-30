module module_boundary
    use :: conditions_boundary, only:abst_bc
    use :: conditions_boundary_manager, only:create_boundary_conditions
    implicit none
    private

    public :: abst_bc
    public :: create_boundary_conditions

end module module_boundary
