module module_boundary
    use :: physics_governing_boundary_manager, only: type_bc_manager
    use :: boundary_strategy, only: type_bc_result
    implicit none
    private

    public :: type_bc_manager
    public :: type_bc_result
end module module_boundary