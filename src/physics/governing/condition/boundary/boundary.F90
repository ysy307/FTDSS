module module_boundary
    use :: conditions_boundary_manager, only: type_bc_manager
    use :: condition_boundary_strategy, only: type_bc_result
    implicit none
    private

    public :: type_bc_manager
    public :: type_bc_result
end module module_boundary