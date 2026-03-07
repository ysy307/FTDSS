module boundary_strategy_factory
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
    use :: boundary_strategy
    implicit none
    private

    public :: create_bc_strategy

contains

    function create_bc_strategy(config) result(bc)
        implicit none
        type(type_config_bc), intent(in) :: config
        class(abst_bc), allocatable :: bc

        type(type_constant_id) :: physics_type

        select case (config%bc_kind%ID)
        case (THERMAL_BC_TYPES%DIRICHLET%ID)
            allocate (type_bc_dirichlet :: bc)
            physics_type = PHYSICS_TYPES%THERMAL
        case (HYDRAULIC_BC_TYPES%DIRICHLET%ID)
            allocate (type_bc_dirichlet :: bc)
            physics_type = PHYSICS_TYPES%HYDRAULIC
        case (THERMAL_BC_TYPES%FLUX%ID)
            allocate (type_bc_flux :: bc)
            physics_type = PHYSICS_TYPES%THERMAL
        case (HYDRAULIC_BC_TYPES%FLUX%ID)
            allocate (type_bc_flux :: bc)
            physics_type = PHYSICS_TYPES%HYDRAULIC
        case (THERMAL_BC_TYPES%ROBIN%ID)
            allocate (type_bc_robin :: bc)
            physics_type = PHYSICS_TYPES%THERMAL
        case (HYDRAULIC_BC_TYPES%ROBIN%ID)
            allocate (type_bc_robin :: bc)
            physics_type = PHYSICS_TYPES%HYDRAULIC
        case (THERMAL_BC_TYPES%ATMOSPHERIC%ID)
            allocate (type_bc_atmospheric :: bc)
            physics_type = PHYSICS_TYPES%THERMAL
        case (HYDRAULIC_BC_TYPES%ATMOSPHERIC%ID)
            allocate (type_bc_atmospheric :: bc)
            physics_type = PHYSICS_TYPES%HYDRAULIC
        case (THERMAL_BC_TYPES%RADIATION%ID)
            allocate (type_bc_radiation :: bc)
            physics_type = PHYSICS_TYPES%THERMAL
        case (THERMAL_BC_TYPES%CONVECTIVE%ID)
            allocate (type_bc_convective :: bc)
            physics_type = PHYSICS_TYPES%THERMAL
        case (HYDRAULIC_BC_TYPES%SEEPAGE%ID)
            allocate (type_bc_seepage :: bc)
            physics_type = PHYSICS_TYPES%HYDRAULIC
        end select

        call bc%initialize(physics_type, config)
    end function create_bc_strategy

end module boundary_strategy_factory
