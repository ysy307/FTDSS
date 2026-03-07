module boundary_data_provider_factory
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
    use :: boundary_data_provider
    implicit none
    private

    public :: create_bc_data_provider

contains

    function create_bc_data_provider(config) result(provider)
        implicit none
        type(type_config_bc), intent(in) :: config
        class(abst_bc_data), allocatable :: provider

        select case (config%bc_data_kind%ID)
        case (BC_DATA_PROVIDERS%CONSTANT%ID)
            allocate (type_bc_data_constant :: provider)
        case (BC_DATA_PROVIDERS%DYNAMIC%ID)
            allocate (type_bc_data_dynamic :: provider)
        case (BC_DATA_PROVIDERS%TABLE%ID)
            allocate (type_bc_data_table :: provider)
        end select

        call provider%initialize(config)
    end function create_bc_data_provider

end module boundary_data_provider_factory
