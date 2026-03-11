module types_config_domain
    use :: types_config_elements, only: &
        type_config_elements, &
        type_config_multicoloring, &
        type_config_colored_elements
    use :: types_config_nodes, only: &
        type_config_nodes
    implicit none
    private

    public :: type_config_elements
    public :: type_config_multicoloring
    public :: type_config_colored_elements

    public :: type_config_nodes

end module types_config_domain
