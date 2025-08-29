module module_properties
    use :: properties_material_manager, only:type_material_manager
    use :: properties_properties_manager, only:type_properties_manager, type_phase_property
    implicit none
    private

    public :: type_material_manager
    public :: type_properties_manager
    public :: type_phase_property

end module module_properties
