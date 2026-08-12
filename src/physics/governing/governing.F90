module module_governing
    use :: physics_governing_base, only: &
        type_assemble_workspace, &
        HYDRAULIC_DRIVER_TOTAL_POTENTIAL
    use :: physics_governing_thermal, only: &
        type_thermal
    use :: physics_governing_hydraulic, only: &
        type_hydraulic, &
        CAPACITY_REGULARIZATION_ENABLED
    use :: governing_atmosphere
    use :: module_boundary
    use :: module_initial, only: &
        abst_ic, &
        type_ic_uniform, &
        holder_ics, &
        type_ic_manager
    implicit none
    public

end module module_governing
