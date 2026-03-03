module module_governing
    use :: governing_base, only: &
        type_assemble_workspace
    use :: governing_thermal, only: &
        type_thermal
    use :: governing_hydraulic, only: &
        type_hydraulic
    use :: governing_atmosphere
    use :: module_boundary, only: &
        abst_bc, &
        type_bc_dirichlet, &
        type_bc_neumann, &
        type_bc_robin, &
        type_bc_zero_flux, &
        create_boundary_conditions
    use :: module_initial, only: &
        abst_ic, &
        type_ic_uniform, &
        holder_ics, &
        type_ic_manager
    implicit none
    public

end module module_governing
