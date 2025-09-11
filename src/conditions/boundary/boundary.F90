module module_boundary
    use :: conditions_boundary, only:abst_bc, mode_value, mode_nr, mode_ic
    use :: conditions_boundary_manager, only:type_bc, holder_bcs
    implicit none
    private

    public :: type_bc
    public :: holder_bcs

    public :: mode_value, mode_nr, mode_ic

end module module_boundary
