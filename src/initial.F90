module module_initial
    use :: condition_initial, only: &
        abst_ic, &
        type_ic_uniform, &
        type_ic_from_file, &
        holder_ics
    use :: condition_initial_manager, &
        only:type_ic_manager
    implicit none
    public

end module module_initial
