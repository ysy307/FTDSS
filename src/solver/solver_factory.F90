module solver_solver_factory
    use :: module_input
    use :: module_core, only:type_crs
    use :: solver_solve
    implicit none

    public :: create_solver

    interface create_solver
        module procedure :: create_solver_crs
    end interface

contains
    function create_solver_crs(input, target_solver, target_matrix, num_node) result(solver)
        implicit none
        type(type_input), intent(in) :: input
        class(abst_solver), allocatable :: solver
        character(*), intent(in) :: target_solver
        type(type_crs), intent(in) :: target_matrix
        integer(int32), intent(in) :: num_node

        if (allocated(solver)) deallocate (solver)

        select case (trim(adjustl(target_solver)))
        case ('thermal')
            select case (input%basic%solver_settings%linear_solver%thermal%method)
            case ('direct')
                solver = type_solver_sparse_crs_lu(N=num_node, &
                                                   MAXFCT=1, &
                                                   MNUM=1, &
                                                   MTYPE=1, &
                                                   PHASE=13, &
                                                   NRHS=1, &
                                                   MSGVLV=0, &
                                                   a=target_matrix)
            case ('iterative')
                select case (input%basic%solver_settings%linear_solver%thermal%iterative_solver%solver_type)
                case (4)
                    solver = type_solver_sparse_crs_bicgstab( &
                             size=num_node, &
                             tolerance=input%basic%solver_settings%linear_solver%thermal%iterative_solver%tolerance, &
                             max_iterations=input%basic%solver_settings%linear_solver%thermal%iterative_solver%max_iterations, &
                             preconditioner=input%basic%solver_settings%linear_solver%thermal%iterative_solver%preconditioner_type)
                end select
            end select
        case ('hydraulic')
            select case (input%basic%solver_settings%linear_solver%hydraulic%method)
            case ('direct')
                solver = type_solver_sparse_crs_lu(N=num_node, &
                                                   MAXFCT=1, &
                                                   MNUM=1, &
                                                   MTYPE=1, &
                                                   PHASE=13, &
                                                   NRHS=1, &
                                                   MSGVLV=0, &
                                                   a=target_matrix)
            case ('iterative')
                select case (input%basic%solver_settings%linear_solver%hydraulic%iterative_solver%solver_type)
                case (4)
                    solver = type_solver_sparse_crs_bicgstab( &
                             size=num_node, &
                             tolerance=input%basic%solver_settings%linear_solver%hydraulic%iterative_solver%tolerance, &
                             max_iterations=input%basic%solver_settings%linear_solver%hydraulic%iterative_solver%max_iterations, &
                             preconditioner=input%basic%solver_settings%linear_solver%hydraulic%iterative_solver%preconditioner_type)
                end select
            end select
        end select

    end function create_solver_crs
end module solver_solver_factory
