submodule(solver_solve) solver_solver_factory
    implicit none

contains
    module function create_solver(input, target_solver, target_matrix, num_node) result(solver)
        implicit none
        type(type_input), intent(in) :: input
        character(*), intent(in) :: target_solver
        type(type_jacobian_matrix), intent(in), target :: target_matrix
        integer(int32), intent(in) :: num_node
        class(abst_solver), allocatable :: solver

        real(real64) :: tolerance
        integer(int32) :: max_iterations
        integer(int32) :: preconditioner
        integer(int32) :: matrix_type

        select case (input%basic%solver_settings%linear_solver%thermal%method)
        case ('iterative')
            select case (trim(adjustl(target_solver)))
            case ("thermal")
                tolerance = input%basic%solver_settings%linear_solver%thermal%iterative_solver%tolerance
                max_iterations = input%basic%solver_settings%linear_solver%thermal%iterative_solver%max_iterations
                preconditioner = input%basic%solver_settings%linear_solver%thermal%iterative_solver%preconditioner_type
            case ("hydraulic")
                tolerance = input%basic%solver_settings%linear_solver%hydraulic%iterative_solver%tolerance
                max_iterations = input%basic%solver_settings%linear_solver%hydraulic%iterative_solver%max_iterations
                preconditioner = input%basic%solver_settings%linear_solver%hydraulic%iterative_solver%preconditioner_type
            end select
        end select

        matrix_type = target_matrix%get_matrix_type()
        select case (input%basic%solver_settings%linear_solver%thermal%method)
        case ('direct')
            select case (matrix_type)
            case (matrix_crs)
                solver = construct_type_solver_sparse_crs_lu(A=target_matrix, &
                                                             MAXFCT=1, &
                                                             MNUM=1, &
                                                             MTYPE=1, &
                                                             PHASE=13, &
                                                             NRHS=1, &
                                                             MSGVLV=0)
            case (matrix_dense)
                solver = type_solver_dense_lu(A=target_matrix)
            end select
        case ('iterative')
            select case (input%basic%solver_settings%linear_solver%thermal%iterative_solver%solver_type)
            case (4)
                solver = construct_type_solver_bicgstab(A=target_matrix, &
                                                        tolerance=tolerance, &
                                                        max_iterations=max_iterations, &
                                                        preconditioner=preconditioner)
            end select
        end select

    end function create_solver
end submodule solver_solver_factory
