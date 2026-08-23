!> Unit test: numerical system layer (Jacobian matrix / residual vector)
!> built directly from a hand-assembled type_system_topology, i.e. WITHOUT
!> constructing a type_domain. This is the regression guard for the
!> domain-decoupling refactor: the system layer must depend only on the
!> lightweight topology carrier, never on the heavyweight domain God object.
!>
!> Fixture mesh: a 1-D chain of 3 nodes / 2 two-node linear elements.
!>   nodes:    1 --- 2 --- 3
!>   elements: e1 = (1,2), e2 = (2,3)
!> CSR node adjacency (including self) for that chain:
!>   row 1: {1,2}  row 2: {1,2,3}  row 3: {2,3}
program test_field
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit, error_unit
    use :: testdrive, only: error_type, check
    use :: module_core
    use :: module_system
    use :: core_types_topology_system_topology, only: type_system_topology
    implicit none

    integer(int32) :: total_failures

    total_failures = 0

    call run_all_tests()

    if (total_failures > 0) then
        write (error_unit, '(A, I0, A)') "FAILED: ", total_failures, " test(s) failed."
        error stop 1
    end if

    write (output_unit, '(A)') "All system (domain-free) tests passed."

contains

    subroutine run_all_tests()
        type(error_type), allocatable :: error
        integer(int32) :: local_failures

        local_failures = 0

        call test_monolithic_sizes(error)
        call report_result("test_monolithic_sizes", error, local_failures)

        call test_staggered_sizes(error)
        call report_result("test_staggered_sizes", error, local_failures)

        call test_residual_assembly(error)
        call report_result("test_residual_assembly", error, local_failures)

        total_failures = total_failures + local_failures
    end subroutine run_all_tests

    subroutine report_result(test_name, error, failure_count)
        character(len=*), intent(in) :: test_name
        type(error_type), allocatable, intent(inout) :: error
        integer(int32), intent(inout) :: failure_count

        if (allocated(error)) then
            write (output_unit, '(A, A)') "  [FAIL] ", test_name
            failure_count = failure_count + 1
            deallocate (error)
        else
            write (output_unit, '(A, A)') "  [PASS] ", test_name
        end if
    end subroutine report_result

    !> Build the 3-node / 2-element chain topology with the requested number of
    !> active physics (1 -> thermal only, 2 -> thermal + hydraulic).
    subroutine build_fixture_topology(topology, num_active_physics)
        type(type_system_topology), intent(inout) :: topology
        integer(int32), intent(in) :: num_active_physics

        integer(int32), parameter :: num_nodes = 3
        integer(int32), parameter :: num_fe = 2
        integer(int32) :: num_dofs_of_physics(PHYSICS_TYPES%NUM_ID)
        integer(int32) :: start_dof_index(PHYSICS_TYPES%NUM_ID)
        integer(int32) :: adj_row(num_nodes + 1)
        integer(int32) :: adj_col(7)
        integer(int32) :: conn_ptr(num_fe + 1)
        integer(int32) :: conn_idx(4)
        integer(int32) :: total_dofs, num_dof_per_node

        ! CSR node adjacency: rows {1,2},{1,2,3},{2,3}
        adj_row = [1, 3, 6, 8]
        adj_col = [1, 2, 1, 2, 3, 2, 3]

        ! CSR element connectivity: e1=(1,2), e2=(2,3)
        conn_ptr = [1, 3, 5]
        conn_idx = [1, 2, 2, 3]

        num_dofs_of_physics = 0
        start_dof_index = 0
        num_dofs_of_physics(PHYSICS_TYPES%THERMAL%ID) = 1
        start_dof_index(PHYSICS_TYPES%THERMAL%ID) = 1
        if (num_active_physics >= 2) then
            num_dofs_of_physics(PHYSICS_TYPES%HYDRAULIC%ID) = 1
            start_dof_index(PHYSICS_TYPES%HYDRAULIC%ID) = 2
        end if

        num_dof_per_node = sum(num_dofs_of_physics)
        total_dofs = num_nodes * num_dof_per_node

        call topology%initialize(num_nodes, total_dofs, num_dof_per_node, &
                                 num_dofs_of_physics, start_dof_index, adj_row, adj_col, &
                                 num_fe, conn_ptr, conn_idx)
    end subroutine build_fixture_topology

    ! ============================================================
    ! Test: monolithic coupling — single coupled system of all DOFs
    ! ============================================================
    subroutine test_monolithic_sizes(error)
        type(error_type), allocatable :: error
        type(type_system_topology) :: topology
        type(type_jacobian_matrix) :: K
        type(type_residual_vector) :: F
        integer(int32) :: jac_size, ndof

        call build_fixture_topology(topology, 2)

        call K%initialize(topology, COUPLING_MODES%MONOLITHIC)
        call F%initialize(topology, COUPLING_MODES%MONOLITHIC)

        ! 3 nodes * (thermal + hydraulic) = 6 DOFs total.
        call K%get_size(jac_size)
        call check(error, jac_size, 6)
        if (allocated(error)) return

        call K%get_num_dofs_per_node(ndof)
        call check(error, ndof, 2)
        if (allocated(error)) return

        call check(error, F%get_size(), 6)
        if (allocated(error)) return

        call K%destroy()
        call F%destroy()
        call topology%destroy()
    end subroutine test_monolithic_sizes

    ! ============================================================
    ! Test: staggered coupling — one diagonal system per active physics
    ! ============================================================
    subroutine test_staggered_sizes(error)
        type(error_type), allocatable :: error
        type(type_system_topology) :: topology
        type(type_jacobian_matrix) :: K
        type(type_residual_vector) :: F

        call build_fixture_topology(topology, 2)

        call K%initialize(topology, COUPLING_MODES%STAGGERED)
        call F%initialize(topology, COUPLING_MODES%STAGGERED)

        ! In staggered mode num_dofs_per_node collapses to 0 (per-physics blocks).
        call check(error, F%get_num_dofs_per_node(), 0)
        if (allocated(error)) return

        ! Total DOFs is still the full system size (6).
        call check(error, F%get_size(), 6)
        if (allocated(error)) return

        call K%destroy()
        call F%destroy()
        call topology%destroy()
    end subroutine test_staggered_sizes

    ! ============================================================
    ! Test: residual scatter/get round-trips through the blocked vector
    ! ============================================================
    subroutine test_residual_assembly(error)
        type(error_type), allocatable :: error
        type(type_system_topology) :: topology
        type(type_residual_vector) :: F
        real(real64), pointer, dimension(:) :: data_ptr

        call build_fixture_topology(topology, 1)

        call F%initialize(topology, COUPLING_MODES%MONOLITHIC)
        call F%zero()

        ! Add a known value at thermal node 2 and read it back.
        call F%add(PHYSICS_TYPES%THERMAL%ID, 2, 3.5d0)

        data_ptr => F%get_data(PHYSICS_TYPES%THERMAL%ID)
        call check(error, associated(data_ptr))
        if (allocated(error)) return

        call check(error, data_ptr(2), 3.5d0)
        if (allocated(error)) return

        call F%destroy()
        call topology%destroy()
    end subroutine test_residual_assembly

end program test_field
