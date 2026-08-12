!> Unit tests for domain_fe_subcell: interface-split subcell quadrature.
!>
!> The rule under test replaces the standard Gauss rule on every element that
!> carries cryo transport (governing_base: the split is built for all elements,
!> not only sign-mixed ones), and the quantities actually assembled on it are
!> the diffusion matrix \(\int D \nabla N_i \cdot \nabla N_j\) and the capacity
!> matrix \(\int C N_i N_j\), with \(D\) and \(C\) evaluated per point so that
!> they jump across the freezing interface.  The tests are therefore organised
!> around what the rule must guarantee for those integrals, not around the
!> subdivision geometry alone.
!>
!> Group 1 - subdivision geometry (must hold for every family, order, depth):
!>   1a tiling invariants: sum(weights) = reference measure, positive weights,
!>      points inside the reference element.
!>   1b degree-2 exactness over the whole reference element, for any cut.
!> Group 2 - interface resolution:
!>   2a a straight interface is exact, hence depth-independent.
!>   2b a curved interface converges as O(4^-d) once the cell resolves it,
!>      i.e. from depth ceil(log2(h_e / l_f)).
!>   2c the per-point side tag agrees with the sign of phi at that point,
!>      except in the sub-resolution band, whose weight vanishes with depth.
!> Group 3 - assembled element matrices (the reason the module exists):
!>   3a on an UNCUT element the rule must reproduce the Gauss result; where the
!>      integrand is degree <= 2 that is exact at any depth, otherwise the
!>      difference must fall as O(4^-d).  This measures what the rule costs on
!>      elements far from the interface.
!>   3b on a CUT element with side-wise coefficients the matrices converge as
!>      O(4^-d) to the deep-refinement reference.
!> Group 4 - continuity in the nodal level set (required for a contracting
!>   Newton map): sweeping the interface must not make the assembled matrices
!>   jump when the refinement pattern changes.
!> Group 5 - contract: capacity bound and sufficiency, overflow and unsupported
!>   families report zero, degenerate level sets stay well defined.
!> Group 6 - regression: fixed analytic values on T3/Q4 at depth 0, including
!>   the side-wise flux residual.
!> Group 7 - chemical-potential helpers used with the same level set.
program test_fe_subcell
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit, error_unit
    use :: testdrive, only: error_type, check
    use :: module_core, only: FE_TYPE, type_coordinate_dp
    use :: domain_base_fe, only: abst_fe
    use :: domain_fe_factory, only: create_fe
    use :: domain_fe_subcell, only: type_subcell_quadrature_point, SUBCELL_QUADRATURE_CAPACITY, &
                                    SUBCELL_MAX_DEPTH, build_interface_quadrature_points, &
                                    calc_subcell_quadrature_capacity
    use :: models_phase_change_chemical_potential, only: calc_psi_ice, calc_dpsi_ice_dT, &
                                                         calc_T_high_celsius
    implicit none

    !> Level-set kinds used by the family sweeps.
    integer(int32), parameter :: LEVEL_SET_LINEAR = 1
    integer(int32), parameter :: LEVEL_SET_CIRCLE = 2
    !> Deepest refinement exercised by the sweeps; 4**3 quad cells * 18 = 1152.
    integer(int32), parameter :: TEST_MAX_DEPTH = 3
    !> Depth of the self-reference used where no analytic value exists.
    integer(int32), parameter :: TEST_REFERENCE_DEPTH = 5
    !> Local output capacity, larger than the depth-3 worst case.
    integer(int32), parameter :: TEST_QUADRATURE_CAPACITY = 2048
    !> Nodes of the largest supported element (Q9).
    integer(int32), parameter :: MAX_NODES = 9
    !> Integration order whose Gauss rule integrates every element integrand
    !> here exactly: 7 points on a triangle, 5 x 5 on a quadrilateral.
    integer(int32), parameter :: EXACT_GAUSS_ORDER = 5
    real(real64), parameter :: PI = 3.141592653589793238d0
    !> Ratio a mesh-size-squared error must beat between consecutive depths.
    !> The theoretical value is 1/4; 0.35 leaves room for the pre-asymptotic
    !> constant without admitting a first-order rate.
    real(real64), parameter :: CONVERGENCE_RATIO = 0.35d0

    integer(int32) :: total_failures

    total_failures = 0
    call run_all_tests(total_failures)

    if (total_failures > 0) then
        write (error_unit, '(A,I0,A)') "FAILED: ", total_failures, " test(s) failed."
        error stop 1
    end if
    write (output_unit, '(A)') "All fe_subcell tests passed."

contains

    subroutine run_all_tests(failures)
        integer(int32), intent(inout) :: failures

        type(error_type), allocatable :: error

        write (output_unit, '(A)') "Group 1: subdivision geometry"
        call test_tiling_invariants(error)
        call report("1a tiling_invariants", error, failures)
        call test_degree2_exactness(error)
        call report("1b degree2_exactness", error, failures)

        write (output_unit, '(A)') "Group 2: interface resolution"
        call test_straight_interface_is_exact(error)
        call report("2a straight_interface_is_exact", error, failures)
        call test_curved_interface_convergence(error)
        call report("2b curved_interface_convergence", error, failures)
        call test_side_tag_consistency(error)
        call report("2c side_tag_consistency", error, failures)

        write (output_unit, '(A)') "Group 3: assembled element matrices"
        call test_uncut_assembly_vs_gauss(error)
        call report("3a uncut_assembly_vs_gauss", error, failures)
        call test_cut_assembly_convergence(error)
        call report("3b cut_assembly_convergence", error, failures)

        write (output_unit, '(A)') "Group 4: continuity in the level set"
        call test_assembly_continuity(error)
        call report("4a assembly_continuity", error, failures)
        call test_levelset_continuity(error)
        call report("4b levelset_continuity", error, failures)
        call test_vanishing_minority_side(error)
        call report("4c vanishing_minority_side", error, failures)

        write (output_unit, '(A)') "Group 5: contract"
        call test_capacity_contract(error)
        call report("5a capacity_contract", error, failures)
        call test_unsupported_and_degenerate(error)
        call report("5b unsupported_and_degenerate", error, failures)

        write (output_unit, '(A)') "Group 6: regression"
        call test_uncut_triangle(error)
        call report("6a uncut_triangle_partition", error, failures)
        call test_cut_triangle_exact_split(error)
        call report("6b cut_triangle_exact_split", error, failures)
        call test_uncut_quad(error)
        call report("6c uncut_quad_partition", error, failures)
        call test_cut_quad_split(error)
        call report("6d cut_quad_split", error, failures)
        call test_sidewise_constant_integration(error)
        call report("6e sidewise_constant_integration", error, failures)
        call test_sidewise_flux_residual(error)
        call report("6f sidewise_flux_residual", error, failures)

        write (output_unit, '(A)') "Group 7: chemical potential helpers"
        call test_psi_ice_at_freezing(error)
        call report("7a psi_ice_at_freezing_point", error, failures)
        call test_T_high_zero_pressure(error)
        call report("7b T_high_at_zero_pressure", error, failures)
    end subroutine run_all_tests

    subroutine report(name, error, failures)
        character(len=*), intent(in) :: name
        type(error_type), allocatable, intent(inout) :: error
        integer(int32), intent(inout) :: failures

        if (allocated(error)) then
            write (output_unit, '(A,A)') "  [FAIL] ", name
            failures = failures + 1
            deallocate (error)
        else
            write (output_unit, '(A,A)') "  [PASS] ", name
        end if
    end subroutine report

    ! =========================================================================
    ! Helpers: element cases and level sets
    ! =========================================================================

    !> The five supported 2D elements, in sweep order.
    subroutine get_family_case_ids(element_ids, num_cases)
        integer(int32), intent(inout) :: element_ids(:)
        integer(int32), intent(inout) :: num_cases

        num_cases = 5
        element_ids(1) = FE_TYPE%TRIANGLE%ID           ! T3
        element_ids(2) = FE_TYPE%QUADRATIC_TRIANGLE%ID ! T6
        element_ids(3) = FE_TYPE%QUAD%ID               ! Q4
        element_ids(4) = FE_TYPE%QUADRATIC_QUAD%ID     ! Q8
        element_ids(5) = FE_TYPE%BIQUADRATIC_QUAD%ID   ! Q9
    end subroutine get_family_case_ids

    pure function is_quadrilateral(element_id) result(is_quad)
        integer(int32), intent(in) :: element_id
        logical :: is_quad
        is_quad = (element_id == FE_TYPE%QUAD%ID .or. element_id == FE_TYPE%QUADRATIC_QUAD%ID &
                   .or. element_id == FE_TYPE%BIQUADRATIC_QUAD%ID)
    end function is_quadrilateral

    !> Reference measure: 1/2 for the unit simplex, 4 for [-1,1]^2.
    pure function get_reference_measure(element_id) result(measure)
        integer(int32), intent(in) :: element_id
        real(real64) :: measure
        if (is_quadrilateral(element_id)) then
            measure = 4.0d0
        else
            measure = 0.5d0
        end if
    end function get_reference_measure

    !> Edge length of the reference element, used as the element size h_e.
    pure function get_reference_size(element_id) result(element_size)
        integer(int32), intent(in) :: element_id
        real(real64) :: element_size
        if (is_quadrilateral(element_id)) then
            element_size = 2.0d0
        else
            element_size = 1.0d0
        end if
    end function get_reference_size

    !> Reference-space node coordinates in VTK ordering.
    subroutine get_reference_nodes(element_id, coordinates, num_nodes)
        integer(int32), intent(in) :: element_id
        real(real64), intent(inout) :: coordinates(:, :)
        integer(int32), intent(inout) :: num_nodes

        coordinates = 0.0d0
        if (is_quadrilateral(element_id)) then
            coordinates(:, 1) = [-1.0d0, -1.0d0]
            coordinates(:, 2) = [1.0d0, -1.0d0]
            coordinates(:, 3) = [1.0d0, 1.0d0]
            coordinates(:, 4) = [-1.0d0, 1.0d0]
            num_nodes = 4
            if (element_id == FE_TYPE%QUAD%ID) return
            coordinates(:, 5) = [0.0d0, -1.0d0]
            coordinates(:, 6) = [1.0d0, 0.0d0]
            coordinates(:, 7) = [0.0d0, 1.0d0]
            coordinates(:, 8) = [-1.0d0, 0.0d0]
            num_nodes = 8
            if (element_id == FE_TYPE%QUADRATIC_QUAD%ID) return
            coordinates(:, 9) = [0.0d0, 0.0d0]
            num_nodes = 9
        else
            coordinates(:, 1) = [0.0d0, 0.0d0]
            coordinates(:, 2) = [1.0d0, 0.0d0]
            coordinates(:, 3) = [0.0d0, 1.0d0]
            num_nodes = 3
            if (element_id == FE_TYPE%TRIANGLE%ID) return
            coordinates(:, 4) = [0.5d0, 0.0d0]
            coordinates(:, 5) = [0.5d0, 0.5d0]
            coordinates(:, 6) = [0.0d0, 0.5d0]
            num_nodes = 6
        end if
    end subroutine get_reference_nodes

    !> Build an element and the nodal level set of a reference-space function.
    !>
    !> LEVEL_SET_LINEAR: phi = xi + eta + offset, reproduced exactly by every
    !> supported element.  LEVEL_SET_CIRCLE: phi = xi^2 + eta^2 - offset,
    !> reproduced exactly by T6/Q8/Q9 (xi^2 and eta^2 lie in all three spaces).
    subroutine build_case(element_id, level_set_kind, offset, fe, phi_nodes, num_nodes)
        integer(int32), intent(in) :: element_id
        integer(int32), intent(in) :: level_set_kind
        real(real64), intent(in) :: offset
        class(abst_fe), allocatable, intent(inout) :: fe
        real(real64), intent(inout) :: phi_nodes(:)
        integer(int32), intent(inout) :: num_nodes

        real(real64) :: coordinates(2, MAX_NODES)
        integer(int32) :: node

        fe = create_fe(element_id, 1)
        call get_reference_nodes(element_id, coordinates, num_nodes)
        do node = 1, num_nodes
            if (level_set_kind == LEVEL_SET_LINEAR) then
                phi_nodes(node) = coordinates(1, node) + coordinates(2, node) + offset
            else
                phi_nodes(node) = coordinates(1, node)**2 + coordinates(2, node)**2 - offset
            end if
        end do
    end subroutine build_case

    !> Circle offset that puts the interface inside the reference element.  On
    !> the quadrilateral the corners stay positive and only the edge midpoints
    !> turn negative, so the cut is invisible to a vertex-only crossing test.
    pure function get_circle_offset(element_id) result(offset)
        integer(int32), intent(in) :: element_id
        real(real64) :: offset
        if (is_quadrilateral(element_id)) then
            offset = 1.2d0
        else
            offset = 0.36d0
        end if
    end function get_circle_offset

    !> Offset of phi = xi + eta + offset that cuts the reference element.
    pure function get_linear_offset(element_id) result(offset)
        integer(int32), intent(in) :: element_id
        real(real64) :: offset
        if (is_quadrilateral(element_id)) then
            offset = 0.5d0
        else
            offset = -1.0d0 / 3.0d0
        end if
    end function get_linear_offset

    !> Minus-side area of that straight cut: a right triangle of legs 3/2 on
    !> [-1,1]^2, of legs 1/3 on the unit simplex.
    pure function get_linear_minus_area(element_id) result(area)
        integer(int32), intent(in) :: element_id
        real(real64) :: area
        if (is_quadrilateral(element_id)) then
            area = 1.125d0
        else
            area = 1.0d0 / 18.0d0
        end if
    end function get_linear_minus_area

    !> Depth from which the cells resolve an interface of curvature scale
    !> radius: d_req = max(0, ceil(log2(h_e / radius))).  Below it the linear
    !> clip may miss the interface entirely, so no rate can be required.
    pure function get_resolution_depth(element_id, radius) result(depth)
        integer(int32), intent(in) :: element_id
        real(real64), intent(in) :: radius
        integer(int32) :: depth
        depth = max(0, ceiling(log(get_reference_size(element_id) / radius) / log(2.0d0)))
    end function get_resolution_depth

    ! =========================================================================
    ! Helpers: quadrature inspection
    ! =========================================================================

    pure function sum_side_weights(quadrature_points, num_points, plus_side) result(total)
        type(type_subcell_quadrature_point), intent(in) :: quadrature_points(:)
        integer(int32), intent(in) :: num_points
        logical, intent(in) :: plus_side
        real(real64) :: total
        integer(int32) :: point
        total = 0.0d0
        do point = 1, num_points
            if (quadrature_points(point)%is_plus_side .eqv. plus_side) then
                total = total + quadrature_points(point)%weight
            end if
        end do
    end function sum_side_weights

    !> True when every point lies inside the reference element.
    pure function are_points_inside(element_id, quadrature_points, num_points) result(inside)
        integer(int32), intent(in) :: element_id
        type(type_subcell_quadrature_point), intent(in) :: quadrature_points(:)
        integer(int32), intent(in) :: num_points
        logical :: inside
        real(real64), parameter :: tolerance = 1.0d-13
        integer(int32) :: point

        inside = .true.
        do point = 1, num_points
            if (is_quadrilateral(element_id)) then
                if (abs(quadrature_points(point)%xi) > 1.0d0 + tolerance .or. &
                    abs(quadrature_points(point)%eta) > 1.0d0 + tolerance) inside = .false.
            else
                if (quadrature_points(point)%xi < -tolerance .or. &
                    quadrature_points(point)%eta < -tolerance .or. &
                    quadrature_points(point)%xi + quadrature_points(point)%eta > 1.0d0 + tolerance) then
                    inside = .false.
                end if
            end if
        end do
    end function are_points_inside

    !> Integral of a reference-space monomial xi^p eta^q over the whole rule.
    pure function integrate_monomial(quadrature_points, num_points, p, q) result(total)
        type(type_subcell_quadrature_point), intent(in) :: quadrature_points(:)
        integer(int32), intent(in) :: num_points
        integer(int32), intent(in) :: p, q
        real(real64) :: total
        integer(int32) :: point
        total = 0.0d0
        do point = 1, num_points
            total = total + quadrature_points(point)%weight &
                    * quadrature_points(point)%xi**p * quadrature_points(point)%eta**q
        end do
    end function integrate_monomial

    !> Exact reference-element integral of xi^p eta^q for p + q <= 2.
    pure function exact_monomial_integral(element_id, p, q) result(total)
        integer(int32), intent(in) :: element_id
        integer(int32), intent(in) :: p, q
        real(real64) :: total

        if (is_quadrilateral(element_id)) then
            total = axis_integral(p) * axis_integral(q)
        else
            ! int_T xi^p eta^q dA = p! q! / (p + q + 2)!
            total = real(factorial(p) * factorial(q), real64) / real(factorial(p + q + 2), real64)
        end if
    end function exact_monomial_integral

    pure function axis_integral(k) result(total)
        integer(int32), intent(in) :: k
        real(real64) :: total
        if (mod(k, 2) == 0) then
            total = 2.0d0 / real(k + 1, real64)
        else
            total = 0.0d0
        end if
    end function axis_integral

    pure function factorial(k) result(value)
        integer(int32), intent(in) :: k
        integer(int32) :: value, i
        value = 1
        do i = 2, k
            value = value * i
        end do
    end function factorial

    !> Label used in failure messages.
    pure function case_label(element_id, depth) result(label)
        integer(int32), intent(in) :: element_id
        integer(int32), intent(in) :: depth
        character(len=24) :: label
        write (label, '(A,I0,A,I0)') "fe=", element_id, " d=", depth
    end function case_label

    ! =========================================================================
    ! Helpers: element matrix assembly
    ! =========================================================================

    !> Assemble the two matrices the governing blocks build on this rule:
    !> diffusion_matrix(i,j) = int D grad N_i . grad N_j dOmega and
    !> capacity_matrix(i,j) = int C N_i N_j dOmega, with D and C taking their
    !> minus-side value where the point is tagged minus.
    subroutine assemble_on_subcell_rule(fe, node_coordinates, num_nodes, quadrature_points, num_points, &
                                        diffusivity_minus, diffusivity_plus, capacity_minus, capacity_plus, &
                                        diffusion_matrix, capacity_matrix)
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: node_coordinates(:, :)
        integer(int32), intent(in) :: num_nodes
        type(type_subcell_quadrature_point), intent(in) :: quadrature_points(:)
        integer(int32), intent(in) :: num_points
        real(real64), intent(in) :: diffusivity_minus, diffusivity_plus
        real(real64), intent(in) :: capacity_minus, capacity_plus
        real(real64), intent(inout) :: diffusion_matrix(:, :), capacity_matrix(:, :)

        type(type_coordinate_dp) :: reference_point
        real(real64) :: shape_values(MAX_NODES), shape_gradients(2, MAX_NODES)
        real(real64) :: determinant_jacobian, effective_weight, diffusivity, capacity
        integer(int32) :: point, i, j

        diffusion_matrix = 0.0d0
        capacity_matrix = 0.0d0

        do point = 1, num_points
            reference_point%x = quadrature_points(point)%xi
            reference_point%y = quadrature_points(point)%eta
            reference_point%z = 0.0d0
            shape_values = 0.0d0
            shape_gradients = 0.0d0
            call fe%calc_shape_function(reference_point, node_coordinates, psi=shape_values(1:num_nodes), &
                                        dpsi_dx=shape_gradients(:, 1:num_nodes), &
                                        determinant_jacobian=determinant_jacobian)

            effective_weight = quadrature_points(point)%weight * abs(determinant_jacobian)
            if (quadrature_points(point)%is_plus_side) then
                diffusivity = diffusivity_plus
                capacity = capacity_plus
            else
                diffusivity = diffusivity_minus
                capacity = capacity_minus
            end if

            do j = 1, num_nodes
                do i = 1, num_nodes
                    diffusion_matrix(i, j) = diffusion_matrix(i, j) + effective_weight * diffusivity &
                                             * dot_product(shape_gradients(:, i), shape_gradients(:, j))
                    capacity_matrix(i, j) = capacity_matrix(i, j) + effective_weight * capacity &
                                            * shape_values(i) * shape_values(j)
                end do
            end do
        end do
    end subroutine assemble_on_subcell_rule

    !> Same two matrices on the element's own Gauss rule, with uniform
    !> coefficients.  Used as the reference on uncut elements.
    subroutine assemble_on_gauss_rule(fe, node_coordinates, num_nodes, diffusivity, capacity, &
                                      diffusion_matrix, capacity_matrix)
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: node_coordinates(:, :)
        integer(int32), intent(in) :: num_nodes
        real(real64), intent(in) :: diffusivity, capacity
        real(real64), intent(inout) :: diffusion_matrix(:, :), capacity_matrix(:, :)

        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: gauss_points
        real(real64), pointer, contiguous, dimension(:) :: gauss_weights
        real(real64) :: shape_values(MAX_NODES), shape_gradients(2, MAX_NODES)
        real(real64) :: determinant_jacobian, effective_weight
        integer(int32) :: num_gauss, point, i, j

        diffusion_matrix = 0.0d0
        capacity_matrix = 0.0d0

        call fe%get_gauss(gauss_points)
        call fe%get_weight(gauss_weights)
        call fe%get_num_gauss(num_gauss)

        do point = 1, num_gauss
            shape_values = 0.0d0
            shape_gradients = 0.0d0
            call fe%calc_shape_function(gauss_points(point), node_coordinates, psi=shape_values(1:num_nodes), &
                                        dpsi_dx=shape_gradients(:, 1:num_nodes), &
                                        determinant_jacobian=determinant_jacobian)

            effective_weight = gauss_weights(point) * abs(determinant_jacobian)
            do j = 1, num_nodes
                do i = 1, num_nodes
                    diffusion_matrix(i, j) = diffusion_matrix(i, j) + effective_weight * diffusivity &
                                             * dot_product(shape_gradients(:, i), shape_gradients(:, j))
                    capacity_matrix(i, j) = capacity_matrix(i, j) + effective_weight * capacity &
                                            * shape_values(i) * shape_values(j)
                end do
            end do
        end do

        nullify (gauss_points)
        nullify (gauss_weights)
    end subroutine assemble_on_gauss_rule

    !> Physical node coordinates: the reference element itself (affine map), so
    !> the Gauss reference stays exact for the integrands compared here.
    subroutine get_affine_element_coordinates(element_id, node_coordinates, num_nodes)
        integer(int32), intent(in) :: element_id
        real(real64), intent(inout) :: node_coordinates(:, :)
        integer(int32), intent(inout) :: num_nodes
        call get_reference_nodes(element_id, node_coordinates, num_nodes)
    end subroutine get_affine_element_coordinates

    !> A T6 with mid-node 4 displaced to (0.5, 0.25): its edge 1-2 is a
    !> parabola, so the element measure is 0.5 - (2/3)(1)(0.25) = 1/3
    !> (Archimedes) and det J varies over the element.
    subroutine get_curved_triangle_coordinates(node_coordinates, num_nodes)
        real(real64), intent(inout) :: node_coordinates(:, :)
        integer(int32), intent(inout) :: num_nodes

        node_coordinates = 0.0d0
        node_coordinates(:, 1) = [0.0d0, 0.0d0]
        node_coordinates(:, 2) = [1.0d0, 0.0d0]
        node_coordinates(:, 3) = [0.0d0, 1.0d0]
        node_coordinates(:, 4) = [0.5d0, 0.25d0]
        node_coordinates(:, 5) = [0.5d0, 0.5d0]
        node_coordinates(:, 6) = [0.0d0, 0.5d0]
        num_nodes = 6
    end subroutine get_curved_triangle_coordinates

    ! =========================================================================
    ! Group 1a. Tiling invariants for every family, order, depth, level set.
    ! =========================================================================
    subroutine test_tiling_invariants(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(TEST_QUADRATURE_CAPACITY)
        real(real64) :: phi_nodes(MAX_NODES), total_weight, offset
        integer(int32) :: element_ids(5), num_cases, num_nodes, num_points, k, depth, level_set

        call get_family_case_ids(element_ids, num_cases)
        do k = 1, num_cases
            do level_set = LEVEL_SET_LINEAR, LEVEL_SET_CIRCLE
                if (level_set == LEVEL_SET_LINEAR) then
                    offset = get_linear_offset(element_ids(k))
                else
                    offset = get_circle_offset(element_ids(k))
                end if
                do depth = 0, TEST_MAX_DEPTH
                    call build_case(element_ids(k), level_set, offset, fe, phi_nodes, num_nodes)
                    num_points = 0
                    call build_interface_quadrature_points(fe, phi_nodes(1:num_nodes), quadrature_points, &
                                                           num_points, max_depth=depth)

                    call check(error, num_points > 0, "No quadrature points: "//trim(case_label(element_ids(k), depth)))
                    if (allocated(error)) return

                    total_weight = sum_side_weights(quadrature_points, num_points, .true.) &
                                   + sum_side_weights(quadrature_points, num_points, .false.)
                    call check(error, abs(total_weight - get_reference_measure(element_ids(k))) < 1.0d-12, &
                               "Weights must sum to the reference measure: "//trim(case_label(element_ids(k), depth)))
                    if (allocated(error)) return

                    call check(error, minval(quadrature_points(1:num_points)%weight) > 0.0d0, &
                               "Every weight must be positive: "//trim(case_label(element_ids(k), depth)))
                    if (allocated(error)) return

                    call check(error, are_points_inside(element_ids(k), quadrature_points, num_points), &
                               "Every point must lie inside the element: "//trim(case_label(element_ids(k), depth)))
                    if (allocated(error)) return
                end do
            end do
        end do
    end subroutine test_tiling_invariants

    ! =========================================================================
    ! Group 1b. Degree-2 exactness over the whole reference element, for any
    ! cut and any depth: the sub-triangles must tile it without overlap or gap.
    ! =========================================================================
    subroutine test_degree2_exactness(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(TEST_QUADRATURE_CAPACITY)
        real(real64) :: phi_nodes(MAX_NODES), computed, expected, scale
        integer(int32) :: element_ids(5), num_cases, num_nodes, num_points, k, depth, p, q

        call get_family_case_ids(element_ids, num_cases)
        do k = 1, num_cases
            do depth = 0, TEST_MAX_DEPTH
                call build_case(element_ids(k), LEVEL_SET_CIRCLE, get_circle_offset(element_ids(k)), &
                                fe, phi_nodes, num_nodes)
                num_points = 0
                call build_interface_quadrature_points(fe, phi_nodes(1:num_nodes), quadrature_points, &
                                                       num_points, max_depth=depth)
                scale = get_reference_measure(element_ids(k))

                do p = 0, 2
                    do q = 0, 2 - p
                        computed = integrate_monomial(quadrature_points, num_points, p, q)
                        expected = exact_monomial_integral(element_ids(k), p, q)
                        call check(error, abs(computed - expected) < 1.0d-12 * scale, &
                                   "Degree-2 exactness lost: "//trim(case_label(element_ids(k), depth)))
                        if (allocated(error)) return
                    end do
                end do
            end do
        end do
    end subroutine test_degree2_exactness

    ! =========================================================================
    ! Group 2a. A straight interface is represented exactly by the linear clip,
    ! so the side areas must not change with depth, family or order.
    ! =========================================================================
    subroutine test_straight_interface_is_exact(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(TEST_QUADRATURE_CAPACITY)
        real(real64) :: phi_nodes(MAX_NODES), expected
        integer(int32) :: element_ids(5), num_cases, num_nodes, num_points, k, depth

        call get_family_case_ids(element_ids, num_cases)
        do k = 1, num_cases
            expected = get_linear_minus_area(element_ids(k))
            do depth = 0, TEST_MAX_DEPTH
                call build_case(element_ids(k), LEVEL_SET_LINEAR, get_linear_offset(element_ids(k)), &
                                fe, phi_nodes, num_nodes)
                num_points = 0
                call build_interface_quadrature_points(fe, phi_nodes(1:num_nodes), quadrature_points, &
                                                       num_points, max_depth=depth)

                call check(error, abs(sum_side_weights(quadrature_points, num_points, .false.) - expected) < 1.0d-12, &
                           "Straight interface must be exact: "//trim(case_label(element_ids(k), depth)))
                if (allocated(error)) return
            end do
        end do
    end subroutine test_straight_interface_is_exact

    ! =========================================================================
    ! Group 2b. Curved interface. phi = xi^2 + eta^2 - r^2 is reproduced
    ! exactly by T6/Q8/Q9, so the minus side is a disc sector of known area.
    ! Chords give an O(h^2) area error, but only once the cells resolve the
    ! interface: below depth ceil(log2(h_e/r)) the whole disc can sit inside a
    ! cell with same-sign vertices and be missed, so only monotone decrease is
    ! required there.
    ! =========================================================================
    subroutine test_curved_interface_convergence(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(TEST_QUADRATURE_CAPACITY)
        real(real64) :: phi_nodes(MAX_NODES), area_error(0:TEST_MAX_DEPTH)
        real(real64) :: radius, offset, expected
        integer(int32) :: element_ids(3), num_cases, num_nodes, num_points, k, depth, resolution_depth

        element_ids(1) = FE_TYPE%QUADRATIC_TRIANGLE%ID
        element_ids(2) = FE_TYPE%QUADRATIC_QUAD%ID
        element_ids(3) = FE_TYPE%BIQUADRATIC_QUAD%ID
        num_cases = 3

        do k = 1, num_cases
            if (is_quadrilateral(element_ids(k))) then
                radius = 0.8d0                    ! disc inside [-1,1]^2
                offset = radius**2
                expected = PI * radius**2
            else
                radius = 0.6d0                    ! quarter disc inside the simplex
                offset = radius**2
                expected = 0.25d0 * PI * radius**2
            end if
            resolution_depth = get_resolution_depth(element_ids(k), radius)

            do depth = 0, TEST_MAX_DEPTH
                call build_case(element_ids(k), LEVEL_SET_CIRCLE, offset, fe, phi_nodes, num_nodes)
                num_points = 0
                call build_interface_quadrature_points(fe, phi_nodes(1:num_nodes), quadrature_points, &
                                                       num_points, max_depth=depth)
                area_error(depth) = abs(sum_side_weights(quadrature_points, num_points, .false.) - expected)
            end do

            do depth = 0, TEST_MAX_DEPTH - 1
                call check(error, area_error(depth + 1) < area_error(depth), &
                           "Area error must fall with depth: "//trim(case_label(element_ids(k), depth)))
                if (allocated(error)) return
                if (depth >= resolution_depth) then
                    call check(error, area_error(depth + 1) < CONVERGENCE_RATIO * area_error(depth), &
                               "Area error must fall as O(4^-d) once resolved: " &
                               //trim(case_label(element_ids(k), depth)))
                    if (allocated(error)) return
                end if
            end do
        end do
    end subroutine test_curved_interface_convergence

    ! =========================================================================
    ! Group 2c. The side tag drives which constitutive branch is evaluated, so
    ! it must agree with the sign of phi at the point itself.  Disagreement is
    ! confined to the band between the chord and the true interface, whose
    ! weight must vanish as O(4^-d).
    ! =========================================================================
    subroutine test_side_tag_consistency(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(TEST_QUADRATURE_CAPACITY)
        type(type_coordinate_dp) :: reference_point
        real(real64) :: phi_nodes(MAX_NODES), mismatched_weight(0:TEST_MAX_DEPTH), phi_at_point
        integer(int32) :: element_ids(3), num_cases, num_nodes, num_points, k, depth, point

        element_ids(1) = FE_TYPE%QUADRATIC_TRIANGLE%ID
        element_ids(2) = FE_TYPE%QUADRATIC_QUAD%ID
        element_ids(3) = FE_TYPE%BIQUADRATIC_QUAD%ID
        num_cases = 3

        do k = 1, num_cases
            do depth = 0, TEST_MAX_DEPTH
                call build_case(element_ids(k), LEVEL_SET_CIRCLE, get_circle_offset(element_ids(k)), &
                                fe, phi_nodes, num_nodes)
                num_points = 0
                call build_interface_quadrature_points(fe, phi_nodes(1:num_nodes), quadrature_points, &
                                                       num_points, max_depth=depth)

                mismatched_weight(depth) = 0.0d0
                do point = 1, num_points
                    reference_point%x = quadrature_points(point)%xi
                    reference_point%y = quadrature_points(point)%eta
                    reference_point%z = 0.0d0
                    call fe%lerp(reference_point, phi_nodes(1:num_nodes), phi_at_point)
                    if ((phi_at_point > 0.0d0) .neqv. quadrature_points(point)%is_plus_side) then
                        mismatched_weight(depth) = mismatched_weight(depth) + quadrature_points(point)%weight
                    end if
                end do
            end do

            do depth = 0, TEST_MAX_DEPTH - 1
                call check(error, mismatched_weight(depth + 1) <= mismatched_weight(depth), &
                           "Mistagged weight must not grow with depth: "//trim(case_label(element_ids(k), depth)))
                if (allocated(error)) return
            end do

            call check(error, mismatched_weight(TEST_MAX_DEPTH) < 0.02d0 * get_reference_measure(element_ids(k)), &
                       "Mistagged weight must be a sub-resolution band: " &
                       //trim(case_label(element_ids(k), TEST_MAX_DEPTH)))
            if (allocated(error)) return
        end do
    end subroutine test_side_tag_consistency

    ! =========================================================================
    ! Group 3a. On an uncut element the subcell rule replaces the Gauss rule.
    ! For integrands of degree <= 2 the midpoint rule is exact, so the two must
    ! agree to roundoff at every depth: that covers grad N . grad N on T3, Q4
    ! and T6 (grad N is linear there) and N N on T3.  Everything else differs,
    ! and the difference must fall as O(4^-d).
    ! =========================================================================
    subroutine test_uncut_assembly_vs_gauss(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe, fe_reference
        type(type_subcell_quadrature_point) :: quadrature_points(TEST_QUADRATURE_CAPACITY)
        real(real64) :: phi_nodes(MAX_NODES), node_coordinates(2, MAX_NODES)
        real(real64) :: diffusion_subcell(MAX_NODES, MAX_NODES), capacity_subcell(MAX_NODES, MAX_NODES)
        real(real64) :: diffusion_gauss(MAX_NODES, MAX_NODES), capacity_gauss(MAX_NODES, MAX_NODES)
        real(real64) :: diffusion_error(0:TEST_MAX_DEPTH), capacity_error(0:TEST_MAX_DEPTH)
        real(real64) :: diffusion_scale, capacity_scale
        real(real64), parameter :: diffusivity = 3.0d0, capacity = 2.0d0
        integer(int32) :: element_ids(5), num_cases, num_nodes, num_points, k, depth
        logical :: diffusion_must_be_exact

        call get_family_case_ids(element_ids, num_cases)
        do k = 1, num_cases
            fe_reference = create_fe(element_ids(k), EXACT_GAUSS_ORDER)
            call get_affine_element_coordinates(element_ids(k), node_coordinates, num_nodes)
            call assemble_on_gauss_rule(fe_reference, node_coordinates(:, 1:num_nodes), num_nodes, &
                                        diffusivity, capacity, diffusion_gauss, capacity_gauss)
            diffusion_scale = maxval(abs(diffusion_gauss(1:num_nodes, 1:num_nodes)))
            capacity_scale = maxval(abs(capacity_gauss(1:num_nodes, 1:num_nodes)))

            ! grad N is constant on T3/Q4 in the direction it varies and linear
            ! on T6, so the diffusion integrand stays within degree 2 there.
            diffusion_must_be_exact = (element_ids(k) == FE_TYPE%TRIANGLE%ID &
                                       .or. element_ids(k) == FE_TYPE%QUAD%ID &
                                       .or. element_ids(k) == FE_TYPE%QUADRATIC_TRIANGLE%ID)

            do depth = 0, TEST_MAX_DEPTH
                ! Uncut: phi > 0 everywhere, so the whole element is plus side.
                call build_case(element_ids(k), LEVEL_SET_LINEAR, 10.0d0, fe, phi_nodes, num_nodes)
                num_points = 0
                call build_interface_quadrature_points(fe, phi_nodes(1:num_nodes), quadrature_points, &
                                                       num_points, max_depth=depth)
                call assemble_on_subcell_rule(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                              quadrature_points, num_points, diffusivity, diffusivity, &
                                              capacity, capacity, diffusion_subcell, capacity_subcell)

                diffusion_error(depth) = maxval(abs(diffusion_subcell(1:num_nodes, 1:num_nodes) &
                                                    - diffusion_gauss(1:num_nodes, 1:num_nodes)))
                capacity_error(depth) = maxval(abs(capacity_subcell(1:num_nodes, 1:num_nodes) &
                                                   - capacity_gauss(1:num_nodes, 1:num_nodes)))

                if (diffusion_must_be_exact) then
                    call check(error, diffusion_error(depth) < 1.0d-12 * diffusion_scale, &
                               "Diffusion matrix must match Gauss exactly: " &
                               //trim(case_label(element_ids(k), depth)))
                    if (allocated(error)) return
                end if

                if (element_ids(k) == FE_TYPE%TRIANGLE%ID) then
                    call check(error, capacity_error(depth) < 1.0d-12 * capacity_scale, &
                               "T3 capacity matrix must match Gauss exactly: " &
                               //trim(case_label(element_ids(k), depth)))
                    if (allocated(error)) return
                end if
            end do

            ! Where the integrand exceeds degree 2 the rule is inexact; the
            ! error must then be second order in the cell size.
            if (.not. diffusion_must_be_exact) then
                do depth = 0, TEST_MAX_DEPTH - 1
                    call check(error, diffusion_error(depth + 1) < CONVERGENCE_RATIO * diffusion_error(depth), &
                               "Diffusion quadrature error must fall as O(4^-d): " &
                               //trim(case_label(element_ids(k), depth)))
                    if (allocated(error)) return
                end do
            end if

            if (element_ids(k) /= FE_TYPE%TRIANGLE%ID) then
                do depth = 0, TEST_MAX_DEPTH - 1
                    call check(error, capacity_error(depth + 1) < CONVERGENCE_RATIO * capacity_error(depth), &
                               "Capacity quadrature error must fall as O(4^-d): " &
                               //trim(case_label(element_ids(k), depth)))
                    if (allocated(error)) return
                end do
            end if
        end do
    end subroutine test_uncut_assembly_vs_gauss

    ! =========================================================================
    ! Group 3b. Cut element with side-wise coefficients, on a curved T6 so the
    ! parent mapping is exercised too.  No analytic value exists, so the deep
    ! refinement is the reference; the error must be second order in the cell
    ! size from the depth that resolves the interface.
    ! Also checks the element measure, which is analytic: 1/3.
    ! =========================================================================
    subroutine test_cut_assembly_convergence(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(TEST_QUADRATURE_CAPACITY)
        type(type_subcell_quadrature_point), allocatable :: reference_points(:)
        real(real64) :: phi_nodes(MAX_NODES), node_coordinates(2, MAX_NODES)
        real(real64) :: diffusion(MAX_NODES, MAX_NODES), capacity(MAX_NODES, MAX_NODES)
        real(real64) :: diffusion_reference(MAX_NODES, MAX_NODES), capacity_reference(MAX_NODES, MAX_NODES)
        real(real64) :: diffusion_error(0:TEST_MAX_DEPTH), capacity_error(0:TEST_MAX_DEPTH)
        real(real64) :: measure, radius, offset
        real(real64), parameter :: diffusivity_minus = 1.0d0, diffusivity_plus = 5.0d0
        real(real64), parameter :: capacity_minus = 2.0d0, capacity_plus = 7.0d0
        integer(int32) :: element_id, num_nodes, num_points, num_reference_points, depth, resolution_depth, point

        element_id = FE_TYPE%QUADRATIC_TRIANGLE%ID
        radius = 0.6d0
        offset = radius**2
        resolution_depth = get_resolution_depth(element_id, radius)
        call get_curved_triangle_coordinates(node_coordinates, num_nodes)

        allocate (reference_points(calc_subcell_quadrature_capacity(element_id, TEST_REFERENCE_DEPTH)))
        call build_case(element_id, LEVEL_SET_CIRCLE, offset, fe, phi_nodes, num_nodes)
        num_reference_points = 0
        call build_interface_quadrature_points(fe, phi_nodes(1:num_nodes), reference_points, &
                                               num_reference_points, max_depth=TEST_REFERENCE_DEPTH)
        call check(error, num_reference_points > 0, "Reference refinement must fit its own capacity")
        if (allocated(error)) return
        call assemble_on_subcell_rule(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                      reference_points, num_reference_points, &
                                      diffusivity_minus, diffusivity_plus, capacity_minus, capacity_plus, &
                                      diffusion_reference, capacity_reference)

        do depth = 0, TEST_MAX_DEPTH
            call build_case(element_id, LEVEL_SET_CIRCLE, offset, fe, phi_nodes, num_nodes)
            num_points = 0
            call build_interface_quadrature_points(fe, phi_nodes(1:num_nodes), quadrature_points, &
                                                   num_points, max_depth=depth)
            call assemble_on_subcell_rule(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                          quadrature_points, num_points, &
                                          diffusivity_minus, diffusivity_plus, capacity_minus, capacity_plus, &
                                          diffusion, capacity)

            diffusion_error(depth) = maxval(abs(diffusion(1:num_nodes, 1:num_nodes) &
                                                - diffusion_reference(1:num_nodes, 1:num_nodes)))
            capacity_error(depth) = maxval(abs(capacity(1:num_nodes, 1:num_nodes) &
                                               - capacity_reference(1:num_nodes, 1:num_nodes)))

            ! The measure is independent of the cut and analytic here.
            measure = 0.0d0
            do point = 1, num_points
                measure = measure + quadrature_points(point)%weight
            end do
            measure = measure * 1.0d0
            call check(error, abs(calc_physical_measure(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                                        quadrature_points, num_points) - 1.0d0 / 3.0d0) < 1.0d-12, &
                       "Curved element measure must be exact at every depth: " &
                       //trim(case_label(element_id, depth)))
            if (allocated(error)) return
        end do

        do depth = resolution_depth, TEST_MAX_DEPTH - 1
            call check(error, diffusion_error(depth + 1) < CONVERGENCE_RATIO * diffusion_error(depth), &
                       "Cut diffusion matrix must converge as O(4^-d): "//trim(case_label(element_id, depth)))
            if (allocated(error)) return
            call check(error, capacity_error(depth + 1) < CONVERGENCE_RATIO * capacity_error(depth), &
                       "Cut capacity matrix must converge as O(4^-d): "//trim(case_label(element_id, depth)))
            if (allocated(error)) return
        end do

        deallocate (reference_points)
    end subroutine test_cut_assembly_convergence

    !> Physical measure of the element as seen by the rule: sum w |det J|.
    function calc_physical_measure(fe, node_coordinates, num_nodes, quadrature_points, num_points) result(measure)
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: node_coordinates(:, :)
        integer(int32), intent(in) :: num_nodes
        type(type_subcell_quadrature_point), intent(in) :: quadrature_points(:)
        integer(int32), intent(in) :: num_points
        real(real64) :: measure

        type(type_coordinate_dp) :: reference_point
        real(real64) :: shape_gradients(2, MAX_NODES), determinant_jacobian
        integer(int32) :: point

        measure = 0.0d0
        do point = 1, num_points
            reference_point%x = quadrature_points(point)%xi
            reference_point%y = quadrature_points(point)%eta
            reference_point%z = 0.0d0
            shape_gradients = 0.0d0
            call fe%calc_shape_function(reference_point, node_coordinates, &
                                        dpsi_dx=shape_gradients(:, 1:num_nodes), &
                                        determinant_jacobian=determinant_jacobian)
            measure = measure + quadrature_points(point)%weight * abs(determinant_jacobian)
        end do
    end function calc_physical_measure

    ! =========================================================================
    ! Group 4a. Continuity of the ASSEMBLED matrices, which is what the Newton
    ! map differentiates.  Sweeping the interface changes the refinement
    ! pattern; a jump there would show up as one step far larger than the mean.
    ! =========================================================================
    subroutine test_assembly_continuity(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(TEST_QUADRATURE_CAPACITY)
        real(real64) :: phi_nodes(MAX_NODES), node_coordinates(2, MAX_NODES)
        real(real64) :: diffusion(MAX_NODES, MAX_NODES), capacity(MAX_NODES, MAX_NODES)
        real(real64) :: diffusion_previous(MAX_NODES, MAX_NODES), capacity_previous(MAX_NODES, MAX_NODES)
        real(real64) :: step_change, max_step_change, mean_step_change, offset
        real(real64), parameter :: offset_start = 0.20d0, offset_step = 1.0d-4
        real(real64), parameter :: diffusivity_minus = 1.0d0, diffusivity_plus = 5.0d0
        real(real64), parameter :: capacity_minus = 2.0d0, capacity_plus = 7.0d0
        integer(int32), parameter :: num_steps = 2000
        integer(int32) :: element_id, num_nodes, num_points, step

        element_id = FE_TYPE%QUADRATIC_TRIANGLE%ID
        call get_curved_triangle_coordinates(node_coordinates, num_nodes)

        max_step_change = 0.0d0
        mean_step_change = 0.0d0
        diffusion_previous = 0.0d0
        capacity_previous = 0.0d0

        do step = 0, num_steps
            offset = offset_start + real(step, real64) * offset_step
            call build_case(element_id, LEVEL_SET_CIRCLE, offset, fe, phi_nodes, num_nodes)
            num_points = 0
            call build_interface_quadrature_points(fe, phi_nodes(1:num_nodes), quadrature_points, &
                                                   num_points, max_depth=2)
            call assemble_on_subcell_rule(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                          quadrature_points, num_points, &
                                          diffusivity_minus, diffusivity_plus, capacity_minus, capacity_plus, &
                                          diffusion, capacity)

            if (step > 0) then
                step_change = maxval(abs(diffusion(1:num_nodes, 1:num_nodes) &
                                         - diffusion_previous(1:num_nodes, 1:num_nodes))) &
                              + maxval(abs(capacity(1:num_nodes, 1:num_nodes) &
                                           - capacity_previous(1:num_nodes, 1:num_nodes)))
                max_step_change = max(max_step_change, step_change)
                mean_step_change = mean_step_change + step_change
            end if
            diffusion_previous = diffusion
            capacity_previous = capacity
        end do
        mean_step_change = mean_step_change / real(num_steps, real64)

        ! A refinement switch would move a whole depth-2 cell (area ~ 1/32,
        ! coefficient jump 4-5) between the sides: about 1e-1, four orders
        ! above the smooth step of a 1e-4 offset increment.
        call check(error, mean_step_change > 0.0d0, "Sweep must actually move the interface")
        if (allocated(error)) return
        call check(error, max_step_change < 10.0d0 * mean_step_change, &
                   "Assembled matrices must not jump when the refinement pattern changes")
    end subroutine test_assembly_continuity

    ! =========================================================================
    ! Group 4b. Side weights are Lipschitz in the nodal level set (T3, exact
    ! clip): perturbing phi by delta changes each side sum by O(delta).
    ! =========================================================================
    subroutine test_levelset_continuity(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(SUBCELL_QUADRATURE_CAPACITY)
        real(real64) :: phi_nodes(3)
        real(real64) :: weight_previous, weight_current, max_jump
        real(real64), parameter :: phi_step = 1.0d-4
        integer(int32) :: num_points, step

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)

        max_jump = 0.0d0
        weight_previous = -1.0d0
        do step = 0, 200
            phi_nodes = [-0.5d0 + real(step, real64) * phi_step, 1.0d0, 1.0d0]
            num_points = 0
            call build_interface_quadrature_points(fe, phi_nodes, quadrature_points, num_points)
            weight_current = sum_side_weights(quadrature_points, num_points, .false.)
            if (step > 0) max_jump = max(max_jump, abs(weight_current - weight_previous))
            weight_previous = weight_current
        end do

        ! d(area)/d(phi_1) is bounded by ~0.25 here; allow a small safety factor.
        call check(error, max_jump < 1.0d-3, &
                   "Side weights must vary continuously with the nodal level set")
    end subroutine test_levelset_continuity

    ! =========================================================================
    ! Group 4c. As the lone vertex value goes to zero the minority-side weight
    ! must vanish, i.e. the split degenerates to the uncut rule.
    ! =========================================================================
    subroutine test_vanishing_minority_side(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(SUBCELL_QUADRATURE_CAPACITY)
        real(real64) :: phi_nodes(3), minus_weight
        integer(int32) :: num_points

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)

        phi_nodes = [-1.0d-9, 1.0d0, 1.0d0]
        num_points = 0
        call build_interface_quadrature_points(fe, phi_nodes, quadrature_points, num_points)
        minus_weight = sum_side_weights(quadrature_points, num_points, .false.)

        call check(error, minus_weight < 1.0d-15, &
                   "Minority side weight must vanish as the interface leaves the element")
    end subroutine test_vanishing_minority_side

    ! =========================================================================
    ! Group 5a. Capacity contract: the advertised bound must hold, an array of
    ! exactly that size must never be reported as too small, and a smaller one
    ! must yield zero rather than a partial fill.
    ! =========================================================================
    subroutine test_capacity_contract(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(TEST_QUADRATURE_CAPACITY)
        type(type_subcell_quadrature_point) :: too_small(5)
        type(type_subcell_quadrature_point), allocatable :: exact_capacity_points(:)
        real(real64) :: phi_nodes(MAX_NODES)
        integer(int32) :: element_ids(5), num_cases, num_nodes, num_points, k, depth, capacity

        call get_family_case_ids(element_ids, num_cases)
        do k = 1, num_cases
            do depth = 0, TEST_MAX_DEPTH
                capacity = calc_subcell_quadrature_capacity(element_ids(k), depth)

                call build_case(element_ids(k), LEVEL_SET_CIRCLE, get_circle_offset(element_ids(k)), &
                                fe, phi_nodes, num_nodes)
                num_points = 0
                call build_interface_quadrature_points(fe, phi_nodes(1:num_nodes), quadrature_points, &
                                                       num_points, max_depth=depth)
                call check(error, num_points <= capacity, &
                           "Capacity must bound the point count: "//trim(case_label(element_ids(k), depth)))
                if (allocated(error)) return

                ! An array of exactly the advertised capacity must succeed.
                allocate (exact_capacity_points(capacity))
                num_points = 0
                call build_interface_quadrature_points(fe, phi_nodes(1:num_nodes), exact_capacity_points, &
                                                       num_points, max_depth=depth)
                call check(error, num_points > 0, &
                           "Capacity-sized array must not overflow: "//trim(case_label(element_ids(k), depth)))
                deallocate (exact_capacity_points)
                if (allocated(error)) return
            end do

            call check(error, calc_subcell_quadrature_capacity(element_ids(k), SUBCELL_MAX_DEPTH) &
                       <= SUBCELL_QUADRATURE_CAPACITY, &
                       "Workspace capacity must cover SUBCELL_MAX_DEPTH: " &
                       //trim(case_label(element_ids(k), SUBCELL_MAX_DEPTH)))
            if (allocated(error)) return
        end do

        ! A cut triangle needs 9 points at depth 0; 5 must be reported as zero.
        call build_case(FE_TYPE%TRIANGLE%ID, LEVEL_SET_LINEAR, get_linear_offset(FE_TYPE%TRIANGLE%ID), &
                        fe, phi_nodes, num_nodes)
        num_points = -1
        call build_interface_quadrature_points(fe, phi_nodes(1:num_nodes), too_small, num_points)
        call check(error, num_points == 0, "Overflow must report zero, not a partial fill")
    end subroutine test_capacity_contract

    ! =========================================================================
    ! Group 5b. Families outside the 2D triangle/quadrilateral scope must be
    ! reported as unsupported, and degenerate level sets must still produce a
    ! valid partition rather than an empty or torn rule.
    ! =========================================================================
    subroutine test_unsupported_and_degenerate(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(TEST_QUADRATURE_CAPACITY)
        real(real64) :: phi_nodes(MAX_NODES), total_weight
        integer(int32) :: num_nodes, num_points, depth

        fe = create_fe(FE_TYPE%LINE%ID, 1)
        phi_nodes = 1.0d0
        num_points = -1
        call build_interface_quadrature_points(fe, phi_nodes(1:2), quadrature_points, num_points)
        call check(error, num_points == 0, "LINE must be reported as unsupported")
        if (allocated(error)) return

        fe = create_fe(FE_TYPE%TETRA%ID, 1)
        num_points = -1
        call build_interface_quadrature_points(fe, phi_nodes(1:4), quadrature_points, num_points)
        call check(error, num_points == 0, "TETRA must be reported as unsupported")
        if (allocated(error)) return

        call check(error, calc_subcell_quadrature_capacity(FE_TYPE%TETRA%ID, 0) == 0, &
                   "Capacity must be zero for unsupported families")
        if (allocated(error)) return

        ! phi identically zero: no point is on the plus side, and the partition
        ! must still cover the element exactly.
        fe = create_fe(FE_TYPE%QUADRATIC_QUAD%ID, 1)
        call get_reference_nodes(FE_TYPE%QUADRATIC_QUAD%ID, phi_nodes_workspace(), num_nodes)
        phi_nodes = 0.0d0
        num_nodes = 8
        do depth = 0, 2
            num_points = 0
            call build_interface_quadrature_points(fe, phi_nodes(1:num_nodes), quadrature_points, &
                                                   num_points, max_depth=depth)
            total_weight = sum_side_weights(quadrature_points, num_points, .false.)
            call check(error, abs(total_weight - 4.0d0) < 1.0d-12, &
                       "A zero level set must put the whole element on the minus side")
            if (allocated(error)) return
        end do

        ! phi exactly zero at one node: the clip must stay well defined.
        phi_nodes = 1.0d0
        phi_nodes(1) = 0.0d0
        do depth = 0, 2
            num_points = 0
            call build_interface_quadrature_points(fe, phi_nodes(1:num_nodes), quadrature_points, &
                                                   num_points, max_depth=depth)
            total_weight = sum_side_weights(quadrature_points, num_points, .true.) &
                           + sum_side_weights(quadrature_points, num_points, .false.)
            call check(error, abs(total_weight - 4.0d0) < 1.0d-12, &
                       "A node-exact zero must keep the partition complete")
            if (allocated(error)) return
        end do
    end subroutine test_unsupported_and_degenerate

    ! =========================================================================
    ! Group 6a. Uncut triangle: everything on one side, total = 0.5.
    ! =========================================================================
    subroutine test_uncut_triangle(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(SUBCELL_QUADRATURE_CAPACITY)
        integer(int32) :: num_points
        real(real64) :: phi_nodes(3)

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)

        phi_nodes = [1.0d0, 2.0d0, 3.0d0]
        num_points = 0
        call build_interface_quadrature_points(fe, phi_nodes, quadrature_points, num_points)

        call check(error, num_points > 0, "Expected quadrature points for uncut triangle")
        if (allocated(error)) return
        call check(error, abs(sum_side_weights(quadrature_points, num_points, .true.) - 0.5d0) < 1.0d-13, &
                   "Plus side should carry the whole reference area 0.5")
        if (allocated(error)) return
        call check(error, abs(sum_side_weights(quadrature_points, num_points, .false.)) < 1.0d-13, &
                   "Minus side should carry zero weight for uncut triangle")
    end subroutine test_uncut_triangle

    ! =========================================================================
    ! Group 6b. phi = [-0.5, 1, 1] gives phi^h = -0.5 + 1.5(xi + eta), zero on
    ! xi + eta = 1/3, so the minus area is 0.5 (1/3)^2 = 1/18.
    ! =========================================================================
    subroutine test_cut_triangle_exact_split(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(SUBCELL_QUADRATURE_CAPACITY)
        integer(int32) :: num_points
        real(real64) :: phi_nodes(3), minus_area

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)

        phi_nodes = [-0.5d0, 1.0d0, 1.0d0]
        minus_area = 0.5d0 * (1.0d0 / 3.0d0)**2
        num_points = 0
        call build_interface_quadrature_points(fe, phi_nodes, quadrature_points, num_points)

        call check(error, num_points > 0, "Expected quadrature points for cut triangle")
        if (allocated(error)) return
        call check(error, abs(sum_side_weights(quadrature_points, num_points, .false.) - minus_area) < 1.0d-13, &
                   "Minus side must equal the analytic area 1/18 exactly")
        if (allocated(error)) return
        call check(error, abs(sum_side_weights(quadrature_points, num_points, .true.) &
                              - (0.5d0 - minus_area)) < 1.0d-13, &
                   "Plus side must equal 0.5 - 1/18 exactly")
    end subroutine test_cut_triangle_exact_split

    ! =========================================================================
    ! Group 6c. Uncut quad: total = reference area 4.0 on the minus side.
    ! =========================================================================
    subroutine test_uncut_quad(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(SUBCELL_QUADRATURE_CAPACITY)
        integer(int32) :: num_points
        real(real64) :: phi_nodes(4)

        fe = create_fe(FE_TYPE%QUAD%ID, 1)

        phi_nodes = [-1.0d0, -1.0d0, -1.0d0, -1.0d0]
        num_points = 0
        call build_interface_quadrature_points(fe, phi_nodes, quadrature_points, num_points)

        call check(error, num_points > 0, "Expected quadrature points for uncut quad")
        if (allocated(error)) return
        call check(error, abs(sum_side_weights(quadrature_points, num_points, .false.) - 4.0d0) < 1.0d-12, &
                   "Minus side should carry the whole reference area 4.0")
    end subroutine test_uncut_quad

    ! =========================================================================
    ! Group 6d. Nodal phi = [-1, 1, 1, -1] (VTK corner order) is phi = xi, so
    ! [-1,1]^2 splits along xi = 0 into two halves of area 2.
    ! =========================================================================
    subroutine test_cut_quad_split(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(SUBCELL_QUADRATURE_CAPACITY)
        integer(int32) :: num_points
        real(real64) :: phi_nodes(4)

        fe = create_fe(FE_TYPE%QUAD%ID, 1)

        phi_nodes = [-1.0d0, 1.0d0, 1.0d0, -1.0d0]
        num_points = 0
        call build_interface_quadrature_points(fe, phi_nodes, quadrature_points, num_points)

        call check(error, num_points > 0, "Expected quadrature points for cut quad")
        if (allocated(error)) return
        call check(error, abs(sum_side_weights(quadrature_points, num_points, .true.) - 2.0d0) < 1.0d-12, &
                   "Plus side of the quad must have area 2")
        if (allocated(error)) return
        call check(error, abs(sum_side_weights(quadrature_points, num_points, .false.) - 2.0d0) < 1.0d-12, &
                   "Minus side of the quad must have area 2")
    end subroutine test_cut_quad_split

    ! =========================================================================
    ! Group 6e. A side-wise-constant coefficient must be integrated exactly on
    ! a cut element with a straight interface.
    ! =========================================================================
    subroutine test_sidewise_constant_integration(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(SUBCELL_QUADRATURE_CAPACITY)
        integer(int32) :: num_points, point
        real(real64) :: phi_nodes(3), integral, expected, minus_area
        real(real64), parameter :: coefficient_minus = 3.0d0, coefficient_plus = 7.0d0

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)

        phi_nodes = [-0.5d0, 1.0d0, 1.0d0]
        minus_area = 0.5d0 * (1.0d0 / 3.0d0)**2
        expected = coefficient_minus * minus_area + coefficient_plus * (0.5d0 - minus_area)

        num_points = 0
        call build_interface_quadrature_points(fe, phi_nodes, quadrature_points, num_points)

        integral = 0.0d0
        do point = 1, num_points
            if (quadrature_points(point)%is_plus_side) then
                integral = integral + coefficient_plus * quadrature_points(point)%weight
            else
                integral = integral + coefficient_minus * quadrature_points(point)%weight
            end if
        end do

        call check(error, abs(integral - expected) < 1.0d-12, &
                   "Side-wise-constant coefficient must be integrated exactly")
    end subroutine test_sidewise_constant_integration

    ! =========================================================================
    ! Group 6f. Flux residual on a cut triangle: R_i = int grad(N_i) . V, the
    ! discrete form used for gravity.  The pressure, temperature and gravity
    ! terms must use this identical partition when their coefficient changes at
    ! the freezing interface.
    ! =========================================================================
    subroutine test_sidewise_flux_residual(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature_point) :: quadrature_points(SUBCELL_QUADRATURE_CAPACITY)
        type(type_coordinate_dp) :: reference_point
        integer(int32) :: num_points, point, i
        real(real64) :: phi_nodes(3), node_coordinates(2, 3), shape_gradients(2, 3), determinant_jacobian
        real(real64) :: flux(2), residual(3), expected(3)

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)
        node_coordinates = reshape([0.0d0, 0.0d0, 1.0d0, 0.0d0, 0.0d0, 1.0d0], shape(node_coordinates))
        phi_nodes = [-0.5d0, 1.0d0, 1.0d0]
        num_points = 0
        call build_interface_quadrature_points(fe, phi_nodes, quadrature_points, num_points)

        residual = 0.0d0
        do point = 1, num_points
            reference_point%x = quadrature_points(point)%xi
            reference_point%y = quadrature_points(point)%eta
            reference_point%z = 0.0d0
            shape_gradients = 0.0d0
            call fe%calc_shape_function(reference_point, node_coordinates, dpsi_dx=shape_gradients, &
                                        determinant_jacobian=determinant_jacobian)
            if (quadrature_points(point)%is_plus_side) then
                flux = [3.0d0, -1.0d0]
            else
                flux = [1.0d0, 2.0d0]
            end if
            do i = 1, 3
                residual(i) = residual(i) + quadrature_points(point)%weight * abs(determinant_jacobian) &
                              * dot_product(shape_gradients(:, i), flux)
            end do
        end do

        expected = [-19.0d0 / 18.0d0, 25.0d0 / 18.0d0, -1.0d0 / 3.0d0]
        call check(error, maxval(abs(residual - expected)) < 1.0d-12, &
                   "Interface-split flux residual must match its analytic sidewise integral")
        if (allocated(error)) return
        call check(error, abs(sum(residual)) < 1.0d-12, &
                   "Internal cut-element flux residual must conserve its nodal sum")
    end subroutine test_sidewise_flux_residual

    ! =========================================================================
    ! Group 7a. calc_psi_ice: at T = 0degC, psi_ice should be zero.
    ! =========================================================================
    subroutine test_psi_ice_at_freezing(error)
        type(error_type), allocatable, intent(inout) :: error

        real(real64) :: psi

        psi = 0.0d0
        call calc_psi_ice(0.0d0, psi)
        call check(error, abs(psi) < 1.0d-12, "psi_ice should be 0 at T = 0degC")
    end subroutine test_psi_ice_at_freezing

    ! =========================================================================
    ! Group 7b. calc_T_high_celsius: at P_w = 0 and rho_w = 1000, T_high = 0degC.
    ! =========================================================================
    subroutine test_T_high_zero_pressure(error)
        type(error_type), allocatable, intent(inout) :: error

        real(real64) :: T_high

        T_high = -99.0d0
        call calc_T_high_celsius(0.0d0, 1000.0d0, T_high)
        call check(error, abs(T_high) < 1.0d-10, "T_high should be 0degC at P_w = 0")
    end subroutine test_T_high_zero_pressure

end program test_fe_subcell
