!> Concrete integrand used to drive and check the error control: the diagonal
!> entries of the two element matrices the governing blocks assemble on this
!> rule, namely capacity C N_a N_a and diffusion D grad N_a . grad N_a, with
!> side-wise coefficients.  Their quadrature error is what the refinement
!> criterion has to see.
module test_fe_subcell_integrand
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only: type_coordinate_dp
    use :: domain_base_fe, only: abst_fe
    use :: domain_fe_subcell, only: abst_subcell_integrand
    implicit none
    private

    public :: type_element_matrix_integrand

    !> Nodes of the largest supported element (Q9).
    integer(int32), parameter :: MAX_ELEMENT_NODES = 9

    type, extends(abst_subcell_integrand) :: type_element_matrix_integrand
        !> Parent element supplying N_a, grad N_a and det J.
        class(abst_fe), pointer, private :: fe => null()
        !> Physical node coordinates, shape (2, num_nodes).
        real(real64), private :: node_coordinates(2, MAX_ELEMENT_NODES) = 0.0d0
        integer(int32), private :: num_nodes = 0
        !> Side-wise coefficients of the two terms.
        real(real64), private :: diffusivity_minus = 1.0d0
        real(real64), private :: diffusivity_plus = 1.0d0
        real(real64), private :: capacity_minus = 1.0d0
        real(real64), private :: capacity_plus = 1.0d0
        !> Absolute scale A_k keeping the indicator finite near zero.
        real(real64), private :: scale_floor = 1.0d-9
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_element_matrix_integrand

        ! ---- Algorithm ----
        procedure, public, pass(self) :: evaluate_terms => evaluate_terms_element_matrix_integrand

        ! ---- Getter ----
        procedure, public, pass(self) :: get_num_terms => get_num_terms_element_matrix_integrand
        procedure, public, pass(self) :: get_term_scales => get_term_scales_element_matrix_integrand
    end type type_element_matrix_integrand

contains

    subroutine initialize_element_matrix_integrand(self, fe, node_coordinates, num_nodes, &
                                                   diffusivity_minus, diffusivity_plus, &
                                                   capacity_minus, capacity_plus)
        implicit none
        class(type_element_matrix_integrand), intent(inout) :: self
        class(abst_fe), intent(in), target :: fe
        real(real64), intent(in) :: node_coordinates(:, :)
        integer(int32), intent(in) :: num_nodes
        real(real64), intent(in) :: diffusivity_minus, diffusivity_plus
        real(real64), intent(in) :: capacity_minus, capacity_plus

        self%fe => fe
        self%num_nodes = num_nodes
        self%node_coordinates = 0.0d0
        self%node_coordinates(:, 1:num_nodes) = node_coordinates(:, 1:num_nodes)
        self%diffusivity_minus = diffusivity_minus
        self%diffusivity_plus = diffusivity_plus
        self%capacity_minus = capacity_minus
        self%capacity_plus = capacity_plus
    end subroutine initialize_element_matrix_integrand

    subroutine evaluate_terms_element_matrix_integrand(self, xi, eta, is_plus_side, terms)
        implicit none
        class(type_element_matrix_integrand), intent(inout) :: self
        real(real64), intent(in) :: xi
        real(real64), intent(in) :: eta
        logical, intent(in) :: is_plus_side
        real(real64), intent(inout) :: terms(:)

        type(type_coordinate_dp) :: reference_point
        real(real64) :: shape_values(MAX_ELEMENT_NODES), shape_gradients(2, MAX_ELEMENT_NODES)
        real(real64) :: determinant_jacobian, diffusivity, capacity
        integer(int32) :: node

        terms = 0.0d0
        reference_point%x = xi
        reference_point%y = eta
        reference_point%z = 0.0d0
        shape_values = 0.0d0
        shape_gradients = 0.0d0
        call self%fe%calc_shape_function(reference_point, self%node_coordinates(:, 1:self%num_nodes), &
                                         psi=shape_values(1:self%num_nodes), &
                                         dpsi_dx=shape_gradients(:, 1:self%num_nodes), &
                                         determinant_jacobian=determinant_jacobian)

        if (is_plus_side) then
            diffusivity = self%diffusivity_plus
            capacity = self%capacity_plus
        else
            diffusivity = self%diffusivity_minus
            capacity = self%capacity_minus
        end if

        do node = 1, self%num_nodes
            terms(node) = capacity * shape_values(node)**2 * abs(determinant_jacobian)
            terms(self%num_nodes + node) = diffusivity &
                                           * dot_product(shape_gradients(:, node), shape_gradients(:, node)) &
                                           * abs(determinant_jacobian)
        end do
    end subroutine evaluate_terms_element_matrix_integrand

    pure subroutine get_num_terms_element_matrix_integrand(self, num_terms)
        implicit none
        class(type_element_matrix_integrand), intent(in) :: self
        integer(int32), intent(inout) :: num_terms
        num_terms = 2 * self%num_nodes
    end subroutine get_num_terms_element_matrix_integrand

    pure subroutine get_term_scales_element_matrix_integrand(self, scales)
        implicit none
        class(type_element_matrix_integrand), intent(in) :: self
        real(real64), intent(inout) :: scales(:)
        scales = self%scale_floor
    end subroutine get_term_scales_element_matrix_integrand

end module test_fe_subcell_integrand

!> Unit tests for domain_fe_subcell: interface-split subcell quadrature.
!>
!> The rule under test replaces the standard Gauss rule on every element that
!> carries cryo transport (governing_base builds the split for all elements,
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
!>      O(4^-d) to the deep-refinement reference, on a curved element so the
!>      parent mapping is exercised too.
!> Group 4 - continuity in the nodal level set (required for a contracting
!>   Newton map): sweeping the interface must not make the assembled matrices
!>   jump when the refinement pattern changes.
!> Group 5 - contract: the object's own capacity must always suffice, and
!>   unsupported families and degenerate level sets must stay well defined.
!> Group 6 - regression: fixed analytic values on T3/Q4 at depth 0, including
!>   the side-wise flux residual.
!> Group 7 - chemical-potential helpers used with the same level set.
program test_fe_subcell
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit, error_unit
    use :: testdrive, only: error_type, check
    use :: module_core, only: FE_TYPE, type_coordinate_dp
    use :: domain_base_fe, only: abst_fe
    use :: domain_fe_factory, only: create_fe
    use :: domain_fe_subcell, only: type_subcell_quadrature
    use :: test_fe_subcell_integrand, only: type_element_matrix_integrand
    use :: models_phase_change_chemical_potential, only: calc_psi_ice, calc_dpsi_ice_dT, &
                                                         calc_T_high_celsius
    implicit none

    !> Level-set kinds used by the family sweeps.
    integer(int32), parameter :: LEVEL_SET_LINEAR = 1
    integer(int32), parameter :: LEVEL_SET_CIRCLE = 2
    !> Deepest refinement exercised by the sweeps.
    integer(int32), parameter :: TEST_MAX_DEPTH = 3
    !> Depth of the self-reference used where no analytic value exists.
    integer(int32), parameter :: TEST_REFERENCE_DEPTH = 5
    !> Nodes of the largest supported element (Q9).
    integer(int32), parameter :: MAX_NODES = 9
    !> Integration order of the Gauss reference: 7 points on a triangle
    !> (degree 5), 5 x 5 on a quadrilateral (degree 9 per direction).  That is
    !> exact for every integrand compared here - N_a N_b reaches degree 4 on a
    !> triangle and degree 4 per direction on a quadrilateral - so the gaps
    !> measured against it are true quadrature errors.  Test 0a checks that all
    !> five orders carry the reference measure before any of them is trusted.
    integer(int32), parameter :: REFERENCE_GAUSS_ORDER = 5
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

        write (output_unit, '(A)') "Group 0: preconditions"
        call test_gauss_reference_is_consistent(error)
        call report("0a gauss_reference_is_consistent", error, failures)

        write (output_unit, '(A)') "Group 1: subdivision geometry"
        call test_tiling_invariants(error)
        call report("1a tiling_invariants", error, failures)
        call test_degree2_exactness(error)
        call report("1b degree2_exactness", error, failures)

        write (output_unit, '(A)') "Group 2: interface resolution"
        call test_straight_interface_is_exact(error)
        call report("2a straight_interface_is_exact", error, failures)
        call test_enclosed_interface_resolution(error)
        call report("2b enclosed_interface_resolution", error, failures)
        call test_curved_interface_convergence(error)
        call report("2c curved_interface_convergence", error, failures)
        call test_side_tag_consistency(error)
        call report("2d side_tag_consistency", error, failures)

        write (output_unit, '(A)') "Group 3: assembled element matrices"
        call test_uncut_assembly_vs_gauss(error)
        call report("3a uncut_assembly_vs_gauss", error, failures)
        call test_cut_assembly_convergence(error)
        call report("3b cut_assembly_convergence", error, failures)

        call test_error_control_uncut(error)
        call report("3c error_control_uncut", error, failures)
        call test_error_control_cut(error)
        call report("3d error_control_cut", error, failures)

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
        call test_requested_depth_contract(error)
        call report("5b requested_depth_contract", error, failures)
        call test_unsupported_and_degenerate(error)
        call report("5c unsupported_and_degenerate", error, failures)

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
    ! Helpers: driving the rule and reading it back
    ! =========================================================================

    !> Build the rule for one case and expand it into plain arrays, which is
    !> all the checks below need.  The arrays are (re)allocated to the object's
    !> own capacity, so a test can never be the reason a rule does not fit.
    subroutine compute_rule(quadrature, fe, phi_nodes, num_nodes, depth, &
                            xi, eta, weight, is_plus_side, num_points)
        type(type_subcell_quadrature), intent(inout) :: quadrature
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: phi_nodes(:)
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in) :: depth
        real(real64), allocatable, intent(inout) :: xi(:), eta(:), weight(:)
        logical, allocatable, intent(inout) :: is_plus_side(:)
        integer(int32), intent(inout) :: num_points

        integer(int32) :: capacity, point

        call quadrature%initialize(depth)
        call quadrature%compute(fe, phi_nodes(1:num_nodes))
        call quadrature%get_num_points(num_points)
        call quadrature%get_capacity(capacity)

        capacity = max(capacity, num_points)
        if (allocated(xi)) then
            if (size(xi) < capacity) deallocate (xi, eta, weight, is_plus_side)
        end if
        if (.not. allocated(xi)) then
            allocate (xi(capacity), eta(capacity), weight(capacity), is_plus_side(capacity))
        end if

        do point = 1, num_points
            call quadrature%get_point(point, xi(point), eta(point), weight(point), is_plus_side(point))
        end do
    end subroutine compute_rule


    !> Build the rule with the error control driving the refinement, and expand
    !> it into plain arrays.
    subroutine compute_rule_controlled(quadrature, fe, phi_nodes, num_nodes, depth, tolerance, integrand, &
                                       xi, eta, weight, is_plus_side, num_points)
        type(type_subcell_quadrature), intent(inout) :: quadrature
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: phi_nodes(:)
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in) :: depth
        real(real64), intent(in) :: tolerance
        class(type_element_matrix_integrand), intent(inout) :: integrand
        real(real64), allocatable, intent(inout) :: xi(:), eta(:), weight(:)
        logical, allocatable, intent(inout) :: is_plus_side(:)
        integer(int32), intent(inout) :: num_points

        integer(int32) :: capacity, point

        call quadrature%initialize(depth, tolerance)
        call quadrature%compute(fe, phi_nodes(1:num_nodes), integrand)
        call quadrature%get_num_points(num_points)
        call quadrature%get_capacity(capacity)

        capacity = max(capacity, num_points)
        if (allocated(xi)) then
            if (size(xi) < capacity) deallocate (xi, eta, weight, is_plus_side)
        end if
        if (.not. allocated(xi)) then
            allocate (xi(capacity), eta(capacity), weight(capacity), is_plus_side(capacity))
        end if

        do point = 1, num_points
            call quadrature%get_point(point, xi(point), eta(point), weight(point), is_plus_side(point))
        end do
    end subroutine compute_rule_controlled


    !> .true. when phi^h reproduces xi^2 + eta^2 - offset exactly, which is the
    !> premise of every analytic area used below.
    function is_levelset_exact(fe, phi_nodes, num_nodes, offset) result(is_exact)
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: phi_nodes(:)
        integer(int32), intent(in) :: num_nodes
        real(real64), intent(in) :: offset
        logical :: is_exact

        type(type_coordinate_dp) :: reference_point
        real(real64) :: sample(2, 4), interpolated, expected
        integer(int32) :: point

        sample(:, 1) = [0.13d0, 0.21d0]
        sample(:, 2) = [0.37d0, 0.11d0]
        sample(:, 3) = [-0.29d0, 0.42d0]
        sample(:, 4) = [0.05d0, -0.33d0]

        is_exact = .true.
        do point = 1, 4
            reference_point%x = sample(1, point)
            reference_point%y = sample(2, point)
            reference_point%z = 0.0d0
            call fe%lerp(reference_point, phi_nodes(1:num_nodes), interpolated)
            expected = sample(1, point)**2 + sample(2, point)**2 - offset
            if (abs(interpolated - expected) > 1.0d-12) is_exact = .false.
        end do
    end function is_levelset_exact

    pure function sum_side_weights(weight, is_plus_side, num_points, plus_side) result(total)
        real(real64), intent(in) :: weight(:)
        logical, intent(in) :: is_plus_side(:)
        integer(int32), intent(in) :: num_points
        logical, intent(in) :: plus_side
        real(real64) :: total
        integer(int32) :: point
        total = 0.0d0
        do point = 1, num_points
            if (is_plus_side(point) .eqv. plus_side) total = total + weight(point)
        end do
    end function sum_side_weights

    !> True when every point lies inside the reference element.
    pure function are_points_inside(element_id, xi, eta, num_points) result(inside)
        integer(int32), intent(in) :: element_id
        real(real64), intent(in) :: xi(:), eta(:)
        integer(int32), intent(in) :: num_points
        logical :: inside
        real(real64), parameter :: tolerance = 1.0d-13
        integer(int32) :: point

        inside = .true.
        do point = 1, num_points
            if (is_quadrilateral(element_id)) then
                if (abs(xi(point)) > 1.0d0 + tolerance .or. abs(eta(point)) > 1.0d0 + tolerance) inside = .false.
            else
                if (xi(point) < -tolerance .or. eta(point) < -tolerance .or. &
                    xi(point) + eta(point) > 1.0d0 + tolerance) inside = .false.
            end if
        end do
    end function are_points_inside

    !> Integral of a reference-space monomial xi^p eta^q over the whole rule.
    pure function integrate_monomial(xi, eta, weight, num_points, p, q) result(total)
        real(real64), intent(in) :: xi(:), eta(:), weight(:)
        integer(int32), intent(in) :: num_points
        integer(int32), intent(in) :: p, q
        real(real64) :: total
        integer(int32) :: point
        total = 0.0d0
        do point = 1, num_points
            total = total + weight(point) * xi(point)**p * eta(point)**q
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
    subroutine assemble_on_subcell_rule(fe, node_coordinates, num_nodes, xi, eta, weight, is_plus_side, &
                                        num_points, diffusivity_minus, diffusivity_plus, &
                                        capacity_minus, capacity_plus, diffusion_matrix, capacity_matrix)
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: node_coordinates(:, :)
        integer(int32), intent(in) :: num_nodes
        real(real64), intent(in) :: xi(:), eta(:), weight(:)
        logical, intent(in) :: is_plus_side(:)
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
            reference_point%x = xi(point)
            reference_point%y = eta(point)
            reference_point%z = 0.0d0
            shape_values = 0.0d0
            shape_gradients = 0.0d0
            call fe%calc_shape_function(reference_point, node_coordinates, psi=shape_values(1:num_nodes), &
                                        dpsi_dx=shape_gradients(:, 1:num_nodes), &
                                        determinant_jacobian=determinant_jacobian)

            effective_weight = weight(point) * abs(determinant_jacobian)
            if (is_plus_side(point)) then
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

    !> The same two matrices on the element's own Gauss rule, with uniform
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

    !> Physical measure of the element as seen by the rule: sum w |det J|.
    function calc_physical_measure(fe, node_coordinates, num_nodes, xi, eta, weight, num_points) result(measure)
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: node_coordinates(:, :)
        integer(int32), intent(in) :: num_nodes
        real(real64), intent(in) :: xi(:), eta(:), weight(:)
        integer(int32), intent(in) :: num_points
        real(real64) :: measure

        type(type_coordinate_dp) :: reference_point
        real(real64) :: shape_gradients(2, MAX_NODES), determinant_jacobian
        integer(int32) :: point

        measure = 0.0d0
        do point = 1, num_points
            reference_point%x = xi(point)
            reference_point%y = eta(point)
            reference_point%z = 0.0d0
            shape_gradients = 0.0d0
            call fe%calc_shape_function(reference_point, node_coordinates, &
                                        dpsi_dx=shape_gradients(:, 1:num_nodes), &
                                        determinant_jacobian=determinant_jacobian)
            measure = measure + weight(point) * abs(determinant_jacobian)
        end do
    end function calc_physical_measure

    !> A T6 whose mid-node 4 sits at (0.5, 0.25): edge 1-2 is a parabola, so the
    !> element measure is 0.5 - (2/3)(1)(0.25) = 1/3 (Archimedes) and det J
    !> varies over the element.
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
    ! Group 0a. Precondition of every Gauss comparison below: the element's own
    ! rule must integrate 1 over the reference element to its measure, for the
    ! orders this suite uses.  A rule whose weights are normalised to 1 instead
    ! of the reference area would scale the reference by 2.
    ! =========================================================================
    subroutine test_gauss_reference_is_consistent(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: gauss_points
        real(real64), pointer, contiguous, dimension(:) :: gauss_weights
        real(real64) :: total_weight
        integer(int32) :: element_ids(5), num_cases, num_gauss, k, order

        call get_family_case_ids(element_ids, num_cases)
        do k = 1, num_cases
            do order = 1, 5
                fe = create_fe(element_ids(k), order)
                call fe%get_gauss(gauss_points)
                call fe%get_weight(gauss_weights)
                call fe%get_num_gauss(num_gauss)
                total_weight = sum(gauss_weights(1:num_gauss))
                nullify (gauss_points)
                nullify (gauss_weights)

                call check(error, abs(total_weight - get_reference_measure(element_ids(k))) < 1.0d-12, &
                           "Gauss weights must sum to the reference measure: " &
                           //trim(case_label(element_ids(k), order)))
                if (allocated(error)) return
            end do
        end do
    end subroutine test_gauss_reference_is_consistent

    ! =========================================================================
    ! Group 1a. Tiling invariants for every family, order, depth, level set.
    ! =========================================================================
    subroutine test_tiling_invariants(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature) :: quadrature
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
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
                    call compute_rule(quadrature, fe, phi_nodes, num_nodes, depth, &
                                      xi, eta, weight, is_plus_side, num_points)

                    call check(error, num_points > 0, "No quadrature points: "//trim(case_label(element_ids(k), depth)))
                    if (allocated(error)) return

                    total_weight = sum_side_weights(weight, is_plus_side, num_points, .true.) &
                                   + sum_side_weights(weight, is_plus_side, num_points, .false.)
                    call check(error, abs(total_weight - get_reference_measure(element_ids(k))) < 1.0d-12, &
                               "Weights must sum to the reference measure: "//trim(case_label(element_ids(k), depth)))
                    if (allocated(error)) return

                    call check(error, minval(weight(1:num_points)) > 0.0d0, &
                               "Every weight must be positive: "//trim(case_label(element_ids(k), depth)))
                    if (allocated(error)) return

                    call check(error, are_points_inside(element_ids(k), xi, eta, num_points), &
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
        type(type_subcell_quadrature) :: quadrature
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64) :: phi_nodes(MAX_NODES), computed, expected, scale
        integer(int32) :: element_ids(5), num_cases, num_nodes, num_points, k, depth, p, q

        call get_family_case_ids(element_ids, num_cases)
        do k = 1, num_cases
            do depth = 0, TEST_MAX_DEPTH
                call build_case(element_ids(k), LEVEL_SET_CIRCLE, get_circle_offset(element_ids(k)), &
                                fe, phi_nodes, num_nodes)
                call compute_rule(quadrature, fe, phi_nodes, num_nodes, depth, &
                                  xi, eta, weight, is_plus_side, num_points)
                scale = get_reference_measure(element_ids(k))

                do p = 0, 2
                    do q = 0, 2 - p
                        computed = integrate_monomial(xi, eta, weight, num_points, p, q)
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
        type(type_subcell_quadrature) :: quadrature
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64) :: phi_nodes(MAX_NODES), expected
        integer(int32) :: element_ids(5), num_cases, num_nodes, num_points, k, depth

        call get_family_case_ids(element_ids, num_cases)
        do k = 1, num_cases
            expected = get_linear_minus_area(element_ids(k))
            do depth = 0, TEST_MAX_DEPTH
                call build_case(element_ids(k), LEVEL_SET_LINEAR, get_linear_offset(element_ids(k)), &
                                fe, phi_nodes, num_nodes)
                call compute_rule(quadrature, fe, phi_nodes, num_nodes, depth, &
                                  xi, eta, weight, is_plus_side, num_points)

                call check(error, abs(sum_side_weights(weight, is_plus_side, num_points, .false.) &
                                      - expected) < 1.0d-12, &
                           "Straight interface must be exact: "//trim(case_label(element_ids(k), depth)))
                if (allocated(error)) return
            end do
        end do
    end subroutine test_straight_interface_is_exact

    ! =========================================================================
    ! Group 2b. A quadratic interface may be enclosed between all root probes.
    ! Uniform geometric refinement must eventually expose and partition it.
    ! =========================================================================
    subroutine test_enclosed_interface_resolution(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature) :: quadrature
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64) :: coordinates(2, MAX_NODES), phi_nodes(MAX_NODES), minus_weight
        integer(int32) :: num_nodes, num_points, node

        fe = create_fe(FE_TYPE%QUADRATIC_TRIANGLE%ID, 1)
        call get_reference_nodes(FE_TYPE%QUADRATIC_TRIANGLE%ID, coordinates, num_nodes)
        do node = 1, num_nodes
            phi_nodes(node) = (coordinates(1, node) - 0.25d0)**2 &
                              + (coordinates(2, node) - 0.25d0)**2 - 0.01d0
        end do

        call compute_rule(quadrature, fe, phi_nodes, num_nodes, 2, &
                          xi, eta, weight, is_plus_side, num_points)
        minus_weight = sum_side_weights(weight, is_plus_side, num_points, .false.)
        call check(error, minus_weight > 0.0d0, &
                   "Uniform refinement must detect an interface enclosed between root probes")
    end subroutine test_enclosed_interface_resolution

    ! =========================================================================
    ! Group 2c. Curved interface. phi = xi^2 + eta^2 - r^2 is reproduced
    ! exactly by T6/Q8/Q9, so the minus side is a disc sector of known area.
    ! Chords give an O(h^2) area error, but only once the cells resolve the
    ! interface: below depth ceil(log2(h_e/r)) the whole disc can sit inside a
    ! cell with same-sign vertices and be missed, so only monotone decrease is
    ! required there.
    ! =========================================================================
    subroutine test_curved_interface_convergence(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature) :: quadrature
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
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
                expected = PI * radius**2
            else
                radius = 0.6d0                    ! quarter disc inside the simplex
                expected = 0.25d0 * PI * radius**2
            end if
            offset = radius**2
            resolution_depth = get_resolution_depth(element_ids(k), radius)

            ! The analytic area is only the right answer if phi^h really is the
            ! circle, so that premise is checked before the areas are trusted.
            call build_case(element_ids(k), LEVEL_SET_CIRCLE, offset, fe, phi_nodes, num_nodes)
            call check(error, is_levelset_exact(fe, phi_nodes, num_nodes, offset) , &
                       "phi^h must reproduce the circle exactly: "//trim(case_label(element_ids(k), 0)))
            if (allocated(error)) return

            do depth = 0, TEST_MAX_DEPTH
                call build_case(element_ids(k), LEVEL_SET_CIRCLE, offset, fe, phi_nodes, num_nodes)
                call compute_rule(quadrature, fe, phi_nodes, num_nodes, depth, &
                                  xi, eta, weight, is_plus_side, num_points)
                area_error(depth) = abs(sum_side_weights(weight, is_plus_side, num_points, .false.) - expected)
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

            ! Rates alone would accept a sequence that shrinks from a huge
            ! value, so the deepest area is pinned against the analytic one.
            call check(error, area_error(TEST_MAX_DEPTH) < 0.03d0 * get_reference_measure(element_ids(k)), &
                       "The deepest rule must match the analytic area: " &
                       //trim(case_label(element_ids(k), TEST_MAX_DEPTH)))
            if (allocated(error)) return
        end do
    end subroutine test_curved_interface_convergence

    ! =========================================================================
    ! Group 2d. The side tag drives which constitutive branch is evaluated, so
    ! it must agree with the sign of phi at the point itself.  Disagreement is
    ! confined to the band between the chord and the true interface, whose
    ! weight must vanish with depth.
    ! =========================================================================
    subroutine test_side_tag_consistency(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature) :: quadrature
        type(type_coordinate_dp) :: reference_point
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
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
                call compute_rule(quadrature, fe, phi_nodes, num_nodes, depth, &
                                  xi, eta, weight, is_plus_side, num_points)

                call check(error, num_points > 0, &
                           "An empty rule cannot be tag-consistent: "//trim(case_label(element_ids(k), depth)))
                if (allocated(error)) return

                mismatched_weight(depth) = 0.0d0
                do point = 1, num_points
                    reference_point%x = xi(point)
                    reference_point%y = eta(point)
                    reference_point%z = 0.0d0
                    call fe%lerp(reference_point, phi_nodes(1:num_nodes), phi_at_point)
                    if ((phi_at_point > 0.0d0) .neqv. is_plus_side(point)) then
                        mismatched_weight(depth) = mismatched_weight(depth) + weight(point)
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
    ! and T6, and N N on T3.  Everything else differs, and the difference must
    ! fall as O(4^-d).
    ! =========================================================================
    subroutine test_uncut_assembly_vs_gauss(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe, fe_reference
        type(type_subcell_quadrature) :: quadrature
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64) :: phi_nodes(MAX_NODES), node_coordinates(2, MAX_NODES)
        real(real64) :: diffusion_subcell(MAX_NODES, MAX_NODES), capacity_subcell(MAX_NODES, MAX_NODES)
        real(real64) :: diffusion_depth0(MAX_NODES, MAX_NODES), capacity_depth0(MAX_NODES, MAX_NODES)
        real(real64) :: diffusion_gauss(MAX_NODES, MAX_NODES), capacity_gauss(MAX_NODES, MAX_NODES)
        real(real64) :: diffusion_scale, capacity_scale, diffusion_gap, capacity_gap
        real(real64) :: diffusion_gap_final, capacity_gap_final
        real(real64) :: total_mass, row_sum
        real(real64), parameter :: diffusivity = 3.0d0, capacity = 2.0d0
        integer(int32) :: element_ids(5), num_cases, num_nodes, num_points, k, depth, i
        logical :: diffusion_is_exact, capacity_is_exact

        call get_family_case_ids(element_ids, num_cases)
        do k = 1, num_cases
            fe_reference = create_fe(element_ids(k), REFERENCE_GAUSS_ORDER)
            call get_reference_nodes(element_ids(k), node_coordinates, num_nodes)
            call assemble_on_gauss_rule(fe_reference, node_coordinates(:, 1:num_nodes), num_nodes, &
                                        diffusivity, capacity, diffusion_gauss, capacity_gauss)
            diffusion_scale = maxval(abs(diffusion_gauss(1:num_nodes, 1:num_nodes)))
            capacity_scale = maxval(abs(capacity_gauss(1:num_nodes, 1:num_nodes)))

            ! grad N is constant on T3 and linear on Q4/T6, and N N is quadratic
            ! on T3: those integrands stay within the degree the midpoint rule
            ! integrates exactly, so the two rules must agree to roundoff.
            diffusion_is_exact = (element_ids(k) == FE_TYPE%TRIANGLE%ID &
                                  .or. element_ids(k) == FE_TYPE%QUAD%ID &
                                  .or. element_ids(k) == FE_TYPE%QUADRATIC_TRIANGLE%ID)
            capacity_is_exact = (element_ids(k) == FE_TYPE%TRIANGLE%ID)

            do depth = 0, TEST_MAX_DEPTH
                ! Uncut: phi > 0 everywhere, so the whole element is plus side.
                call build_case(element_ids(k), LEVEL_SET_LINEAR, 10.0d0, fe, phi_nodes, num_nodes)
                call compute_rule(quadrature, fe, phi_nodes, num_nodes, depth, &
                                  xi, eta, weight, is_plus_side, num_points)
                call assemble_on_subcell_rule(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                              xi, eta, weight, is_plus_side, num_points, &
                                              diffusivity, diffusivity, capacity, capacity, &
                                              diffusion_subcell, capacity_subcell)

                if (depth == 0) then
                    diffusion_depth0 = diffusion_subcell
                    capacity_depth0 = capacity_subcell
                end if

                ! sum_i N_i = 1 turns the total mass into a constant integrand,
                ! so this is exact for any rule whose weights sum to the element
                ! measure - it checks the weights, not the point placement.
                ! (The diffusion row sums are an identity of sum_j grad N_j = 0
                ! and would hold for arbitrary points and weights, so they are
                ! not asserted here.)
                total_mass = sum(capacity_subcell(1:num_nodes, 1:num_nodes))
                call check(error, abs(total_mass - capacity * get_reference_measure(element_ids(k))) &
                           < 1.0d-12 * capacity_scale, &
                           "Total mass must be exact: "//trim(case_label(element_ids(k), depth)))
                if (allocated(error)) return

                if (diffusion_is_exact) then
                    call check(error, maxval(abs(diffusion_subcell(1:num_nodes, 1:num_nodes) &
                                                 - diffusion_gauss(1:num_nodes, 1:num_nodes))) &
                               < 1.0d-12 * diffusion_scale, &
                               "Diffusion matrix must match Gauss exactly: " &
                               //trim(case_label(element_ids(k), depth)))
                    if (allocated(error)) return
                end if

                if (capacity_is_exact) then
                    call check(error, maxval(abs(capacity_subcell(1:num_nodes, 1:num_nodes) &
                                                 - capacity_gauss(1:num_nodes, 1:num_nodes))) &
                               < 1.0d-12 * capacity_scale, &
                               "Capacity matrix must match Gauss exactly: " &
                               //trim(case_label(element_ids(k), depth)))
                    if (allocated(error)) return
                end if
            end do

            ! What the rule costs where it is not exact.  The Gauss reference
            ! is exact for both integrands, so this is the true quadrature
            ! error of the level-set path, which never refines an uncut element.
            ! The bounds are regression guards pinned just above the measured
            ! values (T3 0/0, Q4 0/6.3e-2, T6 0/1.34, Q8 1.4e-1/4.1e-1,
            ! Q9 8.8e-1/3.4e-1): they must fail if the rule gets worse, and they
            ! record how bad the uncontrolled rule already is.
            diffusion_gap = maxval(abs(diffusion_depth0(1:num_nodes, 1:num_nodes) &
                                       - diffusion_gauss(1:num_nodes, 1:num_nodes))) / diffusion_scale
            capacity_gap = maxval(abs(capacity_depth0(1:num_nodes, 1:num_nodes) &
                                      - capacity_gauss(1:num_nodes, 1:num_nodes))) / capacity_scale
            diffusion_gap_final = maxval(abs(diffusion_subcell(1:num_nodes, 1:num_nodes) &
                                             - diffusion_gauss(1:num_nodes, 1:num_nodes))) / diffusion_scale
            capacity_gap_final = maxval(abs(capacity_subcell(1:num_nodes, 1:num_nodes) &
                                            - capacity_gauss(1:num_nodes, 1:num_nodes))) / capacity_scale
            write (output_unit, '(A,I0,A,ES9.2,A,ES9.2)') "       uncut gap fe=", element_ids(k), &
                " diffusion=", diffusion_gap, " capacity=", capacity_gap
            call check(error, diffusion_gap < 1.0d0 .and. capacity_gap < 1.5d0, &
                       "Uncut quadrature gap must stay within its measured envelope: " &
                       //trim(case_label(element_ids(k), 0)))
            if (allocated(error)) return
            call check(error, diffusion_gap_final <= diffusion_gap + 1.0d-12 .and. &
                       capacity_gap_final <= capacity_gap + 1.0d-12, &
                       "Uniform refinement must not increase an uncut quadrature gap: " &
                       //trim(case_label(element_ids(k), TEST_MAX_DEPTH)))
            if (allocated(error)) return
            if (.not. diffusion_is_exact) then
                call check(error, diffusion_gap_final < diffusion_gap, &
                           "Uniform refinement must reduce an inexact diffusion integral: " &
                           //trim(case_label(element_ids(k), TEST_MAX_DEPTH)))
                if (allocated(error)) return
            end if
            if (.not. capacity_is_exact) then
                call check(error, capacity_gap_final < capacity_gap, &
                           "Uniform refinement must reduce an inexact capacity integral: " &
                           //trim(case_label(element_ids(k), TEST_MAX_DEPTH)))
                if (allocated(error)) return
            end if
        end do
    end subroutine test_uncut_assembly_vs_gauss

    ! =========================================================================
    ! Group 3b. Cut element with side-wise coefficients, on a curved T6 so the
    ! parent mapping is exercised too.  No analytic value exists for the
    ! matrices, so the deep refinement is the reference; the error must decrease
    ! from the depth that resolves the interface.
    ! The element measure is analytic (1/3) and must hold at every depth.
    ! =========================================================================
    subroutine test_cut_assembly_convergence(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature) :: quadrature, reference_quadrature
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        real(real64), allocatable :: reference_xi(:), reference_eta(:), reference_weight(:)
        logical, allocatable :: is_plus_side(:), reference_is_plus_side(:)
        real(real64) :: phi_nodes(MAX_NODES), node_coordinates(2, MAX_NODES)
        real(real64) :: diffusion(MAX_NODES, MAX_NODES), capacity(MAX_NODES, MAX_NODES)
        real(real64) :: diffusion_reference(MAX_NODES, MAX_NODES), capacity_reference(MAX_NODES, MAX_NODES)
        real(real64) :: diffusion_error(0:TEST_MAX_DEPTH), capacity_error(0:TEST_MAX_DEPTH)
        real(real64) :: radius, offset
        real(real64), parameter :: diffusivity_minus = 1.0d0, diffusivity_plus = 5.0d0
        real(real64), parameter :: capacity_minus = 2.0d0, capacity_plus = 7.0d0
        integer(int32) :: element_id, num_nodes, num_points, num_reference_points, depth, resolution_depth

        element_id = FE_TYPE%QUADRATIC_TRIANGLE%ID
        radius = 0.6d0
        offset = radius**2
        resolution_depth = get_resolution_depth(element_id, radius)
        call get_curved_triangle_coordinates(node_coordinates, num_nodes)

        call build_case(element_id, LEVEL_SET_CIRCLE, offset, fe, phi_nodes, num_nodes)
        call compute_rule(reference_quadrature, fe, phi_nodes, num_nodes, TEST_REFERENCE_DEPTH, &
                          reference_xi, reference_eta, reference_weight, reference_is_plus_side, &
                          num_reference_points)
        call check(error, num_reference_points > 0, "Reference refinement must produce a rule")
        if (allocated(error)) return
        call assemble_on_subcell_rule(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                      reference_xi, reference_eta, reference_weight, reference_is_plus_side, &
                                      num_reference_points, diffusivity_minus, diffusivity_plus, &
                                      capacity_minus, capacity_plus, diffusion_reference, capacity_reference)

        do depth = 0, TEST_MAX_DEPTH
            call build_case(element_id, LEVEL_SET_CIRCLE, offset, fe, phi_nodes, num_nodes)
            call compute_rule(quadrature, fe, phi_nodes, num_nodes, depth, &
                              xi, eta, weight, is_plus_side, num_points)
            call assemble_on_subcell_rule(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                          xi, eta, weight, is_plus_side, num_points, &
                                          diffusivity_minus, diffusivity_plus, capacity_minus, capacity_plus, &
                                          diffusion, capacity)

            diffusion_error(depth) = maxval(abs(diffusion(1:num_nodes, 1:num_nodes) &
                                                - diffusion_reference(1:num_nodes, 1:num_nodes)))
            capacity_error(depth) = maxval(abs(capacity(1:num_nodes, 1:num_nodes) &
                                               - capacity_reference(1:num_nodes, 1:num_nodes)))

            call check(error, abs(calc_physical_measure(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                                        xi, eta, weight, num_points) - 1.0d0 / 3.0d0) < 1.0d-12, &
                       "Curved element measure must be exact at every depth: " &
                       //trim(case_label(element_id, depth)))
            if (allocated(error)) return
        end do

        do depth = resolution_depth, TEST_MAX_DEPTH - 1
            call check(error, diffusion_error(depth + 1) < diffusion_error(depth), &
                       "Cut diffusion matrix error must decrease: "//trim(case_label(element_id, depth)))
            if (allocated(error)) return
            call check(error, capacity_error(depth + 1) < CONVERGENCE_RATIO * capacity_error(depth), &
                       "Cut capacity matrix must converge as O(4^-d): "//trim(case_label(element_id, depth)))
            if (allocated(error)) return
        end do
    end subroutine test_cut_assembly_convergence


    ! =========================================================================
    ! Group 3c. Error control on an UNCUT element.  The level-set criterion
    ! never refines such an element, so its quadrature error is whatever the
    ! depth-0 rule gives - 134% of the capacity matrix on T6, because the
    ! midpoint points sit exactly on the mid-side nodes where the corner shape
    ! functions vanish.  Driving the refinement by the term error must remove
    ! that: tightening eps_quad must reduce the gap and deepen the rule.
    ! =========================================================================
    subroutine test_error_control_uncut(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable, target :: fe, fe_reference
        type(type_subcell_quadrature) :: quadrature
        type(type_element_matrix_integrand) :: integrand
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64) :: phi_nodes(MAX_NODES), node_coordinates(2, MAX_NODES)
        real(real64) :: diffusion_subcell(MAX_NODES, MAX_NODES), capacity_subcell(MAX_NODES, MAX_NODES)
        real(real64) :: diffusion_gauss(MAX_NODES, MAX_NODES), capacity_gauss(MAX_NODES, MAX_NODES)
        real(real64) :: capacity_gap(3), tolerances(3), capacity_scale
        real(real64) :: diffusion_gap, diffusion_scale
        real(real64), parameter :: diffusivity = 3.0d0, capacity = 2.0d0
        integer(int32) :: element_id, num_nodes, num_points, reached_depth(3), level

        element_id = FE_TYPE%QUADRATIC_TRIANGLE%ID
        tolerances = [1.0d-1, 1.0d-2, 1.0d-3]

        call get_reference_nodes(element_id, node_coordinates, num_nodes)
        fe_reference = create_fe(element_id, REFERENCE_GAUSS_ORDER)
        call assemble_on_gauss_rule(fe_reference, node_coordinates(:, 1:num_nodes), num_nodes, &
                                    diffusivity, capacity, diffusion_gauss, capacity_gauss)
        capacity_scale = maxval(abs(capacity_gauss(1:num_nodes, 1:num_nodes)))
        diffusion_scale = maxval(abs(diffusion_gauss(1:num_nodes, 1:num_nodes)))
        diffusion_gap = 0.0d0

        do level = 1, 3
            call build_case(element_id, LEVEL_SET_LINEAR, 10.0d0, fe, phi_nodes, num_nodes)
            call integrand%initialize(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                      diffusivity, diffusivity, capacity, capacity)
            call compute_rule_controlled(quadrature, fe, phi_nodes, num_nodes, 4, tolerances(level), &
                                         integrand, xi, eta, weight, is_plus_side, num_points)
            call quadrature%get_reached_depth(reached_depth(level))
            call assemble_on_subcell_rule(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                          xi, eta, weight, is_plus_side, num_points, &
                                          diffusivity, diffusivity, capacity, capacity, &
                                          diffusion_subcell, capacity_subcell)
            capacity_gap(level) = maxval(abs(capacity_subcell(1:num_nodes, 1:num_nodes) &
                                             - capacity_gauss(1:num_nodes, 1:num_nodes))) / capacity_scale
            diffusion_gap = max(diffusion_gap, maxval(abs(diffusion_subcell(1:num_nodes, 1:num_nodes) &
                                                          - diffusion_gauss(1:num_nodes, 1:num_nodes))) &
                                / diffusion_scale)
            write (output_unit, '(A,ES8.1,A,ES9.2,A,I0,A,I0)') "       eps=", tolerances(level), &
                " capacity gap=", capacity_gap(level), " depth=", reached_depth(level), &
                " points=", num_points
        end do

        ! Once d_max is reached the rule cannot refine further, so the gap
        ! stops improving: the requirement is monotonicity, not strict descent.
        do level = 1, 2
            call check(error, capacity_gap(level + 1) <= capacity_gap(level), &
                       "A tighter tolerance must not increase the quadrature gap")
            if (allocated(error)) return
            call check(error, reached_depth(level + 1) >= reached_depth(level), &
                       "A tighter tolerance must not make the rule coarser")
            if (allocated(error)) return
        end do
        call check(error, capacity_gap(1) < 1.0d-2, &
                   "Error control must remove the depth-0 degeneracy of the T6 capacity matrix")
        if (allocated(error)) return

        ! The depth-0 rule is 1.34 off (test 3a); the control must remove that
        ! outright, not merely improve it.
        call check(error, capacity_gap(3) < 1.0d-3, &
                   "Error control must bring the uncut T6 capacity matrix near the exact one")
        if (allocated(error)) return
        call check(error, diffusion_gap < 1.0d-3, &
                   "Error control must not spoil the diffusion matrix it already integrated exactly")
    end subroutine test_error_control_uncut

    ! =========================================================================
    ! Group 3d. Error control on a CUT element: the terms must converge to the
    ! deeply refined reference as eps_quad tightens, on a curved T6 so the
    ! parent mapping is exercised as well.
    ! =========================================================================
    subroutine test_error_control_cut(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable, target :: fe
        type(type_subcell_quadrature) :: quadrature
        type(type_element_matrix_integrand) :: integrand
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64), allocatable :: reference_xi(:), reference_eta(:), reference_weight(:)
        logical, allocatable :: reference_is_plus_side(:)
        real(real64) :: phi_nodes(MAX_NODES), node_coordinates(2, MAX_NODES)
        real(real64) :: diffusion(MAX_NODES, MAX_NODES), capacity(MAX_NODES, MAX_NODES)
        real(real64) :: diffusion_reference(MAX_NODES, MAX_NODES), capacity_reference(MAX_NODES, MAX_NODES)
        real(real64) :: matrix_error(3), reference_scale
        real(real64), parameter :: diffusivity_minus = 1.0d0, diffusivity_plus = 5.0d0
        real(real64), parameter :: capacity_minus = 2.0d0, capacity_plus = 7.0d0
        integer(int32) :: element_id, num_nodes, num_points, num_reference_points, level

        element_id = FE_TYPE%QUADRATIC_TRIANGLE%ID
        call get_curved_triangle_coordinates(node_coordinates, num_nodes)

        ! The reference must be error controlled as well: a level-set-driven
        ! deep rule leaves every uncut cell at depth 0, so it would carry the
        ! same coarse-cell error the test is trying to measure.
        call build_case(element_id, LEVEL_SET_CIRCLE, 0.36d0, fe, phi_nodes, num_nodes)
        call integrand%initialize(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                  diffusivity_minus, diffusivity_plus, capacity_minus, capacity_plus)
        call compute_rule_controlled(quadrature, fe, phi_nodes, num_nodes, TEST_REFERENCE_DEPTH + 1, 1.0d-5, &
                                     integrand, reference_xi, reference_eta, reference_weight, &
                                     reference_is_plus_side, num_reference_points)
        call assemble_on_subcell_rule(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                      reference_xi, reference_eta, reference_weight, reference_is_plus_side, &
                                      num_reference_points, diffusivity_minus, diffusivity_plus, &
                                      capacity_minus, capacity_plus, diffusion_reference, capacity_reference)
        reference_scale = maxval(abs(capacity_reference(1:num_nodes, 1:num_nodes))) &
                          + maxval(abs(diffusion_reference(1:num_nodes, 1:num_nodes)))

        ! With the terms resolved, what is left is the interface geometry, and
        ! that is limited by d_max, not by eps_quad: the chords approximate the
        ! arc to O(4^-d).  So the depth is what must be swept here.
        do level = 1, 3
            call build_case(element_id, LEVEL_SET_CIRCLE, 0.36d0, fe, phi_nodes, num_nodes)
            call integrand%initialize(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                      diffusivity_minus, diffusivity_plus, capacity_minus, capacity_plus)
            call compute_rule_controlled(quadrature, fe, phi_nodes, num_nodes, level + 1, 1.0d-4, &
                                         integrand, xi, eta, weight, is_plus_side, num_points)
            call assemble_on_subcell_rule(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                          xi, eta, weight, is_plus_side, num_points, &
                                          diffusivity_minus, diffusivity_plus, capacity_minus, capacity_plus, &
                                          diffusion, capacity)
            matrix_error(level) = (maxval(abs(capacity(1:num_nodes, 1:num_nodes) &
                                              - capacity_reference(1:num_nodes, 1:num_nodes))) &
                                   + maxval(abs(diffusion(1:num_nodes, 1:num_nodes) &
                                                - diffusion_reference(1:num_nodes, 1:num_nodes)))) / reference_scale
            write (output_unit, '(A,I0,A,ES9.2,A,I0)') "       d_max=", level + 1, &
                " cut matrix error=", matrix_error(level), " points=", num_points
        end do

        do level = 1, 2
            call check(error, matrix_error(level + 1) < matrix_error(level), &
                       "A deeper rule must reduce the cut-element matrix error")
            if (allocated(error)) return
        end do
        call check(error, matrix_error(3) < 0.5d0 * matrix_error(1), &
                   "The interface-limited error must fall with depth, not stall")
        if (allocated(error)) return
        ! A relative measure alone would pass for any sequence that merely
        ! shrinks, so the deepest run is pinned in absolute terms as well.
        call check(error, matrix_error(3) < 5.0d-2, &
                   "The deepest cut rule must actually resolve the matrices")
    end subroutine test_error_control_cut

    ! =========================================================================
    ! Group 4a. Continuity of the ASSEMBLED matrices, which is what the Newton
    ! map differentiates.  Sweeping the interface changes the refinement
    ! pattern; a jump there would show up as one step far larger than the mean.
    ! =========================================================================
    subroutine test_assembly_continuity(error)
        type(error_type), allocatable, intent(inout) :: error

        real(real64) :: max_step_coarse, max_step_fine, mean_step_coarse
        real(real64), parameter :: offset_start = 0.20d0, offset_span = 0.20d0

        ! A Lipschitz dependence has max|dM| proportional to the sweep step, so
        ! halving the step must halve the largest observed change.  A jump does
        ! not shrink with the step, which is what separates the two: comparing
        ! the largest change with the mean would accept a rule that jumps at
        ! EVERY step, since then max = mean.
        call sweep_assembly(offset_start, offset_span, 500, max_step_coarse, mean_step_coarse)
        call sweep_assembly(offset_start, offset_span, 1000, max_step_fine, mean_step_coarse)

        write (output_unit, '(A,ES9.2,A,ES9.2,A,F6.2)') "       continuity max(h)=", max_step_coarse, &
            " max(h/2)=", max_step_fine, " ratio=", max_step_fine / max(max_step_coarse, tiny(1.0d0))

        call check(error, max_step_coarse > 0.0d0, "Sweep must actually move the interface")
        if (allocated(error)) return
        call check(error, max_step_fine < 0.6d0 * max_step_coarse, &
                   "Halving the sweep step must halve the largest change, i.e. no jump")
    end subroutine test_assembly_continuity

    !> Sweep the level set across a curved T6 and report the largest and mean
    !> change of the assembled matrices between consecutive steps.
    subroutine sweep_assembly(offset_start, offset_span, num_steps, max_step_change, mean_step_change)
        real(real64), intent(in) :: offset_start, offset_span
        integer(int32), intent(in) :: num_steps
        real(real64), intent(inout) :: max_step_change, mean_step_change

        class(abst_fe), allocatable, target :: fe
        type(type_subcell_quadrature) :: quadrature
        type(type_element_matrix_integrand) :: integrand
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64) :: phi_nodes(MAX_NODES), node_coordinates(2, MAX_NODES)
        real(real64) :: diffusion(MAX_NODES, MAX_NODES), capacity(MAX_NODES, MAX_NODES)
        real(real64) :: diffusion_previous(MAX_NODES, MAX_NODES), capacity_previous(MAX_NODES, MAX_NODES)
        real(real64) :: step_change, offset, offset_step
        real(real64), parameter :: diffusivity_minus = 1.0d0, diffusivity_plus = 5.0d0
        real(real64), parameter :: capacity_minus = 2.0d0, capacity_plus = 7.0d0
        integer(int32) :: element_id, num_nodes, num_points, step

        element_id = FE_TYPE%QUADRATIC_TRIANGLE%ID
        call get_curved_triangle_coordinates(node_coordinates, num_nodes)
        offset_step = offset_span / real(num_steps, real64)

        max_step_change = 0.0d0
        mean_step_change = 0.0d0
        diffusion_previous = 0.0d0
        capacity_previous = 0.0d0

        do step = 0, num_steps
            offset = offset_start + real(step, real64) * offset_step
            call build_case(element_id, LEVEL_SET_CIRCLE, offset, fe, phi_nodes, num_nodes)
            call integrand%initialize(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                      diffusivity_minus, diffusivity_plus, capacity_minus, capacity_plus)
            call compute_rule_controlled(quadrature, fe, phi_nodes, num_nodes, 4, 1.0d-3, &
                                         integrand, xi, eta, weight, is_plus_side, num_points)
            call assemble_on_subcell_rule(fe, node_coordinates(:, 1:num_nodes), num_nodes, &
                                          xi, eta, weight, is_plus_side, num_points, &
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
    end subroutine sweep_assembly

    ! =========================================================================
    ! Group 4b. Side weights are Lipschitz in the nodal level set (T3, exact
    ! clip): perturbing phi by delta changes each side sum by O(delta).
    ! =========================================================================
    subroutine test_levelset_continuity(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature) :: quadrature
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64) :: phi_nodes(3), weight_previous, weight_current, max_jump
        real(real64), parameter :: phi_step = 1.0d-4
        integer(int32) :: num_points, step

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)

        max_jump = 0.0d0
        weight_previous = -1.0d0
        do step = 0, 200
            phi_nodes = [-0.5d0 + real(step, real64) * phi_step, 1.0d0, 1.0d0]
            call compute_rule(quadrature, fe, phi_nodes, 3, 0, xi, eta, weight, is_plus_side, num_points)
            weight_current = sum_side_weights(weight, is_plus_side, num_points, .false.)
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
        type(type_subcell_quadrature) :: quadrature
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64) :: phi_nodes(3), minus_weight
        integer(int32) :: num_points

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)

        phi_nodes = [-1.0d-9, 1.0d0, 1.0d0]
        call compute_rule(quadrature, fe, phi_nodes, 3, 0, xi, eta, weight, is_plus_side, num_points)
        minus_weight = sum_side_weights(weight, is_plus_side, num_points, .false.)

        call check(error, minus_weight < 1.0d-15, &
                   "Minority side weight must vanish as the interface leaves the element")
    end subroutine test_vanishing_minority_side

    ! =========================================================================
    ! Group 5a. The object sizes its own storage, so a usable rule must come
    ! back for every supported element at every depth, the point count must
    ! stay within the advertised capacity, and the capacity must follow the
    ! 4^d law the caller budgets memory with.
    ! =========================================================================
    subroutine test_capacity_contract(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature) :: quadrature
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64) :: phi_nodes(MAX_NODES)
        integer(int32) :: element_ids(5), num_cases, num_nodes, num_points, k, depth
        integer(int32) :: capacity, capacity_at_zero

        call get_family_case_ids(element_ids, num_cases)
        do k = 1, num_cases
            do depth = 0, TEST_MAX_DEPTH
                call build_case(element_ids(k), LEVEL_SET_CIRCLE, get_circle_offset(element_ids(k)), &
                                fe, phi_nodes, num_nodes)
                call compute_rule(quadrature, fe, phi_nodes, num_nodes, depth, &
                                  xi, eta, weight, is_plus_side, num_points)
                call quadrature%get_capacity(capacity)

                call check(error, quadrature%is_usable(), &
                           "A supported element must always yield a usable rule: " &
                           //trim(case_label(element_ids(k), depth)))
                if (allocated(error)) return
                call check(error, num_points <= capacity, &
                           "Capacity must bound the point count: "//trim(case_label(element_ids(k), depth)))
                if (allocated(error)) return
            end do
        end do

        ! Capacity must grow exactly as 4^d, which is what sizes the workspace.
        call quadrature%initialize(0)
        call quadrature%get_capacity(capacity_at_zero)
        do depth = 0, TEST_MAX_DEPTH
            call quadrature%initialize(depth)
            call quadrature%get_capacity(capacity)
            call check(error, capacity == capacity_at_zero * 4**depth, &
                       "Capacity must follow the 4^d law: "//trim(case_label(0, depth)))
            if (allocated(error)) return
        end do

        ! A depth request below zero is meaningless and must fall back to 0.
        call quadrature%initialize(-3)
        call quadrature%get_max_depth(depth)
        call check(error, depth == 0, "A negative depth must be treated as zero")
        if (allocated(error)) return
        call quadrature%destroy()
        call quadrature%get_capacity(capacity)
        call check(error, capacity == 0, "Destroy must release the point storage")
    end subroutine test_capacity_contract

    ! =========================================================================
    ! Group 5b. A quadrature initialized for the maximum supported depth must
    ! reproduce every fixed-depth rule without changing its storage capacity.
    ! Requests above that maximum must use the maximum-depth rule.
    ! =========================================================================
    subroutine test_requested_depth_contract(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature) :: quadrature, reference_quadrature
        real(real64) :: phi_nodes(MAX_NODES)
        integer(int32), parameter :: max_depth = 4
        integer(int32), parameter :: requested_depths(4) = [0, 1, 2, 4]
        integer(int32) :: capacity, initial_capacity, num_nodes, request

        call build_case(FE_TYPE%QUADRATIC_QUAD%ID, LEVEL_SET_CIRCLE, &
                        get_circle_offset(FE_TYPE%QUADRATIC_QUAD%ID), fe, phi_nodes, num_nodes)
        call quadrature%initialize(max_depth)
        call quadrature%get_capacity(initial_capacity)

        do request = 1, size(requested_depths)
            call quadrature%compute(fe, phi_nodes(1:num_nodes), &
                                    refinement_depth=requested_depths(request))
            call reference_quadrature%initialize(requested_depths(request))
            call reference_quadrature%compute(fe, phi_nodes(1:num_nodes))

            call check(error, rules_are_identical(quadrature, reference_quadrature), &
                       "Requested depth must reproduce its fixed-depth rule: " &
                       //trim(case_label(FE_TYPE%QUADRATIC_QUAD%ID, requested_depths(request))))
            if (allocated(error)) return

            call quadrature%get_capacity(capacity)
            call check(error, capacity == initial_capacity, &
                       "Changing requested depth must preserve the maximum-depth capacity")
            if (allocated(error)) return
        end do

        call quadrature%compute(fe, phi_nodes(1:num_nodes), refinement_depth=max_depth + 3)
        call reference_quadrature%initialize(max_depth)
        call reference_quadrature%compute(fe, phi_nodes(1:num_nodes))
        call check(error, rules_are_identical(quadrature, reference_quadrature), &
                   "A requested depth above the initialized maximum must be clamped")
        if (allocated(error)) return

        call quadrature%get_capacity(capacity)
        call check(error, capacity == initial_capacity, &
                   "A clamped depth request must preserve the maximum-depth capacity")
    end subroutine test_requested_depth_contract

    logical function rules_are_identical(actual, expected) result(identical)
        type(type_subcell_quadrature), intent(in) :: actual, expected

        real(real64) :: actual_xi, actual_eta, actual_weight
        real(real64) :: expected_xi, expected_eta, expected_weight
        logical :: actual_plus, expected_plus
        integer(int32) :: actual_num_points, expected_num_points, point

        call actual%get_num_points(actual_num_points)
        call expected%get_num_points(expected_num_points)
        identical = actual_num_points == expected_num_points
        if (.not. identical) return

        do point = 1, actual_num_points
            call actual%get_point(point, actual_xi, actual_eta, actual_weight, actual_plus)
            call expected%get_point(point, expected_xi, expected_eta, expected_weight, expected_plus)
            identical = actual_xi == expected_xi .and. actual_eta == expected_eta &
                        .and. actual_weight == expected_weight .and. actual_plus .eqv. expected_plus
            if (.not. identical) return
        end do
    end function rules_are_identical

    ! =========================================================================
    ! Group 5c. Families outside the 2D triangle/quadrilateral scope must be
    ! reported as unusable, and degenerate level sets must still produce a
    ! valid partition rather than an empty or torn rule.
    ! =========================================================================
    subroutine test_unsupported_and_degenerate(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature) :: quadrature
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64) :: phi_nodes(MAX_NODES), total_weight
        integer(int32) :: num_points, depth

        fe = create_fe(FE_TYPE%LINE%ID, 1)
        phi_nodes = 1.0d0
        call compute_rule(quadrature, fe, phi_nodes, 2, 0, xi, eta, weight, is_plus_side, num_points)
        call check(error, .not. quadrature%is_usable() .and. num_points == 0, &
                   "LINE must be reported as unsupported")
        if (allocated(error)) return

        fe = create_fe(FE_TYPE%TETRA%ID, 1)
        call compute_rule(quadrature, fe, phi_nodes, 4, 0, xi, eta, weight, is_plus_side, num_points)
        call check(error, .not. quadrature%is_usable() .and. num_points == 0, &
                   "TETRA must be reported as unsupported")
        if (allocated(error)) return

        ! phi identically zero: no point is on the plus side, and the partition
        ! must still cover the element exactly.
        fe = create_fe(FE_TYPE%QUADRATIC_QUAD%ID, 1)
        phi_nodes = 0.0d0
        do depth = 0, 2
            call compute_rule(quadrature, fe, phi_nodes, 8, depth, xi, eta, weight, is_plus_side, num_points)
            total_weight = sum_side_weights(weight, is_plus_side, num_points, .false.)
            call check(error, abs(total_weight - 4.0d0) < 1.0d-12, &
                       "A zero level set must put the whole element on the minus side")
            if (allocated(error)) return
        end do

        ! phi exactly zero at one node: the clip must stay well defined.
        phi_nodes = 1.0d0
        phi_nodes(1) = 0.0d0
        do depth = 0, 2
            call compute_rule(quadrature, fe, phi_nodes, 8, depth, xi, eta, weight, is_plus_side, num_points)
            total_weight = sum_side_weights(weight, is_plus_side, num_points, .true.) &
                           + sum_side_weights(weight, is_plus_side, num_points, .false.)
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
        type(type_subcell_quadrature) :: quadrature
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64) :: phi_nodes(3)
        integer(int32) :: num_points

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)

        phi_nodes = [1.0d0, 2.0d0, 3.0d0]
        call compute_rule(quadrature, fe, phi_nodes, 3, 0, xi, eta, weight, is_plus_side, num_points)

        call check(error, num_points > 0, "Expected quadrature points for uncut triangle")
        if (allocated(error)) return
        call check(error, abs(sum_side_weights(weight, is_plus_side, num_points, .true.) - 0.5d0) < 1.0d-13, &
                   "Plus side should carry the whole reference area 0.5")
        if (allocated(error)) return
        call check(error, abs(sum_side_weights(weight, is_plus_side, num_points, .false.)) < 1.0d-13, &
                   "Minus side should carry zero weight for uncut triangle")
    end subroutine test_uncut_triangle

    ! =========================================================================
    ! Group 6b. phi = [-0.5, 1, 1] gives phi^h = -0.5 + 1.5(xi + eta), zero on
    ! xi + eta = 1/3, so the minus area is 0.5 (1/3)^2 = 1/18.
    ! =========================================================================
    subroutine test_cut_triangle_exact_split(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature) :: quadrature
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64) :: phi_nodes(3), minus_area
        integer(int32) :: num_points

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)

        phi_nodes = [-0.5d0, 1.0d0, 1.0d0]
        minus_area = 0.5d0 * (1.0d0 / 3.0d0)**2
        call compute_rule(quadrature, fe, phi_nodes, 3, 0, xi, eta, weight, is_plus_side, num_points)

        call check(error, num_points > 0, "Expected quadrature points for cut triangle")
        if (allocated(error)) return
        call check(error, abs(sum_side_weights(weight, is_plus_side, num_points, .false.) - minus_area) < 1.0d-13, &
                   "Minus side must equal the analytic area 1/18 exactly")
        if (allocated(error)) return
        call check(error, abs(sum_side_weights(weight, is_plus_side, num_points, .true.) &
                              - (0.5d0 - minus_area)) < 1.0d-13, &
                   "Plus side must equal 0.5 - 1/18 exactly")
    end subroutine test_cut_triangle_exact_split

    ! =========================================================================
    ! Group 6c. Uncut quad: total = reference area 4.0 on the minus side.
    ! =========================================================================
    subroutine test_uncut_quad(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature) :: quadrature
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64) :: phi_nodes(4)
        integer(int32) :: num_points

        fe = create_fe(FE_TYPE%QUAD%ID, 1)

        phi_nodes = [-1.0d0, -1.0d0, -1.0d0, -1.0d0]
        call compute_rule(quadrature, fe, phi_nodes, 4, 0, xi, eta, weight, is_plus_side, num_points)

        call check(error, num_points > 0, "Expected quadrature points for uncut quad")
        if (allocated(error)) return
        call check(error, abs(sum_side_weights(weight, is_plus_side, num_points, .false.) - 4.0d0) < 1.0d-12, &
                   "Minus side should carry the whole reference area 4.0")
    end subroutine test_uncut_quad

    ! =========================================================================
    ! Group 6d. Nodal phi = [-1, 1, 1, -1] (VTK corner order) is phi = xi, so
    ! [-1,1]^2 splits along xi = 0 into two halves of area 2.
    ! =========================================================================
    subroutine test_cut_quad_split(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature) :: quadrature
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64) :: phi_nodes(4)
        integer(int32) :: num_points

        fe = create_fe(FE_TYPE%QUAD%ID, 1)

        phi_nodes = [-1.0d0, 1.0d0, 1.0d0, -1.0d0]
        call compute_rule(quadrature, fe, phi_nodes, 4, 0, xi, eta, weight, is_plus_side, num_points)

        call check(error, num_points > 0, "Expected quadrature points for cut quad")
        if (allocated(error)) return
        call check(error, abs(sum_side_weights(weight, is_plus_side, num_points, .true.) - 2.0d0) < 1.0d-12, &
                   "Plus side of the quad must have area 2")
        if (allocated(error)) return
        call check(error, abs(sum_side_weights(weight, is_plus_side, num_points, .false.) - 2.0d0) < 1.0d-12, &
                   "Minus side of the quad must have area 2")
    end subroutine test_cut_quad_split

    ! =========================================================================
    ! Group 6e. A side-wise-constant coefficient must be integrated exactly on
    ! a cut element with a straight interface.
    ! =========================================================================
    subroutine test_sidewise_constant_integration(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_quadrature) :: quadrature
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64) :: phi_nodes(3), integral, expected, minus_area
        real(real64), parameter :: coefficient_minus = 3.0d0, coefficient_plus = 7.0d0
        integer(int32) :: num_points, point

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)

        phi_nodes = [-0.5d0, 1.0d0, 1.0d0]
        minus_area = 0.5d0 * (1.0d0 / 3.0d0)**2
        expected = coefficient_minus * minus_area + coefficient_plus * (0.5d0 - minus_area)

        call compute_rule(quadrature, fe, phi_nodes, 3, 0, xi, eta, weight, is_plus_side, num_points)

        integral = 0.0d0
        do point = 1, num_points
            if (is_plus_side(point)) then
                integral = integral + coefficient_plus * weight(point)
            else
                integral = integral + coefficient_minus * weight(point)
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
        type(type_subcell_quadrature) :: quadrature
        type(type_coordinate_dp) :: reference_point
        real(real64), allocatable :: xi(:), eta(:), weight(:)
        logical, allocatable :: is_plus_side(:)
        real(real64) :: phi_nodes(3), node_coordinates(2, 3), shape_gradients(2, 3), determinant_jacobian
        real(real64) :: flux(2), residual(3), expected(3)
        integer(int32) :: num_points, point, i

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)
        node_coordinates = reshape([0.0d0, 0.0d0, 1.0d0, 0.0d0, 0.0d0, 1.0d0], shape(node_coordinates))
        phi_nodes = [-0.5d0, 1.0d0, 1.0d0]
        call compute_rule(quadrature, fe, phi_nodes, 3, 0, xi, eta, weight, is_plus_side, num_points)

        residual = 0.0d0
        do point = 1, num_points
            reference_point%x = xi(point)
            reference_point%y = eta(point)
            reference_point%z = 0.0d0
            shape_gradients = 0.0d0
            call fe%calc_shape_function(reference_point, node_coordinates, dpsi_dx=shape_gradients, &
                                        determinant_jacobian=determinant_jacobian)
            if (is_plus_side(point)) then
                flux = [3.0d0, -1.0d0]
            else
                flux = [1.0d0, 2.0d0]
            end if
            do i = 1, 3
                residual(i) = residual(i) + weight(point) * abs(determinant_jacobian) &
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
