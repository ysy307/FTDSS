!> @brief Interface-split subcell quadrature for elements cut by a level set.
!>
!> Given a nodal indicator field \(\phi\) (interpolated with the parent shape
!> functions), decomposes the reference element into the sub-domains
!> \(\Omega_e^+ = \{\phi^h > 0\}\) and \(\Omega_e^- = \{\phi^h \le 0\}\) and
!> returns one quadrature rule covering BOTH sub-domains, each point tagged
!> with its side.  The intended use is the freezing interface
!> \(\phi = (s_f - s_m) + \varepsilon_s\) (type_fusion%calc_freezing_level_set,
!> models_phase_change_fusion): plus side = ice present, minus side = ice-free.
!>
!> ### Method
!> The reference element is refined recursively using family topology only
!> (domain_fe_subcell_topology), while \(\phi^h\), \(\boldsymbol{x}(\xi)\) and
!> \(\nabla N_a\) always come from the parent element.  For a cell \(C\) at
!> depth \(d\),
!> \[
!> \int_C f \,\mathrm{d}\hat{\Omega} \approx
!> \begin{cases}
!> \sum_{k=1}^{4} \int_{C_k} f \,\mathrm{d}\hat{\Omega},
!>   & \operatorname{sign}\phi^h \text{ mixed on the probe set} \land d < d_{\max},\\
!> Q(C), & \text{otherwise},
!> \end{cases}
!> \]
!> where the probe set is the union of the children's vertices, and \(Q(C)\)
!> splits \(C\) into triangles, clips each against the straight line obtained by
!> linear interpolation of the vertex values of \(\phi^h\), and puts the
!> degree-2 midpoint rule (3 points, weight = area/3) on every sub-triangle.
!> Cell size in reference space is \(2^{-d}\), so \(d\) alone controls how
!> finely the interface is resolved, identically for T3/T6/Q4/Q8/Q9.
!>
!> ### Numerical guarantees
!> - The subcells tile the reference element exactly, so the rule integrates
!>   polynomials of degree 2 over the whole element exactly at every depth and
!>   sum(weights) equals the reference measure independently of the cut.
!> - Integrands of higher degree - \(N_a N_b\) for every element above T3, and
!>   \(\nabla N_a \cdot \nabla N_b\) above Q4 - are NOT integrated exactly, so
!>   this rule is less accurate than the standard Gauss rule on an UNCUT
!>   element; the difference is \(O(4^{-d})\) and vanishes as \(d\) grows.
!> - A straight interface is resolved exactly at any depth.  For a curved one
!>   the side-area error is \(O(4^{-d})\) once the cell resolves the interface,
!>   i.e. for \(d \gtrsim \log_2 (h_e / \ell_f)\) with \(\ell_f\) the interface
!>   curvature scale; below that depth the interface can be missed entirely.
!> - Weights vary Lipschitz-continuously with the nodal \(\phi\): a cell is left
!>   unrefined only when no child vertex changes sign, so when the refinement
!>   pattern switches the new sub-triangles are degenerate.  This keeps the
!>   assembled FE residual continuous in the nodal unknowns, which is required
!>   for a contracting Picard/Newton map at a moving free boundary.  No
!>   regularization parameters are involved.
!>
!> Computational complexity: \(O(4^{d_{\max}})\) points and \(\phi^h\)
!> evaluations per element in the worst case, \(O(2^{d_{\max}})\) for a single
!> interface crossing the element.
!> Failure behavior: unsupported element families and insufficient output
!> capacity both return num_quadrature_points = 0 so the caller can fall back to
!> the standard rule; a partial fill is never produced.
module domain_fe_subcell
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only: type_coordinate_dp, FE_TYPE
    use :: domain_base_fe, only: abst_fe
    use :: domain_fe_subcell_topology, only: type_subcell_cell, &
                                             SUBCELL_FAMILY_NONE, SUBCELL_FAMILY_TRIANGLE, &
                                             SUBCELL_FAMILY_QUADRILATERAL, &
                                             SUBCELL_MAX_PROBE_POINTS, SUBCELL_NUM_CHILDREN, &
                                             SUBCELL_MAX_TRIANGLES, &
                                             get_subcell_root_cell, get_subcell_probe_points, &
                                             get_subcell_children, get_subcell_triangle_indices
    implicit none
    private

    public :: type_subcell_quadrature_point
    public :: SUBCELL_QUADRATURE_CAPACITY
    public :: SUBCELL_MAX_DEPTH
    public :: build_interface_quadrature_points
    public :: calc_subcell_quadrature_capacity

    !> Deepest refinement the shared workspace capacity covers.
    integer(int32), parameter :: SUBCELL_MAX_DEPTH = 2

    !> Points emitted per triangle by the degree-2 midpoint rule.
    integer(int32), parameter :: POINTS_PER_TRIANGLE = 3
    !> Sub-triangles a clipped triangle is split into.
    integer(int32), parameter :: SUB_TRIANGLES_PER_CLIP = 3
    !> Triangles a quadrilateral cell is decomposed into.
    integer(int32), parameter :: TRIANGLES_PER_QUADRILATERAL = 2
    !> Largest depth calc_subcell_quadrature_capacity evaluates without int32 overflow.
    integer(int32), parameter :: CAPACITY_DEPTH_LIMIT = 13

    !> Hard upper bound on generated quadrature points per element at
    !> SUBCELL_MAX_DEPTH; sizes the workspace arrays of the governing blocks.
    integer(int32), parameter :: SUBCELL_QUADRATURE_CAPACITY = &
                                 TRIANGLES_PER_QUADRILATERAL * SUB_TRIANGLES_PER_CLIP &
                                 * POINTS_PER_TRIANGLE * 4**SUBCELL_MAX_DEPTH

    !> A single subcell quadrature point in parent reference coordinates.
    type :: type_subcell_quadrature_point
        !> Reference \(\xi\) coordinate in the parent element.
        real(real64) :: xi = 0.0d0
        !> Reference \(\eta\) coordinate in the parent element.
        real(real64) :: eta = 0.0d0
        !> Integration weight in reference space; does NOT include \(|\det J_{parent}|\).
        real(real64) :: weight = 0.0d0
        !> Side of the interface: .true. for \(\phi^h > 0\) (e.g. frozen).
        logical :: is_plus_side = .false.
    end type type_subcell_quadrature_point

contains

    !> @brief Build the interface-split quadrature of one element.
    subroutine build_interface_quadrature_points(fe, phi_nodes, quadrature_points, &
                                                 num_quadrature_points, max_depth)
        implicit none
        !> Parent finite element; supplies \(\phi^h\) through lerp.
        class(abst_fe), intent(in) :: fe
        !> Nodal level-set values, size = number of parent nodes; plus side
        !> where \(\phi > 0\).
        real(real64), intent(in) :: phi_nodes(:)
        !> Output array; must hold calc_subcell_quadrature_capacity(fe_id,
        !> max_depth) entries in the worst case, and is overwritten up to
        !> num_quadrature_points.
        type(type_subcell_quadrature_point), intent(inout) :: quadrature_points(:)
        !> Number of valid entries on exit; 0 when the element family is
        !> unsupported or when quadrature_points is too small.
        integer(int32), intent(inout) :: num_quadrature_points
        !> Refinement depth \(d_{\max} \ge 0\); default 0 (single clip of the
        !> whole element).  Negative values are treated as 0.
        integer(int32), intent(in), optional :: max_depth

        integer(int32) :: fe_id, family, depth_limit
        type(type_subcell_cell) :: root_cell
        logical :: has_overflowed

        num_quadrature_points = 0
        depth_limit = 0
        if (present(max_depth)) depth_limit = max(0, max_depth)

        call fe%get_type(fe_id)
        family = get_subcell_family(fe_id)
        if (family == SUBCELL_FAMILY_NONE) return

        call get_subcell_root_cell(family, root_cell)
        has_overflowed = .false.
        call integrate_cell(fe, phi_nodes, root_cell, 0, depth_limit, &
                            quadrature_points, num_quadrature_points, has_overflowed)

        ! A truncated rule loses area silently; report unsupported instead.
        if (has_overflowed) num_quadrature_points = 0
    end subroutine build_interface_quadrature_points

    !> @brief Worst-case number of quadrature points for one element.
    !>
    !> \[ n_{\max} = n_{\mathrm{triangles}} \, n_{\mathrm{clip}} \, n_{q} \, 4^{d} \]
    !> with \(n_{\mathrm{triangles}} = 1\) (triangle family) or \(2\)
    !> (quadrilateral family).  Reached when every cell is refined to depth
    !> \(d\) and every leaf is cut.
    !> Complexity: \(O(1)\).  Failure behavior: returns 0 for an unsupported
    !> family; the depth is capped at CAPACITY_DEPTH_LIMIT to stay within int32.
    pure function calc_subcell_quadrature_capacity(fe_id, max_depth) result(capacity)
        implicit none
        !> FE_TYPE id of the parent element.
        integer(int32), intent(in) :: fe_id
        !> Refinement depth \(d_{\max} \ge 0\).
        integer(int32), intent(in) :: max_depth
        !> Upper bound on num_quadrature_points.
        integer(int32) :: capacity

        integer(int32) :: depth_limit, points_per_cell

        depth_limit = min(max(0, max_depth), CAPACITY_DEPTH_LIMIT)
        select case (get_subcell_family(fe_id))
        case (SUBCELL_FAMILY_TRIANGLE)
            points_per_cell = SUB_TRIANGLES_PER_CLIP * POINTS_PER_TRIANGLE
        case (SUBCELL_FAMILY_QUADRILATERAL)
            points_per_cell = TRIANGLES_PER_QUADRILATERAL * SUB_TRIANGLES_PER_CLIP * POINTS_PER_TRIANGLE
        case default
            capacity = 0
            return
        end select

        capacity = points_per_cell * 4**depth_limit
    end function calc_subcell_quadrature_capacity

    ! =========================================================================
    ! Private helpers
    ! =========================================================================

    !> Integrate one cell: refine while the interface crosses it, clip at the
    !> deepest level, emit the whole cell when it is uncut.
    recursive subroutine integrate_cell(fe, phi_nodes, cell, depth, depth_limit, &
                                        quadrature_points, num_quadrature_points, has_overflowed)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: phi_nodes(:)
        type(type_subcell_cell), intent(in) :: cell
        integer(int32), intent(in) :: depth
        integer(int32), intent(in) :: depth_limit
        type(type_subcell_quadrature_point), intent(inout) :: quadrature_points(:)
        integer(int32), intent(inout) :: num_quadrature_points
        logical, intent(inout) :: has_overflowed

        real(real64) :: probe_points(2, SUBCELL_MAX_PROBE_POINTS)
        real(real64) :: probe_phi(SUBCELL_MAX_PROBE_POINTS)
        real(real64) :: triangle_vertices(2, 3), triangle_phi(3)
        integer(int32) :: triangle_indices(3, SUBCELL_MAX_TRIANGLES)
        type(type_subcell_cell) :: children(SUBCELL_NUM_CHILDREN)
        integer(int32) :: num_probe_points, num_triangles, num_evaluated
        integer(int32) :: point, vertex, triangle, child
        logical :: is_sign_mixed

        if (has_overflowed) return

        call get_subcell_probe_points(cell, probe_points, num_probe_points)

        ! The deepest level decides the split from the cell vertices alone, so
        ! the extra probes are evaluated only where they can trigger a refinement.
        num_evaluated = cell%num_vertices
        if (depth < depth_limit) num_evaluated = num_probe_points
        do point = 1, num_evaluated
            call interpolate_phi(fe, phi_nodes, probe_points(:, point), probe_phi(point))
        end do

        call get_subcell_triangle_indices(cell%num_vertices, triangle_indices, num_triangles)

        if (depth < depth_limit) then
            is_sign_mixed = any(probe_phi(1:num_evaluated) > 0.0d0) &
                            .and. .not. all(probe_phi(1:num_evaluated) > 0.0d0)
            if (is_sign_mixed) then
                call get_subcell_children(cell, probe_points, children)
                do child = 1, SUBCELL_NUM_CHILDREN
                    call integrate_cell(fe, phi_nodes, children(child), depth + 1, depth_limit, &
                                        quadrature_points, num_quadrature_points, has_overflowed)
                end do
                return
            end if

            do triangle = 1, num_triangles
                call emit_triangle(probe_points(:, triangle_indices(1, triangle)), &
                                   probe_points(:, triangle_indices(2, triangle)), &
                                   probe_points(:, triangle_indices(3, triangle)), &
                                   probe_phi(1) > 0.0d0, &
                                   quadrature_points, num_quadrature_points, has_overflowed)
            end do
            return
        end if

        do triangle = 1, num_triangles
            do vertex = 1, 3
                triangle_vertices(:, vertex) = probe_points(:, triangle_indices(vertex, triangle))
                triangle_phi(vertex) = probe_phi(triangle_indices(vertex, triangle))
            end do
            call clip_triangle(triangle_vertices, triangle_phi, quadrature_points, &
                               num_quadrature_points, has_overflowed)
        end do
    end subroutine integrate_cell

    !> Clip one triangle against the line \(\phi^h = 0\) (vertex-linear) and
    !> emit the midpoint rule on every resulting sub-triangle.
    pure subroutine clip_triangle(vertices, phi, quadrature_points, num_quadrature_points, has_overflowed)
        implicit none
        real(real64), intent(in) :: vertices(2, 3)
        real(real64), intent(in) :: phi(3)
        type(type_subcell_quadrature_point), intent(inout) :: quadrature_points(:)
        integer(int32), intent(inout) :: num_quadrature_points
        logical, intent(inout) :: has_overflowed

        logical :: is_plus(3)
        integer(int32) :: vertex, lone, next, last
        real(real64) :: crossing_next(2), crossing_last(2)

        do vertex = 1, 3
            is_plus(vertex) = phi(vertex) > 0.0d0
        end do

        if (all(is_plus) .or. .not. any(is_plus)) then
            call emit_triangle(vertices(:, 1), vertices(:, 2), vertices(:, 3), is_plus(1), &
                               quadrature_points, num_quadrature_points, has_overflowed)
            return
        end if

        ! Exactly one vertex ("lone") lies on the minority side.
        if (is_plus(1) .neqv. is_plus(2)) then
            if (is_plus(1) .neqv. is_plus(3)) then
                lone = 1
            else
                lone = 2
            end if
        else
            lone = 3
        end if
        next = mod(lone, 3) + 1
        last = mod(lone + 1, 3) + 1

        crossing_next = calc_edge_zero_point(vertices(:, lone), phi(lone), vertices(:, next), phi(next))
        crossing_last = calc_edge_zero_point(vertices(:, lone), phi(lone), vertices(:, last), phi(last))

        ! Lone-side triangle and the complementary quadrilateral (as 2 triangles).
        call emit_triangle(vertices(:, lone), crossing_next, crossing_last, is_plus(lone), &
                           quadrature_points, num_quadrature_points, has_overflowed)
        call emit_triangle(crossing_next, vertices(:, next), vertices(:, last), is_plus(next), &
                           quadrature_points, num_quadrature_points, has_overflowed)
        call emit_triangle(crossing_next, vertices(:, last), crossing_last, is_plus(next), &
                           quadrature_points, num_quadrature_points, has_overflowed)
    end subroutine clip_triangle

    !> Zero-crossing of the linear interpolant between two vertices.
    pure function calc_edge_zero_point(vertex_a, phi_a, vertex_b, phi_b) result(crossing)
        implicit none
        real(real64), intent(in) :: vertex_a(2), phi_a, vertex_b(2), phi_b
        real(real64) :: crossing(2), fraction, denominator

        denominator = phi_a - phi_b
        if (abs(denominator) > tiny(1.0d0)) then
            fraction = phi_a / denominator
        else
            fraction = 0.0d0
        end if
        fraction = max(0.0d0, min(1.0d0, fraction))
        crossing = vertex_a + fraction * (vertex_b - vertex_a)
    end function calc_edge_zero_point

    !> Degree-2 midpoint rule (3 points, weight = area/3) on one sub-triangle.
    pure subroutine emit_triangle(vertex_a, vertex_b, vertex_c, is_plus_side, &
                                  quadrature_points, num_quadrature_points, has_overflowed)
        implicit none
        real(real64), intent(in) :: vertex_a(2), vertex_b(2), vertex_c(2)
        logical, intent(in) :: is_plus_side
        type(type_subcell_quadrature_point), intent(inout) :: quadrature_points(:)
        integer(int32), intent(inout) :: num_quadrature_points
        logical, intent(inout) :: has_overflowed

        real(real64) :: area, weight
        real(real64) :: edge_midpoints(2, POINTS_PER_TRIANGLE)
        integer(int32) :: point

        area = 0.5d0 * abs((vertex_b(1) - vertex_a(1)) * (vertex_c(2) - vertex_a(2)) &
                           - (vertex_c(1) - vertex_a(1)) * (vertex_b(2) - vertex_a(2)))
        if (area <= 0.0d0) return

        if (num_quadrature_points + POINTS_PER_TRIANGLE > size(quadrature_points)) then
            has_overflowed = .true.
            return
        end if

        edge_midpoints(:, 1) = 0.5d0 * (vertex_a + vertex_b)
        edge_midpoints(:, 2) = 0.5d0 * (vertex_b + vertex_c)
        edge_midpoints(:, 3) = 0.5d0 * (vertex_c + vertex_a)
        weight = area / real(POINTS_PER_TRIANGLE, real64)

        do point = 1, POINTS_PER_TRIANGLE
            num_quadrature_points = num_quadrature_points + 1
            quadrature_points(num_quadrature_points)%xi = edge_midpoints(1, point)
            quadrature_points(num_quadrature_points)%eta = edge_midpoints(2, point)
            quadrature_points(num_quadrature_points)%weight = weight
            quadrature_points(num_quadrature_points)%is_plus_side = is_plus_side
        end do
    end subroutine emit_triangle

    !> Interpolate the nodal level set at a reference point with the parent
    !> shape functions.
    subroutine interpolate_phi(fe, phi_nodes, reference_point, phi)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: phi_nodes(:)
        real(real64), intent(in) :: reference_point(2)
        real(real64), intent(inout) :: phi

        type(type_coordinate_dp) :: coordinate

        coordinate%x = reference_point(1)
        coordinate%y = reference_point(2)
        coordinate%z = 0.0d0
        call fe%lerp(coordinate, phi_nodes, phi)
    end subroutine interpolate_phi

    !> Map an FE_TYPE id to its subdivision family; the interpolation order is
    !> irrelevant here and stays with the parent shape functions.
    pure function get_subcell_family(fe_id) result(family)
        implicit none
        integer(int32), intent(in) :: fe_id
        integer(int32) :: family

        if (fe_id == FE_TYPE%TRIANGLE%ID .or. &
            fe_id == FE_TYPE%QUADRATIC_TRIANGLE%ID .or. &
            fe_id == FE_TYPE%BIQUADRATIC_TRIANGLE%ID .or. &
            fe_id == FE_TYPE%LAGRANGE_TRIANGLE%ID .or. &
            fe_id == FE_TYPE%HIGHER_ORDER_TRIANGLE%ID) then
            family = SUBCELL_FAMILY_TRIANGLE
        else if (fe_id == FE_TYPE%QUAD%ID .or. &
                 fe_id == FE_TYPE%QUADRATIC_QUAD%ID .or. &
                 fe_id == FE_TYPE%BIQUADRATIC_QUAD%ID .or. &
                 fe_id == FE_TYPE%LAGRANGE_QUADRILATERAL%ID .or. &
                 fe_id == FE_TYPE%HIGHER_ORDER_QUAD%ID) then
            family = SUBCELL_FAMILY_QUADRILATERAL
        else
            family = SUBCELL_FAMILY_NONE
        end if
    end function get_subcell_family

end module domain_fe_subcell
