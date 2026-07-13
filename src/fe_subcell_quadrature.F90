!> @brief Interface-split subcell quadrature for elements cut by a level set.
!>
!> Given a nodal indicator field \(\phi\) (interpolated with the parent shape
!> functions), decomposes the reference element into the sub-domains
!> \(\Omega_e^+ = \{\phi^h > 0\}\) and \(\Omega_e^- = \{\phi^h \le 0\}\) and
!> returns one quadrature rule covering BOTH sub-domains, each point tagged
!> with its side.  The intended use is the freezing interface
!> \(\phi = T_{high}(p_w) - T\): plus side = frozen, minus side = unfrozen.
!>
!> ### Method
!> 1. Partition the reference element into base triangles
!>    (triangle family: the reference simplex; quad family: two triangles).
!> 2. Evaluate \(\phi\) at the base-triangle vertices via the parent shape
!>    functions and clip each triangle against the straight line \(\phi^h = 0\)
!>    obtained by linear interpolation of the vertex values.  For linear
!>    triangles this split is EXACT; for higher-order interpolation it is the
!>    consistent \(O(h^2)\) linearization.
!> 3. Each resulting sub-triangle carries the degree-2 midpoint rule
!>    (3 points at edge midpoints, weight = ref-area/3).
!>
!> ### Numerical guarantees
!> - The rule integrates polynomials of degree 2 exactly on each sub-domain.
!> - Weights vary Lipschitz-continuously with the nodal \(\phi\) values, and the
!>   split degenerates continuously to the uncut rule as the interface leaves
!>   the element.  This keeps the assembled FE residual continuous in the
!>   nodal unknowns, which is required for a contracting Picard/Newton map at
!>   a moving free boundary.  No regularization parameters are involved.
!> - sum(weights) equals the reference-element area independently of the cut.
!>
!> Computational complexity: O(1) per element (at most 3 sub-triangles,
!> 9 points for a triangle; 18 for a quad).
!> Failure behavior: unsupported element families return n_qps = 0 so the
!> caller can fall back to the standard rule.
module domain_fe_subcell
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only: type_coordinate_dp, FE_TYPE
    use :: domain_base_fe, only: abst_fe
    implicit none
    private

    public :: type_subcell_qp
    public :: SUBCELL_QP_CAP
    public :: build_interface_quadrature_points

    !> Hard upper bound on generated quadrature points per element:
    !> 2 base triangles (quad) * 3 sub-triangles * 3 points = 18.
    integer(int32), parameter :: SUBCELL_QP_CAP = 24

    !> A single subcell quadrature point in parent reference coordinates.
    type :: type_subcell_qp
        !> Reference \(\xi\) coordinate in the parent element.
        real(real64) :: xi = 0.0d0
        !> Reference \(\eta\) coordinate in the parent element.
        real(real64) :: eta = 0.0d0
        !> Integration weight in reference space; does NOT include \(|\det J_{parent}|\).
        real(real64) :: weight = 0.0d0
        !> Side of the interface: .true. for \(\phi^h > 0\) (e.g. frozen).
        logical :: is_plus_side = .false.
    end type type_subcell_qp

contains

    !> @brief Build the interface-split quadrature of one element.
    !>
    !> \param fe        Parent finite element (provides lerp for \(\phi\)).
    !> \param phi_nodes Nodal level-set values (plus side where \(\phi > 0\)).
    !> \param qps       Output array, size >= SUBCELL_QP_CAP.
    !> \param n_qps     Number of valid entries in qps on exit (0 if unsupported).
    subroutine build_interface_quadrature_points(fe, phi_nodes, qps, n_qps)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: phi_nodes(:)
        type(type_subcell_qp), intent(inout) :: qps(:)
        integer(int32), intent(inout) :: n_qps

        integer(int32) :: fe_id
        real(real64) :: v1(2), v2(2), v3(2)

        n_qps = 0
        call fe%get_type(fe_id)

        if (is_triangle_element(fe_id)) then
            v1 = [0.0d0, 0.0d0]
            v2 = [1.0d0, 0.0d0]
            v3 = [0.0d0, 1.0d0]
            call clip_and_collect(fe, phi_nodes, v1, v2, v3, qps, n_qps)
        else if (is_quad_element(fe_id)) then
            v1 = [-1.0d0, -1.0d0]; v2 = [1.0d0, -1.0d0]; v3 = [1.0d0, 1.0d0]
            call clip_and_collect(fe, phi_nodes, v1, v2, v3, qps, n_qps)
            v1 = [-1.0d0, -1.0d0]; v2 = [1.0d0, 1.0d0]; v3 = [-1.0d0, 1.0d0]
            call clip_and_collect(fe, phi_nodes, v1, v2, v3, qps, n_qps)
        end if
    end subroutine build_interface_quadrature_points

    ! =========================================================================
    ! Private helpers
    ! =========================================================================

    !> Clip one triangle against the line \(\phi^h = 0\) (vertex-linear) and
    !> emit the midpoint rule on every resulting sub-triangle.
    subroutine clip_and_collect(fe, phi_nodes, v1, v2, v3, qps, n_qps)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: phi_nodes(:)
        real(real64), intent(in) :: v1(2), v2(2), v3(2)
        type(type_subcell_qp), intent(inout) :: qps(:)
        integer(int32), intent(inout) :: n_qps

        real(real64) :: v(2, 3), phi(3)
        logical :: s(3)
        integer(int32) :: i, lone, a, b
        real(real64) :: pa(2), pb(2)

        v(:, 1) = v1; v(:, 2) = v2; v(:, 3) = v3
        do i = 1, 3
            call lerp_at(fe, phi_nodes, v(:, i), phi(i))
            s(i) = phi(i) > 0.0d0
        end do

        if (all(s) .or. .not. any(s)) then
            call emit_triangle(v(:, 1), v(:, 2), v(:, 3), s(1), qps, n_qps)
            return
        end if

        ! Exactly one vertex ("lone") lies on the minority side.
        if (s(1) .neqv. s(2)) then
            if (s(1) .neqv. s(3)) then
                lone = 1
            else
                lone = 2
            end if
        else
            lone = 3
        end if
        a = mod(lone, 3) + 1
        b = mod(lone + 1, 3) + 1

        pa = edge_zero_point(v(:, lone), phi(lone), v(:, a), phi(a))
        pb = edge_zero_point(v(:, lone), phi(lone), v(:, b), phi(b))

        ! Lone-side triangle and the complementary quadrilateral (as 2 triangles).
        call emit_triangle(v(:, lone), pa, pb, s(lone), qps, n_qps)
        call emit_triangle(pa, v(:, a), v(:, b), s(a), qps, n_qps)
        call emit_triangle(pa, v(:, b), pb, s(a), qps, n_qps)
    end subroutine clip_and_collect

    !> Zero-crossing of the linear interpolant between two vertices.
    pure function edge_zero_point(va, fa, vb, fb) result(p)
        implicit none
        real(real64), intent(in) :: va(2), fa, vb(2), fb
        real(real64) :: p(2), t, denom

        denom = fa - fb
        if (abs(denom) > tiny(1.0d0)) then
            t = fa / denom
        else
            t = 0.0d0
        end if
        t = max(0.0d0, min(1.0d0, t))
        p = va + t * (vb - va)
    end function edge_zero_point

    !> Degree-2 midpoint rule (3 points, weight = area/3) on one sub-triangle.
    subroutine emit_triangle(a, b, c, plus_side, qps, n_qps)
        implicit none
        real(real64), intent(in) :: a(2), b(2), c(2)
        logical, intent(in) :: plus_side
        type(type_subcell_qp), intent(inout) :: qps(:)
        integer(int32), intent(inout) :: n_qps

        real(real64) :: area, w
        real(real64) :: pts(2, 3)
        integer(int32) :: k

        area = 0.5d0 * abs((b(1) - a(1)) * (c(2) - a(2)) - (c(1) - a(1)) * (b(2) - a(2)))
        if (area <= 0.0d0) return

        pts(:, 1) = 0.5d0 * (a + b)
        pts(:, 2) = 0.5d0 * (b + c)
        pts(:, 3) = 0.5d0 * (c + a)
        w = area / 3.0d0

        do k = 1, 3
            if (n_qps >= size(qps)) return
            n_qps = n_qps + 1
            qps(n_qps)%xi = pts(1, k)
            qps(n_qps)%eta = pts(2, k)
            qps(n_qps)%weight = w
            qps(n_qps)%is_plus_side = plus_side
        end do
    end subroutine emit_triangle

    !> Interpolate a nodal scalar at reference point using parent shape functions.
    subroutine lerp_at(fe, nodal_values, v, val)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: nodal_values(:)
        real(real64), intent(in) :: v(2)
        real(real64), intent(inout) :: val

        type(type_coordinate_dp) :: r

        r%x = v(1); r%y = v(2); r%z = 0.0d0
        call fe%lerp(r, nodal_values, val)
    end subroutine lerp_at

    !> Return .true. for triangle-family elements.
    pure function is_triangle_element(fe_id) result(is_tri)
        implicit none
        integer(int32), intent(in) :: fe_id
        logical :: is_tri
        is_tri = (fe_id == FE_TYPE%TRIANGLE%ID .or. &
                  fe_id == FE_TYPE%QUADRATIC_TRIANGLE%ID .or. &
                  fe_id == FE_TYPE%LAGRANGE_TRIANGLE%ID .or. &
                  fe_id == FE_TYPE%HIGHER_ORDER_TRIANGLE%ID)
    end function is_triangle_element

    !> Return .true. for quadrilateral-family elements.
    pure function is_quad_element(fe_id) result(is_quad)
        implicit none
        integer(int32), intent(in) :: fe_id
        logical :: is_quad
        is_quad = (fe_id == FE_TYPE%QUAD%ID .or. &
                   fe_id == FE_TYPE%QUADRATIC_QUAD%ID .or. &
                   fe_id == FE_TYPE%LAGRANGE_QUADRILATERAL%ID .or. &
                   fe_id == FE_TYPE%HIGHER_ORDER_QUAD%ID)
    end function is_quad_element

end module domain_fe_subcell
