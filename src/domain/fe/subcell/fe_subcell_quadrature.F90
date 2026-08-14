!> @brief Interface-split, error-controlled subcell quadrature.
!>
!> Given a nodal indicator field \(\phi\) (interpolated with the parent shape
!> functions), decomposes the reference element into the sub-domains
!> \(\Omega_e^+ = \{\phi^h > 0\}\) and \(\Omega_e^- = \{\phi^h \le 0\}\) and
!> holds one quadrature rule covering BOTH sub-domains, each point tagged with
!> its side.  The intended use is the freezing interface
!> \(\phi = (s_f - s_m) + \varepsilon_s\) (type_fusion%calc_freezing_level_set,
!> models_phase_change_fusion): plus side = ice present, minus side = ice-free.
!>
!> ### Method
!> The reference element is refined recursively using cell topology only
!> (type_subcell_cell), while \(\phi^h\), \(\boldsymbol{x}(\xi)\) and
!> \(\nabla N_a\) always come from the parent element.  Each cell is split into
!> triangles, every triangle is clipped against the straight line obtained by
!> linear interpolation of the vertex values of \(\phi^h\), and every
!> sub-triangle carries the degree-2 midpoint rule (3 points, weight = area/3).
!>
!> Whether a cell is refined is decided by the quadrature error itself.  With
!> the weak-form terms \(k\) supplied as an abst_subcell_integrand, the cell
!> value \(Q^{(0)}_{s,k}\) is compared with the sum over its four children
!> \(Q^{(1)}_{s,k} = \sum_{c} Q^{(0)}_{s_c,k}\) through
!> \[
!> \eta_{s,k} = \frac{\left| Q^{(1)}_{s,k} - Q^{(0)}_{s,k} \right|}
!>                   {A_k + \left| Q^{(1)}_{s,k} \right|}, \qquad
!> \eta_s = \max_k \eta_{s,k},
!> \]
!> and the cell is refined while \(\eta_s > \varepsilon_{\mathrm{quad}}\) and
!> \(d < d_{\max}\); otherwise the finer value \(Q^{(1)}_s\) is accepted, so the
!> children's points are the ones emitted.  Terms must be compared
!> individually: a sum such as \(R_{\mathrm{adv}} + R_{\mathrm{cond}}\) can be
!> small while both integrals are badly resolved.
!>
!> Without an integrand every cell is refined uniformly to \(d_{\max}\).  This
!> fixed-depth geometric rule resolves interfaces hidden between the root
!> probes and gives cut and uncut elements the same leaf triangulation.
!>
!> ### Numerical guarantees
!> - The subcells tile the reference element exactly, so the weights sum to the
!>   reference measure and every polynomial of degree 2 is integrated exactly
!>   over the whole element, at any depth and for any cut.
!> - A straight interface is resolved exactly at any depth.
!> - The midpoint rule integrates \(N_a N_b\) exactly only for linear
!>   triangles; for every higher-order element the error control is what keeps
!>   the assembled matrices accurate, and with it the refinement pattern can
!>   change without the accepted terms moving by more than
!>   \(\varepsilon_{\mathrm{quad}}\).
!>
!> Memory: \(O(4^{d_{\max}})\) points, allocated once by initialize.
!> Arithmetic: \(O(4^{d_{\max}})\) integrand evaluations per element in the
!> worst case; the error control costs one extra cell evaluation per level.
!> Failure behavior: for an unsupported element family the rule is marked
!> unusable (is_usable is .false., get_num_points returns 0) so the caller can
!> fall back to the standard Gauss rule; a partial rule is never exposed.
module domain_fe_subcell
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only: type_coordinate_dp, FE_TYPE
    use :: domain_base_fe, only: abst_fe
    use :: domain_fe_subcell_topology, only: type_subcell_cell
    implicit none
    private

    public :: abst_subcell_integrand
    public :: type_subcell_quadrature

    !> Refinement depth used when initialize is called without one.
    integer(int32), parameter :: DEFAULT_MAX_DEPTH = 0
    !> Relative quadrature tolerance used when initialize is called without one.
    real(real64), parameter :: DEFAULT_TOLERANCE = 1.0d-3
    !> Points emitted per triangle by the degree-2 midpoint rule.
    integer(int32), parameter :: POINTS_PER_TRIANGLE = 3
    !> Sub-triangles a clipped triangle is split into.
    integer(int32), parameter :: SUB_TRIANGLES_PER_CLIP = 3
    !> Triangles a quadrilateral cell is decomposed into.
    integer(int32), parameter :: TRIANGLES_PER_QUADRILATERAL = 2
    !> Points one cell can produce: 2 triangles * 3 sub-triangles * 3 points.
    integer(int32), parameter :: MAX_POINTS_PER_CELL = TRIANGLES_PER_QUADRILATERAL &
                                                       * SUB_TRIANGLES_PER_CLIP * POINTS_PER_TRIANGLE
    !> Vertex counts of the two supported cell shapes.
    integer(int32), parameter :: TRIANGLE_CELL_VERTICES = 3
    integer(int32), parameter :: QUADRILATERAL_CELL_VERTICES = 4
    !> Vertex count meaning "this element family is not supported".
    integer(int32), parameter :: UNSUPPORTED_CELL_VERTICES = 0
    !> Largest depth whose capacity still fits in int32.
    integer(int32), parameter :: CAPACITY_DEPTH_LIMIT = 13

    !> Weak-form terms whose quadrature error drives the refinement.
    !>
    !> One term is one scalar integral the caller assembles on this rule, e.g.
    !> the accumulation, advection, conduction and source contributions, and
    !> the tangent entries that go with them.  They are compared term by term,
    !> so a cancellation between terms cannot hide an unresolved integral.
    type, abstract :: abst_subcell_integrand
    contains
        ! ---- Algorithm ----
        procedure(abst_evaluate_terms_subcell_integrand), public, pass(self), deferred :: evaluate_terms

        ! ---- Getter ----
        procedure(abst_get_num_terms_subcell_integrand), public, pass(self), deferred :: get_num_terms
        procedure(abst_get_term_scales_subcell_integrand), public, pass(self), deferred :: get_term_scales
    end type abst_subcell_integrand

    abstract interface
        !> @brief Values of every term at one point of the parent reference
        !> element, including any \(|\det J|\) the caller wants integrated.
        subroutine abst_evaluate_terms_subcell_integrand(self, xi, eta, is_plus_side, terms)
            import :: abst_subcell_integrand, real64
            implicit none
            class(abst_subcell_integrand), intent(inout) :: self
            !> Reference coordinates \((\xi, \eta)\) of the point.
            real(real64), intent(in) :: xi
            real(real64), intent(in) :: eta
            !> Side of the interface the point belongs to.
            logical, intent(in) :: is_plus_side
            !> Term values, size >= get_num_terms.
            real(real64), intent(inout) :: terms(:)
        end subroutine abst_evaluate_terms_subcell_integrand

        !> @brief Number of terms compared by the error control.
        pure subroutine abst_get_num_terms_subcell_integrand(self, num_terms)
            import :: abst_subcell_integrand, int32
            implicit none
            class(abst_subcell_integrand), intent(in) :: self
            integer(int32), intent(inout) :: num_terms
        end subroutine abst_get_num_terms_subcell_integrand

        !> @brief Absolute scales \(A_k\) that keep \(\eta_{s,k}\) finite where a
        !> term integrates to nearly zero.  Must be positive.
        pure subroutine abst_get_term_scales_subcell_integrand(self, scales)
            import :: abst_subcell_integrand, real64
            implicit none
            class(abst_subcell_integrand), intent(in) :: self
            real(real64), intent(inout) :: scales(:)
        end subroutine abst_get_term_scales_subcell_integrand
    end interface

    !> One subcell quadrature point in parent reference coordinates.
    type :: type_quadrature_point
        !> Reference coordinates \((\xi, \eta)\) in the parent element.
        real(real64) :: xi = 0.0d0
        real(real64) :: eta = 0.0d0
        !> Integration weight in reference space; excludes \(|\det J|\).
        real(real64) :: weight = 0.0d0
        !> Side of the interface: .true. for \(\phi^h > 0\).
        logical :: is_plus_side = .false.
    end type type_quadrature_point

    !> Interface-split quadrature rule of one element.
    type :: type_subcell_quadrature
        !> Refinement depth \(d_{\max}\) the storage was sized for.
        integer(int32), private :: max_depth = DEFAULT_MAX_DEPTH
        !> Relative quadrature tolerance \(\varepsilon_{\mathrm{quad}}\).
        real(real64), private :: tolerance = DEFAULT_TOLERANCE
        !> Number of valid points of the current element.
        integer(int32), private :: num_points = 0
        !> Largest \(\eta_s\) met while building the current rule.
        real(real64), private :: max_indicator = 0.0d0
        !> Depth of the deepest cell emitted for the current element.
        integer(int32), private :: reached_depth = 0
        !> .true. once compute has run for the current element.
        logical, private :: is_computed = .false.
        !> .true. if the storage was too small, which invalidates the rule.
        logical, private :: has_overflowed = .false.
        !> Point storage, sized for the worst case at max_depth.
        type(type_quadrature_point), allocatable, private :: points(:)
        !> Term buffers of the error control, sized by the integrand.
        real(real64), allocatable, private :: coarse_terms(:)
        real(real64), allocatable, private :: fine_terms(:)
        real(real64), allocatable, private :: child_terms(:)
        real(real64), allocatable, private :: point_terms(:)
        real(real64), allocatable, private :: term_scales(:)
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_subcell_quadrature
        procedure, public, pass(self) :: destroy => destroy_subcell_quadrature
        procedure, public, pass(self) :: reset => reset_subcell_quadrature

        ! ---- Algorithm ----
        procedure, public, pass(self) :: compute => compute_subcell_quadrature
        procedure, private, pass(self) :: integrate_cell => integrate_cell_subcell_quadrature
        procedure, private, pass(self) :: evaluate_cell => evaluate_cell_subcell_quadrature
        procedure, private, pass(self) :: emit_cell => emit_cell_subcell_quadrature

        ! ---- Inquiry ----
        procedure, public, pass(self) :: is_usable => is_usable_subcell_quadrature

        ! ---- Getter ----
        procedure, public, pass(self) :: get_num_points => get_num_points_subcell_quadrature
        procedure, public, pass(self) :: get_point => get_point_subcell_quadrature
        procedure, public, pass(self) :: get_max_depth => get_max_depth_subcell_quadrature
        procedure, public, pass(self) :: get_tolerance => get_tolerance_subcell_quadrature
        procedure, public, pass(self) :: get_capacity => get_capacity_subcell_quadrature
        procedure, public, pass(self) :: get_max_indicator => get_max_indicator_subcell_quadrature
        procedure, public, pass(self) :: get_reached_depth => get_reached_depth_subcell_quadrature
    end type type_subcell_quadrature

contains

    ! ---- Lifecycle ----

    !> @brief Allocate the point storage for a refinement depth.
    !>
    !> Sizes the storage for the worst case
    !> \[ n_{\max} = n_{\mathrm{triangles}} \, n_{\mathrm{clip}} \, n_{q} \, 4^{d} \]
    !> with \(n_{\mathrm{triangles}} = 2\), i.e. the quadrilateral family, so the
    !> same object serves any 2D element.  Numerical guarantee: compute can
    !> never overflow this storage.  Memory: \(O(4^{d})\).
    !> Failure behavior: a negative depth is treated as 0, a depth beyond the
    !> int32 capacity limit is capped, and a non-positive tolerance is replaced
    !> by the default.
    subroutine initialize_subcell_quadrature(self, max_depth, tolerance)
        implicit none
        class(type_subcell_quadrature), intent(inout) :: self
        !> Refinement depth \(d_{\max} \ge 0\); defaults to 0, a single clip of
        !> the whole element.
        integer(int32), intent(in), optional :: max_depth
        !> Relative quadrature tolerance \(\varepsilon_{\mathrm{quad}} > 0\)
        !> used when an integrand is supplied to compute.
        real(real64), intent(in), optional :: tolerance

        integer(int32) :: capacity

        self%max_depth = DEFAULT_MAX_DEPTH
        if (present(max_depth)) self%max_depth = min(max(0, max_depth), CAPACITY_DEPTH_LIMIT)

        self%tolerance = DEFAULT_TOLERANCE
        if (present(tolerance)) then
            if (tolerance > 0.0d0) self%tolerance = tolerance
        end if

        capacity = calc_capacity(QUADRILATERAL_CELL_VERTICES, self%max_depth)
        if (allocated(self%points)) then
            if (size(self%points) /= capacity) deallocate (self%points)
        end if
        if (.not. allocated(self%points)) allocate (self%points(capacity))

        call self%reset()
    end subroutine initialize_subcell_quadrature

    !> @brief Release the point storage and the error-control buffers.
    subroutine destroy_subcell_quadrature(self)
        implicit none
        class(type_subcell_quadrature), intent(inout) :: self

        if (allocated(self%points)) deallocate (self%points)
        if (allocated(self%coarse_terms)) deallocate (self%coarse_terms)
        if (allocated(self%fine_terms)) deallocate (self%fine_terms)
        if (allocated(self%child_terms)) deallocate (self%child_terms)
        if (allocated(self%point_terms)) deallocate (self%point_terms)
        if (allocated(self%term_scales)) deallocate (self%term_scales)
        self%max_depth = DEFAULT_MAX_DEPTH
        self%tolerance = DEFAULT_TOLERANCE
        call self%reset()
    end subroutine destroy_subcell_quadrature

    !> @brief Discard the rule of the current element, keeping the storage.
    subroutine reset_subcell_quadrature(self)
        implicit none
        class(type_subcell_quadrature), intent(inout) :: self

        self%num_points = 0
        self%max_indicator = 0.0d0
        self%reached_depth = 0
        self%is_computed = .false.
        self%has_overflowed = .false.
    end subroutine reset_subcell_quadrature

    ! ---- Algorithm ----

    !> @brief Build the interface-split rule of one element.
    !>
    !> With an integrand the refinement is error controlled; without one it
    !> uses uniform refinement to the requested depth. Numerical guarantees and
    !> failure behavior are those of the module.
    subroutine compute_subcell_quadrature(self, fe, phi_nodes, integrand, refinement_depth)
        implicit none
        class(type_subcell_quadrature), intent(inout) :: self
        !> Parent finite element; supplies \(\phi^h\) through lerp.
        class(abst_fe), intent(in) :: fe
        !> Nodal level-set values, size = number of parent nodes; plus side
        !> where \(\phi > 0\).
        real(real64), intent(in) :: phi_nodes(:)
        !> Weak-form terms driving the error control; absent means the
        !> level-set criterion is used instead.
        class(abst_subcell_integrand), intent(inout), optional :: integrand
        !> Uniform refinement depth used without an integrand, or the maximum
        !> adaptive depth used with one. Clamped to the initialized capacity.
        integer(int32), intent(in), optional :: refinement_depth

        type(type_subcell_cell) :: root_cell
        integer(int32) :: fe_id, num_cell_vertices, num_terms, target_depth

        if (.not. allocated(self%points)) call self%initialize()
        call self%reset()
        self%is_computed = .true.

        call fe%get_type(fe_id)
        num_cell_vertices = get_cell_vertex_count(fe_id)
        if (num_cell_vertices == UNSUPPORTED_CELL_VERTICES) return

        target_depth = self%max_depth
        if (present(refinement_depth)) target_depth = min(max(0, refinement_depth), self%max_depth)

        if (present(integrand)) then
            num_terms = 0
            call integrand%get_num_terms(num_terms)
            if (num_terms < 1) return
            call resize_term_buffers(self, num_terms)
            call integrand%get_term_scales(self%term_scales)
        end if

        call root_cell%initialize(num_cell_vertices)
        call self%integrate_cell(fe, phi_nodes, root_cell, 0, target_depth, integrand)

        ! A truncated rule loses area silently; report none at all instead.
        if (self%has_overflowed) self%num_points = 0
    end subroutine compute_subcell_quadrature

    !> Integrate one cell: refine while the criterion asks for it, emit
    !> otherwise.
    recursive subroutine integrate_cell_subcell_quadrature(self, fe, phi_nodes, cell, depth, target_depth, integrand)
        implicit none
        class(type_subcell_quadrature), intent(inout) :: self
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: phi_nodes(:)
        type(type_subcell_cell), intent(inout) :: cell
        integer(int32), intent(in) :: depth
        integer(int32), intent(in) :: target_depth
        class(abst_subcell_integrand), intent(inout), optional :: integrand

        type(type_subcell_cell) :: child
        real(real64) :: probe_point(2), phi, indicator
        integer(int32) :: num_probe_points, num_children, probe, index, term

        if (self%has_overflowed) return

        call cell%compute_probe_points()
        call cell%get_num_probe_points(num_probe_points)
        if (num_probe_points == 0) return

        if (depth >= target_depth) then
            call self%emit_cell(fe, phi_nodes, cell, depth)
            return
        end if

        ! The probe samples are what the level-set criterion tests and what the
        ! children are built from, so they are taken whenever refinement is
        ! still possible.
        do probe = 1, num_probe_points
            call cell%get_probe_point(probe, probe_point)
            call interpolate_phi(fe, phi_nodes, probe_point, phi)
            call cell%set_probe_phi(probe, phi)
        end do
        call cell%get_num_children(num_children)

        if (.not. present(integrand)) then
            do index = 1, num_children
                call cell%get_child(index, child)
                call self%integrate_cell(fe, phi_nodes, child, depth + 1, target_depth, integrand)
            end do
            return
        end if

        ! Error control: this cell's value against the sum over its children,
        ! term by term.
        call self%evaluate_cell(fe, phi_nodes, cell, integrand, self%coarse_terms)
        self%fine_terms = 0.0d0
        do index = 1, num_children
            call cell%get_child(index, child)
            call self%evaluate_cell(fe, phi_nodes, child, integrand, self%child_terms)
            self%fine_terms = self%fine_terms + self%child_terms
        end do

        indicator = 0.0d0
        do term = 1, size(self%fine_terms)
            indicator = max(indicator, abs(self%fine_terms(term) - self%coarse_terms(term)) &
                            / (self%term_scales(term) + abs(self%fine_terms(term))))
        end do
        self%max_indicator = max(self%max_indicator, indicator)

        ! The finer value is the accepted one, so its points are emitted.
        if (indicator <= self%tolerance .or. depth + 1 >= target_depth) then
            do index = 1, num_children
                call cell%get_child(index, child)
                call self%emit_cell(fe, phi_nodes, child, depth + 1)
            end do
        else
            do index = 1, num_children
                call cell%get_child(index, child)
                call self%integrate_cell(fe, phi_nodes, child, depth + 1, target_depth, integrand)
            end do
        end if
    end subroutine integrate_cell_subcell_quadrature

    !> Integrate every term over one cell without emitting its points.
    subroutine evaluate_cell_subcell_quadrature(self, fe, phi_nodes, cell, integrand, terms)
        implicit none
        class(type_subcell_quadrature), intent(inout) :: self
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: phi_nodes(:)
        type(type_subcell_cell), intent(inout) :: cell
        class(abst_subcell_integrand), intent(inout) :: integrand
        real(real64), intent(inout) :: terms(:)

        type(type_quadrature_point) :: cell_points(MAX_POINTS_PER_CELL)
        integer(int32) :: num_cell_points, point

        terms = 0.0d0
        call build_cell_points(fe, phi_nodes, cell, cell_points, num_cell_points)
        do point = 1, num_cell_points
            call integrand%evaluate_terms(cell_points(point)%xi, cell_points(point)%eta, &
                                          cell_points(point)%is_plus_side, self%point_terms)
            terms = terms + cell_points(point)%weight * self%point_terms
        end do
    end subroutine evaluate_cell_subcell_quadrature

    !> Append the points of one cell to the rule.
    subroutine emit_cell_subcell_quadrature(self, fe, phi_nodes, cell, depth)
        implicit none
        class(type_subcell_quadrature), intent(inout) :: self
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: phi_nodes(:)
        type(type_subcell_cell), intent(inout) :: cell
        integer(int32), intent(in) :: depth

        type(type_quadrature_point) :: cell_points(MAX_POINTS_PER_CELL)
        integer(int32) :: num_cell_points, point

        call build_cell_points(fe, phi_nodes, cell, cell_points, num_cell_points)
        if (self%num_points + num_cell_points > size(self%points)) then
            self%has_overflowed = .true.
            return
        end if

        do point = 1, num_cell_points
            self%num_points = self%num_points + 1
            self%points(self%num_points) = cell_points(point)
        end do
        self%reached_depth = max(self%reached_depth, depth)
    end subroutine emit_cell_subcell_quadrature

    ! ---- Inquiry ----

    !> @brief .true. when a non-empty rule is available for the current element.
    pure function is_usable_subcell_quadrature(self) result(is_usable)
        implicit none
        class(type_subcell_quadrature), intent(in) :: self
        logical :: is_usable

        is_usable = self%is_computed .and. (self%num_points > 0)
    end function is_usable_subcell_quadrature

    ! ---- Getter ----

    pure subroutine get_num_points_subcell_quadrature(self, num_points)
        implicit none
        class(type_subcell_quadrature), intent(in) :: self
        !> Number of quadrature points; 0 when the rule is unusable.
        integer(int32), intent(inout) :: num_points
        num_points = self%num_points
    end subroutine get_num_points_subcell_quadrature

    !> @brief One quadrature point.  Failure behavior: an out-of-range index
    !> returns a zero-weight point at the origin.
    pure subroutine get_point_subcell_quadrature(self, index, xi, eta, weight, is_plus_side)
        implicit none
        class(type_subcell_quadrature), intent(in) :: self
        !> Point index, \(1 \le\) index \(\le\) get_num_points.
        integer(int32), intent(in) :: index
        !> Reference coordinates \((\xi, \eta)\) in the parent element.
        real(real64), intent(inout) :: xi
        real(real64), intent(inout) :: eta
        !> Reference-space weight; multiply by \(|\det J|\) to integrate.
        real(real64), intent(inout) :: weight
        !> .true. where \(\phi^h > 0\).
        logical, intent(inout) :: is_plus_side

        xi = 0.0d0
        eta = 0.0d0
        weight = 0.0d0
        is_plus_side = .false.
        if (index < 1 .or. index > self%num_points) return

        xi = self%points(index)%xi
        eta = self%points(index)%eta
        weight = self%points(index)%weight
        is_plus_side = self%points(index)%is_plus_side
    end subroutine get_point_subcell_quadrature

    pure subroutine get_max_depth_subcell_quadrature(self, max_depth)
        implicit none
        class(type_subcell_quadrature), intent(in) :: self
        !> Refinement depth the storage was sized for.
        integer(int32), intent(inout) :: max_depth
        max_depth = self%max_depth
    end subroutine get_max_depth_subcell_quadrature

    pure subroutine get_tolerance_subcell_quadrature(self, tolerance)
        implicit none
        class(type_subcell_quadrature), intent(in) :: self
        !> Relative quadrature tolerance of the error control.
        real(real64), intent(inout) :: tolerance
        tolerance = self%tolerance
    end subroutine get_tolerance_subcell_quadrature

    pure subroutine get_capacity_subcell_quadrature(self, capacity)
        implicit none
        class(type_subcell_quadrature), intent(in) :: self
        !> Worst-case number of points the storage holds; 0 before initialize.
        integer(int32), intent(inout) :: capacity
        capacity = 0
        if (allocated(self%points)) capacity = size(self%points)
    end subroutine get_capacity_subcell_quadrature

    pure subroutine get_max_indicator_subcell_quadrature(self, max_indicator)
        implicit none
        class(type_subcell_quadrature), intent(in) :: self
        !> Largest \(\eta_s\) met while building the current rule; 0 when the
        !> level-set criterion was used.
        real(real64), intent(inout) :: max_indicator
        max_indicator = self%max_indicator
    end subroutine get_max_indicator_subcell_quadrature

    pure subroutine get_reached_depth_subcell_quadrature(self, reached_depth)
        implicit none
        class(type_subcell_quadrature), intent(in) :: self
        !> Depth of the deepest emitted cell of the current element.
        integer(int32), intent(inout) :: reached_depth
        reached_depth = self%reached_depth
    end subroutine get_reached_depth_subcell_quadrature

    ! =========================================================================
    ! Module-private helpers
    ! =========================================================================

    !> Allocate the error-control buffers for a given number of terms.
    subroutine resize_term_buffers(self, num_terms)
        implicit none
        class(type_subcell_quadrature), intent(inout) :: self
        integer(int32), intent(in) :: num_terms

        if (allocated(self%coarse_terms)) then
            if (size(self%coarse_terms) /= num_terms) then
                deallocate (self%coarse_terms, self%fine_terms, self%child_terms, &
                            self%point_terms, self%term_scales)
            end if
        end if
        if (.not. allocated(self%coarse_terms)) then
            allocate (self%coarse_terms(num_terms), self%fine_terms(num_terms), &
                      self%child_terms(num_terms), self%point_terms(num_terms), &
                      self%term_scales(num_terms))
        end if
        self%coarse_terms = 0.0d0
        self%fine_terms = 0.0d0
        self%child_terms = 0.0d0
        self%point_terms = 0.0d0
        self%term_scales = 1.0d0
    end subroutine resize_term_buffers

    !> Clip one cell against the interface and return its quadrature points.
    subroutine build_cell_points(fe, phi_nodes, cell, cell_points, num_cell_points)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: phi_nodes(:)
        type(type_subcell_cell), intent(inout) :: cell
        type(type_quadrature_point), intent(inout) :: cell_points(:)
        integer(int32), intent(inout) :: num_cell_points

        real(real64) :: probe_point(2), phi, triangle_vertices(2, 3), triangle_phi(3)
        integer(int32) :: num_vertices, num_triangles, vertex, triangle

        num_cell_points = 0
        call cell%compute_probe_points()
        call cell%get_num_vertices(num_vertices)
        if (num_vertices == 0) return

        do vertex = 1, num_vertices
            call cell%get_probe_point(vertex, probe_point)
            call interpolate_phi(fe, phi_nodes, probe_point, phi)
            call cell%set_probe_phi(vertex, phi)
        end do

        call cell%get_num_triangles(num_triangles)
        do triangle = 1, num_triangles
            call cell%get_triangle(triangle, triangle_vertices, triangle_phi)
            call clip_triangle(triangle_vertices, triangle_phi, cell_points, num_cell_points)
        end do
    end subroutine build_cell_points

    !> Clip one triangle against the line \(\phi^h = 0\) (vertex-linear) and
    !> append the midpoint rule of every resulting sub-triangle.  An uncut
    !> triangle is appended whole, which is the continuous limit of the clip.
    pure subroutine clip_triangle(vertices, phi, cell_points, num_cell_points)
        implicit none
        real(real64), intent(in) :: vertices(2, 3)
        real(real64), intent(in) :: phi(3)
        type(type_quadrature_point), intent(inout) :: cell_points(:)
        integer(int32), intent(inout) :: num_cell_points

        logical :: is_plus(3)
        integer(int32) :: vertex, lone, next, last
        real(real64) :: crossing_next(2), crossing_last(2)

        do vertex = 1, 3
            is_plus(vertex) = phi(vertex) > 0.0d0
        end do

        if (all(is_plus) .or. .not. any(is_plus)) then
            call append_triangle(vertices(:, 1), vertices(:, 2), vertices(:, 3), is_plus(1), &
                                 cell_points, num_cell_points)
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
        call append_triangle(vertices(:, lone), crossing_next, crossing_last, is_plus(lone), &
                             cell_points, num_cell_points)
        call append_triangle(crossing_next, vertices(:, next), vertices(:, last), is_plus(next), &
                             cell_points, num_cell_points)
        call append_triangle(crossing_next, vertices(:, last), crossing_last, is_plus(next), &
                             cell_points, num_cell_points)
    end subroutine clip_triangle

    !> Append the degree-2 midpoint rule (3 points, weight = area/3) of one
    !> sub-triangle.  Degenerate triangles contribute nothing.
    pure subroutine append_triangle(vertex_a, vertex_b, vertex_c, is_plus_side, cell_points, num_cell_points)
        implicit none
        real(real64), intent(in) :: vertex_a(2), vertex_b(2), vertex_c(2)
        logical, intent(in) :: is_plus_side
        type(type_quadrature_point), intent(inout) :: cell_points(:)
        integer(int32), intent(inout) :: num_cell_points

        real(real64) :: area, weight, edge_midpoints(2, POINTS_PER_TRIANGLE)
        integer(int32) :: point

        area = 0.5d0 * abs((vertex_b(1) - vertex_a(1)) * (vertex_c(2) - vertex_a(2)) &
                           - (vertex_c(1) - vertex_a(1)) * (vertex_b(2) - vertex_a(2)))
        if (area <= 0.0d0) return
        if (num_cell_points + POINTS_PER_TRIANGLE > size(cell_points)) return

        edge_midpoints(:, 1) = 0.5d0 * (vertex_a + vertex_b)
        edge_midpoints(:, 2) = 0.5d0 * (vertex_b + vertex_c)
        edge_midpoints(:, 3) = 0.5d0 * (vertex_c + vertex_a)
        weight = area / real(POINTS_PER_TRIANGLE, real64)

        do point = 1, POINTS_PER_TRIANGLE
            num_cell_points = num_cell_points + 1
            cell_points(num_cell_points)%xi = edge_midpoints(1, point)
            cell_points(num_cell_points)%eta = edge_midpoints(2, point)
            cell_points(num_cell_points)%weight = weight
            cell_points(num_cell_points)%is_plus_side = is_plus_side
        end do
    end subroutine append_triangle

    !> Worst-case point count: every cell refined to max_depth, every leaf cut.
    pure function calc_capacity(num_cell_vertices, max_depth) result(capacity)
        implicit none
        integer(int32), intent(in) :: num_cell_vertices
        integer(int32), intent(in) :: max_depth
        integer(int32) :: capacity

        integer(int32) :: points_per_cell

        select case (num_cell_vertices)
        case (TRIANGLE_CELL_VERTICES)
            points_per_cell = SUB_TRIANGLES_PER_CLIP * POINTS_PER_TRIANGLE
        case (QUADRILATERAL_CELL_VERTICES)
            points_per_cell = TRIANGLES_PER_QUADRILATERAL * SUB_TRIANGLES_PER_CLIP * POINTS_PER_TRIANGLE
        case default
            capacity = 0
            return
        end select

        capacity = points_per_cell * 4**min(max(0, max_depth), CAPACITY_DEPTH_LIMIT)
    end function calc_capacity

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

    !> Vertex count of the subdivision cell of an element family; the
    !> interpolation order is irrelevant and stays with the shape functions.
    pure function get_cell_vertex_count(fe_id) result(num_cell_vertices)
        implicit none
        integer(int32), intent(in) :: fe_id
        integer(int32) :: num_cell_vertices

        if (fe_id == FE_TYPE%TRIANGLE%ID .or. &
            fe_id == FE_TYPE%QUADRATIC_TRIANGLE%ID .or. &
            fe_id == FE_TYPE%BIQUADRATIC_TRIANGLE%ID .or. &
            fe_id == FE_TYPE%LAGRANGE_TRIANGLE%ID .or. &
            fe_id == FE_TYPE%HIGHER_ORDER_TRIANGLE%ID) then
            num_cell_vertices = TRIANGLE_CELL_VERTICES
        else if (fe_id == FE_TYPE%QUAD%ID .or. &
                 fe_id == FE_TYPE%QUADRATIC_QUAD%ID .or. &
                 fe_id == FE_TYPE%BIQUADRATIC_QUAD%ID .or. &
                 fe_id == FE_TYPE%LAGRANGE_QUADRILATERAL%ID .or. &
                 fe_id == FE_TYPE%HIGHER_ORDER_QUAD%ID) then
            num_cell_vertices = QUADRILATERAL_CELL_VERTICES
        else
            num_cell_vertices = UNSUPPORTED_CELL_VERTICES
        end if
    end function get_cell_vertex_count

end module domain_fe_subcell
