!> @brief One cell of the reference-space subdivision used by subcell quadrature.
!>
!> The cell knows only how many vertices it has - 3 (reference simplex) or 4
!> (reference square) - and never the interpolation order of the element it
!> lives in.  Refinement joins the edge midpoints, so one step splits the cell
!> into \(4\) similar cells of equal measure and depth \(d\) yields \(4^d\)
!> cells of linear size \(2^{-d}\).  The parent shape functions are applied to
!> the resulting points by the caller; no degrees of freedom are attached to a
!> subcell.
!>
!> The cell also stores the level-set samples taken at its probe points, which
!> are its own vertices followed by the additional vertices of its four
!> children.  A crossing test over that set therefore samples every point the
!> children would use, which is what makes an adaptive refinement decision
!> consistent with the split the children would produce.
module domain_fe_subcell_topology
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    private

    public :: type_subcell_cell

    !> Vertices of the largest cell (quadrilateral).
    integer(int32), parameter :: MAX_VERTICES = 4
    !> Probe points of the largest cell: 4 vertices + 4 edge midpoints + centre.
    integer(int32), parameter :: MAX_PROBE_POINTS = 9
    !> Children produced by one refinement step.
    integer(int32), parameter :: CHILDREN_PER_CELL = 4
    !> Triangles a quadrilateral cell is decomposed into.
    integer(int32), parameter :: MAX_TRIANGLES = 2
    !> Vertex counts identifying the two supported cell shapes.
    integer(int32), parameter :: TRIANGLE_VERTICES = 3
    integer(int32), parameter :: QUADRILATERAL_VERTICES = 4
    !> Measures of the reference cells.
    real(real64), parameter :: SIMPLEX_MEASURE = 0.5d0
    real(real64), parameter :: SQUARE_MEASURE = 4.0d0

    !> One cell of the recursive subdivision, in parent reference coordinates.
    type :: type_subcell_cell
        !> 3 for a triangle cell, 4 for a quadrilateral cell, 0 when unset.
        integer(int32), private :: num_vertices = 0
        !> Vertex coordinates \((\xi, \eta)\), counter-clockwise.
        real(real64), private :: vertices(2, MAX_VERTICES) = 0.0d0
        !> Number of valid probe points; 0 until compute_probe_points is called.
        integer(int32), private :: num_probe_points = 0
        !> Probe coordinates: vertices first, then the children's extra vertices.
        real(real64), private :: probe_points(2, MAX_PROBE_POINTS) = 0.0d0
        !> Level-set samples \(\phi^h\) at the probe points.
        real(real64), private :: probe_phi(MAX_PROBE_POINTS) = 0.0d0
        !> Number of leading probe points whose sample has been set.
        integer(int32), private :: num_sampled = 0
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_subcell_cell
        procedure, public, pass(self) :: reset => reset_subcell_cell

        ! ---- Mutator ----
        procedure, public, pass(self) :: set_probe_phi => set_probe_phi_subcell_cell

        ! ---- Algorithm ----
        procedure, public, pass(self) :: compute_probe_points => compute_probe_points_subcell_cell

        ! ---- Inquiry ----
        procedure, public, pass(self) :: is_valid => is_valid_subcell_cell
        procedure, public, pass(self) :: is_sign_mixed => is_sign_mixed_subcell_cell

        ! ---- Getter ----
        procedure, public, pass(self) :: get_num_vertices => get_num_vertices_subcell_cell
        procedure, public, pass(self) :: get_num_probe_points => get_num_probe_points_subcell_cell
        procedure, public, pass(self) :: get_num_children => get_num_children_subcell_cell
        procedure, public, pass(self) :: get_num_triangles => get_num_triangles_subcell_cell
        procedure, public, pass(self) :: get_reference_measure => get_reference_measure_subcell_cell
        procedure, public, pass(self) :: get_probe_point => get_probe_point_subcell_cell
        procedure, public, pass(self) :: get_child => get_child_subcell_cell
        procedure, public, pass(self) :: get_triangle => get_triangle_subcell_cell
    end type type_subcell_cell

contains

    ! ---- Lifecycle ----

    !> @brief Set the cell to the whole reference element of the given shape.
    !>
    !> Assumption: num_vertices is 3 (simplex \((0,0),(1,0),(0,1)\)) or 4
    !> (square \([-1,1]^2\)); any other value leaves the cell invalid.
    !> Complexity: \(O(1)\).
    pure subroutine initialize_subcell_cell(self, num_vertices)
        implicit none
        class(type_subcell_cell), intent(inout) :: self
        !> Number of vertices of the reference cell: 3 or 4.
        integer(int32), intent(in) :: num_vertices

        call self%reset()

        select case (num_vertices)
        case (TRIANGLE_VERTICES)
            self%num_vertices = TRIANGLE_VERTICES
            self%vertices(:, 1) = [0.0d0, 0.0d0]
            self%vertices(:, 2) = [1.0d0, 0.0d0]
            self%vertices(:, 3) = [0.0d0, 1.0d0]
        case (QUADRILATERAL_VERTICES)
            self%num_vertices = QUADRILATERAL_VERTICES
            self%vertices(:, 1) = [-1.0d0, -1.0d0]
            self%vertices(:, 2) = [1.0d0, -1.0d0]
            self%vertices(:, 3) = [1.0d0, 1.0d0]
            self%vertices(:, 4) = [-1.0d0, 1.0d0]
        end select
    end subroutine initialize_subcell_cell

    !> @brief Discard the cell geometry and its level-set samples.
    pure subroutine reset_subcell_cell(self)
        implicit none
        class(type_subcell_cell), intent(inout) :: self

        self%num_vertices = 0
        self%vertices = 0.0d0
        self%num_probe_points = 0
        self%probe_points = 0.0d0
        self%probe_phi = 0.0d0
        self%num_sampled = 0
    end subroutine reset_subcell_cell

    ! ---- Mutator ----

    !> @brief Store the level-set sample \(\phi^h\) of one probe point.
    !>
    !> Samples must be set from index 1 upwards; the cell records how many
    !> leading probes carry a sample and the crossing test uses only those.
    !> Failure behavior: an out-of-range index is ignored.
    pure subroutine set_probe_phi_subcell_cell(self, index, phi)
        implicit none
        class(type_subcell_cell), intent(inout) :: self
        !> Probe index, \(1 \le\) index \(\le\) get_num_probe_points.
        integer(int32), intent(in) :: index
        !> Level-set value at that probe point.
        real(real64), intent(in) :: phi

        if (index < 1 .or. index > self%num_probe_points) return
        self%probe_phi(index) = phi
        self%num_sampled = max(self%num_sampled, index)
    end subroutine set_probe_phi_subcell_cell

    ! ---- Algorithm ----

    !> @brief Compute the probe points: the cell vertices, then the extra
    !> vertices of the four children (edge midpoints, plus the centre for a
    !> quadrilateral).
    !>
    !> Numerical guarantee: the probe set equals the union of the children's
    !> vertices exactly, so a refinement decision taken on it cannot disagree
    !> with the children it creates.  Complexity: \(O(1)\).
    pure subroutine compute_probe_points_subcell_cell(self)
        implicit none
        class(type_subcell_cell), intent(inout) :: self

        integer(int32) :: vertex

        self%num_probe_points = 0
        self%num_sampled = 0
        do vertex = 1, self%num_vertices
            self%probe_points(:, vertex) = self%vertices(:, vertex)
        end do

        if (self%num_vertices == TRIANGLE_VERTICES) then
            self%probe_points(:, 4) = 0.5d0 * (self%vertices(:, 1) + self%vertices(:, 2))
            self%probe_points(:, 5) = 0.5d0 * (self%vertices(:, 2) + self%vertices(:, 3))
            self%probe_points(:, 6) = 0.5d0 * (self%vertices(:, 3) + self%vertices(:, 1))
            self%num_probe_points = 6
        else if (self%num_vertices == QUADRILATERAL_VERTICES) then
            self%probe_points(:, 5) = 0.5d0 * (self%vertices(:, 1) + self%vertices(:, 2))
            self%probe_points(:, 6) = 0.5d0 * (self%vertices(:, 2) + self%vertices(:, 3))
            self%probe_points(:, 7) = 0.5d0 * (self%vertices(:, 3) + self%vertices(:, 4))
            self%probe_points(:, 8) = 0.5d0 * (self%vertices(:, 4) + self%vertices(:, 1))
            self%probe_points(:, 9) = 0.25d0 * (self%vertices(:, 1) + self%vertices(:, 2) &
                                                + self%vertices(:, 3) + self%vertices(:, 4))
            self%num_probe_points = 9
        end if
    end subroutine compute_probe_points_subcell_cell

    ! ---- Inquiry ----

    !> @brief .true. when the cell holds one of the two supported shapes.
    pure function is_valid_subcell_cell(self) result(is_valid)
        implicit none
        class(type_subcell_cell), intent(in) :: self
        logical :: is_valid

        is_valid = (self%num_vertices == TRIANGLE_VERTICES .or. &
                    self%num_vertices == QUADRILATERAL_VERTICES)
    end function is_valid_subcell_cell

    !> @brief .true. when the sampled level set changes sign over the cell,
    !> i.e. \(\min \phi^h \le 0 < \max \phi^h\) on the sampled probes.
    pure function is_sign_mixed_subcell_cell(self) result(is_mixed)
        implicit none
        class(type_subcell_cell), intent(in) :: self
        logical :: is_mixed

        is_mixed = .false.
        if (self%num_sampled < 1) return
        is_mixed = any(self%probe_phi(1:self%num_sampled) > 0.0d0) .and. &
                   .not. all(self%probe_phi(1:self%num_sampled) > 0.0d0)
    end function is_sign_mixed_subcell_cell

    ! ---- Getter ----

    pure subroutine get_num_vertices_subcell_cell(self, num_vertices)
        implicit none
        class(type_subcell_cell), intent(in) :: self
        !> 3, 4, or 0 when the cell is unset.
        integer(int32), intent(inout) :: num_vertices
        num_vertices = self%num_vertices
    end subroutine get_num_vertices_subcell_cell

    pure subroutine get_num_probe_points_subcell_cell(self, num_probe_points)
        implicit none
        class(type_subcell_cell), intent(in) :: self
        !> 6 (triangle), 9 (quadrilateral), or 0 before compute_probe_points.
        integer(int32), intent(inout) :: num_probe_points
        num_probe_points = self%num_probe_points
    end subroutine get_num_probe_points_subcell_cell

    pure subroutine get_num_children_subcell_cell(self, num_children)
        implicit none
        class(type_subcell_cell), intent(in) :: self
        !> Number of children one refinement step produces; 0 if invalid.
        integer(int32), intent(inout) :: num_children
        num_children = 0
        if (self%is_valid()) num_children = CHILDREN_PER_CELL
    end subroutine get_num_children_subcell_cell

    pure subroutine get_num_triangles_subcell_cell(self, num_triangles)
        implicit none
        class(type_subcell_cell), intent(in) :: self
        !> Triangles the cell decomposes into: 1 (triangle), 2 (quadrilateral).
        integer(int32), intent(inout) :: num_triangles
        num_triangles = 0
        if (self%num_vertices == TRIANGLE_VERTICES) then
            num_triangles = 1
        else if (self%num_vertices == QUADRILATERAL_VERTICES) then
            num_triangles = MAX_TRIANGLES
        end if
    end subroutine get_num_triangles_subcell_cell

    pure subroutine get_reference_measure_subcell_cell(self, measure)
        implicit none
        class(type_subcell_cell), intent(in) :: self
        !> Measure of the reference element this cell shape belongs to:
        !> 1/2 (simplex) or 4 (square); 0 when the cell is unset.
        real(real64), intent(inout) :: measure
        measure = 0.0d0
        if (self%num_vertices == TRIANGLE_VERTICES) then
            measure = SIMPLEX_MEASURE
        else if (self%num_vertices == QUADRILATERAL_VERTICES) then
            measure = SQUARE_MEASURE
        end if
    end subroutine get_reference_measure_subcell_cell

    !> @brief Coordinates of one probe point.  Failure behavior: an
    !> out-of-range index returns the origin.
    pure subroutine get_probe_point_subcell_cell(self, index, point)
        implicit none
        class(type_subcell_cell), intent(in) :: self
        !> Probe index, \(1 \le\) index \(\le\) get_num_probe_points.
        integer(int32), intent(in) :: index
        !> Reference coordinates \((\xi, \eta)\).
        real(real64), intent(inout) :: point(2)

        point = 0.0d0
        if (index < 1 .or. index > self%num_probe_points) return
        point = self%probe_points(:, index)
    end subroutine get_probe_point_subcell_cell

    !> @brief One of the four children of this cell.
    !>
    !> Assumption: compute_probe_points has been called; the children reuse
    !> those points, so they tile the parent exactly and their union of
    !> vertices is the parent probe set.  Failure behavior: an out-of-range
    !> index or an invalid parent leaves the child reset.
    pure subroutine get_child_subcell_cell(self, index, child)
        implicit none
        class(type_subcell_cell), intent(in) :: self
        !> Child index, \(1 \le\) index \(\le\) get_num_children.
        integer(int32), intent(in) :: index
        !> The child cell, with its own probe points not yet computed.
        type(type_subcell_cell), intent(inout) :: child

        integer(int32) :: probe_index(MAX_VERTICES, CHILDREN_PER_CELL)
        integer(int32) :: vertex

        call child%reset()
        if (index < 1 .or. index > CHILDREN_PER_CELL) return
        if (self%num_probe_points == 0) return

        if (self%num_vertices == TRIANGLE_VERTICES) then
            probe_index(1:3, 1) = [1, 4, 6]
            probe_index(1:3, 2) = [4, 2, 5]
            probe_index(1:3, 3) = [6, 5, 3]
            probe_index(1:3, 4) = [4, 5, 6]
        else if (self%num_vertices == QUADRILATERAL_VERTICES) then
            probe_index(1:4, 1) = [1, 5, 9, 8]
            probe_index(1:4, 2) = [5, 2, 6, 9]
            probe_index(1:4, 3) = [9, 6, 3, 7]
            probe_index(1:4, 4) = [8, 9, 7, 4]
        else
            return
        end if

        child%num_vertices = self%num_vertices
        do vertex = 1, self%num_vertices
            child%vertices(:, vertex) = self%probe_points(:, probe_index(vertex, index))
        end do
    end subroutine get_child_subcell_cell

    !> @brief One triangle of the cell decomposition, with the level set at its
    !> vertices.
    !>
    !> Numerical guarantee: the triangles tile the cell exactly.  Failure
    !> behavior: an out-of-range index returns zeros.
    pure subroutine get_triangle_subcell_cell(self, index, vertices, phi)
        implicit none
        class(type_subcell_cell), intent(in) :: self
        !> Triangle index, \(1 \le\) index \(\le\) get_num_triangles.
        integer(int32), intent(in) :: index
        !> Vertex coordinates of the triangle, shape (2, 3).
        real(real64), intent(inout) :: vertices(:, :)
        !> Level-set samples at those vertices, size 3.
        real(real64), intent(inout) :: phi(:)

        integer(int32) :: vertex_index(3, MAX_TRIANGLES)
        integer(int32) :: num_triangles, vertex

        vertices = 0.0d0
        phi = 0.0d0
        call self%get_num_triangles(num_triangles)
        if (index < 1 .or. index > num_triangles) return

        vertex_index(:, 1) = [1, 2, 3]
        if (num_triangles == MAX_TRIANGLES) vertex_index(:, 2) = [1, 3, 4]

        do vertex = 1, 3
            vertices(:, vertex) = self%vertices(:, vertex_index(vertex, index))
            phi(vertex) = self%probe_phi(vertex_index(vertex, index))
        end do
    end subroutine get_triangle_subcell_cell

end module domain_fe_subcell_topology
