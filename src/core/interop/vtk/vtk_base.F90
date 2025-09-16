submodule(core_vtk) core_vtk_base
    implicit none

contains
    module function type_vtk_cell_get_dimension(self) result(dimension)
        !> Get the dimension of the cell
        implicit none
        class(type_vtk_cell), intent(in) :: self !! VTK cell data
        integer(int32) :: dimension
        dimension = self%cell_dimension
    end function type_vtk_cell_get_dimension

    module function type_vtk_cell_get_order(self) result(order)
        !> Get the order of the cell
        implicit none
        class(type_vtk_cell), intent(in) :: self !! VTK cell data
        integer(int32) :: order
        order = self%cell_order
    end function type_vtk_cell_get_order

    module subroutine type_vtk_cell_set(self, num_nodes_in_cell)
        implicit none
        class(type_vtk_cell), intent(inout) :: self !! VTK cells data
        integer(int32), intent(in) :: num_nodes_in_cell !! セルのノード数

        select case (self%cell_type)
        case (VTK_VERTEX)
            self%cell_type_name = "Vertex"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 0 ! 0次元要素
            self%cell_order = 1
        case (VTK_POLY_VERTEX)
            self%cell_type_name = "PolyVertex"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 0 ! 0次元要素の集合
            self%cell_order = 1
        case (VTK_LINE)
            self%cell_type_name = "Line"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 1
            self%cell_order = 1
        case (VTK_POLY_LINE)
            self%cell_type_name = "PolyLine"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 1
            self%cell_order = -1 ! 可変
        case (VTK_TRIANGLE)
            self%cell_type_name = "Triangle"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 1
        case (VTK_TRIANGLE_STRIP)
            self%cell_type_name = "TriangleStrip"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 1
        case (VTK_POLYGON)
            self%cell_type_name = "Polygon"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_PIXEL)
            self%cell_type_name = "Pixel"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 1
        case (VTK_QUAD)
            self%cell_type_name = "Quad"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 1
        case (VTK_TETRA)
            self%cell_type_name = "Tetra"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 1
        case (VTK_VOXEL)
            self%cell_type_name = "Voxel"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 1
        case (VTK_HEXAHEDRON)
            self%cell_type_name = "Hexahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 1
        case (VTK_WEDGE)
            self%cell_type_name = "Wedge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 1
        case (VTK_PYRAMID)
            self%cell_type_name = "Pyramid"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 1
        case (VTK_PENTAGONAL_PRISM)
            self%cell_type_name = "PentagonalPrism"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 1
        case (VTK_HEXAGONAL_PRISM)
            self%cell_type_name = "HexagonalPrism"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 1
        case (VTK_QUADRATIC_EDGE)
            self%cell_type_name = "QuadraticEdge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 1
            self%cell_order = 2
        case (VTK_QUADRATIC_TRIANGLE)
            self%cell_type_name = "QuadraticTriangle"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 2
        case (VTK_QUADRATIC_QUAD)
            self%cell_type_name = "QuadraticQuad"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 2
        case (VTK_QUADRATIC_POLYGON)
            self%cell_type_name = "QuadraticPolygon"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 2
        case (VTK_QUADRATIC_TETRA)
            self%cell_type_name = "QuadraticTetra"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2
        case (VTK_QUADRATIC_HEXAHEDRON)
            self%cell_type_name = "QuadraticHexahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2
        case (VTK_QUADRATIC_WEDGE)
            self%cell_type_name = "QuadraticWedge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2
        case (VTK_QUADRATIC_PYRAMID)
            self%cell_type_name = "QuadraticPyramid"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2
        case (VTK_BIQUADRATIC_QUAD)
            self%cell_type_name = "BiquadraticQuad"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 2 ! 双二次
        case (VTK_TRIQUADRATIC_HEXAHEDRON)
            self%cell_type_name = "TriquadraticHexahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2 ! 三次二次
        case (VTK_TRIQUADRATIC_PYRAMID)
            self%cell_type_name = "TriquadraticPyramid"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2 ! 三次二次
        case (VTK_QUADRATIC_LINEAR_QUAD)
            self%cell_type_name = "QuadraticLinearQuad"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 2 ! 線形と二次
        case (VTK_QUADRATIC_LINEAR_WEDGE)
            self%cell_type_name = "QuadraticLinearWedge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2 ! 線形と二次
        case (VTK_BIQUADRATIC_QUADRATIC_WEDGE)
            self%cell_type_name = "BiquadraticQuadraticWedge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2 ! 双二次と二次
        case (VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON)
            self%cell_type_name = "BiquadraticQuadraticHexahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2 ! 双二次と二次
        case (VTK_BIQUADRATIC_TRIANGLE)
            self%cell_type_name = "BiquadraticTriangle"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 2 ! 双二次
        case (VTK_CUBIC_LINE)
            self%cell_type_name = "CubicLine"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 1
            self%cell_order = 3
        case (VTK_CONVEX_POINT_SET)
            self%cell_type_name = "Convexpointset"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = -1 ! 可変
            self%cell_order = -1 ! 可変
        case (VTK_POLYHEDRON)
            self%cell_type_name = "Polyhedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 1
        case (VTK_PARAMETRIC_CURVE)
            self%cell_type_name = "ParametricCurve"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 1
            self%cell_order = -1 ! 可変
        case (VTK_PARAMETRIC_SURFACE)
            self%cell_type_name = "ParametricSurface"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_PARAMETRIC_TRI_SURFACE)
            self%cell_type_name = "ParametricTriSurface"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_PARAMETRIC_QUAD_SURFACE)
            self%cell_type_name = "ParametricQuadSurface"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_PARAMETRIC_TETRA_REGION)
            self%cell_type_name = "ParametricTetraRegion"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_PARAMETRIC_HEX_REGION)
            self%cell_type_name = "ParametricHexRegion"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_HIGHER_ORDER_EDGE)
            self%cell_type_name = "HigherOrderEdge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 1
            self%cell_order = -1 ! 可変
        case (VTK_HIGHER_ORDER_TRIANGLE)
            self%cell_type_name = "HigherOrderTriangle"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_HIGHER_ORDER_QUAD)
            self%cell_type_name = "HigherOrderQuad"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_HIGHER_ORDER_POLYGON)
            self%cell_type_name = "HigherOrderPolygon"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_HIGHER_ORDER_TETRAHEDRON)
            self%cell_type_name = "HigherOrderTetrahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_HIGHER_ORDER_WEDGE)
            self%cell_type_name = "HigherOrderWedge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_HIGHER_ORDER_PYRAMID)
            self%cell_type_name = "HigherOrderPyramid"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_HIGHER_ORDER_HEXAHEDRON)
            self%cell_type_name = "HigherOrderHexahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_LAGRANGE_CURVE)
            self%cell_type_name = "LagrangeCurve"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 1
            self%cell_order = -1 ! 可変
        case (VTK_LAGRANGE_TRIANGLE)
            self%cell_type_name = "LagrangeTriangle"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_LAGRANGE_QUADRILATERAL)
            self%cell_type_name = "LagrangeQuadrilateral"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_LAGRANGE_TETRAHEDRON)
            self%cell_type_name = "LagrangeTetrahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_LAGRANGE_HEXAHEDRON)
            self%cell_type_name = "LagrangeHexahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_LAGRANGE_WEDGE)
            self%cell_type_name = "LagrangeWedge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_LAGRANGE_PYRAMID)
            self%cell_type_name = "LagrangePyramid"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_BEZIER_CURVE)
            self%cell_type_name = "BezierCurve"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 1
            self%cell_order = -1 ! 可変
        case (VTK_BEZIER_TRIANGLE)
            self%cell_type_name = "BezierTriangle"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_BEZIER_QUADRILATERAL)
            self%cell_type_name = "BezierQuadrilateral"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_BEZIER_TETRAHEDRON)
            self%cell_type_name = "BezierTetrahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_BEZIER_HEXAHEDRON)
            self%cell_type_name = "BezierHexahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_BEZIER_WEDGE)
            self%cell_type_name = "BezierWedge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_BEZIER_PYRAMID)
            self%cell_type_name = "BezierPyramid"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case default
            self%cell_type_name = "Unknown"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 0
            self%cell_order = 0
        end select
    end subroutine type_vtk_cell_set

    module subroutine get_active_region_info(self, unique_ids, ierr)
        ! --- 引数 ---
        implicit none
        class(Type_VTK), intent(in) :: self !! VTK data
        integer(int32), allocatable, intent(out) :: unique_ids(:)
        integer(int32), intent(out) :: ierr

        ! --- ローカル変数 ---
        integer(int32) :: local_max_dim
        integer(int32), allocatable :: collected_ids(:)
        integer(int32) :: i_cell, count
        logical(4) :: is_max_dim_element
        integer(int32) :: max_dim

#ifdef _MPI
        integer(int32) :: global_max_dim
        integer(int32), allocatable :: all_counts(:), displs(:)
        integer(int32), allocatable :: global_collected_ids(:)
        integer(int32) :: total_collected_count, j
#endif

        local_max_dim = 0
        ierr = 0

        ! --- ステップ1: メッシュ内の最大次元を判定 ---
        do i_cell = 1, self%num_total_cells
            local_max_dim = max(local_max_dim, self%CELLS(i_cell)%get_dimension())
        end do

#ifdef _MPI
        call MPI_Allreduce(local_max_dim, global_max_dim, 1, MPI_INTEGER4, MPI_MAX, MPI_COMM_WORLD, ierr)
        if (global_max_dim <= 0) then
            ierr = -1
            allocate (unique_ids(0))
            return
        end if
#else
        if (local_max_dim <= 0) then
            ierr = -1
            allocate (unique_ids(0))
            return ! アクティブな要素がない
        end if
#endif

        ! --- ステップ2: 最大次元を持つ要素から、すべてのCellEntityIdを収集 ---
        allocate (collected_ids(self%num_total_cells))
        count = 0
        do i_cell = 1, self%num_total_cells
#ifdef _MPI
            max_dim = global_max_dim
#else
            max_dim = local_max_dim
#endif

            if (self%CELLS(i_cell)%get_dimension() == max_dim) then

                count = count + 1
                collected_ids(count) = self%CELLS(i_cell)%cell_entity_id
            end if
        end do

        ! --- ステップ3: 収集したIDリストから、ユニークなものだけを抽出 ---
#ifdef _MPI
        ! MPI: 全プロセスのIDを収集し、グローバルにユニークなものを抽出
        allocate (all_counts(self%num_procs))
        allocate (displs(self%num_procs))
        call MPI_Allgather(count, 1, MPI_INTEGER4, all_counts, 1, MPI_INTEGER4, MPI_COMM_WORLD, ierr)

        total_collected_count = sum(all_counts)
        if (total_collected_count > 0) then
            displs(1) = 0
            do j = 2, self%num_procs
                displs(j) = displs(j - 1) + all_counts(j - 1)
            end do
            allocate (global_collected_ids(total_collected_count))
            call MPI_Allgatherv(collected_ids(1:count), count, MPI_INTEGER4, &
                                global_collected_ids, all_counts, displs, MPI_INTEGER4, MPI_COMM_WORLD, ierr)
            call unique(global_collected_ids, unique_ids)
            deallocate (global_collected_ids)
        else
            allocate (unique_ids(0))
        end if
        deallocate (all_counts, displs)
#else
        if (count > 0) then
            call unique(collected_ids(1:count), unique_ids)
        else
            allocate (unique_ids(0))
        end if
#endif
        deallocate (collected_ids)

    end subroutine get_active_region_info

end submodule core_vtk_base

