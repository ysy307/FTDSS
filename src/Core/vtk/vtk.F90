module core_vtk
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: stdlib_sorting, only:sort
    use :: core_types_array, only:type_dp_3d
    use :: core_allocate, only:allocate_array
    use :: core_unique, only:unique
    use :: core_vtk_vtk_constants
    use :: core_vtk_vtk_wrapper, only: vtk_initialize, vtk_read_header, vtk_get_num_points, & !&
                                       vtk_get_points, vtk_get_num_cells, vtk_get_total_connectivity_size, & !&
                                       vtk_get_cell_info, vtk_get_cell_entity_ids, vtk_finalize !&

    implicit none
    private

    public :: type_vtk
    public :: type_vtk_cells

    ! Fortran側のデータ構造

    type :: type_vtk_cells
        integer(int32) :: offset
        integer(int32) :: cell_type
        character(:), allocatable :: cell_type_name
        integer(int32) :: num_nodes_in_cell
        integer(int32) :: cell_entity_id
        integer(int32), allocatable :: connectivity(:)
    contains
        procedure :: set => type_vtk_cells_set
    end type type_vtk_cells

    type :: type_vtk
        character(:), allocatable :: format
        character(:), allocatable :: dataset
        integer(int32) :: num_points
        type(type_dp_3d) :: points
        integer(int32) :: num_total_cells
        type(type_vtk_cells), allocatable :: cells(:)
    contains
        procedure :: initialize => type_vtk_initialize
        procedure :: Is_In => Core_VTK_IN_CellType
        procedure :: get_active_region_info
    end type type_vtk

contains
    ! =================================================================
    ! 公開APIの実装 (内部をC++呼び出しに置き換え)
    ! =================================================================
    subroutine type_vtk_initialize(self, filename)
        !> Read VTK file using C++ backend
        implicit none
        class(type_vtk), intent(inout) :: self
        character(*), intent(in) :: filename

        character(len=256) :: c_filename
        integer(c_int) :: ierr

        ! --- 生データ格納用の一時配列 ---
        integer(int64), allocatable :: raw_connectivity(:)
        integer(int64), allocatable :: raw_offsets(:)
        integer(int32), allocatable :: raw_cell_types(:)
        integer(int32), allocatable :: raw_cell_entity_ids(:)
        integer(int64) :: total_conn_size
        integer(int32) :: i
        integer(int32) :: connectivity_first, connectivity_last, num_nodes_in_cell
        character(50) :: f_format, f_dataset

        c_filename = trim(filename)//c_null_char

        ! 1. C++リーダーを初期化
        call vtk_initialize(c_filename, ierr)
        if (ierr /= 0) then
            write (*, *) "C++ VTK Reader failed to initialize. Error code: ", ierr
            stop
        end if

        ! 2. ヘッダー情報の取得

        call vtk_read_header(f_format, len(f_format), f_dataset, len(f_dataset))
        allocate (character(len=len_trim(f_format)) :: self%format)
        self%format = trim(adjustl(f_format))
        allocate (character(len=len_trim(f_dataset)) :: self%dataset)
        self%dataset = trim(adjustl(f_dataset))

        ! 3. ポイントデータの取得
        call vtk_get_num_points(self%num_points)
        if (self%num_points > 0) then
            call self%POINTS%initialize(self%num_points)
            call vtk_get_points(self%POINTS%x, self%POINTS%y, self%POINTS%z)
        end if

        ! 4. セルデータの取得
        call vtk_get_num_cells(self%num_total_cells)
        if (self%num_total_cells > 0) then
            ! 4a. 生データをCから取得するためのメモリ確保
            call vtk_get_total_connectivity_size(total_conn_size)
            call allocate_array(raw_connectivity, total_conn_size)
            call allocate_array(raw_offsets, self%num_total_cells + 1_int64)
            call allocate_array(raw_cell_types, self%num_total_cells)
            call allocate_array(raw_cell_entity_ids, self%num_total_cells)

            ! 4b. 生データをCから取得 (connectivity, Offsets, Types, CellEntityIds)
            call vtk_get_cell_info(raw_connectivity, raw_offsets, raw_cell_types)
            call vtk_get_cell_entity_ids(raw_cell_entity_ids)

            ! 4c. Fortran構造体にデータを格納し直す
            allocate (self%cells(self%num_total_cells))
            do i = 1, self%num_total_cells
                self%cells(i)%cell_type = raw_cell_types(i)
                self%cells(i)%cell_entity_id = raw_cell_entity_ids(i)
                self%cells(i)%offset = int(raw_offsets(i + 1), kind=int32)

                ! 各セルのconnectivityを抽出し、コピー
                connectivity_first = raw_offsets(i) + 1
                connectivity_last = raw_offsets(i + 1)
                num_nodes_in_cell = connectivity_last - connectivity_first + 1

                call allocate_array(self%cells(i)%connectivity, num_nodes_in_cell)
                self%cells(i)%connectivity(:) = int(raw_connectivity(connectivity_first:connectivity_last), kind=int32)

                call self%cells(i)%set()
            end do
        end if

        ! 5. 後片付け
        call vtk_finalize()

    end subroutine type_vtk_initialize

    subroutine type_vtk_cells_set(self)
        implicit none
        class(type_vtk_cells), intent(inout) :: self !! VTK cells data

        select case (self%cell_type)
        case (VTK_VERTEX)
            self%cell_type_name = "Vertex"
            self%num_nodes_in_cell = 1
        case (VTK_POLY_VERTEX)
            self%cell_type_name = "PolyVertex"
            self%num_nodes_in_cell = -1
        case (VTK_LINE)
            self%cell_type_name = "Line"
            self%num_nodes_in_cell = 2
        case (VTK_POLY_LINE)
            self%cell_type_name = "PolyLine"
            self%num_nodes_in_cell = -1
        case (VTK_TRIANGLE)
            self%cell_type_name = "Triangle"
            self%num_nodes_in_cell = 3
        case (VTK_TRIANGLE_STRIP)
            self%cell_type_name = "TriangleStrip"
            self%num_nodes_in_cell = 3
        case (VTK_POLYGON)
            self%cell_type_name = "Polygon"
            self%num_nodes_in_cell = -1
        case (VTK_PIXEL)
            self%cell_type_name = "Pixel"
            self%num_nodes_in_cell = 4
        case (VTK_QUAD)
            self%cell_type_name = "Quad"
            self%num_nodes_in_cell = 4
        case (VTK_TETRA)
            self%cell_type_name = "Tetra"
            self%num_nodes_in_cell = 4
        case (VTK_VOXEL)
            self%cell_type_name = "Voxel"
            self%num_nodes_in_cell = 8
        case (VTK_HEXAHEDRON)
            self%cell_type_name = "Hexahedron"
            self%num_nodes_in_cell = 8
        case (VTK_WEDGE)
            self%cell_type_name = "Wedge"
            self%num_nodes_in_cell = 6
        case (VTK_PYRAMID)
            self%cell_type_name = "Pyramid"
            self%num_nodes_in_cell = 5
        case (VTK_PENTAGONAL_PRISM)
            self%cell_type_name = "PentagonalPrism"
            self%num_nodes_in_cell = 10
        case (VTK_HEXAGONAL_PRISM)
            self%cell_type_name = "HexagonalPrism"
            self%num_nodes_in_cell = 12
        case (VTK_QUADRATIC_EDGE)
            self%cell_type_name = "QuadraticEdge"
            self%num_nodes_in_cell = 3
        case (VTK_QUADRATIC_TRIANGLE)
            self%cell_type_name = "QuadraticTriangle"
            self%num_nodes_in_cell = 6
        case (VTK_QUADRATIC_QUAD)
            self%cell_type_name = "QuadraticQuad"
            self%num_nodes_in_cell = 8
        case (VTK_QUADRATIC_POLYGON)
            self%cell_type_name = "QuadraticPolygon"
            self%num_nodes_in_cell = -1
        case (VTK_QUADRATIC_TETRA)
            self%cell_type_name = "QuadraticTetra"
            self%num_nodes_in_cell = 10
        case (VTK_QUADRATIC_HEXAHEDRON)
            self%cell_type_name = "QuadraticHexahedron"
            self%num_nodes_in_cell = 20
        case (VTK_QUADRATIC_WEDGE)
            self%cell_type_name = "QuadraticWedge"
            self%num_nodes_in_cell = 15
        case (VTK_QUADRATIC_PYRAMID)
            self%cell_type_name = "QuadraticPyramid"
            self%num_nodes_in_cell = 13
        case (VTK_BIQUADRATIC_QUAD)
            self%cell_type_name = "BiquadraticQuad"
            self%num_nodes_in_cell = 9
        case (VTK_TRIQUADRATIC_HEXAHEDRON)
            self%cell_type_name = "TriquadraticHexahedron"
            self%num_nodes_in_cell = 27
        case (VTK_TRIQUADRATIC_PYRAMID)
            self%cell_type_name = "TriquadraticPyramid"
            self%num_nodes_in_cell = 14
        case (VTK_QUADRATIC_LINEAR_QUAD)
            self%cell_type_name = "QuadraticLinearQuad"
            self%num_nodes_in_cell = 8
        case (VTK_QUADRATIC_LINEAR_WEDGE)
            self%cell_type_name = "QuadraticLinearWedge"
            self%num_nodes_in_cell = 12
        case (VTK_BIQUADRATIC_QUADRATIC_WEDGE)
            self%cell_type_name = "BiquadraticQuadraticWedge"
            self%num_nodes_in_cell = 18
        case (VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON)
            self%cell_type_name = "BiquadraticQuadraticHexahedron"
            self%num_nodes_in_cell = 27
        case (VTK_BIQUADRATIC_TRIANGLE)
            self%cell_type_name = "BiquadraticTriangle"
            self%num_nodes_in_cell = 6
        case (VTK_CUBIC_LINE)
            self%cell_type_name = "CubicLine"
            self%num_nodes_in_cell = 4
        case (VTK_CONVEX_POINT_SET)
            self%cell_type_name = "ConvexPointSet"
            self%num_nodes_in_cell = 1
        case (VTK_POLYHEDRON)
            self%cell_type_name = "Polyhedron"
            self%num_nodes_in_cell = -1
        case (VTK_PARAMETRIC_CURVE)
            self%cell_type_name = "ParametricCurve"
            self%num_nodes_in_cell = 2
        case (VTK_PARAMETRIC_SURFACE)
            self%cell_type_name = "ParametricSurface"
            self%num_nodes_in_cell = 3
        case (VTK_PARAMETRIC_TRI_SURFACE)
            self%cell_type_name = "ParametricTriSurface"
            self%num_nodes_in_cell = 3
        case (VTK_PARAMETRIC_QUAD_SURFACE)
            self%cell_type_name = "ParametricQuadSurface"
            self%num_nodes_in_cell = 4
        case (VTK_PARAMETRIC_TETRA_REGION)
            self%cell_type_name = "ParametricTetraRegion"
            self%num_nodes_in_cell = 4
        case (VTK_PARAMETRIC_HEX_REGION)
            self%cell_type_name = "ParametricHexRegion"
            self%num_nodes_in_cell = 8
        case (VTK_HIGHER_ORDER_EDGE)
            self%cell_type_name = "HigherOrderEdge"
            self%num_nodes_in_cell = 3
        case (VTK_HIGHER_ORDER_TRIANGLE)
            self%cell_type_name = "HigherOrderTriangle"
            self%num_nodes_in_cell = 6
        case (VTK_HIGHER_ORDER_QUAD)
            self%cell_type_name = "HigherOrderQuad"
            self%num_nodes_in_cell = 8
        case (VTK_HIGHER_ORDER_POLYGON)
            self%cell_type_name = "HigherOrderPolygon"
            self%num_nodes_in_cell = -1
        case (VTK_HIGHER_ORDER_TETRAHEDRON)
            self%cell_type_name = "HigherOrderTetrahedron"
            self%num_nodes_in_cell = 10
        case (VTK_HIGHER_ORDER_WEDGE)
            self%cell_type_name = "HigherOrderWedge"
            self%num_nodes_in_cell = 15
        case (VTK_HIGHER_ORDER_PYRAMID)
            self%cell_type_name = "HigherOrderPyramid"
            self%num_nodes_in_cell = 13
        case (VTK_HIGHER_ORDER_HEXAHEDRON)
            self%cell_type_name = "HigherOrderHexahedron"
            self%num_nodes_in_cell = 20
        case (VTK_LAGRANGE_CURVE)
            self%cell_type_name = "LagrangeCurve"
            self%num_nodes_in_cell = 2
        case (VTK_LAGRANGE_TRIANGLE)
            self%cell_type_name = "LagrangeTriangle"
            self%num_nodes_in_cell = 3
        case (VTK_LAGRANGE_QUADRILATERAL)
            self%cell_type_name = "LagrangeQuadrilateral"
            self%num_nodes_in_cell = 4
        case (VTK_LAGRANGE_TETRAHEDRON)
            self%cell_type_name = "LagrangeTetrahedron"
            self%num_nodes_in_cell = 4
        case (VTK_LAGRANGE_HEXAHEDRON)
            self%cell_type_name = "LagrangeHexahedron"
            self%num_nodes_in_cell = 8
        case (VTK_LAGRANGE_WEDGE)
            self%cell_type_name = "LagrangeWedge"
            self%num_nodes_in_cell = 6
        case (VTK_LAGRANGE_PYRAMID)
            self%cell_type_name = "LagrangePyramid"
            self%num_nodes_in_cell = 5
        case (VTK_BEZIER_CURVE)
            self%cell_type_name = "BezierCurve"
            self%num_nodes_in_cell = 2
        case (VTK_BEZIER_TRIANGLE)
            self%cell_type_name = "BezierTriangle"
            self%num_nodes_in_cell = 3
        case (VTK_BEZIER_QUADRILATERAL)
            self%cell_type_name = "BezierQuadrilateral"
            self%num_nodes_in_cell = 4
        case (VTK_BEZIER_TETRAHEDRON)
            self%cell_type_name = "BezierTetrahedron"
            self%num_nodes_in_cell = 4
        case (VTK_BEZIER_HEXAHEDRON)
            self%cell_type_name = "BezierHexahedron"
            self%num_nodes_in_cell = 8
        case (VTK_BEZIER_WEDGE)
            self%cell_type_name = "BezierWedge"
            self%num_nodes_in_cell = 6
        case (VTK_BEZIER_PYRAMID)
            self%cell_type_name = "BezierPyramid"
            self%num_nodes_in_cell = 5
        case default
            self%cell_type_name = "Unknown"
            self%num_nodes_in_cell = 0
        end select
    end subroutine type_vtk_cells_set

    function Core_VTK_IN_CellType(self, iCellType, Shape_Dimention) result(isIn)
        !> Check if cell type is in VTK
        implicit none
        class(Type_VTK), intent(in) :: self !! VTK data
        integer(int32), intent(in) :: iCellType !! Cell type
        integer(int32), intent(in) :: Shape_Dimention !! Shape dimension
        logical(4) :: isIn
        integer(int32) :: i

        isIn = .false.
        select case (Shape_Dimention)
        case (1)
            if (iCellType == VTK_LINE .or. &
                iCellType == VTK_QUADRATIC_EDGE &
                ) then
                isIn = .true.
            end if
        case (2)
            if (iCellType == VTK_TRIANGLE .or. &
                iCellType == VTK_PIXEL .or. &
                iCellType == VTK_QUAD .or. &
                iCellType == VTK_QUADRATIC_TRIANGLE .or. &
                iCellType == VTK_QUADRATIC_QUAD &
                ) then
                isIn = .true.
            end if
        end select

    end function Core_VTK_IN_CellType

    subroutine get_active_region_info(self, unique_ids, ierr)
        ! --- 引数 ---
        implicit none
        class(Type_VTK), intent(in) :: self !! VTK data
        integer(int32), allocatable, intent(inout) :: unique_ids(:)
        integer(int32), intent(out) :: ierr

        ! --- ローカル変数 ---
        integer(int32) :: max_dim
        integer(int32), allocatable :: collected_ids(:)
        integer(int32) :: i_cell, count
        logical(4) :: is_max_dim_element

        max_dim = 0
        ierr = 0

        ! --- ステップ1: メッシュ内の最大次元を判定 ---
        do i_cell = 1, self%num_total_cells
            select case (self%CELLS(i_cell)%cell_type)
            case (VTK_TETRA, VTK_HEXAHEDRON, &
                  VTK_WEDGE, VTK_PYRAMID, &
                  VTK_QUADRATIC_TETRA, VTK_QUADRATIC_HEXAHEDRON)
                max_dim = 3
                exit ! 3Dが見つかったら、それ以上探す必要はない
            case (VTK_TRIANGLE, VTK_PIXEL, &
                  VTK_QUAD, VTK_QUADRATIC_TRIANGLE, &
                  VTK_QUADRATIC_QUAD)
                max_dim = max(max_dim, 2)
            case (VTK_LINE, VTK_QUADRATIC_EDGE)
                max_dim = max(max_dim, 1)
            end select
        end do

        if (max_dim == 0) then
            ierr = -1
            return ! アクティブな要素がない
        end if

        ! --- ステップ2: 最大次元を持つ要素から、すべてのCellEntityIdを収集 ---
        allocate (collected_ids(self%num_total_cells))
        count = 0
        do i_cell = 1, self%num_total_cells
            is_max_dim_element = .false.
            select case (self%CELLS(i_cell)%cell_type)
            case (VTK_TETRA, VTK_HEXAHEDRON, &
                  VTK_WEDGE, VTK_PYRAMID, &
                  VTK_QUADRATIC_TETRA, VTK_QUADRATIC_HEXAHEDRON)
                if (max_dim == 3) is_max_dim_element = .true.
            case (VTK_TRIANGLE, VTK_PIXEL, &
                  VTK_QUAD, VTK_QUADRATIC_TRIANGLE, &
                  VTK_QUADRATIC_QUAD)
                if (max_dim == 2) is_max_dim_element = .true.
            case (VTK_LINE, VTK_QUADRATIC_EDGE)
                if (max_dim == 1) is_max_dim_element = .true.
            end select

            if (is_max_dim_element) then
                count = count + 1
                collected_ids(count) = self%CELLS(i_cell)%cell_entity_id
            end if
        end do

        ! --- ステップ3: 収集したIDリストから、ユニークなものだけを抽出 ---
        ! (これはFortranの標準的なユニーク化のアルゴリズム)
        if (count > 0) then
            call sort(collected_ids(1:count))
            call unique(collected_ids(1:count), unique_ids)
        else
            allocate (unique_ids(0))
        end if

    end subroutine get_active_region_info

    ! self

end module core_vtk
