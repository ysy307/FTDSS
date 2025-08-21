module domain_element
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_logger
    use :: module_core, only:type_dp_3d, type_dp_vector_3d, assignment(=), type_dp_pointer, type_vtk_cell, allocate_array, deallocate_array
    use :: module_input, only:type_geometry_settings
    implicit none
    private

    public :: abst_element
    public :: type_triangle_first
    public :: type_triangle_second
    public :: type_square_first
    public :: type_square_second
    public :: holder_elements

    public :: interpolate
    public :: interpolate_reordered

    public :: get_connectivity
    public :: get_connectivity_reordered

    !--------------------------------------------------------------------------------------
    ! Holder for polymorphic element objects
    !--------------------------------------------------------------------------------------
    type :: holder_elements
        class(abst_element), allocatable :: e
    end type holder_elements

    !--------------------------------------------------------------------------------------
    !   Abstract base type for 2D elements
    !--------------------------------------------------------------------------------------
    type, abstract :: abst_element
        integer(int32), private :: id !! Element ID
        integer(int32), private :: type !! Element type (5: triangle 1st, 9: square 1st)
        integer(int32), private :: num_nodes !! Number of nodes in the element
        integer(int32), private :: group !! Element group number
        integer(int32), private :: dimension
        integer(int32), private :: order
        integer(int32), allocatable :: connectivity(:) !! connectivity information
        integer(int32), allocatable :: connectivity_reordered(:) !! reordered connectivity information
        type(type_dp_pointer), allocatable :: x(:) !! X coordinate
        type(type_dp_pointer), allocatable :: y(:) !! Y coordinate
        type(type_dp_pointer), allocatable :: z(:) !! Z coordinate
        !----------------------------------------------------------------------------------
        ! Gauss Quadrature points and weights
        !  - Gauss Quadrature points are defined in the local coordinate system
        !  - The number of Gauss points is determined by the element type
        !  - The weights are used for numerical integration over the element
        !  - The Gauss points are used to evaluate the shape functions and their derivatives
        !----------------------------------------------------------------------------------
        integer(int32) :: num_gauss !! Number of Gauss Quadrature points
        real(real64), allocatable :: weight(:) !! Gauss weight
        type(type_dp_vector_3d), allocatable :: gauss(:) !! Gauss Quadrature points Coordinate
        !----------------------------------------------------------------------------------
        ! Interpolation functions
        !----------------------------------------------------------------------------------
        procedure(abst_interpolate),      pass(self), pointer :: interpolate => null() !&
        procedure(abst_get_connectivity), pass(self), pointer :: get_connectivity => null() !&
    contains
        procedure, pass(self) :: get_id !&
        procedure, pass(self) :: get_type !&
        procedure, pass(self) :: get_num_nodes !&
        procedure, pass(self) :: get_group !&
        procedure, pass(self) :: get_order !&
        procedure, pass(self) :: get_dimension !&
        procedure, pass(self) :: get_num_gauss !&
        !----------------------------------------------------------------------------------
        procedure(abst_get_area),      pass(self), deferred :: get_area !&
        procedure(abst_psi),           pass(self), deferred :: psi !&
        procedure(abst_dpsi_dxi),      pass(self), deferred :: dpsi_dxi !&
        procedure(abst_dpsi_deta),     pass(self), deferred :: dpsi_deta !&
        procedure(abst_jacobian),      pass(self), deferred :: jacobian !&
        procedure(abst_jacobian_det),  pass(self), deferred :: jacobian_det !&
        procedure(abst_is_inside),     pass(self), deferred :: is_inside !&
    end type abst_element

    !--------------------------------------------------------------------------------------
    !   Triangle First Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(abst_element) :: type_triangle_first
    contains
        procedure, pass(self) :: get_area      => get_area_triangle_first !&
        procedure, pass(self) :: psi           => psi_triangle_first !&
        procedure, pass(self) :: dpsi_dxi      => dpsi_dxi_triangle_first !&
        procedure, pass(self) :: dpsi_deta     => dpsi_deta_triangle_first !&
        procedure, pass(self) :: jacobian      => jacobian_triangle_first !&
        procedure, pass(self) :: jacobian_det  => jacobian_det_triangle_first !&
        procedure, pass(self) :: is_inside     => is_in_triangle_first !&
    end type type_triangle_first

    !--------------------------------------------------------------------------------------
    !   Square First Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(abst_element) :: type_square_first
    contains
        procedure, pass(self) :: get_area      => get_area_square_first !&
        procedure, pass(self) :: psi           => psi_square_first !&
        procedure, pass(self) :: dpsi_dxi      => dpsi_dxi_square_first !&
        procedure, pass(self) :: dpsi_deta     => dpsi_deta_square_first !&
        procedure, pass(self) :: jacobian      => jacobian_square_first !&
        procedure, pass(self) :: jacobian_det  => jacobian_det_square_first !&
        procedure, pass(self) :: is_inside     => is_in_square_first !&
    end type type_square_first

    !--------------------------------------------------------------------------------------
    !   Triangle Second Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(abst_element) :: type_triangle_second
    contains
        procedure, pass(self) :: get_area      => get_area_triangle_second !&
        procedure, pass(self) :: psi           => psi_triangle_second !&
        procedure, pass(self) :: dpsi_dxi      => dpsi_dxi_triangle_second !&
        procedure, pass(self) :: dpsi_deta     => dpsi_deta_triangle_second !&
        procedure, pass(self) :: jacobian      => jacobian_triangle_second !&
        procedure, pass(self) :: jacobian_det  => jacobian_det_triangle_second !&
        procedure, pass(self) :: is_inside     => is_in_triangle_second !&
    end type type_triangle_second

    !--------------------------------------------------------------------------------------
    !   Square Second Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(abst_element) :: type_square_second
    contains
        procedure, pass(self) :: get_area      => get_area_square_second !&
        procedure, pass(self) :: psi           => psi_square_second !&
        procedure, pass(self) :: dpsi_dxi      => dpsi_dxi_square_second !&
        procedure, pass(self) :: dpsi_deta     => dpsi_deta_square_second !&
        procedure, pass(self) :: jacobian      => jacobian_square_second !&
        procedure, pass(self) :: jacobian_det  => jacobian_det_square_second !&
        procedure, pass(self) :: is_inside     => is_in_square_second !&
    end type type_square_second

    !
    !----- 抽象インターフェース定義 -----
    !
    abstract interface
        function abst_get_connectivity(self, index) result(connectivity)
            import :: abst_element, int32
            implicit none
            class(abst_element), intent(in) :: self
            integer(int32), intent(in) :: index
            integer(int32) :: connectivity

        end function abst_get_connectivity

        pure function abst_get_area(self) result(area)
            import :: abst_element, real64
            implicit none
            class(abst_element), intent(in) :: self
            real(real64) :: area

        end function abst_get_area

        pure function abst_psi(self, i, r) result(psi)
            import :: abst_element, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_element), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi
        end function abst_psi

        pure function abst_dpsi_dxi(self, i, r) result(dpsi)
            import :: abst_element, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_element), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi
        end function abst_dpsi_dxi

        pure function abst_dpsi_deta(self, i, r) result(dpsi)
            import :: abst_element, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_element), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi
        end function abst_dpsi_deta

        pure function abst_jacobian(self, i, j, r) result(Jval)
            import :: abst_element, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_element), intent(in) :: self
            integer(int32), intent(in) :: i, j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: Jval
        end function abst_jacobian

        pure function abst_jacobian_det(self, r) result(J_Det)
            import :: abst_element, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_element), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: J_Det
        end function abst_jacobian_det

        pure function abst_interpolate(self, r, value) result(interpolated_value)
            import :: abst_element, type_dp_vector_3d, real64
            implicit none
            class(abst_element), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: value(:)
            real(real64) :: interpolated_value
        end function abst_interpolate

        subroutine abst_is_inside(self, cartesian, normalized, is_in)
            import abst_element, type_dp_vector_3d
            implicit none
            class(abst_element), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: cartesian
            type(type_dp_vector_3d), intent(inout) :: normalized
            logical, intent(inout) :: is_in
        end subroutine abst_is_inside

    end interface

    !--------------------------------------------------------------------------------------
    !   三角形一次要素型 procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function construct_triangle_first(id, global_coordinate, cell_info, integration) result(element)
            implicit none
            integer(int32), intent(in) :: id
            type(type_dp_3d), pointer, intent(in) :: global_coordinate
            type(type_vtk_cell), intent(in) :: cell_info
            type(type_geometry_settings), intent(in) :: integration
            class(abst_element), allocatable :: element

        end function construct_triangle_first

        pure module function get_area_triangle_first(self) result(area)
            implicit none
            class(type_triangle_first), intent(in) :: self
            real(real64) :: area

        end function get_area_triangle_first

        pure module function psi_triangle_first(self, i, r) result(N)
            implicit none
            class(type_triangle_first), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: N

        end function psi_triangle_first

        pure module function dpsi_dxi_triangle_first(self, i, r) result(dpsi)
            implicit none
            class(type_triangle_first), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi

        end function dpsi_dxi_triangle_first

        pure module function dpsi_deta_triangle_first(self, i, r) result(dpsi)
            implicit none
            class(type_triangle_first), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi

        end function dpsi_deta_triangle_first

        pure module function jacobian_triangle_first(self, i, j, r) result(Jval)
            implicit none
            class(type_triangle_first), intent(in) :: self
            integer(int32), intent(in) :: i, j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: Jval

        end function jacobian_triangle_first

        pure module function jacobian_det_triangle_first(self, r) result(J_Det)
            implicit none
            class(type_triangle_first), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: J_Det

        end function jacobian_det_triangle_first

        module subroutine is_in_triangle_first(self, cartesian, normalized, is_in)
            implicit none
            class(type_triangle_first), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: cartesian
            type(type_dp_vector_3d), intent(inout) :: normalized
            logical, intent(inout) :: is_in

        end subroutine is_in_triangle_first
    end interface

    !--------------------------------------------------------------------------------------
    !   四角形一次要素型 procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function construct_square_first(id, global_coordinate, cell_info, integration) result(element)
            implicit none
            integer(int32), intent(in) :: id
            type(type_dp_3d), pointer, intent(in) :: global_coordinate
            type(type_vtk_cell), intent(in) :: cell_info
            type(type_geometry_settings), intent(in) :: integration
            class(abst_element), allocatable :: element

        end function construct_square_first

        pure module function get_area_square_first(self) result(area)
            implicit none
            class(type_square_first), intent(in) :: self
            real(real64) :: area

        end function get_area_square_first

        pure module function psi_square_first(self, i, r) result(psi)
            implicit none
            class(type_square_first), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi

        end function psi_square_first

        pure module function dpsi_dxi_square_first(self, i, r) result(dpsi)
            implicit none
            class(type_square_first), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi

        end function dpsi_dxi_square_first

        pure module function dpsi_deta_square_first(self, i, r) result(dpsi)
            implicit none
            class(type_square_first), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi

        end function dpsi_deta_square_first

        pure module function jacobian_square_first(self, i, j, r) result(Jval)
            implicit none
            class(type_square_first), intent(in) :: self
            integer(int32), intent(in) :: i, j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: Jval

        end function jacobian_square_first

        pure module function jacobian_det_square_first(self, r) result(J_Det)
            implicit none
            class(type_square_first), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: J_Det

        end function jacobian_det_square_first

        module subroutine is_in_square_first(self, cartesian, normalized, is_in)
            implicit none
            class(type_square_first), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: cartesian
            type(type_dp_vector_3d), intent(inout) :: normalized
            logical, intent(inout) :: is_in

        end subroutine is_in_square_first
    end interface

    !--------------------------------------------------------------------------------------
    !   三角形二次要素型 procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function construct_triangle_second(id, global_coordinate, cell_info, integration) result(element)
            implicit none
            integer(int32), intent(in) :: id
            type(type_dp_3d), pointer, intent(in) :: global_coordinate
            type(type_vtk_cell), intent(in) :: cell_info
            type(type_geometry_settings), intent(in) :: integration
            class(abst_element), allocatable :: element

        end function construct_triangle_second

        pure module function get_area_triangle_second(self) result(area)
            implicit none
            class(type_triangle_second), intent(in) :: self
            real(real64) :: area

        end function get_area_triangle_second

        pure module function psi_triangle_second(self, i, r) result(N)
            implicit none
            class(type_triangle_second), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: N

        end function psi_triangle_second

        pure module function dpsi_dxi_triangle_second(self, i, r) result(dpsi)
            implicit none
            class(type_triangle_second), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi

        end function dpsi_dxi_triangle_second

        pure module function dpsi_deta_triangle_second(self, i, r) result(dpsi)
            implicit none
            class(type_triangle_second), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi

        end function dpsi_deta_triangle_second

        pure module function jacobian_triangle_second(self, i, j, r) result(Jval)
            implicit none
            class(type_triangle_second), intent(in) :: self
            integer(int32), intent(in) :: i, j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: Jval

        end function jacobian_triangle_second

        pure module function jacobian_det_triangle_second(self, r) result(J_Det)
            implicit none
            class(type_triangle_second), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: J_Det

        end function jacobian_det_triangle_second

        module subroutine is_in_triangle_second(self, cartesian, normalized, is_in)
            implicit none
            class(type_triangle_second), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: cartesian
            type(type_dp_vector_3d), intent(inout) :: normalized
            logical, intent(inout) :: is_in

        end subroutine is_in_triangle_second

    end interface

    !--------------------------------------------------------------------------------------
    !   四角形二次要素型 procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function construct_square_second(id, global_coordinate, cell_info, integration) result(element)
            implicit none
            integer(int32), intent(in) :: id
            type(type_dp_3d), pointer, intent(in) :: global_coordinate
            type(type_vtk_cell), intent(in) :: cell_info
            type(type_geometry_settings), intent(in) :: integration
            class(abst_element), allocatable :: element

        end function construct_square_second

        pure module function get_area_square_second(self) result(area)
            implicit none
            class(type_square_second), intent(in) :: self
            real(real64) :: area

        end function get_area_square_second

        pure module function psi_square_second(self, i, r) result(psi)
            implicit none
            class(type_square_second), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi

        end function psi_square_second

        pure module function dpsi_dxi_square_second(self, i, r) result(dpsi)
            implicit none
            class(type_square_second), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi

        end function dpsi_dxi_square_second

        pure module function dpsi_deta_square_second(self, i, r) result(dpsi)
            implicit none
            class(type_square_second), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi

        end function dpsi_deta_square_second

        pure module function jacobian_square_second(self, i, j, r) result(Jval)
            implicit none
            class(type_square_second), intent(in) :: self
            integer(int32), intent(in) :: i, j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: Jval

        end function jacobian_square_second

        pure module function jacobian_det_square_second(self, r) result(J_Det)
            implicit none
            class(type_square_second), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: J_Det

        end function jacobian_det_square_second

        module subroutine is_in_square_second(self, cartesian, normalized, is_in)
            implicit none
            class(type_square_second), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: cartesian
            type(type_dp_vector_3d), intent(inout) :: normalized
            logical, intent(inout) :: is_in

        end subroutine is_in_square_second
    end interface

    interface type_triangle_first
        module procedure :: construct_triangle_first
    end interface

    interface type_square_first
        module procedure :: construct_square_first
    end interface

    interface type_triangle_second
        module procedure :: construct_triangle_second
    end interface

    interface type_square_second
        module procedure :: construct_square_second
    end interface

contains
    pure function get_id(self) result(id)
        implicit none
        class(abst_element), intent(in) :: self
        integer(int32) :: id

        id = self%id
    end function get_id

    pure function get_type(self) result(type)
        implicit none
        class(abst_element), intent(in) :: self
        integer(int32) :: type

        type = self%type
    end function get_type

    pure function get_num_nodes(self) result(num_nodes)
        implicit none
        class(abst_element), intent(in) :: self
        integer(int32) :: num_nodes

        num_nodes = self%num_nodes
    end function get_num_nodes

    pure function get_group(self) result(group)
        implicit none
        class(abst_element), intent(in) :: self
        integer(int32) :: group

        group = self%group
    end function get_group

    pure function get_order(self) result(order)
        implicit none
        class(abst_element), intent(in) :: self
        integer(int32) :: order

        order = self%order
    end function get_order

    pure function get_dimension(self) result(dimension)
        implicit none
        class(abst_element), intent(in) :: self
        integer(int32) :: dimension

        dimension = self%dimension
    end function get_dimension

    pure function get_num_gauss(self) result(num_gauss)
        implicit none
        class(abst_element), intent(in) :: self
        integer(int32) :: num_gauss

        num_gauss = self%num_gauss
    end function get_num_gauss

    pure function interpolate(self, r, value) result(interpolated_value)
        implicit none
        class(abst_element), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: value(:)
        real(real64) :: interpolated_value
        integer(int32) :: i

        interpolated_value = 0.0d0
        do i = 1, self%num_nodes
            interpolated_value = interpolated_value + self%psi(i, r) * value(self%connectivity(i))
        end do
    end function interpolate

    pure function interpolate_reordered(self, r, value) result(interpolated_value)
        implicit none
        class(abst_element), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: value(:)
        real(real64) :: interpolated_value
        integer(int32) :: i

        interpolated_value = 0.0d0
        do i = 1, self%num_nodes
            interpolated_value = interpolated_value + self%psi(i, r) * value(self%connectivity_reordered(i))
        end do
    end function interpolate_reordered

    pure function get_connectivity(self, index) result(connectivity)
        implicit none
        class(abst_element), intent(in) :: self
        integer(int32), intent(in) :: index
        integer(int32) :: connectivity

        connectivity = self%connectivity(index)
    end function get_connectivity

    pure function get_connectivity_reordered(self, index) result(connectivity)
        implicit none
        class(abst_element), intent(in) :: self
        integer(int32), intent(in) :: index
        integer(int32) :: connectivity

        connectivity = self%connectivity_reordered(index)
    end function get_connectivity_reordered

end module domain_element
