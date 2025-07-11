module domain_side
    !---------------------------------------------------------------------------------------
    !  Module: domain_side
    !  Purpose: Define 1D finite element types (square and triangle) and their
    !           associated operations (shape functions, Jacobian, Gauss points).
    !  Ford Coding Standard:
    !    - Use ISO_FORTRAN_ENV for portable kinds
    !    - Maintain explicit interfaces and consistent indentation
    !    - Preserve original function and type names
    !--------------------------------------------------------------------------------------
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_dp_3d, type_dp_pointer, type_vtk_cell, allocate_array
    implicit none
    private

    public :: abst_side
    public :: type_side_first
    public :: type_side_second
    public :: holder_sides

    !--------------------------------------------------------------------------------------
    ! Holder for polymorphic element objects
    !--------------------------------------------------------------------------------------
    type :: holder_sides
        class(abst_side), allocatable :: s
    end type holder_sides

    !--------------------------------------------------------------------------------------
    !   Abstract base type for 1D elements
    !--------------------------------------------------------------------------------------
    type, abstract :: abst_side
        integer(int32), private :: id
        integer(int32), private :: type ! Edge type
        integer(int32), private :: num_nodes ! Number of nodes in the Edge
        integer(int32), private :: group ! Group ID
        integer(int32), private :: dimension
        integer(int32), private :: order
        integer(int32), allocatable :: connectivity(:) !! connectivity information
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
        real(real64), allocatable :: gauss(:) !! Gauss Quadrature points Coordinate
    contains
        procedure(abst_get_id),        pass(self), deferred :: get_id !&
        procedure(abst_get_type),      pass(self), deferred :: get_type !&
        procedure(abst_get_num_nodes), pass(self), deferred :: get_num_nodes !&
        procedure(abst_get_group),     pass(self), deferred :: get_group !&
        procedure(abst_get_order),     pass(self), deferred :: get_order !&
        procedure(abst_get_dimension), pass(self), deferred :: get_dimension !&
        procedure(abst_get_num_gauss), pass(self), deferred :: get_num_gauss !&
        !----------------------------------------------------------------------------------
        procedure(abst_psi),           pass(self), deferred :: psi !&
        procedure(abst_dpsi_dxi),      pass(self), deferred :: dpsi_dxi !&
    end type abst_side

    !--------------------------------------------------------------------------------------
    !   Triangle First Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(abst_side) :: type_side_first
    contains
        procedure, pass(self) :: get_id        => get_id_side_first !&
        procedure, pass(self) :: get_type      => get_type_side_first !&
        procedure, pass(self) :: get_num_nodes => get_num_nodes_side_first !&
        procedure, pass(self) :: get_group     => get_group_side_first !&
        procedure, pass(self) :: get_order     => get_order_side_first !&
        procedure, pass(self) :: get_dimension => get_dimension_side_first !&
        procedure, pass(self) :: get_num_gauss => get_num_gauss_side_first !
        !----------------------------------------------------------------------------------
        procedure, pass(self) :: psi           => psi_side_first !&
        procedure, pass(self) :: dpsi_dxi      => dpsi_dxi_side_first !&
    end type type_side_first

    !--------------------------------------------------------------------------------------
    !   Triangle Second Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(abst_side) :: type_side_second
    contains
        procedure, pass(self) :: get_id        => get_id_side_second !&
        procedure, pass(self) :: get_type      => get_type_side_second !&
        procedure, pass(self) :: get_num_nodes => get_num_nodes_side_second !&
        procedure, pass(self) :: get_group     => get_group_side_second !&
        procedure, pass(self) :: get_order     => get_order_side_second !&
        procedure, pass(self) :: get_dimension => get_dimension_side_second !&
        procedure, pass(self) :: get_num_gauss => get_num_gauss_side_second !
        !----------------------------------------------------------------------------------
        procedure, pass(self) :: psi           => psi_side_second !&
        procedure, pass(self) :: dpsi_dxi      => dpsi_dxi_side_second !&
    end type type_side_second

    !--------------------------------------------------------------------------------------
    !  Abstract interface for the 1D element
    !--------------------------------------------------------------------------------------
    abstract interface
        function abst_get_id(self) result(id)
            import :: abst_side, int32
            implicit none
            class(abst_side), intent(in) :: self
            integer(int32) :: id
        end function abst_get_id

        function abst_get_type(self) result(type)
            import :: abst_side, int32
            implicit none
            class(abst_side), intent(in) :: self
            integer(int32) :: type
        end function abst_get_type

        function abst_get_num_nodes(self) result(num_nodes)
            import :: abst_side, int32
            implicit none
            class(abst_side), intent(in) :: self
            integer(int32) :: num_nodes
        end function abst_get_num_nodes

        function abst_get_order(self) result(order)
            import :: abst_side, int32
            implicit none
            class(abst_side), intent(in) :: self
            integer(int32) :: order
        end function abst_get_order

        function abst_get_dimension(self) result(dimension)
            import :: abst_side, int32
            implicit none
            class(abst_side), intent(in) :: self
            integer(int32) :: dimension
        end function abst_get_dimension

        function abst_get_num_gauss(self) result(num_gauss)
            import :: abst_side, int32
            implicit none
            class(abst_side), intent(in) :: self
            integer(int32) :: num_gauss
        end function abst_get_num_gauss

        function abst_get_group(self) result(group)
            import :: abst_side, int32
            implicit none
            class(abst_side), intent(in) :: self
            integer(int32) :: group
        end function abst_get_group

        function abst_psi(self, i, xi) result(psi)
            import :: abst_side, int32, real64
            implicit none
            class(abst_side), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi
            real(real64) :: psi
        end function abst_psi

        function abst_dpsi_dxi(self, i) result(dpsi)
            import :: abst_side, int32, real64
            implicit none
            class(abst_side), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64) :: dpsi
        end function abst_dpsi_dxi
    end interface
    !--------------------------------------------------------------------------------------
    !   Edge first order procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function construct_side_first(id, global_coordinate, cell_info) result(side)
            implicit none
            integer(int32), intent(in) :: id
            type(type_dp_3d), pointer, intent(in) :: global_coordinate
            type(type_vtk_cell), intent(in) :: cell_info
            class(abst_side), allocatable :: side

        end function construct_side_first

        module function get_id_side_first(self) result(id)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32) :: id
        end function get_id_side_first

        module function get_type_side_first(self) result(type)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32) :: type
        end function get_type_side_first

        module function get_num_nodes_side_first(self) result(n)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32) :: n
        end function get_num_nodes_side_first

        module function get_group_side_first(self) result(group)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32) :: group
        end function get_group_side_first

        module function get_order_side_first(self) result(order)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32) :: order
        end function get_order_side_first

        module function get_dimension_side_first(self) result(dimension)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32) :: dimension
        end function get_dimension_side_first

        module function get_num_gauss_side_first(self) result(num_gauss)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32) :: num_gauss
        end function get_num_gauss_side_first

        module function psi_side_first(self, i, xi) result(psi)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi
            real(real64) :: psi
        end function psi_side_first

        module function dpsi_dxi_side_first(self, i) result(dpsi)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64) :: dpsi
        end function dpsi_dxi_side_first
    end interface

    !--------------------------------------------------------------------------------------
    !   Edge Second order procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function construct_side_second(id, global_coordinate, cell_info) result(side)
            implicit none
            integer(int32), intent(in) :: id
            type(type_dp_3d), pointer, intent(in) :: global_coordinate
            type(type_vtk_cell), intent(in) :: cell_info
            class(abst_side), allocatable :: side

        end function construct_side_second

        module function get_id_side_second(self) result(id)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32) :: id
        end function get_id_side_second

        module function get_type_side_second(self) result(type)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32) :: type
        end function get_type_side_second

        module function get_num_nodes_side_second(self) result(n)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32) :: n
        end function get_num_nodes_side_second

        module function get_group_side_second(self) result(group)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32) :: group
        end function get_group_side_second

        module function get_order_side_second(self) result(order)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32) :: order
        end function get_order_side_second

        module function get_dimension_side_second(self) result(dimension)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32) :: dimension
        end function get_dimension_side_second

        module function get_num_gauss_side_second(self) result(num_gauss)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32) :: num_gauss
        end function get_num_gauss_side_second

        module function psi_side_second(self, i, xi) result(psi)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi
            real(real64) :: psi
        end function psi_side_second

        module function dpsi_dxi_side_second(self, i) result(dpsi)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64) :: dpsi
        end function dpsi_dxi_side_second
    end interface

    interface type_side_first
        module procedure :: construct_side_first
    end interface

    interface type_side_second
        module procedure :: construct_side_second
    end interface

end module domain_side
