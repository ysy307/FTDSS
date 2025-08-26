module domain_mesh
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_logger
    use :: module_core, only:type_dp_3d, type_dp_vector_3d, assignment(=), type_dp_pointer, allocate_array, deallocate_array
    implicit none
    private

    public :: abst_mesh

    type, abstract :: abst_mesh
        integer(int32), private :: id !! Element ID
        integer(int32), private :: type !! Element type (5: triangle 1st, 9: square 1st)
        integer(int32), private :: num_nodes !! Number of nodes in the element
        integer(int32), private :: group !! Element group number
        integer(int32), private :: dimension
        integer(int32), private :: order
        integer(int32), allocatable, private :: connectivity(:) !! connectivity information
        type(type_dp_pointer), allocatable, private :: x(:) !! X coordinate
        type(type_dp_pointer), allocatable, private :: y(:) !! Y coordinate
        type(type_dp_pointer), allocatable, private :: z(:) !! Z coordinate
        !----------------------------------------------------------------------------------
        ! Gauss Quadrature points and weights
        !  - Gauss Quadrature points are defined in the local coordinate system
        !  - The number of Gauss points is determined by the element type
        !  - The weights are used for numerical integration over the element
        !  - The Gauss points are used to evaluate the shape functions and their derivatives
        !----------------------------------------------------------------------------------
        integer(int32), private :: num_gauss !! Number of Gauss Quadrature points
        real(real64), allocatable, private :: weight(:) !! Gauss weight
        type(type_dp_vector_3d), allocatable, private :: gauss(:) !! Gauss Quadrature points Coordinate
    contains
        procedure, pass(self), public :: initialize => initialize_abst_mesh !&
        procedure, pass(self), public :: get_id !&
        procedure, pass(self), public :: get_type !&
        procedure, pass(self), public :: get_num_nodes !&
        procedure, pass(self), public :: get_group !&
        procedure, pass(self), public :: get_order !&
        procedure, pass(self), public :: get_dimension !&
        procedure, pass(self), public :: get_num_gauss !&
        procedure, pass(self), public :: get_weight !&
        procedure, pass(self), public :: get_gauss !&
        procedure, pass(self), public :: get_coordinate !&
        procedure, pass(self), public :: get_connectivity !&
        procedure, pass(self), public :: lerp !&
        procedure, pass(self), public :: dlerp !&

        procedure(abst_get_geometry), pass(self), public, deferred :: get_geometry
        procedure(abst_psi), pass(self), public, deferred :: psi
        procedure(abst_dpsi), pass(self), public, deferred :: dpsi
        procedure(abst_jacobian), pass(self), public, deferred :: jacobian
        procedure(abst_jacobian_det), pass(self), public, deferred :: jacobian_det

    end type

    abstract interface
        pure function abst_get_geometry(self) result(geometry)
            import :: abst_mesh, real64
            implicit none
            class(abst_mesh), intent(in) :: self
            real(real64) :: geometry
        end function

        pure elemental function abst_psi(self, i, r) result(psi)
            import :: abst_mesh, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_mesh), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi
        end function abst_psi

        pure elemental function abst_dpsi(self, i, j, r) result(dpsi)
            import :: abst_mesh, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_mesh), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi
        end function abst_dpsi

        pure elemental function abst_jacobian(self, i, j, r) result(jacobian)
            import :: abst_mesh, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_mesh), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: jacobian
        end function abst_jacobian

        pure elemental function abst_jacobian_det(self, r) result(jacobian_det)
            import :: abst_mesh, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_mesh), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: jacobian_det

        end function abst_jacobian_det

    end interface

contains
    subroutine initialize_abst_mesh(self, id, type, group, dimension, order, num_nodes, connectivity, &
                                    num_gauss, weight, gauss, global_coordinate)
        implicit none
        class(abst_mesh), intent(inout) :: self
        integer(int32), intent(in) :: id
        integer(int32), intent(in) :: type
        integer(int32), intent(in) :: group
        integer(int32), intent(in) :: dimension
        integer(int32), intent(in) :: order
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in) :: connectivity(:)
        integer(int32), intent(in) :: num_gauss
        real(real64), intent(in) :: weight(:)
        real(real64), intent(in) :: gauss(:, :)
        type(type_dp_3d), pointer, intent(in) :: global_coordinate

        integer(int32) :: i

        self%id = id
        self%type = type
        self%group = group
        self%dimension = dimension
        self%order = order
        self%num_nodes = num_nodes
        self%num_gauss = num_gauss

        call allocate_array(self%connectivity, self%num_nodes)
        do i = 1, self%num_nodes
            self%connectivity(i) = connectivity(i)
        end do

        call allocate_array(self%weight, self%num_gauss)
        do i = 1, self%num_gauss
            self%weight(i) = weight(i)
        end do

        allocate (self%gauss(self%num_gauss))
        do i = 1, self%num_gauss
            call self%gauss(i)%set(gauss(:, i))
        end do

        allocate (self%x(self%num_nodes))
        allocate (self%y(self%num_nodes))
        allocate (self%z(self%num_nodes))
        do i = 1, self%num_nodes
            nullify (self%x(i)%val)
            nullify (self%y(i)%val)
            nullify (self%z(i)%val)
            self%x(i)%val => global_coordinate%x(self%connectivity(i))
            self%y(i)%val => global_coordinate%y(self%connectivity(i))
            self%z(i)%val => global_coordinate%z(self%connectivity(i))
        end do

    end subroutine initialize_abst_mesh

    pure function get_id(self) result(id)
        implicit none
        class(abst_mesh), intent(in) :: self
        integer(int32) :: id

        id = self%id
    end function get_id

    pure function get_type(self) result(type)
        implicit none
        class(abst_mesh), intent(in) :: self
        integer(int32) :: type

        type = self%type
    end function get_type

    pure function get_num_nodes(self) result(num_nodes)
        implicit none
        class(abst_mesh), intent(in) :: self
        integer(int32) :: num_nodes

        num_nodes = self%num_nodes
    end function get_num_nodes

    pure function get_group(self) result(group)
        implicit none
        class(abst_mesh), intent(in) :: self
        integer(int32) :: group

        group = self%group
    end function get_group

    pure function get_order(self) result(order)
        implicit none
        class(abst_mesh), intent(in) :: self
        integer(int32) :: order

        order = self%order
    end function get_order

    pure function get_dimension(self) result(dimension)
        implicit none
        class(abst_mesh), intent(in) :: self
        integer(int32) :: dimension

        dimension = self%dimension
    end function get_dimension

    pure function get_num_gauss(self) result(num_gauss)
        implicit none
        class(abst_mesh), intent(in) :: self
        integer(int32) :: num_gauss

        num_gauss = self%num_gauss
    end function get_num_gauss

    function get_weight(self) result(weight)
        implicit none
        class(abst_mesh), intent(in), target :: self
        real(real64), dimension(:), pointer :: weight

        weight => self%weight(:)
    end function get_weight

    function get_gauss(self) result(gauss)
        implicit none
        class(abst_mesh), intent(in), target :: self
        type(type_dp_vector_3d), dimension(:), pointer :: gauss

        gauss => self%gauss(:)
    end function get_gauss

    pure function get_coordinate(self, index) result(coordinate)
        implicit none
        class(abst_mesh), intent(in) :: self
        integer(int32), intent(in) :: index
        type(type_dp_vector_3d) :: coordinate

        coordinate%x = self%x(index)%val
        coordinate%y = self%y(index)%val
        coordinate%z = self%z(index)%val
    end function get_coordinate

    function get_connectivity(self) result(connectivity)
        implicit none
        class(abst_mesh), intent(in), target :: self
        integer(int32), dimension(:), pointer :: connectivity

        connectivity => self%connectivity(:)
    end function get_connectivity

    pure function lerp(self, r, value) result(val)
        implicit none
        class(abst_mesh), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: value(:)
        real(real64) :: val

        integer(int32) :: i

        val = 0.0d0

        do i = 1, self%num_nodes
            val = val + self%psi(i, r) * value(self%connectivity(i))
        end do

    end function lerp

    pure function dlerp(self, r, value) result(val)
        implicit none
        class(abst_mesh), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: value(:)
        type(type_dp_vector_3d) :: val

        integer(int32) :: i

        val%x = 0.0d0
        val%y = 0.0d0
        val%z = 0.0d0

        do i = 1, self%num_nodes
            if (self%dimension >= 1) val%x = val%x + self%dpsi(i, 1, r) * value(self%connectivity(i))
            if (self%dimension >= 2) val%y = val%y + self%dpsi(i, 2, r) * value(self%connectivity(i))
            if (self%dimension >= 3) val%z = val%z + self%dpsi(i, 3, r) * value(self%connectivity(i))
        end do

    end function dlerp

end module domain_mesh
