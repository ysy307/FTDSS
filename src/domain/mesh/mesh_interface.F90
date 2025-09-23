module domain_mesh
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_logger
    use :: module_core, only:type_dp_vector_3d, assignment(=), allocate_array, deallocate_array

    implicit none
    private

    public :: abst_mesh

    ! ==========================================================
    ! 型定義
    ! ==========================================================
    type, abstract :: abst_mesh
        private
        integer(int32) :: type
        integer(int32) :: num_nodes
        integer(int32) :: dimension
        integer(int32) :: order
        integer(int32) :: num_gauss
        real(real64), allocatable :: weight(:)
        type(type_dp_vector_3d), allocatable :: gauss(:)
    contains
        procedure, pass(self), public :: initialize => initialize_abst_mesh
        procedure, pass(self), public :: destroy => destroy_abst_mesh
        procedure, pass(self), public :: get_type
        procedure, pass(self), public :: get_num_nodes
        procedure, pass(self), public :: get_dimension
        procedure, pass(self), public :: get_order
        procedure, pass(self), public :: get_num_gauss
        procedure, pass(self), public :: get_weight
        procedure, pass(self), public :: get_gauss
        procedure, pass(self), public :: lerp
        procedure, pass(self), public :: dlerp

        procedure(abst_get_geometry), pass(self), public, deferred :: get_geometry
        procedure(abst_psi), pass(self), public, deferred :: psi
        procedure(abst_dpsi), pass(self), public, deferred :: dpsi
        procedure(abst_jacobian), pass(self), public, deferred :: jacobian
        procedure(abst_jacobian_det), pass(self), public, deferred :: jacobian_det
    end type

    abstract interface

        function abst_get_geometry(self, node_coords) result(geometry)
            import :: abst_mesh, real64
            implicit none
            class(abst_mesh), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            real(real64) :: geometry
        end function

        !----------------------------------------------------------------------
        ! abst_psi: Shape function
        !----------------------------------------------------------------------
        ! Evaluates the i-th shape function N_i at a given local coordinate
        ! r = (xi, eta, zeta).
        !
        ! Arguments:
        !   self: The mesh element object.
        !   i   : The index of the shape function (corresponding to a node).
        !   r   : The local coordinate vector.
        !
        ! Returns:
        !   The value of the shape function, N_i(r).
        !----------------------------------------------------------------------
        pure elemental function abst_psi(self, i, r) result(psi_val)
            import :: abst_mesh, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_mesh), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi_val
        end function abst_psi

        !----------------------------------------------------------------------
        ! abst_dpsi: Derivative of the shape function
        !----------------------------------------------------------------------
        ! Evaluates the partial derivative of the i-th shape function N_i with
        ! respect to the j-th local coordinate (e.g., d(N_i)/d(xi) for j=1).
        !
        ! Arguments:
        !   self: The mesh element object.
        !   i   : The index of the shape function (corresponding to a node).
        !   j   : The index of the local coordinate (1=xi, 2=eta, 3=zeta).
        !   r   : The local coordinate vector.
        !
        ! Returns:
        !   The value of the derivative, d(N_i)/d(r_j).
        !----------------------------------------------------------------------
        pure elemental function abst_dpsi(self, i, j, r) result(dpsi_val)
            import :: abst_mesh, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_mesh), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi_val
        end function abst_dpsi

        !----------------------------------------------------------------------
        ! abst_jacobian: Jacobian matrix
        !----------------------------------------------------------------------
        ! Computes the Jacobian matrix J, which maps derivatives from local
        ! coordinates (xi, eta) to global coordinates (x, y).
        ! J_ij = d(x_i) / d(xi_j)
        !
        ! Arguments:
        !   self        : The mesh element object.
        !   r           : The local coordinate vector where the Jacobian is evaluated.
        !   node_coords : The global coordinates of the element's nodes.
        !
        ! Returns:
        !   The Jacobian matrix at the specified local coordinate.
        !----------------------------------------------------------------------
        pure function abst_jacobian(self, r, node_coords) result(jac)
            import :: abst_mesh, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_mesh), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :) ! (dimension, num_nodes)
            real(real64) :: jac(self%dimension, self%dimension)
        end function abst_jacobian

        !----------------------------------------------------------------------
        ! abst_jacobian_det: Determinant of the Jacobian matrix
        !----------------------------------------------------------------------
        ! Computes the determinant of the Jacobian matrix, |J|. This value is
        ! used as the scaling factor for integration (e.g., dV = |J| d(xi)d(eta)).
        !
        ! Arguments:
        !   self        : The mesh element object.
        !   r           : The local coordinate vector where the determinant is evaluated.
        !   node_coords : The global coordinates of the element's nodes.
        !
        ! Returns:
        !   The determinant of the Jacobian matrix.
        !----------------------------------------------------------------------
        pure function abst_jacobian_det(self, r, node_coords) result(det_j)
            import :: abst_mesh, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_mesh), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :) ! (dimension, num_nodes)
            real(real64) :: det_j
        end function abst_jacobian_det

    end interface

contains

    subroutine initialize_abst_mesh(self, type, dimension, order, num_nodes, num_gauss, weight, gauss)
        implicit none
        class(abst_mesh), intent(inout) :: self
        integer(int32), intent(in) :: type, dimension, order, num_nodes, num_gauss
        real(real64), intent(in) :: weight(:)
        real(real64), intent(in) :: gauss(:, :) ! (3, num_gauss)
        integer(int32) :: i

        self%type = type
        self%dimension = dimension
        self%order = order
        self%num_nodes = num_nodes
        self%num_gauss = num_gauss

        call allocate_array(self%weight, self%num_gauss)
        self%weight(:) = weight(:)

        allocate (self%gauss(self%num_gauss))
        do i = 1, self%num_gauss
            call self%gauss(i)%set(gauss(1, i), gauss(2, i), gauss(3, i))
        end do
    end subroutine initialize_abst_mesh

    pure function get_type(self) result(val)
        implicit none
        class(abst_mesh), intent(in) :: self
        integer(int32) :: val
        val = self%type
    end function get_type

    pure function get_num_nodes(self) result(val)
        implicit none
        class(abst_mesh), intent(in) :: self
        integer(int32) :: val
        val = self%num_nodes
    end function get_num_nodes

    pure function get_dimension(self) result(val)
        implicit none
        class(abst_mesh), intent(in) :: self
        integer(int32) :: val
        val = self%dimension
    end function get_dimension

    pure function get_order(self) result(val)
        implicit none
        class(abst_mesh), intent(in) :: self
        integer(int32) :: val
        val = self%order
    end function get_order

    pure function get_num_gauss(self) result(val)
        implicit none
        class(abst_mesh), intent(in) :: self
        integer(int32) :: val
        val = self%num_gauss
    end function get_num_gauss

    function get_weight(self) result(val)
        implicit none
        class(abst_mesh), intent(in) :: self
        real(real64), allocatable :: val(:)
        val = self%weight
    end function get_weight

    function get_gauss(self) result(val)
        implicit none
        class(abst_mesh), intent(in) :: self
        type(type_dp_vector_3d), allocatable :: val(:)
        val = self%gauss
    end function get_gauss

    pure function lerp(self, r, global_values, connectivity) result(val)
        implicit none
        class(abst_mesh), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: global_values(:)
        integer(int32), intent(in) :: connectivity(:)
        real(real64) :: val
        integer(int32) :: i

        val = 0.0d0
        do i = 1, self%num_nodes
            val = val + self%psi(i, r) * global_values(connectivity(i))
        end do
    end function lerp

    pure function dlerp(self, r, global_values, connectivity) result(val)
        implicit none
        class(abst_mesh), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: global_values(:)
        integer(int32), intent(in) :: connectivity(:)
        type(type_dp_vector_3d) :: val
        integer(int32) :: i

        val%x = 0.0d0
        val%y = 0.0d0
        val%z = 0.0d0
        do i = 1, self%num_nodes
            if (self%dimension >= 1) then
                val%x = val%x + self%dpsi(i, 1, r) * global_values(connectivity(i))
            end if
            if (self%dimension >= 2) then
                val%y = val%y + self%dpsi(i, 2, r) * global_values(connectivity(i))
            end if
            if (self%dimension >= 3) then
                val%z = val%z + self%dpsi(i, 3, r) * global_values(connectivity(i))
            end if
        end do
    end function dlerp

    subroutine destroy_abst_mesh(self)
        implicit none
        class(abst_mesh), intent(inout) :: self
        if (allocated(self%weight)) then
            deallocate (self%weight)
        end if
        if (allocated(self%gauss)) then
            deallocate (self%gauss)
        end if
    end subroutine destroy_abst_mesh

end module domain_mesh
