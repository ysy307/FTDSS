module domain_mesh_side
    !---------------------------------------------------------------------------------------
    !  Module: domain_mesh_side
    !  Purpose: Define 1D finite element types (square and triangle) and their
    !           associated operations (shape functions, Jacobian, Gauss points).
    !  Ford Coding Standard:
    !    - Use ISO_FORTRAN_ENV for portable kinds
    !    - Maintain explicit interfaces and consistent indentation
    !    - Preserve original function and type names
    !--------------------------------------------------------------------------------------
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_logger
    use :: module_core, only:type_dp_3d, type_dp_pointer, allocate_array, type_dp_vector_3d, assignment(=)
    use :: module_input, only:type_input
    use :: domain_mesh, only:abst_mesh
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
    type, abstract, extends(abst_mesh) :: abst_side
    contains
        procedure,                     pass(self)           :: lerp => interpolate_side !&
        procedure,                     pass(self)           :: dlerp => deriv_interpolate_side !&
        procedure(abst_psi),           pass(self), deferred :: psi !&
        procedure(abst_dpsi_dxi),      pass(self), deferred :: dpsi_dxi !&
    end type abst_side

    !--------------------------------------------------------------------------------------
    !   Triangle First Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(abst_side) :: type_side_first
    contains
        procedure, pass(self) :: psi           => psi_side_first !&
        procedure, pass(self) :: dpsi_dxi      => dpsi_dxi_side_first !&
    end type type_side_first

    !--------------------------------------------------------------------------------------
    !   Triangle Second Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(abst_side) :: type_side_second
    contains
        procedure, pass(self) :: psi           => psi_side_second !&
        procedure, pass(self) :: dpsi_dxi      => dpsi_dxi_side_second !&
    end type type_side_second

    !--------------------------------------------------------------------------------------
    !  Abstract interface for the 1D element
    !--------------------------------------------------------------------------------------
    abstract interface
        pure elemental function abst_psi(self, i, r) result(psi)
            import :: abst_side, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_side), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi
        end function abst_psi

        pure elemental function abst_dpsi_dxi(self, i, r) result(dpsi)
            import :: abst_side, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_side), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi
        end function abst_dpsi_dxi
    end interface
    !--------------------------------------------------------------------------------------
    !   Edge first order procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function construct_side_first(id, global_coordinate, input) result(side)
            implicit none
            integer(int32), intent(in) :: id
            type(type_dp_3d), pointer, intent(in) :: global_coordinate
            type(type_input), intent(in) :: input
            class(abst_side), allocatable :: side

        end function construct_side_first

        module pure elemental function psi_side_first(self, i, r) result(psi)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi
        end function psi_side_first

        module pure elemental function dpsi_dxi_side_first(self, i, r) result(dpsi)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi
        end function dpsi_dxi_side_first
    end interface

    !--------------------------------------------------------------------------------------
    !   Edge Second order procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function construct_side_second(id, global_coordinate, input) result(side)
            implicit none
            integer(int32), intent(in) :: id
            type(type_dp_3d), pointer, intent(in) :: global_coordinate
            type(type_input), intent(in) :: input
            class(abst_side), allocatable :: side

        end function construct_side_second

        module pure elemental function psi_side_second(self, i, r) result(psi)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi
        end function psi_side_second

        module pure elemental function dpsi_dxi_side_second(self, i, r) result(dpsi)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi
        end function dpsi_dxi_side_second
    end interface

    interface type_side_first
        module procedure :: construct_side_first
    end interface

    interface type_side_second
        module procedure :: construct_side_second
    end interface

contains
    function interpolate_side(self, r, value) result(interpolated_value)
        implicit none
        class(abst_side), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: value(:)
        real(real64) :: interpolated_value

        integer(int32), dimension(:), pointer :: connectivity
        integer(int32) :: i

        interpolated_value = 0.0d0

        connectivity => self%get_connectivity()

        do i = 1, self%get_num_nodes()
            interpolated_value = interpolated_value + self%psi(i, r) * value(connectivity(i))
        end do
    end function interpolate_side

    function deriv_interpolate_side(self, r, value) result(interpolated_value)
        implicit none
        class(abst_side), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: value(:)
        type(type_dp_vector_3d) :: interpolated_value

        integer(int32), dimension(:), pointer :: connectivity
        integer(int32) :: i

        interpolated_value%x = 0.0d0
        interpolated_value%y = 0.0d0
        interpolated_value%z = 0.0d0

        connectivity => self%get_connectivity()

        do i = 1, self%get_num_nodes()
            interpolated_value%x = interpolated_value%x + self%dpsi_dxi(i, r) * value(connectivity(i))
        end do
    end function deriv_interpolate_side

end module domain_mesh_side
