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
    use :: module_core, only:type_dp_3d, type_dp_pointer, allocate_array, type_dp_vector_3d, assignment(=), operator(-)
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
    end type abst_side

    !--------------------------------------------------------------------------------------
    !   Triangle First Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(abst_side) :: type_side_first
    contains
        procedure, pass(self) :: get_geometry => get_length_side_first !&
        procedure, pass(self) :: psi          => psi_side_first !&
        procedure, pass(self) :: dpsi         => dpsi_side_first !&
        procedure, pass(self) :: jacobian     => jacobian_side_first !&
        procedure, pass(self) :: jacobian_det => jacobian_det_side_first !&
    end type type_side_first

    !--------------------------------------------------------------------------------------
    !   Triangle Second Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(abst_side) :: type_side_second
    contains
        procedure, pass(self) :: get_geometry => get_length_side_second !&
        procedure, pass(self) :: psi          => psi_side_second !&
        procedure, pass(self) :: dpsi         => dpsi_side_second !&
        procedure, pass(self) :: jacobian     => jacobian_side_second !&
        procedure, pass(self) :: jacobian_det => jacobian_det_side_second !&
    end type type_side_second

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

        module pure function get_length_side_first(self) result(length)
            implicit none
            class(type_side_first), intent(in) :: self
            real(real64) :: length

        end function get_length_side_first

        module pure elemental function psi_side_first(self, i, r) result(psi)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi
        end function psi_side_first

        module pure elemental function dpsi_side_first(self, i, j, r) result(dpsi)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi
        end function dpsi_side_first

        pure elemental module function jacobian_side_first(self, i, j, r) result(jacobian)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: jacobian

        end function jacobian_side_first

        pure elemental module function jacobian_det_side_first(self, r) result(jacobian_det)
            implicit none
            class(type_side_first), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: jacobian_det

        end function jacobian_det_side_first
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

        module pure function get_length_side_second(self) result(length)
            implicit none
            class(type_side_second), intent(in) :: self
            real(real64) :: length

        end function get_length_side_second

        module pure elemental function psi_side_second(self, i, r) result(psi)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi
        end function psi_side_second

        module pure elemental function dpsi_side_second(self, i, j, r) result(dpsi)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi
        end function dpsi_side_second

        pure elemental module function jacobian_side_second(self, i, j, r) result(jacobian)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: jacobian

        end function jacobian_side_second

        pure elemental module function jacobian_det_side_second(self, r) result(jacobian_det)
            implicit none
            class(type_side_second), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: jacobian_det

        end function jacobian_det_side_second
    end interface

    interface type_side_first
        module procedure :: construct_side_first
    end interface

    interface type_side_second
        module procedure :: construct_side_second
    end interface

end module domain_mesh_side
