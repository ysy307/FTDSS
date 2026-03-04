!> Defines strongly-typed data containers for boundary condition parameters.
module condition_boundary_dto
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private

    public :: abst_bc_dto
    public :: type_bc_data_scalar
    public :: type_bc_data_robin
    public :: type_bc_data_hydraulic
    public :: type_bc_data_cauchy

    !> Abstract base class for all boundary condition DTOs.
    type, abstract :: abst_bc_dto
    contains
        procedure, public, pass(self) :: reset => abst_reset_bc_dto
    end type abst_bc_dto

    abstract interface
        subroutine abst_reset_bc_dto(self)
            import :: abst_bc_dto
            implicit none
            class(abst_bc_dto), intent(inout) :: self
        end subroutine abst_reset_bc_dto
    end interface

    !> Data for Dirichlet or Neumann boundaries.
    type, extends(abst_bc_dto) :: type_bc_data_scalar
        real(real64) :: prescribed_value = 0.0d0
    contains
        procedure, public, pass(self) :: reset => reset_bc_data_scalar
    end type type_bc_data_scalar

    !> Data for Robin boundaries [ q = h(u - u_env) ].
    type, extends(abst_bc_dto) :: type_bc_data_robin
        real(real64) :: transfer_coeff = 0.0d0
        real(real64) :: environment_value = 0.0d0
    contains
        procedure, public, pass(self) :: reset => reset_bc_data_robin
    end type type_bc_data_robin

    !> Data for Hydraulic (switching) boundaries.
    type, extends(abst_bc_dto) :: type_bc_data_hydraulic
        real(real64) :: potential_flux = 0.0d0
        real(real64) :: limit_min = 0.0d0
        real(real64) :: limit_max = 0.0d0
    contains
        procedure, public, pass(self) :: reset => reset_bc_data_hydraulic
    end type type_bc_data_hydraulic

    ! --- module condition_boundary_dto の型宣言部に追加 ---

    !> Data for Cauchy boundaries (mixed Dirichlet and Neumann).
    type, extends(abst_bc_dto) :: type_bc_data_cauchy
        real(real64) :: prescribed_value = 0.0d0
        real(real64) :: flux_value = 0.0d0
        real(real64) :: flux_derivative = 0.0d0
    contains
        procedure, public, pass(self) :: reset => reset_bc_data_cauchy
    end type type_bc_data_cauchy

contains

    subroutine reset_bc_data_scalar(self)
        implicit none
        class(type_bc_data_scalar), intent(inout) :: self

        self%prescribed_value = 0.0d0
    end subroutine reset_bc_data_scalar

    subroutine reset_bc_data_robin(self)
        implicit none
        class(type_bc_data_robin), intent(inout) :: self

        self%transfer_coeff = 0.0d0
        self%environment_value = 0.0d0
    end subroutine reset_bc_data_robin

    subroutine reset_bc_data_hydraulic(self)
        implicit none
        class(type_bc_data_hydraulic), intent(inout) :: self

        self%potential_flux = 0.0d0
        self%limit_min = 0.0d0
        self%limit_max = 0.0d0
    end subroutine reset_bc_data_hydraulic

    subroutine reset_bc_data_cauchy(self)
        implicit none
        class(type_bc_data_cauchy), intent(inout) :: self

        self%prescribed_value = 0.0d0
        self%flux_value = 0.0d0
        self%flux_derivative = 0.0d0
    end subroutine reset_bc_data_cauchy

end module condition_boundary_dto
