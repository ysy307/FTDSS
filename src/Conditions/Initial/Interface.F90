! =============================================================================
! module Condition_Initial
! Purpose: Defines the abstract IC types and the interfaces for their methods.
!          Implementations are delegated to a submodule.
! =============================================================================
module Condition_Initial
    use, intrinsic :: iso_fortran_env
    use :: core_core, only:type_variable
    use :: Domain_Module, only:type_domain
    use :: Inout_Input, only:Type_Input
    implicit none
    private

    public :: Abstract_IC, IC_Uniform, IC_Laplace

    !
    ! Abstract Base Class
    !
    type, abstract :: Abstract_IC
        character(:), allocatable :: type ! Type of IC, e.g., "Uniform", "Laplace"
    contains
        procedure(setup_ic_abstract), pass(self), deferred :: setup
        procedure(apply_ic_abstract), pass(self), deferred :: apply
    end type Abstract_IC

    !
    ! Concrete Type: Uniform
    !
    type, extends(Abstract_IC) :: IC_Uniform

        real(real64) :: value = 0.0d0
    contains
        procedure, pass(self) :: setup => setup_uniform
        procedure, pass(self) :: apply => apply_uniform
    end type

    !
    ! Concrete Type: Laplace
    !
    type, extends(Abstract_IC) :: IC_Laplace
        ! Member type_variable for Laplace would be defined here
    contains
        procedure, pass(self) :: setup => setup_laplace
        procedure, pass(self) :: apply => apply_laplace
    end type

    !
    ! Abstract Interfaces (required for deferred procedures)
    !
    abstract interface
        subroutine setup_ic_abstract(self, Input, IC_target)
            import :: Abstract_IC, Type_Input
            class(Abstract_IC), intent(inout) :: self
            type(Type_Input), intent(in) :: Input
            character(1), intent(in) :: IC_target ! Target for the IC, e.g., 'T' for Thermal

        end subroutine

        subroutine apply_ic_abstract(self, domain, var)
            import :: Abstract_IC, type_domain, type_variable
            class(Abstract_IC), intent(in) :: self
            type(type_domain), intent(in) :: domain
            type(type_variable), intent(inout) :: var
        end subroutine
    end interface

    !
    ! Interfaces for Submodule Procedures
    !
    interface
        module subroutine setup_uniform(self, Input, IC_target)
            class(IC_Uniform), intent(inout) :: self
            type(Type_Input), intent(in) :: Input
            character(1), intent(in) :: IC_target
        end subroutine

        module subroutine apply_uniform(self, domain, var)
            class(IC_Uniform), intent(in) :: self
            type(type_domain), intent(in) :: domain
            type(type_variable), intent(inout) :: var
        end subroutine

        module subroutine setup_laplace(self, Input, IC_target)
            class(IC_Laplace), intent(inout) :: self
            type(Type_Input), intent(in) :: Input
            character(1), intent(in) :: IC_target
        end subroutine

        module subroutine apply_laplace(self, domain, var)
            class(IC_Laplace), intent(in) :: self
            type(type_domain), intent(in) :: domain
            type(type_variable), intent(inout) :: var
        end subroutine
    end interface

end module Condition_Initial
