! =============================================================================
! module conditions_initial
! Purpose: Defines the abstract IC types and the interfaces for their methods.
!          Implementations are delegated to a submodule.
! =============================================================================
module conditions_initial
    use, intrinsic :: iso_fortran_env
    use :: module_core, only:type_variable
    use :: module_domain, only:type_domain
    use :: module_input, only:type_input
    use :: module_boundary
    implicit none
    private

    public :: abst_ic
    public :: type_ic_uniform
    public :: type_ic_laplace

    type, abstract :: abst_ic
        character(:), allocatable :: type
    contains
        procedure(abst_ic_initialize), pass(self), deferred :: initialize !&
        procedure(abst_ic_apply),      pass(self), deferred :: apply !&
    end type abst_ic

    type, extends(abst_ic) :: type_ic_uniform
        real(real64) :: value
    contains
        procedure, pass(self) :: initialize => initialize_type_ic_uniform !&
        procedure, pass(self) :: apply      => apply_uniform !&
    end type

    type, extends(abst_ic) :: type_ic_laplace
    contains
        procedure, pass(self) :: initialize => initialize_type_ic_laplace !&
        procedure, pass(self) :: apply      => apply_laplace !&
    end type

    abstract interface
        subroutine abst_ic_initialize(self, input, initial_target)
            import :: abst_ic, type_input
            implicit none
            class(abst_ic), intent(inout) :: self
            type(type_input), intent(in) :: input
            character(*), intent(in) :: initial_target

        end subroutine

        subroutine abst_ic_apply(self, domain, variable)
            import :: abst_ic, type_domain, type_variable
            implicit none
            class(abst_ic), intent(in) :: self
            type(type_domain), intent(in) :: domain
            type(type_variable), intent(inout) :: variable
        end subroutine
    end interface

    interface
        module subroutine initialize_type_ic_uniform(self, input, initial_target)
            implicit none
            class(type_ic_uniform), intent(inout) :: self
            type(type_input), intent(in) :: input
            character(*), intent(in) :: initial_target
        end subroutine

        module subroutine apply_uniform(self, domain, variable)
            implicit none
            class(type_ic_uniform), intent(in) :: self
            type(type_domain), intent(in) :: domain
            type(type_variable), intent(inout) :: variable
        end subroutine

        module subroutine initialize_type_ic_laplace(self, input, initial_target)
            implicit none
            class(type_ic_laplace), intent(inout) :: self
            type(type_input), intent(in) :: input
            character(*), intent(in) :: initial_target
        end subroutine

        module subroutine apply_laplace(self, domain, variable)
            implicit none
            class(type_ic_laplace), intent(in) :: self
            type(type_domain), intent(in) :: domain
            type(type_variable), intent(inout) :: variable
        end subroutine
    end interface

end module conditions_initial
