module condition_initial
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_input, only:type_input
    implicit none
    private

    public :: abst_ic
    public :: type_ic_uniform

    public :: holder_ics

    type :: holder_ics
        class(abst_ic), allocatable :: p
    end type holder_ics

    type, abstract :: abst_ic
        logical, private :: initialized = .false.
        type(type_constant_id), private :: physics_type = type_constant_id("", "", -1)
        type(type_constant_id), private :: ic_kind = type_constant_id("", "", -1)
    contains
        procedure(abst_initialize_ic), public, pass(self), deferred :: initialize
        procedure(abst_ic_apply), public, pass(self), deferred :: apply
    end type abst_ic

    type, extends(abst_ic) :: type_ic_uniform
        real(real64) :: value
    contains
        procedure, public, pass(self) :: initialize => initialize_ic
        procedure, public, pass(self) :: apply => apply_ic_uniform
    end type type_ic_uniform

    abstract interface
        subroutine abst_initialize_ic(self, config)
            import :: abst_ic, type_config_ic
            implicit none
            class(abst_ic), intent(inout) :: self
            type(type_config_ic), intent(in) :: config

        end subroutine abst_initialize_ic

        subroutine abst_ic_apply(self, variable)
            import :: abst_ic, type_variable
            implicit none
            class(abst_ic), intent(in) :: self
            type(type_variable), intent(inout) :: variable
        end subroutine abst_ic_apply
    end interface

    interface
        module subroutine initialize_ic(self, config)
            implicit none
            class(type_ic_uniform), intent(inout) :: self
            type(type_config_ic), intent(in) :: config
        end subroutine initialize_ic

        module subroutine apply_ic_uniform(self, variable)
            implicit none
            class(type_ic_uniform), intent(in) :: self
            type(type_variable), intent(inout) :: variable
        end subroutine apply_ic_uniform
    end interface

end module condition_initial
