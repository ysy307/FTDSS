module conditions_initial
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_input, only:type_input
    implicit none
    private

    public :: abst_ic
    public :: type_ic_uniform

    public :: holder_ics

    type :: holder_ics
        class(abst_ic), allocatable :: ic
    end type holder_ics

    type, abstract :: abst_ic
        logical, private :: initialized = .false.
        type(type_config_ic), private :: config
    contains
        procedure, public, pass(self) :: initialize => initialize_ic
        procedure(abst_ic_apply), pass(self), deferred :: apply
    end type abst_ic

    type, extends(abst_ic) :: type_ic_uniform
    contains
        procedure, pass(self) :: apply => apply_ic_uniform
    end type type_ic_uniform

    abstract interface
        subroutine abst_ic_apply(self, variable)
            import :: abst_ic, type_variable
            implicit none
            class(abst_ic), intent(in) :: self
            type(type_variable), intent(inout) :: variable
        end subroutine abst_ic_apply
    end interface

    interface
        module subroutine apply_ic_uniform(self, variable)
            implicit none
            class(type_ic_uniform), intent(in) :: self
            type(type_variable), intent(inout) :: variable
        end subroutine apply_ic_uniform
    end interface

contains

    subroutine initialize_ic(self, config_ic)
        implicit none
        class(abst_ic), intent(inout) :: self
        type(type_config_ic), intent(in) :: config_ic

        call self%config%copy(config_ic)
        self%initialized = .true.
    end subroutine initialize_ic

end module conditions_initial
