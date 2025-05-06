module Condition_Initial
    use, intrinsic :: iso_fortran_env
    use :: Core_BaseTypes
    use :: Core_Side
    use :: Condition_Boundary
    use :: Inout_Input
    implicit none
    private

    public :: Abstract_Condition_IC
    public :: Type_Condition_IC_CRS

    type, abstract :: Abstract_Condition_IC
        character(:), allocatable :: type
        real(real64) :: value
    contains
        procedure(Abstract_Fix_IC), pass(self), deferred :: Fix
    end type Abstract_Condition_IC

    type, extends(Abstract_Condition_IC) :: Type_Condition_IC_CRS
    contains
        procedure, pass(self) :: Fix => Fix_IC_CRS
    end type Type_Condition_IC_CRS

    abstract interface
        subroutine Abstract_Fix_IC(self, value, Sides, BC)
            import :: Abstract_Condition_IC, Variables, SideHolder, Abstract_Condition_BC
            implicit none
            class(Abstract_Condition_IC), intent(inout) :: self
            type(Variables), intent(inout) :: value
            type(SideHolder), intent(inout) :: Sides(:)
            class(Abstract_Condition_BC), intent(inout) :: BC

        end subroutine Abstract_Fix_IC
    end interface

    interface
        module function Type_Condition_IC_CRS_Construct(Input, type) result(Structure)
            implicit none
            type(Type_Input), intent(in) :: Input
            class(Abstract_Condition_IC), allocatable :: Structure
            character(*), intent(in) :: type

        end function Type_Condition_IC_CRS_Construct

        module subroutine Fix_IC_CRS(self, value, Sides, BC)
            implicit none
            class(Type_Condition_IC_CRS), intent(inout) :: self
            type(Variables), intent(inout) :: value
            type(SideHolder), intent(inout) :: Sides(:)
            class(Abstract_Condition_BC), intent(inout) :: BC

        end subroutine Fix_IC_CRS
    end interface

    interface Type_Condition_IC_CRS
        module procedure :: Type_Condition_IC_CRS_Construct
    end interface

contains

end module Condition_Initial
