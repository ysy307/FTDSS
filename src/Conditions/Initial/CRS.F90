submodule(Condition_Initial) Condition_Initial_CRS
    use, intrinsic :: iso_fortran_env
    ! use :: Core_BaseTypes
    ! use :: Core_Side
    ! use :: Condition_Boundary
    ! use :: Inout_Input
    implicit none
contains

    module function Type_Condition_IC_CRS_Construct(Input, type) result(Structure)
        implicit none
        type(Type_Input), intent(in) :: Input
        class(Abstract_Condition_IC), allocatable :: Structure
        character(*), intent(in) :: type

        if (allocated(Structure)) deallocate (Structure)
        allocate (Type_Condition_IC_CRS :: Structure)

        select case (type)
        case ("Thermal")
            Structure%type = Input%IC%Heat%type
            Structure%value = Input%IC%Heat%value
        end select

    end function Type_Condition_IC_CRS_Construct

    module subroutine Fix_IC_CRS(self, value, Sides, BC)
        implicit none
        class(Type_Condition_IC_CRS), intent(inout) :: self
        type(Variables), intent(inout) :: value
        type(SideHolder), intent(inout) :: Sides(:)
        class(Abstract_Condition_BC), intent(inout) :: BC

        select case (self%type)
        case ("Constant")
            value%new(:) = self%value
            call BC%Fix_BC(b=value%new(:), &
                           Sides=Sides)

            value%pre(:) = value%new(:)
        end select

    end subroutine Fix_IC_CRS

end submodule Condition_Initial_CRS
