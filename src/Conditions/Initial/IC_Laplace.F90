! =============================================================================
! submodule (Condition_Initial) Condition_Initial_Laplace
! Purpose: Provides the concrete implementations for the procedures
!          defined in the Condition_Initial module.
! =============================================================================
submodule(Condition_Initial) Condition_Initial_Laplace
    implicit none

contains

    module subroutine setup_Laplace(self, Input, IC_target)
        class(IC_Laplace), intent(inout) :: self
        type(Type_Input), intent(in) :: Input
        character(1), intent(in) :: IC_target
        ! Your implementation for setting up IC_Laplace
        select case (trim(adjustl(IC_target)))
        case ('T')
            self%type = Input%IC%Heat%type
            ! self%value = Input%IC%Heat%value

        end select
    end subroutine

    module subroutine apply_Laplace(self, domain, var)
        class(IC_Laplace), intent(in) :: self
        type(Domain_t), intent(in) :: domain
        type(type_variable), intent(inout) :: var
        ! Your implementation for applying IC_Laplace
        ! var%new(:) = self%value
        ! var%pre(:) = var%new(:)
    end subroutine

end submodule Condition_Initial_Laplace
