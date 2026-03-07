module conditions_boundary_manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
    use :: condition_boundary_strategy
    use :: condition_boundary_data_provider
    implicit none
    private

    public :: type_bc_manager

    type :: holder_bc_strategy
        class(abst_bc), allocatable :: p
    end type holder_bc_strategy

    type :: holder_bc_provider
        class(abst_bc_data), allocatable :: p
    end type holder_bc_provider

    type :: type_bc_manager
        type(holder_bc_strategy), allocatable, private :: strategies(:)
        type(holder_bc_provider), allocatable, private :: providers(:)
    contains
        procedure, public, pass(self) :: initialize => initialize_bc_manager
        procedure, public, pass(self) :: destroy => destroy_bc_manager
        procedure, public, pass(self) :: evaluate => evaluate_bc_manager
    end type type_bc_manager

contains

    subroutine initialize_bc_manager(self, configs)
        implicit none
        class(type_bc_manager), intent(inout) :: self
        type(type_config_bc), intent(in) :: configs(:)
        
        ! Data initialization will be implemented by the user.
        ! Allocate self%strategies and self%providers here,
        ! and call inject_provider.
    end subroutine initialize_bc_manager

    subroutine destroy_bc_manager(self)
        implicit none
        class(type_bc_manager), intent(inout) :: self
        integer(int32) :: i

        if (allocated(self%strategies)) then
            do i = 1, size(self%strategies)
                if (allocated(self%strategies(i)%p)) then
                    call self%strategies(i)%p%destroy()
                    deallocate(self%strategies(i)%p)
                end if
            end do
            deallocate(self%strategies)
        end if

        if (allocated(self%providers)) then
            do i = 1, size(self%providers)
                if (allocated(self%providers(i)%p)) then
                    call self%providers(i)%p%destroy()
                    deallocate(self%providers(i)%p)
                end if
            end do
            deallocate(self%providers)
        end if
    end subroutine destroy_bc_manager

    subroutine evaluate_bc_manager(self, bc_id, current_time, u_curr, result)
        implicit none
        class(type_bc_manager), intent(in) :: self
        integer(int32), intent(in) :: bc_id
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        type(type_bc_result), intent(inout) :: result

        if (bc_id > 0 .and. bc_id <= size(self%strategies)) then
            call self%strategies(bc_id)%p%evaluate(current_time, u_curr, result)
        else
            result%is_dirichlet = .false.
            result%prescribed_value = 0.0d0
            result%flux_value = 0.0d0
            result%flux_derivative = 0.0d0
        end if
    end subroutine evaluate_bc_manager

end module conditions_boundary_manager