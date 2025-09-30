module conditions_boundary_manager
    use :: iso_fortran_env
    use :: module_core
    use :: module_input
    use :: module_control
    use :: conditions_boundary
    implicit none
    private

    public :: create_boundary_conditions

contains

    function create_boundary_conditions(target_bc_id, cell_id, input, controls) result(structure)
        implicit none
        integer(int32), intent(in) :: target_bc_id
        integer(int32), intent(in) :: cell_id
        type(type_input), intent(in) :: input
        type(type_controls), intent(in) :: controls
        class(abst_bc), allocatable :: structure

        select case (target_bc_id)
        case (THERMAL_BC_ADIABATIC)
            structure = construct_type_bc_thermal_adiabatic(cell_id, input, controls)
        case (THERMAL_BC_DIRICHLET)
            structure = construct_type_bc_thermal_dirichlet(cell_id, input, controls)
        case default
            write (*, *) "Error: Unknown boundary condition type ID: ", target_bc_id
            stop
        end select

    end function create_boundary_conditions

!     subroutine initialize_type_bc(self, input, domain, controls)
!         class(type_bc), intent(inout) :: self
!         type(type_input), intent(in) :: input
!         type(type_domain), intent(inout) :: domain
!         type(type_controls), intent(in) :: controls

!         integer(int32), allocatable :: counts_thermal(:)
!         integer(int32) :: num_boundaries
!         integer(int32) :: i, j
!         integer(int32) :: current_index

!         num_boundaries = input%conditions%num_boundaries
!         self%num_thermal_types = 0

!         call allocate_array(counts_thermal, size(thermal_bc_types))
!         counts_thermal = 0
!         do i = 1, num_boundaries
!             if (input%basic%analysis_controls%calculate_thermal) then
!                 bc_type: do j = 1, size(thermal_bc_types)
!                     if (trim(input%conditions%boundary_conditions(i)%thermal%type) == trim(thermal_bc_types(j))) then
!                         counts_thermal(j) = counts_thermal(j) + 1
!                         exit bc_type
!                     end if
!                 end do bc_type
!             end if
!         end do

!         self%num_thermal_types = sum(counts_thermal)
!         if (self%num_thermal_types == 0) return

!         if (allocated(self%bc_thermal)) deallocate (self%bc_thermal)
!         allocate (self%bc_thermal(self%num_thermal_types))
!         current_index = 0

!         do j = 1, size(thermal_bc_types)
!             do i = 1, num_boundaries
!                 if (input%basic%analysis_controls%calculate_thermal .and. &
!                     trim(input%conditions%boundary_conditions(i)%thermal%type) == trim(thermal_bc_types(j))) then
!                     current_index = current_index + 1
!                     select case (trim(adjustl(thermal_bc_types(j))))
!                     case ("neumann")
!                     case ("adiabatic")
!                         self%bc_thermal(current_index)%p = type_bc_thermal_adiabatic(input, domain, controls, i)
!                     case ("dirichlet")
!                         self%bc_thermal(current_index)%p = type_bc_thermal_dirichlet(input, domain, controls, i)
!                     case default
!                     end select
!                 end if
!             end do
!         end do

!         call deallocate_array(counts_thermal)
!     end subroutine initialize_type_bc

!     subroutine apply_bc(self, boundary_target, current_time, A, b, domain, mode)
!         implicit none
!         class(type_bc), intent(inout) :: self
!         integer(int32), intent(in) :: boundary_target
!         real(real64), intent(in) :: current_time
!         type(type_jacobian_matrix), intent(inout), optional :: A
!         type(type_residual_vector), intent(inout) :: b
!         type(type_domain), intent(inout) :: domain
!         integer(int32), intent(in), optional :: mode

!         integer(int32) :: i
!         integer(int32) :: num_thermal_bcs

!         num_thermal_bcs = size(self%bc_thermal)
!         if (num_thermal_bcs == 0) return

!         select case (boundary_target)
!         case (calc_thermal)
!             do i = 1, num_thermal_bcs
!             if (allocated(self%bc_thermal(i)%p)) then
!                 select type (bc => self%bc_thermal(i)%p)
!                 class default
!                     if (present(mode)) then
!                         call bc%apply(current_time=current_time, A=A, b=b, domain=domain, mode=mode)
!                     else
!                         call bc%apply(current_time=current_time, A=A, b=b, domain=domain)
!                     end if
!                 end select
!             end if
!             end do
!         end select
!     end subroutine apply_bc

end module conditions_boundary_manager
