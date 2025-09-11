module main_thermal
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_logger
    use :: stdlib_strings
    use :: module_core
    use :: module_domain, only:type_domain
    use :: module_field, only:type_residual_vector, type_jacobian_matrix
    use :: physics_material_density, only:abst_den
    use :: module_properties, only:type_properties_manager, type_phase_property
    use :: module_input, only:type_input
    use :: module_boundary, only:type_bc, mode_value, mode_nr
    use :: module_solver
    use :: module_control
    use :: main_thermal_assemble
    implicit none
    private

    ! public :: abst_thermal
    public :: type_thermal

    type :: type_thermal
        type(type_variable) :: Qw
        type(type_variable) :: Qice
        type(type_variable) :: Si

        type(type_jacobian_matrix) :: KT_star
        type(type_residual_vector) :: PHIT

        !! Nonlinear solver
        character(:), allocatable :: algorithm

        !! Solver
        class(abst_solver), allocatable :: solver
        integer(int32) :: order

        procedure(abst_assemble_global_thermal), nopass, pointer :: assemble_global => null()
    contains
        procedure, pass(self) :: initialize => initialize_type_thermal
        procedure, pass(self) :: update => update_type_thermal
        procedure, pass(self) :: shift => shift_type_thermal
        procedure, pass(self) :: solve => solve_type_thermal
        procedure, pass(self) :: compute => compute_type_thermal
    end type type_thermal

    interface
        module subroutine initialize_type_thermal(self, input, coordinate, domain)
            implicit none
            class(type_thermal), intent(inout) :: self
            type(type_input), intent(inout) :: input
            type(type_dp_3d), intent(inout), pointer :: coordinate
            type(type_domain), intent(inout) :: domain

        end subroutine initialize_type_thermal

        module subroutine update_type_thermal(self, domain, property, temperature, porosity, controls)
            implicit none
            class(type_thermal), intent(inout) :: self
            type(type_domain), intent(inout), target :: domain
            type(type_properties_manager), intent(inout) :: property
            real(real64), intent(in) :: temperature(:)
            real(real64), intent(in) :: porosity(:)
            type(type_controls), intent(in) :: controls

        end subroutine update_type_thermal

        module subroutine shift_type_thermal(self)
            implicit none
            class(type_thermal), intent(inout) :: self

        end subroutine shift_type_thermal

        module subroutine solve_type_thermal(self, temperature, controls)
            implicit none
            class(type_thermal), intent(inout) :: self
            type(type_variable), intent(inout) :: temperature
            type(type_controls), intent(in) :: controls

        end subroutine solve_type_thermal

        module subroutine compute_type_thermal(self, domain, property, temperature, porosity, controls, bc)
            implicit none
            class(type_thermal), intent(inout) :: self
            type(type_domain), intent(inout) :: domain
            type(type_properties_manager), intent(in) :: property
            type(type_variable), intent(inout) :: temperature
            type(type_variable), intent(inout) :: porosity
            type(type_controls), intent(inout) :: controls
            type(type_bc), intent(inout) :: bc

        end subroutine compute_type_thermal

    end interface

end module main_thermal
