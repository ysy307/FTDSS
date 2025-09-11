module conditions_boundary
    use, intrinsic :: iso_fortran_env
    use :: stdlib_strings, only:to_string
    use :: stdlib_logger
    use :: module_core
    use :: module_domain, only:type_domain, holder_sides, abst_mesh
    use :: module_input
    use :: module_control, only:type_controls
    use :: module_field
    implicit none
    private

    public :: abst_bc
    public :: type_bc_thermal_dirichlet
    public :: type_bc_thermal_adiabatic

    integer(int32), parameter, public :: mode_value = 1
    integer(int32), parameter, public :: mode_nr = 0
    integer(int32), parameter, public :: mode_ic = -1

    character(*), parameter, public :: thermal_bc_types(8) = [character(len=16) :: &
                                                              "neumann", "flux", "robin", "free", "heat_transfer", &
                                                              "head_radiation", "adiabatic", "dirichlet"]

    type, abstract :: abst_bc
        private
        integer(int32) :: num_targets
        integer(int32) :: dimension
        integer(int32) :: group_id
        integer(int32), allocatable :: target_ids(:)
    contains
        procedure(abst_apply), public, pass(self), deferred :: apply
    end type abst_bc

    abstract interface
        subroutine abst_apply(self, current_time, A, b, domain, mode)
            import :: abst_bc, type_jacobian_matrix, type_residual_vector, type_domain, real64, int32
            implicit none
            class(abst_bc), intent(in) :: self
            real(real64), intent(in) :: current_time
            type(type_jacobian_matrix), intent(inout), optional :: A
            type(type_residual_vector), intent(inout) :: b
            type(type_domain), intent(inout), target :: domain
            integer(int32), intent(in), optional :: mode
        end subroutine abst_apply
    end interface

    type, extends(abst_bc) :: type_bc_thermal_dirichlet
        real(real64), allocatable :: time_points(:)
        real(real64), allocatable :: values(:)
    contains
        procedure :: apply => apply_thermal_dirichlet
        ! procedure, pass(self) :: initialize  => initialize_type_bc_thermal_dirichlet !&
        ! procedure, pass(self) :: apply_dense => apply_dense_thermal_dirichlet !&
        ! procedure, pass(self) :: apply_crs   => apply_crs_thermal_dirichlet !&
    end type type_bc_thermal_dirichlet

    interface
        module function construct_type_bc_thermal_dirichlet(input, domain, controls, id) result(structure)
            implicit none
            type(type_input), intent(in) :: input
            type(type_domain), intent(in) :: domain
            type(type_controls), intent(in) :: controls
            integer(int32), intent(in) :: id
            class(abst_bc), allocatable :: structure
        end function construct_type_bc_thermal_dirichlet

        ! module subroutine initialize_type_bc_thermal_dirichlet(self, input, domain, id, i_material, time_conv)
        !     implicit none
        !     class(type_bc_thermal_dirichlet), intent(inout) :: self
        !     type(type_input), intent(in) :: input
        !     type(type_domain), intent(inout) :: domain
        !     integer(int32), intent(in) :: id
        !     integer(int32), intent(in) :: i_material
        !     real(real64), intent(in) :: time_conv

        ! end subroutine initialize_type_bc_thermal_dirichlet

        ! module subroutine apply_dense_thermal_dirichlet(self, current_time, A, b, domain, mode)
        !     implicit none
        !     class(type_bc_thermal_dirichlet), intent(in) :: self
        !     real(real64), intent(in) :: current_time
        !     real(real64), intent(inout), optional :: A(:, :)
        !     real(real64), intent(inout) :: b(:)
        !     type(type_domain), intent(inout) :: domain
        !     integer(int32), intent(in), optional :: mode

        ! end subroutine apply_dense_thermal_dirichlet

        module subroutine apply_thermal_dirichlet(self, current_time, A, b, domain, mode)
            implicit none
            class(type_bc_thermal_dirichlet), intent(in) :: self
            real(real64), intent(in) :: current_time
            type(type_jacobian_matrix), intent(inout), optional :: A
            type(type_residual_vector), intent(inout) :: b
            type(type_domain), intent(inout), target :: domain
            integer(int32), intent(in), optional :: mode

        end subroutine apply_thermal_dirichlet

        ! module subroutine apply_crs_thermal_dirichlet(self, current_time, A, b, domain, mode)
        !     implicit none
        !     class(type_bc_thermal_dirichlet), intent(in) :: self
        !     real(real64), intent(in) :: current_time
        !     type(type_crs), intent(inout), optional :: A
        !     real(real64), intent(inout) :: b(:)
        !     type(type_domain), intent(inout) :: domain
        !     integer(int32), intent(in), optional :: mode

        ! end subroutine apply_crs_thermal_dirichlet
    end interface

    interface type_bc_thermal_dirichlet
        module procedure :: construct_type_bc_thermal_dirichlet
    end interface

    type, extends(abst_bc) :: type_bc_thermal_adiabatic
    contains
        procedure :: apply => apply_thermal_adiabatic
        ! procedure, pass(self) :: initialize  => initialize_type_bc_thermal_adiabatic !&
        ! procedure, pass(self) :: apply_dense => apply_dense_thermal_adiabatic !&
        ! procedure, pass(self) :: apply_crs   => apply_crs_thermal_adiabatic !&
    end type type_bc_thermal_adiabatic

    interface
        module function construct_type_bc_thermal_adiabatic(input, domain, controls, id) result(structure)
            implicit none
            type(type_input), intent(in) :: input
            type(type_domain), intent(in) :: domain
            type(type_controls), intent(in) :: controls
            integer(int32), intent(in) :: id
            class(abst_bc), allocatable :: structure
        end function construct_type_bc_thermal_adiabatic

        module subroutine apply_thermal_adiabatic(self, current_time, A, b, domain, mode)
            implicit none
            class(type_bc_thermal_adiabatic), intent(in) :: self
            real(real64), intent(in) :: current_time
            type(type_jacobian_matrix), intent(inout), optional :: A
            type(type_residual_vector), intent(inout) :: b
            type(type_domain), intent(inout), target :: domain
            integer(int32), intent(in), optional :: mode

        end subroutine apply_thermal_adiabatic

        ! module subroutine initialize_type_bc_thermal_adiabatic(self, input, domain, id, i_material, time_conv)
        !     implicit none
        !     class(type_bc_thermal_adiabatic), intent(inout) :: self
        !     type(type_input), intent(in) :: input
        !     type(type_domain), intent(inout) :: domain
        !     integer(int32), intent(in) :: id
        !     integer(int32), intent(in) :: i_material
        !     real(real64), intent(in) :: time_conv

        ! end subroutine initialize_type_bc_thermal_adiabatic

        ! module subroutine apply_dense_thermal_adiabatic(self, current_time, A, b, domain, mode)
        !     implicit none
        !     class(type_bc_thermal_adiabatic), intent(in) :: self
        !     real(real64), intent(in) :: current_time
        !     real(real64), intent(inout), optional :: A(:, :)
        !     real(real64), intent(inout) :: b(:)
        !     type(type_domain), intent(inout) :: domain
        !     integer(int32), intent(in), optional :: mode

        ! end subroutine apply_dense_thermal_adiabatic

        ! module subroutine apply_crs_thermal_adiabatic(self, current_time, A, b, domain, mode)
        !     implicit none
        !     class(type_bc_thermal_adiabatic), intent(in) :: self
        !     real(real64), intent(in) :: current_time
        !     type(type_crs), intent(inout), optional :: A
        !     real(real64), intent(inout) :: b(:)
        !     type(type_domain), intent(inout) :: domain
        !     integer(int32), intent(in), optional :: mode

        ! end subroutine apply_crs_thermal_adiabatic
    end interface

    interface type_bc_thermal_adiabatic
        module procedure :: construct_type_bc_thermal_adiabatic
    end interface

    interface
        module subroutine calculate_time_coefficient(time, arr_time, time_coefficient, idx)
            implicit none
            real(real64), intent(in) :: time
            real(real64), intent(in) :: arr_time(:)
            real(real64), intent(inout) :: time_coefficient
            integer(int32), intent(inout) :: idx

        end subroutine calculate_time_coefficient

        module subroutine find_target_by_group(domain, dimension, group_id, target_ids)
            implicit none
            type(type_domain), intent(in) :: domain
            integer(int32), intent(in) :: dimension
            integer(int32), intent(in) :: group_id
            integer(int32), allocatable, intent(inout) :: target_ids(:)

        end subroutine find_target_by_group
    end interface

end module conditions_boundary
