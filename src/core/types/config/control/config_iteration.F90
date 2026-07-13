module types_config_control_iteration
    use, intrinsic :: iso_fortran_env
    use :: core_constants, only:type_constant_id, PHYSICS_TYPES
    use :: core_types_config_base, only:abst_config
    implicit none
    private

    public :: type_config_iteration
    public :: type_config_iteration_nonlinear
    public :: type_config_iteration_criterion

    type, extends(abst_config) :: type_config_iteration_criterion
        type(type_constant_id) :: criterion = type_constant_id("", "", -1)
        real(real64) :: absolute_tolerance = 0.0d0
        real(real64) :: relative_tolerance = 0.0d0
    contains
        procedure, public, pass(self) :: copy => copy_config_criterion
        procedure, public, pass(self) :: reset => reset_config_criterion
    end type type_config_iteration_criterion

    type, extends(abst_config) :: type_config_iteration_nonlinear
        integer(int32) :: max_iterations = 0
        integer(int32) :: update_frequency = 0
        type(type_constant_id) :: norm_type = type_constant_id("", "", -1)
        type(type_constant_id) :: combination_logic = type_constant_id("", "", -1)
        type(type_constant_id) :: convergence_norm_type = type_constant_id("", "", -1)

        type(type_config_iteration_criterion) :: residual(PHYSICS_TYPES%NUM_ID)
        type(type_config_iteration_criterion) :: update(PHYSICS_TYPES%NUM_ID)

        !> Conserved-quantity convergence tolerances (used when convergence_norm_type == CONSERVED, PDF 6.2.4)
        real(real64) :: atol_enthalpy = 1.0d2    ! [J/m3]  absolute weight floor for enthalpy
        real(real64) :: atol_density = 1.0d-3    ! [kg/m3] absolute weight floor for effective density
        real(real64) :: rtol_conserved = 1.0d-3  ! [-]     relative weight for both conserved quantities
        real(real64) :: residual_eps = 1.0d-1    ! [-]     per-block residual ratio tolerance eps_R

    contains
        procedure, public, pass(self) :: copy => copy_config_nonlinear
        procedure, public, pass(self) :: reset => reset_config_nonlinear
    end type

    type, extends(abst_config) :: type_config_iteration
        type(type_constant_id) :: nonlinear_solver_type = type_constant_id("", "", -1)
        type(type_config_iteration_nonlinear) :: nonlinear
    contains
        procedure, public, pass(self) :: copy => copy_config_iteration
        procedure, public, pass(self) :: reset => reset_config_iteration
    end type type_config_iteration

contains

    subroutine copy_config_criterion(self, source)
        implicit none
        class(type_config_iteration_criterion), intent(inout) :: self
        class(abst_config), intent(in) :: source

        call self%reset()

        select type (source)
        type is (type_config_iteration_criterion)
            call self%set(self%criterion, source%criterion)
            call self%set(self%absolute_tolerance, source%absolute_tolerance)
            call self%set(self%relative_tolerance, source%relative_tolerance)
        end select

    end subroutine copy_config_criterion

    subroutine reset_config_criterion(self)
        implicit none
        class(type_config_iteration_criterion), intent(inout) :: self

        self%criterion = type_constant_id("", "", -1)
        self%absolute_tolerance = 0.0d0
        self%relative_tolerance = 0.0d0
    end subroutine reset_config_criterion

    subroutine copy_config_nonlinear(self, source)
        implicit none
        class(type_config_iteration_nonlinear), intent(inout) :: self
        class(abst_config), intent(in) :: source
        integer(int32) :: i

        call self%reset()

        select type (source)
        type is (type_config_iteration_nonlinear)
            call self%set(self%max_iterations, source%max_iterations)
            call self%set(self%update_frequency, source%update_frequency)
            call self%set(self%norm_type, source%norm_type)
            call self%set(self%combination_logic, source%combination_logic)
            call self%set(self%convergence_norm_type, source%convergence_norm_type)

            call self%set(self%atol_enthalpy, source%atol_enthalpy)
            call self%set(self%atol_density, source%atol_density)
            call self%set(self%rtol_conserved, source%rtol_conserved)
            call self%set(self%residual_eps, source%residual_eps)

            do i = 1, PHYSICS_TYPES%NUM_ID
                call self%residual(i)%copy(source%residual(i))
                call self%update(i)%copy(source%update(i))
            end do
        end select

    end subroutine copy_config_nonlinear

    subroutine reset_config_nonlinear(self)
        implicit none
        class(type_config_iteration_nonlinear), intent(inout) :: self
        integer(int32) :: i

        self%max_iterations = 0
        self%update_frequency = 0
        self%norm_type = type_constant_id("", "", -1)
        self%combination_logic = type_constant_id("", "", -1)
        self%convergence_norm_type = type_constant_id("", "", -1)

        self%atol_enthalpy = 1.0d2
        self%atol_density = 1.0d-3
        self%rtol_conserved = 1.0d-3
        self%residual_eps = 1.0d-1

        do i = 1, PHYSICS_TYPES%NUM_ID
            call self%residual(i)%reset()
            call self%update(i)%reset()
        end do
    end subroutine reset_config_nonlinear

    subroutine copy_config_iteration(self, source)
        implicit none
        class(type_config_iteration), intent(inout) :: self
        class(abst_config), intent(in) :: source

        call self%reset()

        select type (source)
        type is (type_config_iteration)
            call self%set(self%nonlinear_solver_type, source%nonlinear_solver_type)
            call self%nonlinear%copy(source%nonlinear)
        end select

    end subroutine copy_config_iteration

    subroutine reset_config_iteration(self)
        implicit none
        class(type_config_iteration), intent(inout) :: self

        self%nonlinear_solver_type = type_constant_id("", "", -1)
        call self%nonlinear%reset()
    end subroutine reset_config_iteration

end module types_config_control_iteration
