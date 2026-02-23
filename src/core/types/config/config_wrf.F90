module core_types_physics_config_wrf
    use, intrinsic :: iso_fortran_env
    use :: core_memory, only:allocate_array, deallocate_array
    use :: core_constants, only:type_constant_id
    use :: core_types_physics_config_base, only:abst_config
    implicit none
    private

    public :: type_config_wrf

    !> Structure to hold common parameters for all WRF models.
    type, extends(abst_config) :: type_config_wrf
        ! !> Unit identification code
        ! integer(int32) :: unit_id
        !> Model identification number
        type(type_constant_id) :: model = type_constant_id("none", "none", -1)
        !> Residual water content, $\theta_\mathrm{r}$ [-]
        real(real64) :: theta_r = 0.0d0
        !> Saturated water content, $\theta_\mathrm{s}$ [-]
        real(real64) :: theta_s = 0.0d0
        !> Inverse of the air-entry value or scaling parameter, $\alpha_1$ [1/l]
        real(real64) :: alpha1 = 0.0d0
        !> Pore-size distribution index, $n_1$ [-]
        real(real64) :: n1 = 0.0d0
        !> Asymmetry parameter, $m_1$ [-]
        real(real64) :: m1 = 0.0d0
        !> Critical pressure head for modified models, $h_\mathrm{crit}$ [l]
        real(real64) :: h_crit = 0.0d0
        !> Scaling parameter for secondary porosity, $\alpha_2$ [1/l]
        real(real64) :: alpha2 = 0.0d0
        !> Pore-size distribution index for secondary porosity, $n_2$ [-]
        real(real64) :: n2 = 0.0d0
        !> Asymmetry parameter for secondary porosity, $m_2$ [-]
        real(real64) :: m2 = 0.0d0
        !> Weighting factor for primary porosity, $w_1$ [-]
        real(real64) :: w1 = 0.0d0
        !> Weighting factor for secondary porosity, $w_2$ [-]
        real(real64) :: w2 = 0.0d0
    contains
        procedure, pass(self), public :: copy => copy_config_wrf
        procedure, pass(self), public :: reset => reset_config_wrf
    end type type_config_wrf

contains
    subroutine copy_config_wrf(self, source)
        implicit none
        class(type_config_wrf), intent(inout) :: self
        class(abst_config), intent(in) :: source

        select type (source)
        type is (type_config_wrf)
            call self%set(self%model, source%model)
            call self%set(self%theta_s, source%theta_s)
            call self%set(self%theta_r, source%theta_r)
            call self%set(self%alpha1, source%alpha1)
            call self%set(self%n1, source%n1)
            call self%set(self%m1, source%m1)
            call self%set(self%h_crit, source%h_crit)
            call self%set(self%alpha2, source%alpha2)
            call self%set(self%n2, source%n2)
            call self%set(self%m2, source%m2)
            call self%set(self%w1, source%w1)
            call self%set(self%w2, source%w2)
        class default
            call self%reset()
        end select
    end subroutine copy_config_wrf

    subroutine reset_config_wrf(self)
        implicit none
        class(type_config_wrf), intent(inout) :: self

        self%model = type_constant_id("none", "none", -1)
        self%theta_s = 0.0d0
        self%theta_r = 0.0d0
        self%alpha1 = 0.0d0
        self%n1 = 0.0d0
        self%m1 = 0.0d0
        self%h_crit = 0.0d0
        self%alpha2 = 0.0d0
        self%n2 = 0.0d0
        self%m2 = 0.0d0
        self%w1 = 0.0d0
        self%w2 = 0.0d0

    end subroutine reset_config_wrf

end module core_types_physics_config_wrf
