!> Module containing Soil Water Retention Function (WRF) models.
!> These models describe the relationship between soil water potential (pressure head) and volumetric water content.
!>
!> @note
!> In this project,
!> - Setting model parameters is called initialization (corresponding procedure: [[initialize(procedure)]])
!> @endnote
module physics_models_wrf
    use, intrinsic :: iso_fortran_env
    use :: module_core, only:type_config_wrf
    ! use :: module_core, only:WRF_BC, WRF_VG, WRF_KO, WRF_MVG, WRF_DURNER, WRF_DVGCH, &
    !     PHYSICS_UNIT_M, PHYSICS_UNIT_CM, PHYSICS_UNIT_PA
    use :: physics_constants, only:pi => circle_ratio, g => gravity_acceleration, rho_std => reference_water_density
    implicit none
    private

    !-------------------------------------
    ! Public Types and Interfaces
    !-------------------------------------
    public :: holder_wrfs
    public :: abst_wrf
    public :: type_wrf_bc
    public :: type_wrf_vg
    public :: type_wrf_ko
    public :: type_wrf_mvg
    public :: type_wrf_durner
    public :: type_wrf_dvgch
    ! public :: type_config_wrf
    !-------------------------------------

    ! !> Structure to hold common parameters for all WRF models.
    ! type :: type_config_wrf
    !     !> Unit identification code
    !     integer(int32) :: unit_id
    !     !> Model identification number
    !     integer(int32) :: model_number
    !     !> Residual water content, \(\theta_\mathrm{r}\) [-]
    !     real(real64) :: theta_r
    !     !> Saturated water content, \(\theta_\mathrm{s}\) [-]
    !     real(real64) :: theta_s
    !     !> Inverse of the air-entry value or scaling parameter, \(\alpha_1\) [1/m]
    !     real(real64) :: alpha1
    !     !> Pore-size distribution index, \(n_1\) [-]
    !     real(real64) :: n1
    !     !> Asymmetry parameter, \(m_1\) [-]
    !     real(real64) :: m1
    !     !> Critical pressure head for modified models, \(h_\mathrm{crit}\) [m]
    !     real(real64) :: h_crit
    !     !> Scaling parameter for secondary porosity, \(\alpha_2\) [1/m]
    !     real(real64) :: alpha2
    !     !> Pore-size distribution index for secondary porosity, \(n_2\) [-]
    !     real(real64) :: n2
    !     !> Asymmetry parameter for secondary porosity, \(m_2\) [-]
    !     real(real64) :: m2
    !     !> Weighting factor for primary porosity, \(w_1\) [-]
    !     real(real64) :: w1
    !     !> Weighting factor for secondary porosity, \(w_2\) [-]
    !     real(real64) :: w2
    ! contains
    !     procedure, pass(self), public :: reset => reset_config_wrf
    !     procedure, pass(self), public :: copy => copy_config_wrf
    !     procedure, pass(self), public :: convert => convert_config_wrf
    ! end type type_config_wrf

    ! interface
    !     !> Reset parameters to default zero values.
    !     module subroutine reset_config_wrf(self)
    !         implicit none
    !         class(type_config_wrf), intent(inout) :: self
    !     end subroutine reset_config_wrf

    !     !> Copy parameters from a source instance.
    !     module subroutine copy_config_wrf(self, source)
    !         implicit none
    !         class(type_config_wrf), intent(inout) :: self
    !         type(type_config_wrf), intent(in) :: source
    !     end subroutine copy_config_wrf

    !     !> Convert parameter units.
    !     module subroutine convert_config_wrf(self, unit_id, factor)
    !         implicit none
    !         class(type_config_wrf), intent(inout) :: self
    !         integer(int32), intent(in) :: unit_id
    !         real(real64), intent(in), optional :: factor
    !     end subroutine convert_config_wrf
    ! end interface

    !> Polymorphic wrapper/holder for WRF objects.
    type :: holder_wrfs
        class(abst_wrf), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_wrfs
    end type holder_wrfs

    interface
        !> Initialize the polymorphic holder with a specific material and its parameters.
        module subroutine initialize_holder_wrfs(self, material_id, config)
            implicit none
            class(holder_wrfs), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_config_wrf), intent(in) :: config
        end subroutine initialize_holder_wrfs
    end interface

    !> Abstract base class for all Water Retention Function models.
    type, abstract :: abst_wrf
        logical :: initialized = .false.
        type(type_config_wrf) :: config
    contains
        procedure, pass(self), public :: initialize => initialize_abst_wrf
        procedure(abst_calc_wrf), pass(self), public, deferred :: calc
        procedure(abst_calc_wrf_derivative), pass(self), public, deferred :: deriv
        procedure, pass(self), public :: is_initialized => is_initialized_wrf
    end type abst_wrf

    abstract interface
        !> Calculate water content based on pressure head.
        subroutine abst_calc_wrf(self, h, Qw)
            import :: abst_wrf, real64
            implicit none
            class(abst_wrf), intent(in) :: self
            !> Pressure head, \(h\) [m]
            real(real64), intent(in) :: h
            !> Water content, \(\theta_\mathrm{w}\) [-]
            real(real64), intent(inout) :: Qw
        end subroutine abst_calc_wrf

        !> Calculate derivative of water content with respect to pressure head.
        subroutine abst_calc_wrf_derivative(self, h, dQw_dh)
            import :: abst_wrf, real64
            implicit none
            class(abst_wrf), intent(in) :: self
            !> Pressure head, \(h\) [m]
            real(real64), intent(in) :: h
            !> Water capacity, \(C(h) = \mathrm{d}\theta_\mathrm{w}/\mathrm{d}h\) [1/m]
            real(real64), intent(inout) :: dQw_dh
        end subroutine abst_calc_wrf_derivative
    end interface

    interface
        module subroutine initialize_abst_wrf(self, config)
            implicit none
            class(abst_wrf), intent(inout) :: self
            type(type_config_wrf), intent(in) :: config
        end subroutine initialize_abst_wrf

        module pure function is_initialized_wrf(self) result(initialized)
            implicit none
            class(abst_wrf), intent(in) :: self
            logical :: initialized
        end function is_initialized_wrf
    end interface

    !> Brooks-Corey model implementation.
    type, extends(abst_wrf) :: type_wrf_bc
    contains
        procedure, pass(self) :: calc => calculate_wrf_bc
        procedure, pass(self) :: deriv => calculate_wrf_bc_derivative
    end type type_wrf_bc

    !> van Genuchten model implementation.
    type, extends(abst_wrf) :: type_wrf_vg
    contains
        procedure, pass(self) :: calc => calculate_wrf_vg
        procedure, pass(self) :: deriv => calculate_wrf_vg_derivative
    end type type_wrf_vg

    !> Kosugi log-normal model implementation.
    type, extends(abst_wrf) :: type_wrf_ko
    contains
        procedure, pass(self) :: calc => calculate_wrf_ko
        procedure, pass(self) :: deriv => calculate_wrf_ko_derivative
    end type type_wrf_ko

    !> Modified van Genuchten model (Vogel et al., 2001) implementation.
    type, extends(abst_wrf) :: type_wrf_mvg
    contains
        procedure, pass(self) :: calc => calculate_wrf_mvg
        procedure, pass(self) :: deriv => calculate_wrf_mvg_derivative
    end type type_wrf_mvg

    !> Durner dual-porosity model implementation.
    type, extends(abst_wrf) :: type_wrf_durner
    contains
        procedure, pass(self) :: calc => calculate_wrf_durner
        procedure, pass(self) :: deriv => calculate_wrf_durner_derivative
    end type type_wrf_durner

    !> Dual van Genuchten with Common Head model (Seki, 2023) implementation.
    type, extends(abst_wrf) :: type_wrf_dvgch
    contains
        procedure, pass(self) :: calc => calculate_wrf_dvgch
        procedure, pass(self) :: deriv => calculate_wrf_dvgch_derivative
    end type type_wrf_dvgch

    interface
        !> Calculate water content using the Brooks-Corey model.
        !>
        !> \[
        !> \theta_\mathrm{w}(h) = \theta_\mathrm{r} + (\theta_\mathrm{s} - \theta_\mathrm{r}) (\alpha |h|)^{-n}
        !> \]
        !>
        !> \(\theta_\mathrm{w}\) : water content [-]
        !> \(\theta_\mathrm{r}\) : residual water content [-]
        !> \(\theta_\mathrm{s}\) : saturated water content [-]
        !> \(\alpha\) : scaling parameter [1/m]
        !> \(h\) : pressure head [m]
        !> \(n\) : pore-size distribution index [-]
        !>
        !> @note
        !> - Model assumes saturation for \(h \geq -1/\alpha\)
        !> - Physically valid for \(\alpha > 0\) and \(n > 1\)
        !> @endnote
        module subroutine calculate_wrf_bc(self, h, Qw)
            implicit none
            class(type_wrf_bc), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: Qw
        end subroutine calculate_wrf_bc

        !> Calculate Water capacity for the Brooks-Corey model.
        !>
        !> \[
        !> \frac{\mathrm{d}\theta_\mathrm{w}}{\mathrm{d}h} = (\theta_\mathrm{s} - \theta_\mathrm{r}) \alpha n (\alpha |h|)^{-n-1}
        !> \]
        !>
        !> @note
        !> - Use same valid ranges as water content function
        !> - Physically valid for \(\alpha > 0\) and \(n > 1\)
        !> @endnote
        module subroutine calculate_wrf_bc_derivative(self, h, dQw_dh)
            implicit none
            class(type_wrf_bc), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: dQw_dh
        end subroutine calculate_wrf_bc_derivative

        !> Calculate water content using the van Genuchten model.
        !>
        !> \[
        !> \theta_\mathrm{w}(h) = \theta_\mathrm{r} + \frac{\theta_\mathrm{s} - \theta_\mathrm{r}}{[1 + (\alpha |h|)^n]^m}
        !> \]
        !>
        !> \(\theta_\mathrm{w}\) : water content [-]
        !> \(\theta_\mathrm{r}\) : residual water content [-]
        !> \(\theta_\mathrm{s}\) : saturated water content [-]
        !> \(\alpha\) : scaling parameter [1/m]
        !> \(h\) : pressure head [m]
        !> \(n\) : pore-size distribution index [-]
        !> \(m\) : asymmetry parameter [-]
        !>
        !> @note
        !> - Common assumption: \(m = 1 - 1/n\) for \(n > 1\)
        !> - Physically valid for \(\alpha>0\) and \(n>1\)
        !> @endnote
        module subroutine calculate_wrf_vg(self, h, Qw)
            implicit none
            class(type_wrf_vg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: Qw
        end subroutine calculate_wrf_vg

        !> Calculate derivative for van Genuchten model.
        !>
        !> \[
        !> \frac{\mathrm{d}\theta_\mathrm{w}}{\mathrm{d}h} = -\frac{(\theta_\mathrm{s} - \theta_\mathrm{r}) \alpha n m (\alpha |h|)^{n-1}}{[1 + (\alpha |h|)^n]^{m+1}}
        !> \]
        !>
        !> @note
        !> - Use same valid ranges as water content function
        !> - Physically valid for \(\alpha>0\), \(n>1\)
        !> @endnote
        module subroutine calculate_wrf_vg_derivative(self, h, dQw_dh)
            implicit none
            class(type_wrf_vg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: dQw_dh
        end subroutine calculate_wrf_vg_derivative

        !> Calculate water content using the Kosugi log-normal model.
        !>
        !> \[
        !> \theta_\mathrm{w}(h) = \theta_\mathrm{r} + (\theta_\mathrm{s} - \theta_\mathrm{r}) \Phi \left( \frac{\ln(-h/\alpha)}{\sigma} \right)
        !> \]
        !>
        !> \(\Phi\) : standard normal cumulative distribution function
        !> \(\theta_\mathrm{r}\) : residual water content [-]
        !> \(\theta_\mathrm{s}\) : saturated water content [-]
        !> \(\alpha\) : scaling parameter [m]
        !> \(h\) : pressure head [m]
        !> \(\sigma\) : log-pore radius standard deviation [-]
        !>
        !> @note
        !> - Requires \(h < 0\) for logarithmic operation
        !> - Physically valid for \(\sigma>0\)
        !> @endnote
        module subroutine calculate_wrf_ko(self, h, Qw)
            implicit none
            class(type_wrf_ko), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: Qw
        end subroutine calculate_wrf_ko

        !> Calculate derivative for Kosugi model.
        !>
        !> \[
        !> \frac{\mathrm{d}\theta_\mathrm{w}}{\mathrm{d}h} = -\frac{(\theta_\mathrm{s} - \theta_\mathrm{r})}{h \sigma \sqrt{2\pi}} \exp \left[ -\frac{1}{2} \left( \frac{\ln(-h/\alpha)}{\sigma} \right)^2 \right]
        !> \]
        !>
        !> @note
        !> - Requires \(h < 0\)
        !> - Physically valid for \(\sigma>0\)
        !> @endnote
        module subroutine calculate_wrf_ko_derivative(self, h, dQw_dh)
            implicit none
            class(type_wrf_ko), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: dQw_dh
        end subroutine calculate_wrf_ko_derivative

        !> Calculate water content using the Modified van Genuchten model (Vogel et al., 2001)
        !>
        !> \[
        !> \theta_\mathrm{w}(h) = \theta_\mathrm{r} + (\theta_\mathrm{s} - \theta_\mathrm{r}) [1 + (\alpha |h|)^n]^{-m}
        !> \]
        !>
        !> @note
        !> - Uses \(h_\mathrm{crit}\) to prevent air-entry problems near saturation
        !> - Physically valid for \(\alpha>0\), \(n>0\)
        !> @endnote
        module subroutine calculate_wrf_mvg(self, h, Qw)
            implicit none
            class(type_wrf_mvg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: Qw
        end subroutine calculate_wrf_mvg

        !> Derivative for Modified van Genuchten model
        !>
        !> \[
        !> \frac{\mathrm{d}\theta_\mathrm{w}}{\mathrm{d}h} = -\frac{(\theta_\mathrm{s} - \theta_\mathrm{r}) \alpha n m (\alpha |h|)^{n-1}}{[1 + (\alpha |h|)^n]^{m+1}}
        !> \]
        !>
        !> @note
        !> - Use same valid ranges as water content function
        !> @endnote
        module subroutine calculate_wrf_mvg_derivative(self, h, dQw_dh)
            implicit none
            class(type_wrf_mvg), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: dQw_dh
        end subroutine calculate_wrf_mvg_derivative

        !> Calculate water content using the Durner dual-porosity model
        !>
        !> \[
        !> \theta_\mathrm{w}(h) = \theta_\mathrm{r} + (\theta_\mathrm{s} - \theta_\mathrm{r}) \sum_{i=1}^{2} w_i [1 + (\alpha_i |h|)^{n_i}]^{-m_i}
        !> \]
        !>
        !> @note
        !> - Requires \(w_1 + w_2 = 1\)
        !> @endnote
        module subroutine calculate_wrf_durner(self, h, Qw)
            implicit none
            class(type_wrf_durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: Qw
        end subroutine calculate_wrf_durner

        !> Derivative for Durner dual-porosity model
        !>
        !> \[
        !> \frac{\mathrm{d}\theta_\mathrm{w}}{\mathrm{d}h} = -(\theta_\mathrm{s} - \theta_\mathrm{r}) \sum_{i=1}^{2} w_i n_i \alpha_i (\alpha_i |h|)^{n_i-1} [1 + (\alpha_i |h|)^{n_i}]^{-m_i-1}
        !> \]
        !>
        !> @note
        !> - Use same valid ranges as water content function
        !> @endnote
        module subroutine calculate_wrf_durner_derivative(self, h, dQw_dh)
            implicit none
            class(type_wrf_durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: dQw_dh
        end subroutine calculate_wrf_durner_derivative

        !> Calculate water content using the Dual van Genuchten with Common Head model (Seki, 2023)
        !>
        !> \[
        !> \theta_\mathrm{w}(h) = \theta_\mathrm{r} + (\theta_\mathrm{s} - \theta_\mathrm{r}) \sum_{i=1}^{2} w_i [1 + (\alpha |h|)^{n_i}]^{-m_i}
        !> \]
        !>
        !> @note
        !> - Assumes a common scaling parameter \(\alpha\) (common head)
        !> @endnote
        module subroutine calculate_wrf_dvgch(self, h, Qw)
            implicit none
            class(type_wrf_dvgch), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: Qw
        end subroutine calculate_wrf_dvgch

        !> Derivative for Dual van Genuchten with Common Head model
        !>
        !> \[
        !> \frac{\mathrm{d}\theta_\mathrm{w}}{\mathrm{d}h} = -(\theta_\mathrm{s} - \theta_\mathrm{r}) \alpha \sum_{i=1}^{2} w_i n_i (\alpha |h|)^{n_i-1} [1 + (\alpha |h|)^{n_i}]^{-m_i-1}
        !> \]
        !>
        !> @note
        !> - Use same valid ranges as water content function
        !> @endnote
        module subroutine calculate_wrf_dvgch_derivative(self, h, dQw_dh)
            implicit none
            class(type_wrf_dvgch), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(inout) :: dQw_dh
        end subroutine calculate_wrf_dvgch_derivative
    end interface

end module physics_models_wrf
