submodule(models_wrf) calculate_wrf_base
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
contains
    ! module subroutine reset_config_wrf(self)
    !     implicit none
    !     class(type_config_wrf), intent(inout) :: self

    !     self%unit_id = 0
    !     self%model_number = 0
    !     self%theta_s = 0.0d0
    !     self%theta_r = 0.0d0
    !     self%alpha1 = 0.0d0
    !     self%n1 = 0.0d0
    !     self%m1 = 0.0d0
    !     self%h_crit = 0.0d0
    !     self%alpha2 = 0.0d0
    !     self%n2 = 0.0d0
    !     self%m2 = 0.0d0
    !     self%w1 = 0.0d0
    !     self%w2 = 0.0d0

    ! end subroutine reset_config_wrf

    ! module subroutine copy_config_wrf(self, source)
    !     implicit none
    !     class(type_config_wrf), intent(inout) :: self
    !     type(type_config_wrf), intent(in) :: source

    !     self%unit_id = source%unit_id
    !     self%model_number = source%model_number
    !     self%theta_s = source%theta_s
    !     self%theta_r = source%theta_r
    !     self%alpha1 = source%alpha1
    !     self%n1 = source%n1
    !     self%m1 = source%m1
    !     self%h_crit = source%h_crit
    !     self%alpha2 = source%alpha2
    !     self%n2 = source%n2
    !     self%m2 = source%m2
    !     self%w1 = source%w1
    !     self%w2 = source%w2

    ! end subroutine copy_config_wrf

    ! module subroutine convert_config_wrf(self, unit_id, factor)
    !     implicit none
    !     class(type_config_wrf), intent(inout) :: self
    !     integer(int32), intent(in) :: unit_id
    !     real(real64), intent(in), optional :: factor

    !     real(real64) :: pg_val
    !     real(real64) :: scale_pres

    !     ! --- Set specific weight (rho*g) ---
    !     if (present(factor)) then
    !         pg_val = factor
    !     else
    !         pg_val = rho_std * g
    !     end if

    !     ! --- Determine conversion factor ---
    !     select case (unit_id)
    !     case (constitutive_UNIT_M)
    !         ! m -> Pa
    !         scale_pres = pg_val
    !     case (constitutive_UNIT_CM)
    !         ! cm -> m -> Pa
    !         scale_pres = pg_val * 1.0d-2
    !     case (constitutive_UNIT_PA)
    !         ! Pa -> Pa (factor is 1.0)
    !         scale_pres = 1.0d0
    !     case default
    !         scale_pres = 1.0d0
    !     end select

    !     ! --- Model-specific parameter conversion ---
    !     select case (self%model_number)
    !     case (WRF_BC, WRF_KO)
    !         self%alpha1 = self%alpha1 * scale_pres
    !         self%h_crit = self%h_crit * scale_pres
    !         self%alpha2 = self%alpha2 * scale_pres

    !     case (WRF_VG, WRF_MVG, WRF_DURNER, WRF_DVGCH)
    !         self%alpha1 = self%alpha1 / scale_pres
    !         self%alpha2 = self%alpha2 / scale_pres
    !         self%h_crit = self%h_crit * scale_pres
    !     case default
    !         self%alpha1 = self%alpha1 / scale_pres
    !         self%h_crit = self%h_crit * scale_pres
    !     end select

    ! end subroutine convert_config_wrf

    module subroutine initialize_holder_wrfs(self, config)
        implicit none
        class(holder_wrfs), intent(inout) :: self
        type(type_config_wrf), intent(in) :: config

        if (allocated(self%p)) then
            deallocate (self%p)
        end if

        if (config%swcc_model == SWCC_MODELS%BC) then
            allocate (type_wrf_bc :: self%p)
        else if (config%swcc_model == SWCC_MODELS%VG) then
            allocate (type_wrf_vg :: self%p)
        else if (config%swcc_model == SWCC_MODELS%KO) then
            allocate (type_wrf_ko :: self%p)
        else if (config%swcc_model == SWCC_MODELS%MVG) then
            allocate (type_wrf_mvg :: self%p)
        else if (config%swcc_model == SWCC_MODELS%DURNER) then
            allocate (type_wrf_durner :: self%p)
        else if (config%swcc_model == SWCC_MODELS%DVGCH) then
            allocate (type_wrf_dvgch :: self%p)
        else
            write (*, *) 'Error: Unknown WRF model ', config%swcc_model
            stop
        end if
        call self%p%initialize(config)
    end subroutine initialize_holder_wrfs

    module subroutine initialize_abst_wrf(self, config)
        implicit none
        class(abst_wrf), intent(inout) :: self
        type(type_config_wrf), intent(in) :: config

        call self%config%copy(config)
        self%initialized = .true.
        call self%update_pressure_capacity_bound()
    end subroutine initialize_abst_wrf

    module pure function is_initialized_wrf(self) result(initialized)
        implicit none
        class(abst_wrf), intent(in) :: self
        logical :: initialized

        initialized = self%initialized

    end function is_initialized_wrf

    module subroutine calc_lscheme_capacity_wrf(self, capacity)
        implicit none
        class(abst_wrf), intent(in) :: self
        real(real64), intent(inout) :: capacity

        capacity = self%pressure_capacity_bound
    end subroutine calc_lscheme_capacity_wrf

    !> Precompute the pressure-capacity upper bound once per material initialization.
    module subroutine update_pressure_capacity_bound(self)
        implicit none
        class(abst_wrf), intent(inout) :: self

        integer(int32), parameter :: num_scan = 96
        real(real64) :: h_abs_min, h_abs_max, h_abs, fraction
        real(real64) :: max_dtheta_dh
        integer(int32) :: i

        self%pressure_capacity_bound = 0.0d0
        if (.not. self%initialized) return

        h_abs_min = huge(1.0d0)
        h_abs_max = 0.0d0
        max_dtheta_dh = 0.0d0

        call include_scale(abs(self%config%h_crit))
        if (self%config%alpha1 > 0.0d0) then
            call include_scale(1.0d0 / self%config%alpha1)
        else
            call include_scale(abs(self%config%alpha1))
        end if
        if (self%config%alpha2 > 0.0d0) then
            call include_scale(1.0d0 / self%config%alpha2)
        else
            call include_scale(abs(self%config%alpha2))
        end if

        if (h_abs_max <= 0.0d0) return

        ! Evaluate the WRF capacity over the material's own characteristic suction
        ! heads. This gives the L-scheme a material-derived diagonal scale instead
        ! of a case-specific numerical damping factor.
        h_abs_min = max(h_abs_min * 1.0d-4, sqrt(tiny(1.0d0)))
        h_abs_max = max(h_abs_max * 1.0d4, 10.0d0 * h_abs_min)
        do i = 0, num_scan
            fraction = real(i, real64) / real(num_scan, real64)
            h_abs = exp(log(h_abs_min) + fraction * (log(h_abs_max) - log(h_abs_min)))
            call sample_head(h_abs)
        end do

        call sample_vg_peak(self%config%alpha1, self%config%n1, self%config%m1)
        call sample_vg_peak(self%config%alpha2, self%config%n2, self%config%m2)
        if (self%config%swcc_model == SWCC_MODELS%DVGCH) then
            call sample_vg_peak(self%config%alpha1, self%config%n2, self%config%m2)
        end if
        if (self%config%h_crit < 0.0d0) call sample_head(abs(self%config%h_crit) * (1.0d0 + 1.0d-8))
        if (self%config%alpha1 < 0.0d0) call sample_head(abs(self%config%alpha1) * (1.0d0 + 1.0d-8))

        self%pressure_capacity_bound = max_dtheta_dh / (rho_std * g)

    contains
        subroutine include_scale(scale)
            implicit none
            real(real64), intent(in) :: scale

            if (scale <= 0.0d0) return
            if (.not. ieee_is_finite(scale)) return
            h_abs_min = min(h_abs_min, scale)
            h_abs_max = max(h_abs_max, scale)
        end subroutine include_scale

        subroutine sample_head(h_abs_value)
            implicit none
            real(real64), intent(in) :: h_abs_value
            real(real64) :: dtheta_dh

            if (h_abs_value <= 0.0d0) return
            if (.not. ieee_is_finite(h_abs_value)) return

            dtheta_dh = 0.0d0
            call self%deriv(-h_abs_value, dtheta_dh)
            if (ieee_is_finite(dtheta_dh)) max_dtheta_dh = max(max_dtheta_dh, dtheta_dh)
        end subroutine sample_head

        subroutine sample_vg_peak(alpha, n, m)
            implicit none
            real(real64), intent(in) :: alpha, n, m
            real(real64) :: x_peak

            if (alpha <= 0.0d0 .or. n <= 1.0d0 .or. m <= 0.0d0) return
            x_peak = ((n - 1.0d0) / (m * n + 1.0d0))**(1.0d0 / n)
            call sample_head(x_peak / alpha)
        end subroutine sample_vg_peak
    end subroutine update_pressure_capacity_bound

end submodule calculate_wrf_Base
