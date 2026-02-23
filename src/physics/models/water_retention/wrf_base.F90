submodule(physics_models_wrf) calculate_wrf_base
    implicit none
contains
    ! module subroutine reset_params_wrf(self)
    !     implicit none
    !     class(type_wrf_params), intent(inout) :: self

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

    ! end subroutine reset_params_wrf

    ! module subroutine copy_params_wrf(self, source)
    !     implicit none
    !     class(type_wrf_params), intent(inout) :: self
    !     type(type_wrf_params), intent(in) :: source

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

    ! end subroutine copy_params_wrf

    ! module subroutine convert_params_wrf(self, unit_id, factor)
    !     implicit none
    !     class(type_wrf_params), intent(inout) :: self
    !     integer(int32), intent(in) :: unit_id
    !     real(real64), intent(in), optional :: factor

    !     real(real64) :: pg_val
    !     real(real64) :: scale_pres

    !     ! --- 比重量 (rho*g) の設定 ---
    !     if (present(factor)) then
    !         pg_val = factor
    !     else
    !         pg_val = rho_std * g
    !     end if

    !     ! --- 変換係数の決定 ---
    !     select case (unit_id)
    !     case (PHYSICS_UNIT_M)
    !         ! m -> Pa
    !         scale_pres = pg_val
    !     case (PHYSICS_UNIT_CM)
    !         ! cm -> m -> Pa
    !         scale_pres = pg_val * 1.0d-2
    !     case (PHYSICS_UNIT_PA)
    !         ! Pa -> Pa (係数は1.0)
    !         scale_pres = 1.0d0
    !     case default
    !         scale_pres = 1.0d0
    !     end select

    !     ! --- モデルごとのパラメータ変換 ---
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

    ! end subroutine convert_params_wrf

    module subroutine initialize_holder_wrfs(self, material_id, params)
        implicit none
        class(holder_wrfs), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_wrf_params), intent(in) :: params

        if (allocated(self%p)) then
            deallocate (self%p)
        end if

        select case (params%model_number)
        case (WRF_BC)
            allocate (type_wrf_bc :: self%p)
        case (WRF_VG)
            allocate (type_wrf_vg :: self%p)
        case (WRF_KO)
            allocate (type_wrf_ko :: self%p)
        case (WRF_MVG)
            allocate (type_wrf_mvg :: self%p)
        case (WRF_DURNER)
            allocate (type_wrf_durner :: self%p)
        case (WRF_DVGCH)
            allocate (type_wrf_dvgch :: self%p)
        case default
            write (*, *) 'Error: Unknown WRF model number ', params%model_number
            stop
        end select
        call self%p%initialize(params)
    end subroutine initialize_holder_wrfs

    module subroutine initialize_abst_wrf(self, params)
        implicit none
        class(abst_wrf), intent(inout) :: self
        type(type_wrf_params), intent(in) :: params

        call self%params%copy(params)
        call self%params%convert(params%unit_id)

        self%initialized = .true.
    end subroutine initialize_abst_wrf

    module pure function is_initialized_wrf(self) result(initialized)
        implicit none
        class(abst_wrf), intent(in) :: self
        logical :: initialized

        initialized = self%initialized

    end function is_initialized_wrf

end submodule calculate_wrf_Base
