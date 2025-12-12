submodule(physics_models_wrf) calculate_wrf_base
    implicit none
contains
    module subroutine reset_params_wrf(self)
        implicit none
        class(type_params_wrf), intent(inout) :: self

        self%model_number = 0
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

    end subroutine reset_params_wrf

    module subroutine copy_params_wrf(self, source)
        implicit none
        class(type_params_wrf), intent(inout) :: self
        type(type_params_wrf), intent(in) :: source

        self%model_number = source%model_number
        self%theta_s = source%theta_s
        self%theta_r = source%theta_r
        self%alpha1 = source%alpha1
        self%n1 = source%n1
        self%m1 = source%m1
        self%h_crit = source%h_crit
        self%alpha2 = source%alpha2
        self%n2 = source%n2
        self%m2 = source%m2
        self%w1 = source%w1
        self%w2 = source%w2

    end subroutine copy_params_wrf

    module subroutine initialize_holder_wrfs(self, material_id, params)
        implicit none
        class(holder_wrfs), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_params_wrf), intent(in) :: params

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
        type(type_params_wrf), intent(in) :: params

        call self%params%copy(params)

    end subroutine initialize_abst_wrf

end submodule calculate_wrf_Base
