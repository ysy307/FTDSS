module properties_properties_manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_gauss_point_state
    use :: module_input
    use :: module_calculate, only:holder_gccs, holder_wrfs, holder_dens, holder_vhcs, holder_thcs, & !&
                                        abst_gcc, abst_wrf, abst_den, abst_vhc, abst_thc
    use :: Properties_material_Manager, only:type_material_manager

    implicit none
    private
    public :: type_proereties_manager

    type :: type_proereties_manager
        type(type_material_manager) :: materials
        procedure(abst_get_value), pointer, pass(self) :: get_thc => null()
        procedure(abst_get_value), pointer, pass(self) :: get_vhc => null()
        ! procedure(abst_get_value), pointer, pass(self) :: get_dCa_dT => null()
        procedure(abst_get_value), pointer, pass(self) :: get_qw => null()
    contains
        procedure, pass(self) :: initialize => type_proereties_manager_initialize
    end type

    abstract interface
        function abst_get_value(self, state, region_id) result(val)
            import :: type_proereties_manager, type_gauss_point_state, int32, real64
            implicit none
            class(type_proereties_manager), intent(inout) :: self
            type(type_gauss_point_state), intent(in) :: state
            integer(int32), intent(in) :: region_id
            real(real64) :: val
        end function abst_get_value
    end interface

contains

    subroutine type_proereties_manager_initialize(self, Input, ierr)
        implicit none
        class(type_proereties_manager), intent(inout) :: self
        type(Type_Input), intent(in) :: Input
        integer(int32), intent(inout) :: ierr

        logical :: is_ptr = .true.

        ! Initialize the materials manager
        call self%materials%initialize(Input, ierr)

        if (ierr /= 0) then
            print *, "Error initializing materials manager."
            return
        end if

        if (is_ptr) then
            self%get_thc => calculate_thc_ptr
            self%get_vhc => calculate_heat_capacity_ptr
            ! self%get_dCa_dT => calculate_dcdt_ptr
            self%get_qw => calculate_water_content_ptr
        else
            self%get_thc => calculate_thc_holder
            self%get_vhc => calculate_heat_capacity_holder
            ! self%get_dCa_dT => calculate_dcdt_holder
            self%get_qw => calculate_water_content_holder
        end if

    end subroutine type_proereties_manager_initialize

    function calculate_thc_holder(self, state, region_id) result(val)
        implicit none
        class(type_proereties_manager), intent(inout) :: self
        type(type_gauss_point_state), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64) :: val

        type(holder_thcs) :: holder_thc

        call self%materials%get_THC(region_id, holder_thc)
        val = holder_thc%p%calc_gauss_point(state)

    end function calculate_thc_holder

    function calculate_thc_ptr(self, state, region_id) result(val)
        implicit none
        class(type_proereties_manager), intent(inout) :: self
        type(type_gauss_point_state), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64) :: val

        class(abst_thc), pointer :: local_thc

        call self%materials%get_THC(region_id, local_thc)

        val = local_thc%calc_gauss_point(state)

    end function calculate_thc_ptr

    function calculate_heat_capacity_holder(self, state, region_id) result(val)
        implicit none
        class(type_proereties_manager), intent(inout) :: self
        type(type_gauss_point_state), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64) :: val

        real(real64) :: Lf, dQi_dT

        type(holder_vhcs) :: holder_vhc
        type(holder_gccs) :: holder_gcc
        type(holder_dens) :: holder_den
        type(holder_wrfs) :: holder_wrf

        call self%materials%get_vhc(region_id, holder_vhc)
        call self%materials%get_den(region_id, holder_den)
        call self%materials%get_gcc(region_id, holder_gcc)
        call self%materials%get_wrf(region_id, holder_wrf)

        Lf = holder_gcc%p%Lf
        dQi_dT = holder_wrf%p%deriv(-holder_gcc%p%calc(T=state%temperature, &
                                                       Pw=state%pressure, &
                                                       rhoW=holder_den%p%material2, &
                                                       rhoI=holder_den%p%material3)) &
                 * holder_gcc%p%deriv(T=state%temperature, &
                                      Pw=state%pressure, &
                                      rhoW=holder_den%p%material2, &
                                      rhoI=holder_den%p%material3)

        val = holder_vhc%p%calc_gauss_point_holder(state=state, &
                                                   DEN=holder_den, &
                                                   LatentHeat=Lf, &
                                                   dQi_dT=dQi_dT)

    end function calculate_heat_capacity_holder

    function calculate_heat_capacity_ptr(self, state, region_id) result(val)
        implicit none
        class(type_proereties_manager), intent(inout) :: self
        type(type_gauss_point_state), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64) :: val

        class(abst_gcc), pointer :: local_gcc
        class(abst_wrf), pointer :: local_wrf
        class(abst_den), pointer :: local_den
        class(abst_vhc), pointer :: local_vhc

        call self%materials%get_gcc(region_id, local_gcc)
        call self%materials%get_wrf(region_id, local_wrf)
        call self%materials%get_den(region_id, local_den)
        call self%materials%get_vhc(region_id, local_vhc)

        val = local_vhc%calc_gauss_point_ptr(state=state, &
                                             DEN=local_den, &
                                             LatentHeat=local_gcc%Lf, &
                                             dQi_dT=local_wrf%deriv(-local_gcc%calc(T=state%temperature, &
                                                                                    Pw=state%pressure, &
                                                                                    rhoW=local_den%material2, &
                                                                                    rhoI=local_den%material3)))

    end function calculate_heat_capacity_ptr

    ! function calculate_dcdt(self, state, region_id) result(dCa_dT)
    !     implicit none
    !     class(type_proereties_manager), intent(in) :: self
    !     type(type_gauss_point_state), intent(in) :: state
    !     integer(int32), intent(in) :: region_id
    !     real(real64) :: dCa_dT
    !     ! ... 熱容量の温度微分を計算 ...
    ! end function

    function calculate_water_content_holder(self, state, region_id) result(val)
        implicit none
        class(type_proereties_manager), intent(inout) :: self
        type(type_gauss_point_state), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64) :: val

        type(holder_gccs) :: holder_gcc
        type(holder_wrfs) :: holder_wrf
        type(holder_dens) :: holder_den

        call self%materials%get_gcc(region_id, holder_gcc)
        call self%materials%get_wrf(region_id, holder_wrf)
        call self%materials%get_den(region_id, holder_den)

        ! ... 水分量の計算ロジックをここに追加 ...
        val = holder_wrf%p%calc(-holder_gcc%p%calc(T=state%temperature, &
                                                   Pw=state%pressure, &
                                                   rhoW=holder_den%p%material2, &
                                                   rhoI=holder_den%p%material3))
    end function calculate_water_content_holder

    function calculate_water_content_ptr(self, state, region_id) result(val)
        implicit none
        class(type_proereties_manager), intent(inout) :: self
        type(type_gauss_point_state), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64) :: val

        class(abst_gcc), pointer :: local_gcc
        class(abst_wrf), pointer :: local_wrf
        class(abst_den), pointer :: local_den

        call self%materials%get_gcc(region_id, local_gcc)
        call self%materials%get_wrf(region_id, local_wrf)
        call self%materials%get_den(region_id, local_den)

        ! ... 水分量の計算ロジックをここに追加 ...
        val = -local_wrf%calc(-local_gcc%calc(T=state%temperature, &
                                              Pw=state%pressure, &
                                              rhoW=local_den%material2, &
                                              rhoI=local_den%material3))
    end function calculate_water_content_ptr

end module properties_properties_manager
