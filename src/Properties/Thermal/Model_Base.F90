module Properties_Thermal_Model_Base
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes, only:GaussPointState_t

    implicit none
    private
    public :: ThermalModel_Base_t

    type, abstract :: ThermalModel_Base_t
    contains
        procedure(Abst_get_lambda), deferred        :: get_lambda !&
        procedure(Abst_get_Ca), deferred            :: get_Ca !&
        procedure(Abst_get_Ca_Derivative), deferred :: get_dCa_dT !&
    end type

    abstract interface
        function Abst_get_lambda(self, state, region_id) result(lambda)
            import :: ThermalModel_Base_t, GaussPointState_t, real64, int32
            implicit none
            class(ThermalModel_Base_t), intent(in) :: self
            type(GaussPointState_t), intent(in) :: state
            integer(int32), intent(in) :: region_id
            real(real64) :: lambda
        end function Abst_get_lambda

        function Abst_get_Ca(self, state) result(Ca)
            import :: ThermalModel_Base_t, GaussPointState_t, real64
            class(ThermalModel_Base_t), intent(in) :: self
            type(GaussPointState_t), intent(in) :: state
            real(real64) :: Ca
        end function Abst_get_Ca

        function Abst_get_Ca_Derivative(self, state) result(Ca_Derivative)
            import :: ThermalModel_Base_t, GaussPointState_t, real64
            class(ThermalModel_Base_t), intent(in) :: self
            type(GaussPointState_t), intent(in) :: state
            real(real64) :: Ca_Derivative
        end function Abst_get_Ca_Derivative
    end interface
end module Properties_Thermal_Model_Base
