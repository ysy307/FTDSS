module core_types_physics_state_thc
    use, intrinsic :: iso_fortran_env, only: real64, int32
    implicit none
    private

    public :: type_state_thc

    type :: type_state_thc
        real(real64) :: lambda_xx = 0.0d0
        real(real64) :: lambda_yy = 0.0d0
        real(real64) :: lambda_zz = 0.0d0
        real(real64) :: lambda_xy = 0.0d0
        real(real64) :: lambda_yz = 0.0d0
        real(real64) :: lambda_zx = 0.0d0
    contains
        procedure, pass(self), public :: reset => reset_thc_dispersivity
    end type type_state_thc

contains

    subroutine reset_thc_dispersivity(self)
        implicit none
        class(type_state_thc), intent(inout) :: self

        self%lambda_xx = 0.0d0
        self%lambda_yy = 0.0d0
        self%lambda_zz = 0.0d0
        self%lambda_xy = 0.0d0
        self%lambda_yz = 0.0d0
        self%lambda_zx = 0.0d0
    end subroutine reset_thc_dispersivity

end module core_types_physics_state_thc
