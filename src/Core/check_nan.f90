module core_check_nan
    use, intrinsic :: iso_fortran_env, only: real32, real64, real128
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    implicit none
    private

    public :: has_nan

    interface has_nan
        procedure :: has_nan_scalar_real32
        procedure :: has_nan_scalar_real64
        procedure :: has_nan_scalar_real128
        procedure :: has_nan_rank1_real32
        procedure :: has_nan_rank1_real64
        procedure :: has_nan_rank1_real128
        procedure :: has_nan_rank2_real32
        procedure :: has_nan_rank2_real64
        procedure :: has_nan_rank2_real128
    end interface

contains
    logical function has_nan_scalar_real32(scalar)
        implicit none
        real(real32), intent(in) :: scalar

        has_nan_scalar_real32 = ieee_is_nan(scalar)
    end function has_nan_scalar_real32

    logical function has_nan_scalar_real64(scalar)
        implicit none
        real(real64), intent(in) :: scalar

        has_nan_scalar_real64 = ieee_is_nan(scalar)
    end function has_nan_scalar_real64

    logical function has_nan_scalar_real128(scalar)
        implicit none
        real(real128), intent(in) :: scalar

        has_nan_scalar_real128 = ieee_is_nan(scalar)
    end function has_nan_scalar_real128

    logical function has_nan_rank1_real32(array)
        implicit none
        real(real32), intent(in) :: array(:)

        has_nan_rank1_real32 = any(ieee_is_nan(array))
    end function has_nan_rank1_real32

    logical function has_nan_rank1_real64(array)
        implicit none
        real(real64), intent(in) :: array(:)

        has_nan_rank1_real64 = any(ieee_is_nan(array))
    end function has_nan_rank1_real64

    logical function has_nan_rank1_real128(array)
        implicit none
        real(real128), intent(in) :: array(:)

        has_nan_rank1_real128 = any(ieee_is_nan(array))
    end function has_nan_rank1_real128

    logical function has_nan_rank2_real32(array)
        implicit none
        real(real32), intent(in) :: array(:, :)

        has_nan_rank2_real32 = any(ieee_is_nan(array))
    end function has_nan_rank2_real32

    logical function has_nan_rank2_real64(array)
        implicit none
        real(real64), intent(in) :: array(:, :)

        has_nan_rank2_real64 = any(ieee_is_nan(array))
    end function has_nan_rank2_real64

    logical function has_nan_rank2_real128(array)
        implicit none
        real(real128), intent(in) :: array(:, :)

        has_nan_rank2_real128 = any(ieee_is_nan(array))
    end function has_nan_rank2_real128

end module core_check_nan
