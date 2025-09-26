!>
!> Provides generic functions to check for the presence of IEEE Not-a-Number (NaN)
!> values in real scalars and arrays of different ranks and kinds.
!>
module core_check_nan
    use, intrinsic :: iso_fortran_env, only: real32, real64, real128
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    implicit none
    private

    public :: has_nan

    !>
    !> A generic interface for checking for NaN values.
    !>
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
    !>
    !> Checks if a scalar single precision real value is NaN.
    !>
    logical function has_nan_scalar_real32(scalar)
        implicit none
        !> The scalar value to check.
        real(real32), intent(in) :: scalar

        has_nan_scalar_real32 = ieee_is_nan(scalar)
    end function has_nan_scalar_real32

    !>
    !> Checks if a scalar double precision real value is NaN.
    !>
    logical function has_nan_scalar_real64(scalar)
        implicit none
        !> The scalar value to check.
        real(real64), intent(in) :: scalar

        has_nan_scalar_real64 = ieee_is_nan(scalar)
    end function has_nan_scalar_real64

    !>
    !> Checks if a scalar quad precision real value is NaN.
    !>
    logical function has_nan_scalar_real128(scalar)
        implicit none
        !> The scalar value to check.
        real(real128), intent(in) :: scalar

        has_nan_scalar_real128 = ieee_is_nan(scalar)
    end function has_nan_scalar_real128

    !>
    !> Checks if any element in a rank-1 single precision real array is NaN.
    !>
    logical function has_nan_rank1_real32(array)
        implicit none
        !> The rank-1 array to check.
        real(real32), intent(in) :: array(:)

        has_nan_rank1_real32 = any(ieee_is_nan(array))
    end function has_nan_rank1_real32

    !>
    !> Checks if any element in a rank-1 double precision real array is NaN.
    !>
    logical function has_nan_rank1_real64(array)
        implicit none
        !> The rank-1 array to check.
        real(real64), intent(in) :: array(:)

        has_nan_rank1_real64 = any(ieee_is_nan(array))
    end function has_nan_rank1_real64

    !>
    !> Checks if any element in a rank-1 quad precision real array is NaN.
    !>
    logical function has_nan_rank1_real128(array)
        implicit none
        !> The rank-1 array to check.
        real(real128), intent(in) :: array(:)

        has_nan_rank1_real128 = any(ieee_is_nan(array))
    end function has_nan_rank1_real128

    !>
    !> Checks if any element in a rank-2 single precision real array is NaN.
    !>
    logical function has_nan_rank2_real32(array)
        implicit none
        !> The rank-2 array to check.
        real(real32), intent(in) :: array(:, :)

        has_nan_rank2_real32 = any(ieee_is_nan(array))
    end function has_nan_rank2_real32

    !>
    !> Checks if any element in a rank-2 double precision real array is NaN.
    !>
    logical function has_nan_rank2_real64(array)
        implicit none
        !> The rank-2 array to check.
        real(real64), intent(in) :: array(:, :)

        has_nan_rank2_real64 = any(ieee_is_nan(array))
    end function has_nan_rank2_real64

    !>
    !> Checks if any element in a rank-2 quad precision real array is NaN.
    !>
    logical function has_nan_rank2_real128(array)
        implicit none
        !> The rank-2 array to check.
        real(real128), intent(in) :: array(:, :)

        has_nan_rank2_real128 = any(ieee_is_nan(array))
    end function has_nan_rank2_real128

end module core_check_nan
