program Calculate_Ice
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Calculate_WRF
    use :: Calculate_GCC
    implicit none

    type, abstract :: Abstract_Ice
        real(real64) :: LatentHeat
    end type

    type, extends(Abstract_Ice) :: Type_Ice_TRM
        real(real64) :: Tf !! Freezing point
    end type

    type, extends(Abstract_Ice) :: Type_Ice_GCC
        real(real64) :: Tf !! Freezing point
        integer(int32) :: ModelType
        class(Abstract_WRF), allocatable :: WRF
    end type Type_Ice_GCC

    type, extends(Abstract_Ice) :: Type_Ice_EXP
        real(real64) :: Tf !! Freezing point
        real(real64) :: a !! power model parameter
    end type Type_Ice_EXP

end program Calculate_Ice
