program test_GCC
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Calculate_GCC
    implicit none
    real(real64) :: T(501)
    integer(int32) :: i
    integer(int32) :: case_num
    real(real64) :: Pw, rhoW

    class(Abstract_GCC), allocatable :: GCC

    case_num = 4

    rhoW = 1000.0d0

    do i = 1, 501
        T(i) = -40.0d0 + 0.1d0 * (i - 1)
    end do
    select case (case_num)
    case (1)
        allocate (Type_GCC_NonSegregation_m :: GCC)
    case (2)
        allocate (Type_GCC_NonSegregation_Pa :: GCC)
    case (3)
        allocate (Type_GCC_Segregation_m :: GCC)
    case (4)
        allocate (Type_GCC_Segregation_Pa :: GCC)
    end select

    select type (g => GCC)
    type is (Type_GCC_NonSegregation_m)
        g%Lf = 334560d0
        g%Tf = 0.0d0

        do i = 1, size(T)
            print *, T(i), g%Calculate_GCC(T(i)), g%Calculate_GCC_Derivative(T(i))
        end do
    type is (Type_GCC_NonSegregation_Pa)
        g%Lf = 334560d0
        g%Tf = 0.0d0

        do i = 1, size(T)
            print *, T(i), g%Calculate_GCC(T(i), rhoW), g%Calculate_GCC_Derivative(T(i), rhoW)
        end do
    type is (Type_GCC_Segregation_m)
        g%Lf = 334560d0
        g%Tf = 0.0d0
        g%rhoI = 917.0d0
        Pw = 10.0d0

        do i = 1, size(T)
            print *, T(i), g%Calculate_GCC(T(i), Pw, rhoW), g%Calculate_GCC_Derivative(T(i), Pw, rhoW)
        end do
    type is (Type_GCC_Segregation_Pa)
        g%Lf = 334560d0
        g%Tf = 0.0d0
        g%rhoI = 917.0d0
        Pw = 10000.0d0

        do i = 1, size(T)
            print *, T(i), g%Calculate_GCC(T(i), Pw, rhoW), g%Calculate_GCC_Derivative(T(i), Pw, rhoW)
        end do
    end select

end program test_GCC
