program test_Ice
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Calculate_Ice
    implicit none
    real(real64) :: T(501)
    real(real64) :: Qice(size(T)), D_Qice(size(T)), rhoW(size(T)), Pw(size(T))
    integer(int32) :: i

    real(real64) :: thetaS_BC, thetaR_BC, alpha1_BC, n1_BC
    real(real64) :: thetaS_vG, thetaR_vG, alpha1_vG, n1_vG
    real(real64) :: thetaS_KO, thetaR_KO, alpha1_KO, n1_KO
    real(real64) :: thetaS_MVG, thetaR_MVG, alpha1_MVG, n1_MVG, theatM_MVG
    real(real64) :: thetaS_Durner, thetaR_Durner, alpha1_Durner, n1_Durner, w1_Durner, alpha2_Durner, n2_Durner
    real(real64) :: thetaS_DVGCH, thetaR_DVGCH, alpha1_DVGCH, n1_DVGCH, w1_DVGCH, alpha2_DVGCH, n2_DVGCH
    real(real64) :: EXP_phi, EXP_a

    real(real64) :: Tf, Lf, rhoI

    integer(int32) :: case_num

    class(Abstract_Ice), allocatable :: Ice

    ! BC
    thetaS_BC = 0.3d0
    thetaR_BC = 0.0d0
    alpha1_BC = -2.558d0
    n1_BC = 0.57087d0

    ! vG
    thetaS_vG = 0.3d0
    thetaR_vG = 0.0d0
    alpha1_vG = 0.2d0
    n1_vG = 1.8d0

    ! KO
    thetaS_KO = 0.3d0
    thetaR_KO = 0.0d0
    alpha1_KO = -11.473d0
    n1_KO = 1.3685d0

    ! MVG
    thetaS_MVG = 0.3d0
    thetaR_MVG = 0.0d0
    alpha1_MVG = 0.2d0
    n1_MVG = 1.8d0
    theatM_MVG = 0.0d0

    ! Durner
    thetaS_Durner = 0.39971d0
    thetaR_Durner = 0.00671d0
    alpha1_Durner = 0.04034d0
    n1_Durner = 8.46152d0
    w1_Durner = 0.72352d0
    alpha2_Durner = 0.04034d0
    n2_Durner = 1.30984d0

    ! DVGCH
    thetaS_DVGCH = 0.39971d0
    thetaR_DVGCH = 0.00671d0
    alpha1_DVGCH = 0.04034d0
    n1_DVGCH = 8.46152d0
    w1_DVGCH = 0.72352d0
    n2_DVGCH = 1.30984d0

    Tf = 0.0d0
    Lf = 334560d0
    rhoI = 917.0d0

    EXP_phi = 0.3d0
    EXP_a = -6.02d0

    ! allocate (Type_Ice_GCC :: Ice)
    ! allocate (Type_Ice_EXP :: Ice)
    Ice = Type_Ice_EXP(EXP_phi, Tf, EXP_a, size(T))

    case_num = 1

    do i = 1, 501
        T(i) = -4.0d0 + 0.01d0 * (i - 1)
    end do
    print *, "T Qice dQice/dT"
    select type (tI => Ice)
    type is (Type_Ice_GCC)
        call tI%Set_WRF(case_num)
        select type (w => tI%WRF)
        type is (Type_WRF_BC)
            w%thetaS = thetaS_BC
            w%thetaR = thetaR_BC
            w%alpha1 = alpha1_BC
            w%n1 = n1_BC
        type is (Type_WRF_VG)
            w%thetaS = thetaS_vG
            w%thetaR = thetaR_vG
            w%alpha1 = alpha1_vG
            w%n1 = n1_vG
            w%m1 = 1.0d0 - 1.0d0 / n1_vG
        type is (Type_WRF_KO)
            w%thetaS = thetaS_KO
            w%thetaR = thetaR_KO
            w%alpha1 = alpha1_KO
            w%n1 = n1_KO
        type is (Type_WRF_MVG)
            w%thetaS = thetaS_MVG
            w%thetaR = thetaR_MVG
            w%alpha1 = alpha1_MVG
            w%n1 = n1_MVG
            w%m1 = 1.0d0 - 1.0d0 / n1_MVG
            w%hcrit = theatM_MVG
        type is (Type_WRF_Durner)
            w%thetaS = thetaS_Durner
            w%thetaR = thetaR_Durner
            w%alpha1 = alpha1_Durner
            w%n1 = n1_Durner
            w%m1 = 1.0d0 - 1.0d0 / n1_Durner
            w%w1 = w1_Durner
            w%alpha2 = alpha2_Durner
            w%n2 = n2_Durner
            w%m2 = 1.0d0 - 1.0d0 / n2_Durner
            w%w2 = 1.0d0 - w1_Durner
        type is (Type_WRF_DVGCH)
            w%thetaS = thetaS_DVGCH
            w%thetaR = thetaR_DVGCH
            w%alpha1 = alpha1_DVGCH
            w%n1 = n1_DVGCH
            w%m1 = 1.0d0 - 1.0d0 / n1_DVGCH
            w%w1 = w1_DVGCH
            w%n2 = n2_DVGCH
            w%m2 = 1.0d0 - 1.0d0 / n2_DVGCH
            w%w2 = 1.0d0 - w1_DVGCH
        end select
        call tI%Set_GCC(.false., 'm')
        select type (tGCC => tI%GCC)
        type is (Type_GCC_NonSegregation_m)
            tGCC%Lf = Lf
            tGCC%Tf = Tf
            allocate (tI%Qice%pre(size(T)))
            allocate (tI%D_Qice%pre(size(T)))

            call tI%Calculate_Ice(T)
            call tI%Calculate_Ice_Derivative(T)
            do i = 1, size(T)
                print *, T(i), tI%Qice%pre(i), tI%D_Qice%pre(i)
            end do
        type is (Type_GCC_NonSegregation_Pa)
            tGCC%Lf = Lf
            tGCC%Tf = Tf
        type is (Type_GCC_Segregation_m)
            tGCC%Lf = Lf
            tGCC%Tf = Tf
            tGCC%rhoI = rhoI
        type is (Type_GCC_Segregation_Pa)
            tGCC%Lf = Lf
            tGCC%Tf = Tf
            tGCC%rhoI = rhoI
        end select
    type is (Type_Ice_EXP)

        call tI%Update_Ice(T)
        call tI%Update_Ice_Derivative(T)

        do i = 1, size(T)
            print *, T(i), tI%Qice%pre(i), tI%D_Qice%pre(i)
        end do
    end select

end program test_Ice
