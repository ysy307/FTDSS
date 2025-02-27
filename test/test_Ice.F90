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

    integer(int32) :: case_num, case_type_num, nsize

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

    case_type_num = 2
    case_num = 6

    do i = 1, 501
        T(i) = -4.0d0 + 0.01d0 * (i - 1)
    end do

    nsize = size(T)

    select case (case_type_num)
    case (1)
        Ice = Construct_Type_Ice_TRM(Lf, Tf, nsize)
    case (2)
        select case (case_num)
        case (1)
            Ice = Construct_Type_Ice_GCC(ModelType=case_num, &
                                         isSegregation=.false., &
                                         c_unit='m', &
                                         nsize=nsize, &
                                         thetaR=thetaR_BC, &
                                         thetaS=thetaS_BC, &
                                         alpha1=alpha1_BC, &
                                         n1=n1_BC, &
                                         Lf=Lf, &
                                         Tf=Tf)
        case (2)
            Ice = Construct_Type_Ice_GCC(ModelType=case_num, &
                                         isSegregation=.false., &
                                         c_unit='m', &
                                         nsize=nsize, &
                                         thetaS=thetaS_vG, &
                                         thetaR=thetaR_vG, &
                                         alpha1=alpha1_vG, &
                                         n1=n1_vG, &
                                         Lf=Lf, &
                                         Tf=Tf)
        case (3)
            Ice = Construct_Type_Ice_GCC(ModelType=case_num, &
                                         isSegregation=.false., &
                                         c_unit='m', &
                                         nsize=nsize, &
                                         thetaS=thetaS_KO, &
                                         thetaR=thetaR_KO, &
                                         alpha1=alpha1_KO, &
                                         n1=n1_KO, &
                                         Lf=Lf, &
                                         Tf=Tf)
        case (4)
            Ice = Construct_Type_Ice_GCC(ModelType=case_num, &
                                         isSegregation=.false., &
                                         c_unit='m', &
                                         nsize=nsize, &
                                         thetaS=thetaS_MVG, &
                                         thetaR=thetaR_MVG, &
                                         alpha1=alpha1_MVG, &
                                         n1=n1_MVG, &
                                         Lf=Lf, &
                                         Tf=Tf, &
                                         hcrit=theatM_MVG)
        case (5)
            Ice = Construct_Type_Ice_GCC(ModelType=case_num, &
                                         isSegregation=.false., &
                                         c_unit='m', &
                                         nsize=nsize, &
                                         thetaS=thetaS_Durner, &
                                         thetaR=thetaR_Durner, &
                                         alpha1=alpha1_Durner, &
                                         n1=n1_Durner, &
                                         w1=w1_Durner, &
                                         alpha2=alpha2_Durner, &
                                         n2=n2_Durner, &
                                         Lf=Lf, &
                                         Tf=Tf)
        case (6)
            Ice = Construct_Type_Ice_GCC(ModelType=case_num, &
                                         isSegregation=.false., &
                                         c_unit='m', &
                                         nsize=nsize, &
                                         thetaS=thetaS_DVGCH, &
                                         thetaR=thetaR_DVGCH, &
                                         alpha1=alpha1_DVGCH, &
                                         n1=n1_DVGCH, &
                                         w1=w1_DVGCH, &
                                         alpha2=alpha2_DVGCH, &
                                         n2=n2_DVGCH, &
                                         Lf=Lf, &
                                         Tf=Tf)
        end select

        select type (tI => Ice)
        type is (Type_Ice_GCC)
            call tI%Update_Ice(T)
            call tI%Update_Ice_Derivative(T)
            print *, "T Qice dQice/dT"
            do i = 1, size(T)
                print *, T(i), tI%Qice%pre(i), tI%D_Qice%pre(i)
            end do

        end select
    case (3)
        Ice = Construct_Type_Ice_EXP(EXP_phi, Tf, EXP_a, nsize)
        select type (tI => Ice)
        type is (Type_Ice_EXP)
            call tI%Update_Ice(T)
            call tI%Update_Ice_Derivative(T)
            print *, "T Qice dQice/dT"
            do i = 1, size(T)
                print *, T(i), tI%Qice%pre(i), tI%D_Qice%pre(i)
            end do
        end select
    end select

end program test_Ice
