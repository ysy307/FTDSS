program test_VHC
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Calculate_VolumetricHeatCapacity
    use :: Calculate_Ice
    use :: Allocate_Allocate, only:Allocate_Array
    implicit none
    class(Abstract_VolumetricHeatCapacity), allocatable :: VHC

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

    real(real64) :: Cp_soil, Cp_ice, Cp_water, Cp_air
    real(real64) :: Den_ice, Den_water

    real(real64) :: Temp, l_Cp, l_Ca, phi

    integer(int32) :: case_num, case_type_num, nsize
    integer(int32) :: case_num_VHC

    class(Abstract_Ice), pointer :: Ice
    type(Variables), pointer :: Temperature
    ! class(Abstract_Ice), allocatable :: Ice

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
    alpha1_Durner = 0.04034d2
    n1_Durner = 8.46152d0
    w1_Durner = 0.72352d0
    alpha2_Durner = 0.04034d2
    n2_Durner = 1.30984d0

    ! DVGCH
    thetaS_DVGCH = 0.39971d0
    thetaR_DVGCH = 0.00671d0
    alpha1_DVGCH = 0.04034d2
    n1_DVGCH = 8.46152d0
    w1_DVGCH = 0.72352d0
    n2_DVGCH = 1.30984d0

    Tf = 0.0d0
    Lf = 334560d0
    rhoI = 917.0d0

    EXP_phi = 0.3d0
    EXP_a = -6.02d0

    ! Cp_soil = 2864.d0 * 636.8d0
    Cp_soil = 2800.0d0 * 912.0d0
    Cp_ice = 2100.d0 * 917.0d0
    Cp_water = 4180.d0 * 1000.0d0
    Cp_air = 1005.0d0
    Den_ice = 917.0d0
    Den_water = 1000.0d0

    Temp = -1.0d0

    do i = 1, 501
        T(i) = -4.0d0 + 0.01d0 * (i - 1)
    end do

    nsize = size(T)
    allocate (Temperature)
    call Allocate_Array(Temperature%old, nsize)
    call Allocate_Array(Temperature%pre, nsize)
    call Allocate_Array(Temperature%new, nsize)
    case_type_num = 3
    case_num = 2

    Temperature%pre(:) = T(:)

    select case (case_type_num)
    case (1)
        Ice => Construct_Type_Ice_TRM_Pointer(Lf, Tf, nsize)
    case (2)
        select case (case_num)
        case (1)
            Ice => Construct_Type_Ice_GCC_Pointer(ModelType=case_num, &
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
            Ice => Construct_Type_Ice_GCC_Pointer(ModelType=case_num, &
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
            Ice => Construct_Type_Ice_GCC_Pointer(ModelType=case_num, &
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
            Ice => Construct_Type_Ice_GCC_Pointer(ModelType=case_num, &
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
            Ice => Construct_Type_Ice_GCC_Pointer(ModelType=case_num, &
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
            Ice => Construct_Type_Ice_GCC_Pointer(ModelType=case_num, &
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
    case (3)
        Ice => Construct_Type_Ice_EXP_Pointer(Lf, EXP_phi, Tf, EXP_a, Temperature, nsize)
    end select

    case_num_VHC = 3
    select case (case_num_VHC)
    case (1)
        VHC = Type_VolumetricHeatCapacity_1Phase(Cp_soil, nsize)
    case (2)
        VHC = Type_VolumetricHeatCapacity_2Phase(Cp_soil, Cp_water, nsize)
    case (3)
        VHC = Type_VolumetricHeatCapacity_3Phase(Cp_soil, Cp_water, Cp_ice, Ice, Den_ice, Den_water, nsize)
    case (4)
        VHC = Type_VolumetricHeatCapacity_4Phase(Cp_soil, Cp_water, Cp_ice, Cp_air, Ice, Den_ice, Den_water, nsize)
    end select

    open (unit=10, file='res.txt', status='replace')
    select type (v_p => VHC)
    type is (Type_VolumetricHeatCapacity_3Phase)
        select type (i_ps => Ice)
        type is (Type_Ice_GCC)
            phi = i_ps%WRF%thetaS
            print '(es12.3)', v_p%Ca_max

            call i_ps%Update_Ice(T)
            i_ps%Qw%pre(:) = phi - i_ps%Qice%pre(:)
            call v_p%Update(1.0d0 - phi)
            call v_p%Update_Ca(rho_ice=Den_ice, arr_Temperature=T)
            do i = 1, nsize
                write (10, '(3es14.6)'), T(i), v_p%value(i), v_p%Apparent(i)
            end do
        type is (Type_Ice_EXP)
            phi = i_ps%phi
            print '(es14.6)', v_p%Ca_max

            call i_ps%Update_Ice()
            i_ps%Qw%pre(:) = phi - i_ps%Qice%pre(:)
            call v_p%Update(1.0d0 - phi)
            call v_p%Update_Ca(rho_ice=Den_ice, arr_Temperature=T)
            do i = 1, nsize
                write (10, '(3es14.6)'), T(i), v_p%value(i), v_p%Apparent(i)
            end do
        end select
    end select

end program test_VHC
