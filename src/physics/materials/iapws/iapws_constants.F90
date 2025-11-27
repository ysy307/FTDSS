module physics_material_iapws_constants
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: physics_constants, only: &
        water_critical_point_temperature, &
        water_critical_point_pressure, &
        water_critical_point_density, &
        water_triple_point_temperature, &
        water_triple_point_pressure
    implicit none
    private

    !=========================================================
    ! 1. Region Identifiers (Domain IDs)
    !=========================================================
    integer(int32), parameter, public :: IAPWS_INVALID = -1
    ! --- IAPWS-97 Regions ---
    integer(int32), parameter, public :: IAPWS97_R1_LIQ = 1 ! Liquid
    integer(int32), parameter, public :: IAPWS97_R2_VAP = 2 ! Vapor
    integer(int32), parameter, public :: IAPWS97_R3_CRIT = 3 ! Critical / High P
    integer(int32), parameter, public :: IAPWS97_R4_SAT = 4 ! Saturation
    integer(int32), parameter, public :: IAPWS97_R5_GAS = 5 ! High Temp Gas
    ! --- IAPWS-06 Ice Phases ---
    integer(int32), parameter, public :: IAPWS06_ICE_IH = 14

    !=========================================================
    ! 2. Algorithm Boundaries (判定ロジック用閾値)
    !=========================================================
    ! IAPWS-97 適用範囲上限
    real(real64), parameter, public :: IAPWS97_LIMIT_T_MAX = 2273.15d0
    real(real64), parameter, public :: IAPWS97_LIMIT_P_MAX = 100.0d6 ! 100 MPa

    ! Region 5 (高温蒸気) の境界
    real(real64), parameter, public :: IAPWS97_R5_T_MIN = 1073.15d0
    real(real64), parameter, public :: IAPWS97_R5_P_MAX = 50.0d6 ! 50 MPa

    ! Region 2/3 (気体/超臨界) の境界温度
    real(real64), parameter, public :: IAPWS97_R23_T_BOUNDARY = 623.15d0

    !=========================================================
    ! 3. Reference Constants (各領域の計算式用基準値)
    !=========================================================
    ! Region 2/3 boundary in auxiliary equations (IAPWS-IF97)
    real(real64), parameter, public :: T_starb23 = 1.0d0 ! K
    real(real64), parameter, public :: p_starb23 = 1.0d6 ! Pa
    ! Region 1: Saturated liquid water (IAPWS-IF97)
    real(real64), parameter, public :: T_star1 = 1386.0d0
    real(real64), parameter, public :: p_star1 = 16.53d6

    ! Region 2: Superheated Steam (IAPWS-IF97)
    real(real64), parameter, public :: T_star2 = 540.0d0
    real(real64), parameter, public :: p_star2 = 1.0d6

    ! Region 3: High Pressure Liquid Water and Steam (IAPWS-IF97)
    real(real64), parameter, public :: T_star3 = water_critical_point_temperature
    real(real64), parameter, public :: p_star3 = water_critical_point_pressure
    real(real64), parameter, public :: rho_star3 = water_critical_point_density

    ! Region 4: Saturation curve (IAPWS-IF97)
    real(real64), parameter, public :: T_star4 = 1.0d0
    real(real64), parameter, public :: p_star4 = 1.0d6

    ! Region 5: High Temperature Steam (IAPWS-IF97)
    real(real64), parameter, public :: T_star5 = 1000.0d0
    real(real64), parameter, public :: p_star5 = 1.0d6

    ! Ice Ih properties (IAPWS-06)
    real(real64), parameter, public :: T_starIh = water_triple_point_temperature
    real(real64), parameter, public :: p_starIh = water_triple_point_pressure

    ! Ice III properties (IAPWS-08)
    real(real64), parameter, public :: T_starIII = 251.165d0 ! K
    real(real64), parameter, public :: P_starIII = 208.566d6 ! Pa

    real(real64), parameter, public :: T_starV = 256.164d0 ! K
    real(real64), parameter, public :: P_starV = 350.1d6 ! Pa

    real(real64), parameter, public :: T_starVI = 273.31d0 ! K
    real(real64), parameter, public :: P_starVI = 632.4d6 ! Pa

    real(real64), parameter, public :: T_starVII = 355.0d0 ! K
    real(real64), parameter, public :: P_starVII = 2216.0d6 ! Pa

end module physics_material_iapws_constants
