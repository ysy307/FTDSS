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

    ! --- IAPWS-97 Main Regions ---
    integer(int32), parameter, public :: IAPWS97_R1_LIQ = 1 ! Liquid
    integer(int32), parameter, public :: IAPWS97_R2_VAP = 2 ! Vapor
    integer(int32), parameter, public :: IAPWS97_R3_CRIT = 3 ! Critical / High P
    integer(int32), parameter, public :: IAPWS97_R4_SAT = 4 ! Saturation
    integer(int32), parameter, public :: IAPWS97_R5_GAS = 5 ! High Temp Gas

    ! --- IAPWS-06 Ice Phases ---
    integer(int32), parameter, public :: IAPWS06_ICE_IH = 14

    ! --- IAPWS-97 Region 3 Subregions (SR5-05) ---
    ! 3a - 3t (Backward Equations)
    integer(int32), parameter, public :: IAPWS97_R3_A = 301
    integer(int32), parameter, public :: IAPWS97_R3_B = 302
    integer(int32), parameter, public :: IAPWS97_R3_C = 303
    integer(int32), parameter, public :: IAPWS97_R3_D = 304
    integer(int32), parameter, public :: IAPWS97_R3_E = 305
    integer(int32), parameter, public :: IAPWS97_R3_F = 306
    integer(int32), parameter, public :: IAPWS97_R3_G = 307
    integer(int32), parameter, public :: IAPWS97_R3_H = 308
    integer(int32), parameter, public :: IAPWS97_R3_I = 309
    integer(int32), parameter, public :: IAPWS97_R3_J = 310
    integer(int32), parameter, public :: IAPWS97_R3_K = 311
    integer(int32), parameter, public :: IAPWS97_R3_L = 312
    integer(int32), parameter, public :: IAPWS97_R3_M = 313
    integer(int32), parameter, public :: IAPWS97_R3_N = 314
    integer(int32), parameter, public :: IAPWS97_R3_O = 315
    integer(int32), parameter, public :: IAPWS97_R3_P = 316
    integer(int32), parameter, public :: IAPWS97_R3_Q = 317
    integer(int32), parameter, public :: IAPWS97_R3_R = 318
    integer(int32), parameter, public :: IAPWS97_R3_S = 319
    integer(int32), parameter, public :: IAPWS97_R3_T = 320

    ! 3u - 3z (Auxiliary Equations near Critical Point)
    integer(int32), parameter, public :: IAPWS97_R3_U = 321
    integer(int32), parameter, public :: IAPWS97_R3_V = 322
    integer(int32), parameter, public :: IAPWS97_R3_W = 323
    integer(int32), parameter, public :: IAPWS97_R3_X = 324
    integer(int32), parameter, public :: IAPWS97_R3_Y = 325
    integer(int32), parameter, public :: IAPWS97_R3_Z = 326

    !=========================================================
    ! Region 3 Boundary Constants [MPa] (SR5-05 Table 2)
    !=========================================================
    real(real64), parameter, public :: P3_BOUND_100 = 100.0d6
    real(real64), parameter, public :: P3_BOUND_40 = 40.0d6
    real(real64), parameter, public :: P3_BOUND_25 = 25.0d6
    real(real64), parameter, public :: P3_BOUND_23_5 = 23.5d6
    real(real64), parameter, public :: P3_BOUND_23 = 23.0d6
    real(real64), parameter, public :: P3_BOUND_22_5 = 22.5d6

    ! For Auxiliary Equations logic (Table 10)
    real(real64), parameter, public :: P3_BOUND_22_11 = 22.11d6
    real(real64), parameter, public :: P3_BOUND_22_064 = 22.064d6 ! Critical Pressure
    real(real64), parameter, public :: P3_BOUND_20_5 = 20.5d6
    real(real64), parameter, public :: P3_BOUND_2193 = 2.193161551d7 ! psat(0.00264 m3/kg)
    real(real64), parameter, public :: P3_BOUND_2190 = 2.190096265d7 ! psat(0.00385 m3/kg)

    ! Calculated/Specific pressures
    ! psat(643.15 K) approx 21.043 MPa
    real(real64), parameter, public :: P3_SAT_643_15 = 2.104336732d7
    ! p3cd boundary approx 19.008 MPa
    real(real64), parameter, public :: P3_BOUND_3CD = 1.900881189173929d7
    ! psat(623.15 K) approx 16.529 MPa
    real(real64), parameter, public :: P3_SAT_623_15 = 1.652916425d7

    !=========================================================
    ! 2. Algorithm Boundaries
    !=========================================================
    real(real64), parameter, public :: IAPWS97_LIMIT_T_MAX = 2273.15d0
    real(real64), parameter, public :: IAPWS97_LIMIT_P_MAX = 100.0d6

    real(real64), parameter, public :: IAPWS97_R5_T_MIN = 1073.15d0
    real(real64), parameter, public :: IAPWS97_R5_P_MAX = 50.0d6

    real(real64), parameter, public :: IAPWS97_R23_T_BOUNDARY = 623.15d0

    !=========================================================
    ! 3. Reference Constants (Reduction parameters)
    !=========================================================
    ! Region 3 equations typically use T_star = 1K, P_star = 1MPa for boundaries
    real(real64), parameter, public :: T_star_r3_bound = 1.0d0
    real(real64), parameter, public :: P_star_r3_bound = 1.0d6

    ! Region 2/3 boundary (B23)
    real(real64), parameter, public :: T_starb23 = 1.0d0
    real(real64), parameter, public :: p_starb23 = 1.0d6

    ! Region 1
    real(real64), parameter, public :: T_star1 = 1386.0d0
    real(real64), parameter, public :: p_star1 = 16.53d6

    ! Region 2
    real(real64), parameter, public :: T_star2 = 540.0d0
    real(real64), parameter, public :: p_star2 = 1.0d6

    ! Region 3 (Basic Eq)
    real(real64), parameter, public :: T_star3 = water_critical_point_temperature
    real(real64), parameter, public :: p_star3 = water_critical_point_pressure
    real(real64), parameter, public :: rho_star3 = water_critical_point_density

    ! Region 4
    real(real64), parameter, public :: T_star4 = 1.0d0
    real(real64), parameter, public :: p_star4 = 1.0d6

    ! Region 5
    real(real64), parameter, public :: T_star5 = 1000.0d0
    real(real64), parameter, public :: p_star5 = 1.0d6

    ! Ice Ih
    real(real64), parameter, public :: T_starIh = water_triple_point_temperature
    real(real64), parameter, public :: p_starIh = water_triple_point_pressure

    ! Ice III, V, VI, VII
    real(real64), parameter, public :: T_starIII = 251.165d0
    real(real64), parameter, public :: P_starIII = 208.566d6
    real(real64), parameter, public :: T_starV = 256.164d0
    real(real64), parameter, public :: P_starV = 350.1d6
    real(real64), parameter, public :: T_starVI = 273.31d0
    real(real64), parameter, public :: P_starVI = 632.4d6
    real(real64), parameter, public :: T_starVII = 355.0d0
    real(real64), parameter, public :: P_starVII = 2216.0d6

end module physics_material_iapws_constants
