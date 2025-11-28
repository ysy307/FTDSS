submodule(physics_material_iapws_wrapper) iapws_determine_regions
    implicit none
contains

    module pure elemental function determine_iapws_region(T_in, P_in, is_supercooled) result(region_id)
        implicit none
        real(real64), intent(in) :: T_in ! Temperature [K]
        real(real64), intent(in) :: P_in ! Pressure [Pa]
        logical, intent(in), optional :: is_supercooled
        integer(int32) :: region_id

        real(real64) :: p_sat
        real(real64) :: p_melting, p_sublimation
        logical :: supercooled_mode

        ! 0. 異常値チェック
        if (T_in <= 0.0d0 .or. P_in < 0.0d0) then
            region_id = IAPWS_INVALID
            return
        end if

        ! IAPWS-97 Region 5 上限チェック (2273.15 K)
        ! 圧力上限は通常100MPaだが、Region 5は50MPa
        if (T_in > IAPWS97_LIMIT_T_MAX .or. P_in > IAPWS97_LIMIT_P_MAX) then
            region_id = IAPWS_INVALID
            return
        end if

        ! 1. 過冷却モードの処理
        ! オプショナル引数の処理を整理
        if (optval(is_supercooled, .false.)) then
            region_id = IAPWS97_R1_LIQ
            return
        end if

        ! 2. 高温領域 (Region 5)
        if (T_in > IAPWS97_R5_T_MIN) then
            if (P_in <= IAPWS97_R5_P_MAX) then
                region_id = IAPWS97_R5_GAS
            else
                ! 1073.15 K超えかつ50MPa超えは未定義(Out of Range)
                region_id = IAPWS_INVALID
            end if
            return
        end if

        ! 3. 三重点温度以上 (通常の流体領域)
        if (T_in >= water_triple_point_temperature) then
            if (T_in >= IAPWS97_R23_T_BOUNDARY) then
                ! Region 2 (Vapor) vs Region 3 (High P/Critical)
                p_sat = calc_p_boundary_iapws97_region23(T_in)
                if (P_in > p_sat) then
                    region_id = determine_r3_subregion(T_in, P_in, calc_Tsat_iapws97_region4(P_in))
                else
                    region_id = IAPWS97_R2_VAP
                end if
            else
                ! Region 1 (Liquid) vs Region 2 (Vapor)
                p_sat = calc_psat_iapws97_region4(T_in)
                if (P_in > p_sat) then
                    region_id = IAPWS97_R1_LIQ
                else
                    region_id = IAPWS97_R2_VAP
                end if
            end if
            return

        else
            ! 4. 三重点温度未満 (氷・昇華・融解)
            ! 融解圧力を計算 (IAPWS-08)
            p_melting = calc_p_boundary_iapws08_iceIh_melting(T_in)

            if (P_in > p_melting) then
                ! 融解圧力より高い圧力 -> 液体 (Region 1)
                ! ※氷点下でも高圧なら水は液体として存在します
                region_id = IAPWS97_R1_LIQ
            else
                ! 昇華圧力を計算
                p_sublimation = calc_p_boundary_iapws08_iceIh_sublimation(T_in)

                if (P_in > p_sublimation) then
                    ! 昇華圧 < P < 融解圧 -> 氷 (Ice Ih)
                    region_id = IAPWS06_ICE_IH
                else
                    ! 昇華圧より低い -> 気体 (Region 2)
                    region_id = IAPWS97_R2_VAP
                end if
            end if
            return
        end if
    end function determine_iapws_region

    !---------------------------------------------------------------------
    ! Region 3 (Critical) 詳細サブ領域判定 (SR5-05 Table 2 & 10)
    ! T_in: [K]
    ! P_in: [Pa]
    ! T_sat_in: [K] (同圧力における飽和温度。3s/3t/3uの判定に必須)
    !---------------------------------------------------------------------
    pure elemental function determine_r3_subregion(T_in, P_in, T_sat_in) result(id)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64), intent(in) :: T_sat_in
        integer(int32) :: id

        !-----------------------------------------------------
        ! 1. 補助領域 (Auxiliary Equations 3u-3z) のチェック
        !    範囲: psat(643.15) < p <= 22.5 MPa かつ T3qu < T <= T3rx
        !-----------------------------------------------------
        if (P_in <= P3_BOUND_22_5 .and. P_in > P3_SAT_643_15) then
            if (T_in > calc_T3qu(P_in) .and. T_in <= calc_T3rx(P_in)) then

                ! === Auxiliary Region Logic (Table 10) ===
                if (P_in > P3_BOUND_22_064) then
                    ! --- Supercritical (22.064 < p <= 22.5) ---
                    if (P_in > P3_BOUND_22_11) then
                        ! 22.11 < p <= 22.5
                        if (T_in <= calc_T3uv(P_in)) then
                            id = IAPWS97_R3_U
                        else if (T_in <= calc_T3ef(P_in)) then
                            id = IAPWS97_R3_V
                        else if (T_in <= calc_T3wx(P_in)) then
                            id = IAPWS97_R3_W
                        else
                            id = IAPWS97_R3_X
                        end if
                    else
                        ! 22.064 < p <= 22.11
                        if (T_in <= calc_T3uv(P_in)) then
                            id = IAPWS97_R3_U
                        else if (T_in <= calc_T3ef(P_in)) then
                            id = IAPWS97_R3_Y
                        else if (T_in <= calc_T3wx(P_in)) then
                            id = IAPWS97_R3_Z
                        else
                            id = IAPWS97_R3_X
                        end if
                    end if
                else
                    ! --- Subcritical (p <= 22.064) ---
                    if (T_in <= T_sat_in) then
                        ! Liquid side
                        if (P_in > P3_BOUND_2193) then ! psat(0.00264) < p <= 22.064
                            if (T_in <= calc_T3uv(P_in)) then
                                id = IAPWS97_R3_U
                            else
                                id = IAPWS97_R3_Y
                            end if
                        else ! psat(643.15) < p <= psat(0.00264)
                            ! T3qu < T は外側のifで保証済
                            id = IAPWS97_R3_U
                        end if
                    else
                        ! Vapor side
                        if (P_in > P3_BOUND_2190) then ! psat(0.00385) < p <= 22.064
                            if (T_in <= calc_T3wx(P_in)) then
                                id = IAPWS97_R3_Z
                            else
                                id = IAPWS97_R3_X
                            end if
                        else ! psat(643.15) < p <= psat(0.00385)
                            id = IAPWS97_R3_X
                        end if
                    end if
                end if
                return ! 補助領域確定
            end if
        end if

        !-----------------------------------------------------
        ! 2. Backward Equations (3a-3t) Logic (Table 2)
        !-----------------------------------------------------

        ! --- 40 MPa < p <= 100 MPa ---
        if (P_in > P3_BOUND_40) then
            if (T_in <= calc_T3ab(P_in)) then
                id = IAPWS97_R3_A
            else
                id = IAPWS97_R3_B
            end if
            return
        end if

        ! --- 25 MPa < p <= 40 MPa ---
        if (P_in > P3_BOUND_25) then
            if (T_in <= calc_T3cd(P_in)) then
                id = IAPWS97_R3_C
            else if (T_in <= calc_T3ab(P_in)) then
                id = IAPWS97_R3_D
            else if (T_in <= calc_T3ef(P_in)) then
                id = IAPWS97_R3_E
            else
                id = IAPWS97_R3_F
            end if
            return
        end if

        ! --- 23.5 MPa < p <= 25 MPa ---
        if (P_in > P3_BOUND_23_5) then
            if (T_in <= calc_T3cd(P_in)) then
                id = IAPWS97_R3_C
            else if (T_in <= calc_T3gh(P_in)) then
                id = IAPWS97_R3_G
            else if (T_in <= calc_T3ef(P_in)) then
                id = IAPWS97_R3_H
            else if (T_in <= calc_T3ij(P_in)) then
                id = IAPWS97_R3_I
            else if (T_in <= calc_T3jk(P_in)) then
                id = IAPWS97_R3_J
            else
                id = IAPWS97_R3_K
            end if
            return
        end if

        ! --- 23 MPa < p <= 23.5 MPa ---
        if (P_in > P3_BOUND_23) then
            if (T_in <= calc_T3cd(P_in)) then
                id = IAPWS97_R3_C
            else if (T_in <= calc_T3gh(P_in)) then
                id = IAPWS97_R3_L ! <--- 3l here
            else if (T_in <= calc_T3ef(P_in)) then
                id = IAPWS97_R3_H
            else if (T_in <= calc_T3ij(P_in)) then
                id = IAPWS97_R3_I
            else if (T_in <= calc_T3jk(P_in)) then
                id = IAPWS97_R3_J
            else
                id = IAPWS97_R3_K
            end if
            return
        end if

        ! --- 22.5 MPa < p <= 23 MPa ---
        if (P_in > P3_BOUND_22_5) then
            if (T_in <= calc_T3cd(P_in)) then
                id = IAPWS97_R3_C
            else if (T_in <= calc_T3gh(P_in)) then
                id = IAPWS97_R3_L
            else if (T_in <= calc_T3mn(P_in)) then
                id = IAPWS97_R3_M
            else if (T_in <= calc_T3ef(P_in)) then
                id = IAPWS97_R3_N
            else if (T_in <= calc_T3op(P_in)) then
                id = IAPWS97_R3_O
            else if (T_in <= calc_T3ij(P_in)) then
                id = IAPWS97_R3_P
            else if (T_in <= calc_T3jk(P_in)) then
                id = IAPWS97_R3_J
            else
                id = IAPWS97_R3_K
            end if
            return
        end if

        ! --- psat(643.15) < p <= 22.5 MPa ---
        if (P_in > P3_SAT_643_15) then
            ! Aux check passed. Outliers:
            if (T_in <= calc_T3cd(P_in)) then
                id = IAPWS97_R3_C
            else if (T_in <= calc_T3qu(P_in)) then
                id = IAPWS97_R3_Q
            else if (T_in <= calc_T3jk(P_in)) then
                ! T > T3rx (Aux limit)
                id = IAPWS97_R3_R
            else
                id = IAPWS97_R3_K
            end if
            return
        end if

        ! --- 20.5 MPa < p <= psat(643.15) ---
        if (P_in > P3_BOUND_20_5) then
            if (T_in <= calc_T3cd(P_in)) then
                id = IAPWS97_R3_C
            else if (T_in <= T_sat_in) then
                id = IAPWS97_R3_S
            else if (T_in <= calc_T3jk(P_in)) then
                id = IAPWS97_R3_R
            else
                id = IAPWS97_R3_K
            end if
            return
        end if

        ! --- p3cd < p <= 20.5 MPa ---
        if (P_in > P3_BOUND_3CD) then
            if (T_in <= calc_T3cd(P_in)) then
                id = IAPWS97_R3_C
            else if (T_in <= T_sat_in) then
                id = IAPWS97_R3_S
            else
                id = IAPWS97_R3_T
            end if
            return
        end if

        ! --- psat(623.15) < p <= p3cd ---
        ! Assumes p > pB23
        if (T_in <= T_sat_in) then
            id = IAPWS97_R3_C
        else
            id = IAPWS97_R3_T
        end if

    end function determine_r3_subregion
end submodule iapws_determine_regions
