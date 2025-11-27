submodule(physics_material_iapws_wrapper) determine_regions
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
                    region_id = IAPWS97_R3_CRIT
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
end submodule determine_regions
