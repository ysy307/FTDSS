module physics_material_iapws_wrapper
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: physics_material_iapws
    implicit none

contains

    pure elemental function determine_iapws_region(T_in, P_in, is_supercooled) result(region_id)
        implicit none
        real(real64), intent(in) :: T_in ! Temperature [K]
        real(real64), intent(in) :: P_in ! Pressure [Pa]
        logical, intent(in), optional :: is_supercooled
        integer(int32) :: region_id

        real(real64) :: p_sat
        real(real64) :: p_melting, p_sublimation

        if (T_in <= 0.0d0 .or. P_in < 0.0d0) then
            region_id = IAPWS_OUT_OF_RANGE
            return
        end if

        if (T_in > 2273.15d0 .or. P_in > 1.0d8) then
            region_id = IAPWS_OUT_OF_RANGE
            return
        end if

        if (value_in_range(T_in, 1073.15d0, 2273.15d0)) then
            if (value_in_range(P_in, 0.0d0, 5.0d7)) then
                ! Region 5: High Temperature Vapor Region
                region_id = IAPWS97_REGION_5
            else
                ! Out of Range
                region_id = IAPWS_OUT_OF_RANGE
            end if
            return
        end if

        if (present(is_supercooled)) then
            if (is_supercooled) then
                ! 過冷却状態として扱う場合、氷領域判定をスキップしてRegion 1とする
                region_id = IAPWS97_REGION_1
                return
            end if
        end if

        if (T_in >= water_triple_point_temperature) then
            if (T_in >= water_critical_point_temperature) then
                p_sat = calc_p_boundary_iapws97_region23(T_in)
                if (P_in > p_sat) then
                    region_id = IAPWS97_REGION_3
                else
                    region_id = IAPWS97_REGION_2
                end if
                return
            else
                p_sat = calc_psat_iapws97_region4(T_in)
                if (P_in > p_sat) then
                    region_id = IAPWS97_REGION_1
                else
                    region_id = IAPWS97_REGION_2
                end if
                return
            end if
        else
        end if

    end function determine_iapws_region

end module physics_material_iapws_wrapper
