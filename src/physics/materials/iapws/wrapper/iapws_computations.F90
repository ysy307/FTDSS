submodule(physics_material_iapws_wrapper) iapws_computations
    implicit none
contains

    module pure elemental subroutine calc_iapws_properties(T_in, P_in, properties, is_supercooled)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        type(type_iapws_property), intent(inout) :: properties
        logical, intent(in), optional :: is_supercooled

        integer(int32) :: region_id

        ! 1. 領域の特定
        region_id = determine_iapws_region(T_in, P_in, is_supercooled)

        ! 2. 各領域の計算ルーチンを呼び出し
        select case (region_id)
        case (IAPWS97_R1_LIQ)
            call calc_iapws97_region1_properties(T_in, P_in, properties, region_id)
        case (IAPWS97_R2_VAP)
            call calc_iapws97_region2_properties(T_in, P_in, properties, region_id)
        case (IAPWS97_R3_CRIT)
            call calc_iapws97_region3_properties(T_in, P_in, properties, region_id)
        case (IAPWS97_R5_GAS)
            call calc_iapws97_region5_properties(T_in, P_in, properties, region_id)
        case (IAPWS06_ICE_IH)
            call calc_iapws06_Ih_properties(T_in, P_in, properties, region_id)
        case default
            properties%nu = 0.0d0
            properties%rho = 0.0d0
            properties%u = 0.0d0
            properties%s = 0.0d0
            properties%h = 0.0d0
            properties%cp = 0.0d0
            properties%cv = 0.0d0
            properties%w = 0.0d0
            properties%p = 0.0d0
            properties%alpha = 0.0d0
            properties%beta = 0.0d0
            properties%kappa_s = 0.0d0
            properties%kappa_T = 0.0d0
        end select

    end subroutine calc_iapws_properties

    pure elemental subroutine calc_iapws97_region1_properties(T_in, P_in, properties, region_id)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        type(type_iapws_property), intent(inout) :: properties
        integer(int32), intent(in) :: region_id

        properties%region_id = region_id
        properties%nu = calc_nu_iapws97_region1(T_in, P_in)
        properties%rho = calc_rho_iapws97_region1(T_in, P_in)
        properties%u = calc_u_iapws97_region1(T_in, P_in)
        properties%s = calc_s_iapws97_region1(T_in, P_in)
        properties%h = calc_h_iapws97_region1(T_in, P_in)
        properties%cp = calc_cp_iapws97_region1(T_in, P_in)
        properties%cv = calc_cv_iapws97_region1(T_in, P_in)
        properties%w = calc_w_iapws97_region1(T_in, P_in)
        properties%p = P_in
        properties%T = T_in
        properties%alpha = 0.0d0
        properties%beta = 0.0d0
        properties%kappa_s = 0.0d0
        properties%kappa_T = 0.0d0
    end subroutine calc_iapws97_region1_properties

    pure elemental subroutine calc_iapws97_region2_properties(T_in, P_in, properties, region_id)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        type(type_iapws_property), intent(inout) :: properties
        integer(int32), intent(in) :: region_id

        properties%region_id = region_id
        properties%nu = calc_nu_iapws97_region2(T_in, P_in)
        properties%rho = calc_rho_iapws97_region2(T_in, P_in)
        properties%u = calc_u_iapws97_region2(T_in, P_in)
        properties%s = calc_s_iapws97_region2(T_in, P_in)
        properties%h = calc_h_iapws97_region2(T_in, P_in)
        properties%cp = calc_cp_iapws97_region2(T_in, P_in)
        properties%cv = calc_cv_iapws97_region2(T_in, P_in)
        properties%w = calc_w_iapws97_region2(T_in, P_in)
        properties%p = P_in
        properties%T = T_in
        properties%alpha = 0.0d0
        properties%beta = 0.0d0
        properties%kappa_s = 0.0d0
        properties%kappa_T = 0.0d0
    end subroutine calc_iapws97_region2_properties

    pure elemental subroutine calc_iapws97_region3_properties(T_in, P_in, properties, region_id)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        type(type_iapws_property), intent(inout) :: properties
        integer(int32), intent(in) :: region_id

        properties%region_id = region_id
        properties%nu = calc_nu_iapws97_region3(T_in, P_in, properties%region_id)
        properties%rho = calc_rho_iapws97_region3(T_in, P_in, properties%region_id)
        properties%u = calc_u_iapws97_region3(T_in, properties%rho)
        properties%s = calc_s_iapws97_region3(T_in, properties%rho)
        properties%h = calc_h_iapws97_region3(T_in, properties%rho)
        properties%cp = calc_cp_iapws97_region3(T_in, properties%rho)
        properties%cv = calc_cv_iapws97_region3(T_in, properties%rho)
        properties%w = calc_w_iapws97_region3(T_in, properties%rho)
        properties%p = calc_p_iapws97_region3(T_in, properties%rho)
        properties%alpha = 0.0d0
        properties%beta = 0.0d0
        properties%kappa_s = 0.0d0
        properties%kappa_T = 0.0d0
    end subroutine calc_iapws97_region3_properties

    pure elemental subroutine calc_iapws97_region5_properties(T_in, P_in, properties, region_id)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        type(type_iapws_property), intent(inout) :: properties
        integer(int32), intent(in) :: region_id

        properties%region_id = region_id
        properties%nu = calc_nu_iapws97_region5(T_in, P_in)
        properties%rho = calc_rho_iapws97_region5(T_in, P_in)
        properties%u = calc_u_iapws97_region5(T_in, P_in)
        properties%s = calc_s_iapws97_region5(T_in, P_in)
        properties%h = calc_h_iapws97_region5(T_in, P_in)
        properties%cp = calc_cp_iapws97_region5(T_in, P_in)
        properties%cv = calc_cv_iapws97_region5(T_in, P_in)
        properties%w = calc_w_iapws97_region5(T_in, P_in)
        properties%p = P_in
        properties%T = T_in
        properties%alpha = 0.0d0
        properties%beta = 0.0d0
        properties%kappa_s = 0.0d0
        properties%kappa_T = 0.0d0
    end subroutine calc_iapws97_region5_properties

    pure elemental subroutine calc_iapws06_Ih_properties(T_in, P_in, properties, region_id)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        type(type_iapws_property), intent(inout) :: properties
        integer(int32), intent(in) :: region_id

        properties%region_id = region_id
        properties%nu = calc_nu_iapws06_Ih(T_in, P_in)
        properties%rho = calc_rho_iapws06_Ih(T_in, P_in)
        properties%u = calc_u_iapws06_Ih(T_in, P_in)
        properties%s = calc_s_iapws06_Ih(T_in, P_in)
        properties%h = calc_h_iapws06_Ih(T_in, P_in)
        properties%cp = calc_cp_iapws06_Ih(T_in, P_in)
        properties%cv = 0.0d0
        properties%w = 0.0d0
        properties%p = P_in
        properties%T = T_in
        properties%alpha = calc_alpha_iapws06_Ih(T_in, P_in)
        properties%beta = calc_beta_iapws06_Ih(T_in, P_in)
        properties%kappa_s = calc_kappa_T_iapws06_Ih(T_in, P_in)
        properties%kappa_T = calc_kappa_T_iapws06_Ih(T_in, P_in)
    end subroutine calc_iapws06_Ih_properties

end submodule iapws_computations
