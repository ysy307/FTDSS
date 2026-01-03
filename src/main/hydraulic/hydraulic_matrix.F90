submodule(main_hydraulic) hydraulic_matrix
    implicit none
contains

    module pure elemental subroutine compute_C_H(self, target_id, state, C_HH, C_HT)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout), optional :: C_HH
        real(real64), intent(inout), optional :: C_HT

        real(real64) :: Qw, Qi, Qv
        real(real64) :: rho_w, rho_i
        real(real64) :: drho_w_dT, drho_ice_dT
        real(real64) :: drho_w_dP, drho_ice_dP
        real(real64) :: dP_ice_dP_water

        real(real64) :: dQw_dT, dQi_dT, dQv_dT
        real(real64) :: dQw_dP, dQi_dP, dQv_dP

        ! Get state variables
        call self%physics%update_water_phases(target_id, state)
        call state%water_content%get(Qw)
        call state%ice_content%get(Qi)
        call state%vapor_content%get(Qv)

        ! Get derivatives of contents (from retention curve / freezing curve)
        call state%dQw_dT%get(dQw_dT)
        call state%dQi_dT%get(dQi_dT)
        call state%dQv_dT%get(dQv_dT)
        call state%dQw_dP%get(dQw_dP)
        call state%dQi_dP%get(dQi_dP)
        call state%dQv_dP%get(dQv_dP)

        ! Get densities and their derivatives
        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_density_ice(state, rho_i)
        call self%physics%calc_density_water_derivatives(target_id, state, drho_w_dT, drho_w_dP)
        call self%physics%calc_density_ice_derivatives(target_id, state, drho_ice_dT, drho_ice_dP)
        call self%physics%calc_pressure_ice_water_derivative(target_id, state, dP_ice_dP_water)

        if (present(C_HH)) then
            ! C_HH = d(rho_void)/dP_w
            C_HH = rho_w * dQw_dP + Qw * drho_w_dP &
                   + rho_i * dQi_dP + Qi * drho_ice_dP * dP_ice_dP_water &
                   + rho_w * dQv_dP + Qv * drho_w_dP
        end if

        if (present(C_HT)) then
            ! C_HT = d(rho_void)/dT
            C_HT = rho_w * dQw_dT + Qw * drho_w_dT &
                   + rho_i * dQi_dT + Qi * drho_ice_dT &
                   + rho_w * dQv_dT + Qv * drho_w_dT
        end if

    end subroutine compute_C_H

    module pure subroutine compute_D_H(self, target_id, state, D_HH, D_HT)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout), optional :: D_HH(:, :)
        real(real64), intent(inout), optional :: D_HT(:, :)

        real(real64) :: rho_w
        real(real64) :: K_flh, K_wT
        real(real64) :: K_vP, K_vT
        real(real64) :: coeff_mass_HH, coeff_mass_HT
        integer(int32) :: i

        ! 1. Get Properties
        call self%physics%calc_density_water(state, rho_w)

        ! 2. Get Transport Coefficients
        ! K_flh: Liquid Hydraulic (K_P)
        ! K_wT : Liquid Thermal Osmosis (K_T)
        ! K_vP : Vapor Hydraulic
        ! K_vT : Vapor Thermal
        call self%physics%calc_Kflh(target_id, state, K_flh)
        call self%physics%calc_KlT(target_id, state, K_wT)
        call self%physics%calc_Kvh(target_id, state, K_vP)
        call self%physics%calc_KvT(target_id, state, K_vT)

        ! 3. Calculate D_HH (Pressure Gradient Term)
        ! D_HH = rho_w * (K_P + K_vP) * I
        if (present(D_HH)) then
            D_HH(:, :) = 0.0d0
            coeff_mass_HH = rho_w * (K_flh + K_vP)

            do i = 1, self%computation_dimension
                D_HH(i, i) = coeff_mass_HH
            end do
        end if

        ! 4. Calculate D_HT (Temperature Gradient Term)
        ! D_HT = rho_w * (K_T + K_vT) * I
        if (present(D_HT)) then
            D_HT(:, :) = 0.0d0
            coeff_mass_HT = rho_w * (K_wT + K_vT)

            do i = 1, self%computation_dimension
                D_HT(i, i) = coeff_mass_HT
            end do
        end if

    end subroutine compute_D_H

    module pure subroutine compute_V_H(self, target_id, state, V_HH, V_HT)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout), optional :: V_HH(:)
        real(real64), intent(inout), optional :: V_HT(:)

        real(real64) :: rho_w, drho_w_dT, drho_w_dP
        real(real64) :: K_P, K_T, K_wP
        ! Derivatives of conductivities w.r.t Temperature
        real(real64) :: dKp_dT, dKwP_dT, dKT_dT
        ! Derivatives of conductivities w.r.t Pressure
        real(real64) :: dKp_dP, dKwP_dP, dKT_dP

        real(real64) :: gravity_mag
        type(type_coordinate_dp) :: grad_P, grad_T, grad_z

        ! Initialize gradients
        call state%grad_P%get(grad_P)
        call state%grad_T%get(grad_T)

        ! Set gravity vector
        gravity_mag = 9.81d0
        select case (self%computation_type)
        case (COMP_TYPE_2D_XY)
            grad_z%x = 0.0d0
            grad_z%y = 1.0d0
        case (COMP_TYPE_2D_XZ)
            grad_z%x = 0.0d0
            grad_z%z = 1.0d0
        case (COMP_TYPE_3D)
            grad_z%x = 0.0d0
            grad_z%y = 0.0d0
            grad_z%z = 1.0d0
        end select

        ! Get properties
        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_density_water_derivatives(target_id, state, drho_w_dT, drho_w_dP)

        call self%physics%calc_Kflh(target_id, state, K_P)
        call self%physics%calc_KlT(target_id, state, K_T)

        ! K_wP calculation
        K_wP = K_P / max(rho_w * gravity_mag, 1.0d-10)

        ! ----------------------------------------------------------------------
        ! Note: Derivatives of K (Conductivity)
        ! For now, these are set to 0.0.
        ! In unsaturated soil, dKp_dP is significant (slope of K-function).
        ! ----------------------------------------------------------------------
        dKp_dT = 0.0d0
        dKwP_dT = 0.0d0
        dKT_dT = 0.0d0
        dKp_dP = 0.0d0
        dKwP_dP = 0.0d0
        dKT_dP = 0.0d0

        ! V_HH: Pressure sensitivity vector
        ! V_PP = (drho/dP * K_P + rho * dKP/dP) * grad P
        !        - (2*rho*drho/dP * K_wP + rho^2 * dKwP/dP) * g * grad z
        !        + (drho/dP * K_T + rho * dKT/dP) * grad T
        if (present(V_HH)) then
            select case (self%computation_type)
            case (COMP_TYPE_2D_XY)
                V_HH(1) = (drho_w_dP * K_P + rho_w * dKp_dP) * grad_P%x &
                          - (2.0d0 * rho_w * drho_w_dP * K_wP + rho_w**2 * dKwP_dP) * gravity_mag * grad_z%x &
                          + (drho_w_dP * K_T + rho_w * dKT_dP) * grad_T%x

                V_HH(2) = (drho_w_dP * K_P + rho_w * dKp_dP) * grad_P%y &
                          - (2.0d0 * rho_w * drho_w_dP * K_wP + rho_w**2 * dKwP_dP) * gravity_mag * grad_z%y &
                          + (drho_w_dP * K_T + rho_w * dKT_dP) * grad_T%y

            case (COMP_TYPE_2D_XZ)
                V_HH(1) = (drho_w_dP * K_P + rho_w * dKp_dP) * grad_P%x &
                          - (2.0d0 * rho_w * drho_w_dP * K_wP + rho_w**2 * dKwP_dP) * gravity_mag * grad_z%x &
                          + (drho_w_dP * K_T + rho_w * dKT_dP) * grad_T%x

                V_HH(2) = (drho_w_dP * K_P + rho_w * dKp_dP) * grad_P%z &
                          - (2.0d0 * rho_w * drho_w_dP * K_wP + rho_w**2 * dKwP_dP) * gravity_mag * grad_z%z &
                          + (drho_w_dP * K_T + rho_w * dKT_dP) * grad_T%z

            case (COMP_TYPE_3D)
                V_HH(1) = (drho_w_dP * K_P + rho_w * dKp_dP) * grad_P%x &
                          - (2.0d0 * rho_w * drho_w_dP * K_wP + rho_w**2 * dKwP_dP) * gravity_mag * grad_z%x &
                          + (drho_w_dP * K_T + rho_w * dKT_dP) * grad_T%x

                V_HH(2) = (drho_w_dP * K_P + rho_w * dKp_dP) * grad_P%y &
                          - (2.0d0 * rho_w * drho_w_dP * K_wP + rho_w**2 * dKwP_dP) * gravity_mag * grad_z%y &
                          + (drho_w_dP * K_T + rho_w * dKT_dP) * grad_T%y

                V_HH(3) = (drho_w_dP * K_P + rho_w * dKp_dP) * grad_P%z &
                          - (2.0d0 * rho_w * drho_w_dP * K_wP + rho_w**2 * dKwP_dP) * gravity_mag * grad_z%z &
                          + (drho_w_dP * K_T + rho_w * dKT_dP) * grad_T%z
            end select
        end if

        ! V_HT: Temperature sensitivity vector
        if (present(V_HT)) then
            select case (self%computation_type)
            case (COMP_TYPE_2D_XY)
                V_HT(1) = (drho_w_dT * K_P + rho_w * dKp_dT) * grad_P%x &
                          - (2.0d0 * rho_w * drho_w_dT * K_wP + rho_w**2 * dKwP_dT) * gravity_mag * grad_z%x &
                          + (drho_w_dT * K_T + rho_w * dKT_dT) * grad_T%x

                V_HT(2) = (drho_w_dT * K_P + rho_w * dKp_dT) * grad_P%y &
                          - (2.0d0 * rho_w * drho_w_dT * K_wP + rho_w**2 * dKwP_dT) * gravity_mag * grad_z%y &
                          + (drho_w_dT * K_T + rho_w * dKT_dT) * grad_T%y

            case (COMP_TYPE_2D_XZ)
                V_HT(1) = (drho_w_dT * K_P + rho_w * dKp_dT) * grad_P%x &
                          - (2.0d0 * rho_w * drho_w_dT * K_wP + rho_w**2 * dKwP_dT) * gravity_mag * grad_z%x &
                          + (drho_w_dT * K_T + rho_w * dKT_dT) * grad_T%x

                V_HT(2) = (drho_w_dT * K_P + rho_w * dKp_dT) * grad_P%z &
                          - (2.0d0 * rho_w * drho_w_dT * K_wP + rho_w**2 * dKwP_dT) * gravity_mag * grad_z%z &
                          + (drho_w_dT * K_T + rho_w * dKT_dT) * grad_T%z

            case (COMP_TYPE_3D)
                V_HT(1) = (drho_w_dT * K_P + rho_w * dKp_dT) * grad_P%x &
                          - (2.0d0 * rho_w * drho_w_dT * K_wP + rho_w**2 * dKwP_dT) * gravity_mag * grad_z%x &
                          + (drho_w_dT * K_T + rho_w * dKT_dT) * grad_T%x

                V_HT(2) = (drho_w_dT * K_P + rho_w * dKp_dT) * grad_P%y &
                          - (2.0d0 * rho_w * drho_w_dT * K_wP + rho_w**2 * dKwP_dT) * gravity_mag * grad_z%y &
                          + (drho_w_dT * K_T + rho_w * dKT_dT) * grad_T%y

                V_HT(3) = (drho_w_dT * K_P + rho_w * dKp_dT) * grad_P%z &
                          - (2.0d0 * rho_w * drho_w_dT * K_wP + rho_w**2 * dKwP_dT) * gravity_mag * grad_z%z &
                          + (drho_w_dT * K_T + rho_w * dKT_dT) * grad_T%z
            end select
        end if

    end subroutine compute_V_H

    module pure subroutine compute_R_H(self, target_id, state, R_H_C, R_H_D)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: R_H_C
        real(real64), intent(inout) :: R_H_D(:)

        real(real64) :: dot_P, dot_T
        real(real64) :: C_HH, C_HT
        real(real64) :: rho_w
        type(type_coordinate_dp) :: water_flux, vapor_flux

        ! 1. Calculate Storage Term (R_H_C)
        ! R_H_C = C_HH * dP/dt + C_HT * dT/dt
        call state%dot_P%get(dot_P)
        call state%dot_T%get(dot_T)

        call self%compute_C_H(target_id, state, C_HH, C_HT)

        R_H_C = C_HH * dot_P + C_HT * dot_T

        ! 2. Calculate Flux Term (R_H_D)
        ! R_H_D = Mass Flux = rho_w * (q_liquid + q_vapor)
        call state%water_flux%get(water_flux)
        call state%vapor_flux%get(vapor_flux)
        call self%physics%calc_density_water(state, rho_w)

        R_H_D(:) = 0.0d0
        select case (self%computation_type)
        case (COMP_TYPE_2D_XY)
            R_H_D(1) = rho_w * (water_flux%x + vapor_flux%x)
            R_H_D(2) = rho_w * (water_flux%y + vapor_flux%y)
        case (COMP_TYPE_2D_XZ)
            R_H_D(1) = rho_w * (water_flux%x + vapor_flux%x)
            R_H_D(2) = rho_w * (water_flux%z + vapor_flux%z)
        case (COMP_TYPE_3D)
            R_H_D(1) = rho_w * (water_flux%x + vapor_flux%x)
            R_H_D(2) = rho_w * (water_flux%y + vapor_flux%y)
            R_H_D(3) = rho_w * (water_flux%z + vapor_flux%z)
        end select

    end subroutine compute_R_H

end submodule hydraulic_matrix
