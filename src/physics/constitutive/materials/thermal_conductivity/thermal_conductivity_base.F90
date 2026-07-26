submodule(materials_thermal_conductivity) thermal_conductivity_base
    implicit none
contains
    module subroutine initialize_abst_thc(self, material_id, constitutive_info, water, ice)
        implicit none
        class(abst_thc), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_config_constitutive), intent(in) :: constitutive_info
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        self%material_id = material_id

        self%material1 = constitutive_info%solid
        self%material2 = constitutive_info%water
        self%material3 = constitutive_info%ice
        self%material4 = constitutive_info%vapor

        if (allocated(constitutive_info%dispersivity)) then
            call allocate_array(self%dispersivity, source=constitutive_info%dispersivity)
            self%use_dispersivity = .true.
        end if

        if (allocated(constitutive_info%params)) then
            call allocate_array(self%params, source=constitutive_info%params)
        end if

        self%water => water
        self%ice => ice

        self%initialized = .true.
    end subroutine initialize_abst_thc

    module subroutine initialize_holder_thcs(self, material_id, constitutive_info, water, ice)
        implicit none
        class(holder_thcs), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_config_constitutive), intent(in) :: constitutive_info
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        select case (constitutive_info%num_phases)
        case (1)
            allocate (type_thc_1phase :: self%p)
            call self%p%initialize(material_id, constitutive_info, water, ice)
        case (2)
            allocate (type_thc_2phase :: self%p)
            call self%p%initialize(material_id, constitutive_info, water, ice)
        case (3)
            allocate (type_thc_3phase :: self%p)
            call self%p%initialize(material_id, constitutive_info, water, ice)
        case (4)
            allocate (type_thc_4phase :: self%p)
            call self%p%initialize(material_id, constitutive_info, water, ice)
        end select

    end subroutine initialize_holder_thcs

    module subroutine calc_thc_2(lambda_soil, phi_soil, &
                                 lambda_water, phi_water, lambda)
        implicit none
        real(real64), intent(in) :: lambda_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: lambda_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(inout) :: lambda

        lambda = lambda_soil**phi_soil &
                 * lambda_water**phi_water

    end subroutine calc_thc_2

    module subroutine calc_thc_3(lambda_soil, phi_soil, &
                                 lambda_water, phi_water, &
                                 lambda_ice, phi_ice, lambda)
        implicit none
        real(real64), intent(in) :: lambda_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: lambda_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: lambda_ice
        real(real64), intent(in) :: phi_ice
        real(real64), intent(inout) :: lambda

        lambda = lambda_soil**phi_soil &
                 * lambda_water**phi_water &
                 * lambda_ice**phi_ice

    end subroutine calc_thc_3

    module subroutine calc_thc_4(lambda_soil, phi_soil, &
                                 lambda_water, phi_water, &
                                 lambda_ice, phi_ice, &
                                 lambda_vapor, phi_vapor, lambda)
        implicit none
        real(real64), intent(in) :: lambda_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: lambda_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: lambda_ice
        real(real64), intent(in) :: phi_ice
        real(real64), intent(in) :: lambda_vapor
        real(real64), intent(in) :: phi_vapor
        real(real64), intent(inout) :: lambda

        lambda = lambda_soil**phi_soil &
                 * lambda_water**phi_water &
                 * lambda_ice**phi_ice &
                 * lambda_vapor**phi_vapor

    end subroutine calc_thc_4

    !> Campbell (1985) apparent thermal conductivity with the frozen-soil
    !> extension of Hansson et al. (2004), Eq. [13b], [14] and [15]:
    !>
    !>   F        = 1 + F1 * theta_i**F2
    !>   lambda_0 = C1 + C2*(theta + F*theta_i)
    !>              - (C1 - C4)*exp{-[C3*(theta + F*theta_i)]**C5},  C5 = 4
    !>
    !> theta is the LIQUID water content only. Hansson et al. state this
    !> explicitly: "Equation [13b] cannot be used for subzero temperatures since
    !> ice conducts heat much better than water; hence, only the liquid water
    !> content is included in the present equation" - the ice enters solely
    !> through F*theta_i, which is what F is for.
    !>
    !> The gas-filled pore fraction must NOT be added here. It previously was
    !> (the caller passed get_phi's phi4), and because phi_w + phi_gas + phi_i
    !> equals the porosity that made the argument
    !>   theta_eff = porosity + F1*theta_i**(1+F2),
    !> i.e. independent of the liquid content: the conductivity stopped
    !> responding to drying at all, which is meaningless for a vadose-zone model
    !> and wrong for this correlation.
    module subroutine calc_thc_4_vadoze(A, B, C, D, F1, F2, phi_water, phi_ice, lambda)
        implicit none
        real(real64), intent(in) :: A, B, C, D
        real(real64), intent(in) :: F1, F2
        real(real64), intent(in) :: phi_water, phi_ice
        real(real64), intent(inout) :: lambda

        real(real64) :: F_ice
        real(real64) :: theta

        F_ice = 1.0d0 + F1 * phi_ice**F2
        theta = phi_water + F_ice * phi_ice

        lambda = A + B * theta - (A - D) * exp(-(C * theta)**4)

    end subroutine calc_thc_4_vadoze

    module subroutine calc_lambda_dispersivity_abst_thc(self, lambda_0, htc_water, q_x, q_y, q_z, lambda)
        implicit none
        class(abst_thc), intent(in) :: self
        real(real64), intent(in) :: lambda_0
        real(real64), intent(in) :: htc_water
        real(real64), intent(in) :: q_x
        real(real64), intent(in) :: q_y
        real(real64), intent(in) :: q_z
        type(type_state_thc), intent(inout) :: lambda

        real(real64) :: lambda_L, lambda_T
        real(real64) :: q_norm, inv_q_norm
        real(real64) :: alpha_L, alpha_T

        call lambda%reset()

        if (self%use_dispersivity) then

            lambda_L = self%dispersivity(1)
            lambda_T = self%dispersivity(2)

            q_norm = sqrt(q_x**2 + q_y**2 + q_z**2)

            if (q_norm <= tiny(1.0d0)) then
                lambda%lambda_xx = lambda_0
                lambda%lambda_yy = lambda_0
                lambda%lambda_zz = lambda_0
                return
            end if

            inv_q_norm = 1.0d0 / q_norm

            alpha_L = lambda_L * htc_water
            alpha_T = lambda_T * htc_water

            lambda%lambda_xx = lambda_0 + (alpha_L * q_x**2 + alpha_T * (q_y**2 + q_z**2)) * inv_q_norm
            lambda%lambda_yy = lambda_0 + (alpha_L * q_y**2 + alpha_T * (q_z**2 + q_x**2)) * inv_q_norm
            lambda%lambda_zz = lambda_0 + (alpha_L * q_z**2 + alpha_T * (q_x**2 + q_y**2)) * inv_q_norm

            lambda%lambda_xy = (alpha_L - alpha_T) * q_x * q_y * inv_q_norm
            lambda%lambda_yz = (alpha_L - alpha_T) * q_y * q_z * inv_q_norm
            lambda%lambda_zx = (alpha_L - alpha_T) * q_z * q_x * inv_q_norm

        else
            lambda%lambda_xx = lambda_0
            lambda%lambda_yy = lambda_0
            lambda%lambda_zz = lambda_0
        end if

    end subroutine calc_lambda_dispersivity_abst_thc

end submodule thermal_conductivity_base
