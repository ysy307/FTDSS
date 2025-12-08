submodule(physics_material_thermal_conductivity) thermal_conductivity_base
    implicit none
contains
    module subroutine initialize_holder_thcs(self, material_id, physics_info, water, ice)
        implicit none
        class(holder_thcs), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_physics_info), intent(in) :: physics_info
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        select case (physics_info%num_phases)
        case (3)
            allocate (type_thc_3phase :: self%p)
            call self%p%initialize(material_id, physics_info, water, ice)
        end select

    end subroutine initialize_holder_thcs

    module pure elemental subroutine reset_thc_dispersity(self)
        implicit none
        class(type_thc_dispersity), intent(inout) :: self

        self%lambda_xx = 0.0d0
        self%lambda_yy = 0.0d0
        self%lambda_zz = 0.0d0
        self%lambda_xy = 0.0d0
        self%lambda_yz = 0.0d0
        self%lambda_zx = 0.0d0

    end subroutine reset_thc_dispersity

    module pure elemental subroutine calc_thc_2(lambda_soil, phi_soil, &
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

    module pure elemental subroutine calc_thc_3(lambda_soil, phi_soil, &
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

    module pure elemental subroutine calc_thc_4(lambda_soil, phi_soil, &
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

    module pure elemental subroutine calc_thc_dispersity(lambda_0, lambda_T, lambda_L, &
                                                         htc_water, q_x, q_y, q_z, lambda)
        implicit none
        real(real64), intent(in) :: lambda_0
        real(real64), intent(in) :: lambda_T
        real(real64), intent(in) :: lambda_L
        real(real64), intent(in) :: htc_water
        real(real64), intent(in) :: q_x
        real(real64), intent(in) :: q_y
        real(real64), intent(in) :: q_z
        type(type_thc_dispersity), intent(inout) :: lambda

        real(real64) :: q_norm, inv_q_norm
        real(real64) :: alpha_L, alpha_T

        call lambda%reset()

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

    end subroutine calc_thc_dispersity

end submodule thermal_conductivity_base
