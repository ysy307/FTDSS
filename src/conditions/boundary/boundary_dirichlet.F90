submodule(conditions_boundary) conditions_boundary_dirichlet
    implicit none
contains

    module function construct_type_bc_thermal_dirichlet(input, domain, controls, id) result(structure)
        implicit none
        type(type_input), intent(in) :: input
        type(type_domain), intent(in) :: domain
        type(type_controls), intent(in) :: controls
        integer(int32), intent(in) :: id
        class(abst_bc), allocatable :: structure

        real(real64) :: time_conv

        allocate (type_bc_thermal_dirichlet :: structure)

        select type (this => structure)
        type is (type_bc_thermal_dirichlet)
            this%group_id = input%conditions%boundary_conditions(id)%id

            if (allocated(this%time_points)) deallocate (this%time_points)
            allocate (this%time_points, source=input%conditions%time_control%boundary_time_points)
            time_conv = controls%time%convert_time_unit(trim(input%conditions%time_control%simulation_period%unit), "second")
            this%time_points = this%time_points * time_conv

            if (allocated(this%values)) deallocate (this%values)
            allocate (this%values, source=input%conditions%boundary_conditions(id)%thermal%values)

            this%dimension = input%basic%simulation_settings%calculate_dimension - 1
            call find_target_by_group(domain, this%dimension, this%group_id, this%target_ids)
        end select
    end function construct_type_bc_thermal_dirichlet

    module subroutine apply_thermal_dirichlet(self, current_time, A, b, domain, mode)
        implicit none
        class(type_bc_thermal_dirichlet), intent(in) :: self
        real(real64), intent(in) :: current_time
        type(type_jacobian_matrix), intent(inout), optional :: A
        type(type_residual_vector), intent(inout) :: b
        type(type_domain), intent(inout), target :: domain
        integer(int32), intent(in), optional :: mode

        integer(int32) :: i, j, idx
        real(real64) :: bc_value, timeCoe
        class(abst_mesh), pointer :: mesh
        integer(int32), dimension(:), pointer :: p_conn => null()

        if (present(mode)) then
            select case (mode)
            case (mode_value)
                ! 現在の時間に基づいてディリクレ境界条件の値を計算
                call calculate_time_coefficient(current_time, self%time_points, timeCoe, idx)
                bc_value = (self%values(idx) * (1.0d0 - timeCoe) + self%values(idx + 1) * timeCoe)
            case (mode_nr)
                !! Newton-Raphson step
                bc_value = 0.0d0
            case (mode_ic)
                !! initial condition
                bc_value = self%values(1)
            end select
        end if

        if (present(A)) then
            do i = 1, self%num_targets
                select case (self%dimension)
                case (1)
                    ! 1Dの場合の処理
                case (2)
                    mesh => domain%sides(i)%s
                    p_conn => mesh%get_connectivity_ptr()
                    do j = 1, mesh%get_num_nodes()
                        call A%set(p_conn(j), 0.0d0)
                        call A%set(p_conn(j), p_conn(j), 1.0d0)
                    end do
                    do j = 1, mesh%get_num_nodes()
                        call b%set(p_conn(j), bc_value)
                    end do
                case (3)
                    ! 3Dの場合の処理
                end select
            end do
        else
            do i = 1, self%num_targets
                select case (self%dimension)
                case (1)
                    ! 1Dの場合の処理
                case (2)
                    mesh => domain%sides(i)%s
                    p_conn => mesh%get_connectivity_ptr()
                    do j = 1, mesh%get_num_nodes()
                        call b%set(p_conn(j), bc_value)
                    end do
                case (3)
                    ! 3Dの場合の処理
                end select
            end do
        end if
    end subroutine apply_thermal_dirichlet

end submodule conditions_boundary_dirichlet
