submodule(conditions_boundary) conditions_boundary_adiabatic
    implicit none
contains

    module subroutine initialize_type_bc_thermal_adiabatic(self, input, domain, id, i_material, time_conv)
        implicit none
        class(type_bc_thermal_adiabatic), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_domain), intent(inout) :: domain
        integer(int32), intent(in) :: id
        integer(int32), intent(in) :: i_material
        real(real64), intent(in) :: time_conv

        integer(int32) :: i
        integer(int32), allocatable :: tmp_indices(:)

        self%material_id = input%conditions%boundary_conditions(id)%id
        self%boundary_name = input%conditions%boundary_conditions(id)%thermal%type

        !! Time settings
        if (allocated(self%time_points)) deallocate (self%time_points)
        allocate (self%time_points, source=input%conditions%time_control%boundary_time_points)
        self%time_points = self%time_points * time_conv

        if (allocated(self%values)) deallocate (self%values)
        allocate (self%values, source=input%conditions%boundary_conditions(id)%thermal%values)

        call find_target_edges_by_group(domain, i_material, self%target_edges)
        self%num_target_edges = size(self%target_edges, 2)

        select case (input%basic%solver_settings%reordering)
        case ("cm", "rcm")
            call allocate_array(tmp_indices, 2_int32)
            do i = 1, self%num_target_edges
                call domain%reordering%to_reordered(self%target_edges(:, i), tmp_indices)
                self%target_edges(:, i) = tmp_indices(:)
            end do
            call deallocate_array(tmp_indices)
        end select

    end subroutine initialize_type_bc_thermal_adiabatic

    module subroutine apply_Dense_Thermal_Adiabatic(self, current_time, A, b, Domain, mode)
        implicit none
        class(type_bc_thermal_adiabatic), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(inout), optional :: A(:, :)
        real(real64), intent(inout) :: b(:)
        type(type_domain), intent(inout) :: domain
        integer(int32), intent(in), optional :: mode

    end subroutine apply_Dense_Thermal_Adiabatic

    module subroutine apply_CRS_Thermal_Adiabatic(self, current_time, A, b, Domain, mode)
        implicit none
        class(type_bc_thermal_adiabatic), intent(in) :: self
        real(real64), intent(in) :: current_time
        type(Type_CRS), intent(inout), optional :: A
        real(real64), intent(inout) :: b(:)
        type(type_domain), intent(inout) :: domain
        integer(int32), intent(in), optional :: mode

        ! real(real64) :: Dval, timeCoe
        ! integer(int32) :: idx, iEdge

        ! if (.not. present(mode)) then
        !     call Calc_Time_Coefficients(current_time, self%time_points, timeCoe, idx)
        !     Dval = (self%values(idx) * (1.0d0 - timeCoe) + self%values(idx + 1) * timeCoe)

        !     do iEdge = 1, self%num_target_edges
        !         call apply_CRS_Adiabatic_base(A=A, &
        !                                       b=b, &
        !                                       isUniform=self%is_uniform, &
        !                                       Edge=self%target_edges(:, iEdge), &
        !                                       Dval=Dval, &
        !                                       perm=Domain%RCM_perm)
        !     end do
        ! else
        !     select case (mode)
        !     case (0)
        !         call Calc_Time_Coefficients(current_time, self%time_points, timeCoe, idx)
        !         Dval = (self%values(idx) * (1.0d0 - timeCoe) + self%values(idx + 1) * timeCoe)
        !     case (1)
        !         !! Newton-Raphson step
        !         Dval = 0.0d0
        !     case (2)
        !         !! initial condition
        !         Dval = self%values(1)
        !     end select

        !     do iEdge = 1, self%num_target_edges
        !         call apply_CRS_Adiabatic_base(A=A, &
        !                                       b=b, &
        !                                       isUniform=self%is_uniform, &
        !                                       Edge=self%target_edges(:, iEdge), &
        !                                       Dval=Dval, &
        !                                       perm=Domain%RCM_perm)
        !     end do
        ! end if

    end subroutine apply_CRS_Thermal_Adiabatic

    subroutine apply_CRS_Adiabatic_base(A, b, isUniform, Edge, Dval, perm)
        implicit none
        type(Type_CRS), intent(inout), optional :: A
        real(real64), intent(inout) :: b(:)
        logical(logical32), intent(in) :: isUniform
        integer(int32), intent(in) :: Edge(2)
        real(real64), intent(in) :: Dval
        integer(int32), intent(in) :: perm(:)

        integer(int32) :: i, ind, ps, pe
        integer(int32) :: p1, p2

        ! if (isUniform) then
        !     p1 = perm(Edge(1))
        !     p2 = perm(Edge(2))

        !     if (present(A)) then
        !         call A%Find(p1, p1, ind)
        !         ps = A%Ptr(p1)
        !         pe = A%Ptr(p1 + 1) - 1
        !         A%val(ps:pe) = 0.0d0
        !         A%val(ind) = 1.0d0

        !         call A%Find(p2, p2, ind)
        !         ps = A%Ptr(p2)
        !         pe = A%Ptr(p2 + 1) - 1
        !         A%val(ps:pe) = 0.0d0
        !         A%val(ind) = 1.0d0
        !     end if

        !     b(p1) = Dval
        !     b(p2) = Dval
        ! end if

    end subroutine apply_CRS_Adiabatic_base

    subroutine apply_Dense_Adiabatic_base(A, b, isUniform, Edge, Dval, perm)
        implicit none
        real(real64), intent(inout), optional :: A(:, :)
        real(real64), intent(inout) :: b(:)
        logical(logical32), intent(in) :: isUniform
        integer(int32), intent(in) :: Edge(2)
        real(real64), intent(in) :: Dval
        integer(int32), intent(in) :: perm(:)

        integer(int32) :: i, ind, ps, pe
        integer(int32) :: p1, p2

        ! if (isUniform) then
        !     p1 = perm(Edge(1))
        !     p2 = perm(Edge(2))

        !     if (present(A)) then
        !         A(p1, :) = 0.0d0
        !         A(p1, p1) = 1.0d0

        !         A(p2, :) = 0.0d0
        !         A(p2, p2) = 1.0d0
        !     end if

        !     b(p1) = Dval
        !     b(p2) = Dval
        ! end if

    end subroutine apply_Dense_Adiabatic_base

end submodule conditions_boundary_adiabatic
