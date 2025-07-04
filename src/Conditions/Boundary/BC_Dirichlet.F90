submodule(Condition_Boundary) Condition_Boundary_Dirichlet
    ! use, intrinsic :: iso_fortran_env
    ! use :: Domain_Module, only:Domain_t
    ! use :: Matrix_CRS, only:Type_CRS
    ! use :: Inout_Input, only:Input_Boundary
    implicit none
contains

    module subroutine setup_Thermal_Dirichlet(self, Input_BC, time_conv, iGroup, Domain)
        implicit none
        class(BC_Thermal_Dirichlet), intent(inout) :: self
        type(Input_Boundary), intent(in) :: Input_BC
        real(real64), intent(in) :: time_conv
        integer(int32), intent(in) :: iGroup
        type(Domain_t), intent(in) :: Domain

        self%is_uniform = Input_BC%Heat(iGroup)%isUniform
        allocate (self%time_points, source=Input_BC%Time)
        self%time_points = self%time_points * time_conv
        allocate (self%values, source=Input_BC%Heat(iGroup)%value)

        call Find_Target_Edges_By_Group(Domain, Input_BC, iGroup, self%target_edges)
        self%num_target_edges = size(self%target_edges, 2)

    end subroutine setup_Thermal_Dirichlet

    module subroutine apply_Dense_Thermal_Dirichlet(self, current_time, A, b, Domain, mode)
        implicit none
        class(BC_Thermal_Dirichlet), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(inout), optional :: A(:, :)
        real(real64), intent(inout) :: b(:)
        type(Domain_t), intent(in) :: Domain
        integer(int32), intent(in), optional :: mode

    end subroutine apply_Dense_Thermal_Dirichlet

    module subroutine apply_CRS_Thermal_Dirichlet(self, current_time, A, b, Domain, mode)
        implicit none
        class(BC_Thermal_Dirichlet), intent(in) :: self
        real(real64), intent(in) :: current_time
        type(Type_CRS), intent(inout), optional :: A
        real(real64), intent(inout) :: b(:)
        type(Domain_t), intent(in) :: Domain
        integer(int32), intent(in), optional :: mode

        real(real64) :: Dval, timeCoe
        integer(int32) :: idx, iEdge

        if (present(A)) then
        if (.not. present(mode)) then
            call Calc_Time_Coefficients(current_time, self%time_points, timeCoe, idx)
            Dval = (self%values(idx) * (1.0d0 - timeCoe) + self%values(idx + 1) * timeCoe)

            do iEdge = 1, self%num_target_edges
                call apply_CRS_Dirichlet_base(A=A, &
                                              b=b, &
                                              isUniform=self%is_uniform, &
                                              Edge=self%target_edges(:, iEdge), &
                                              Dval=Dval, &
                                              perm=Domain%RCM_inv_perm)
            end do
        else
            select case (mode)
            case (0)
                call Calc_Time_Coefficients(current_time, self%time_points, timeCoe, idx)
                Dval = (self%values(idx) * (1.0d0 - timeCoe) + self%values(idx + 1) * timeCoe)
            case (1)
                !! Newton-Raphson step
                Dval = 0.0d0
            case (2)
                !! initial condition
                Dval = self%values(1)
            end select

            do iEdge = 1, self%num_target_edges
                call apply_CRS_Dirichlet_base(A=A, &
                                              b=b, &
                                              isUniform=self%is_uniform, &
                                              Edge=self%target_edges(:, iEdge), &
                                              Dval=Dval, &
                                              perm=Domain%RCM_inv_perm)
            end do
        end if
        else
        if (.not. present(mode)) then
            call Calc_Time_Coefficients(current_time, self%time_points, timeCoe, idx)
            Dval = (self%values(idx) * (1.0d0 - timeCoe) + self%values(idx + 1) * timeCoe)

            do iEdge = 1, self%num_target_edges
                call apply_Dense_Dirichlet_base(b=b, &
                                                isUniform=self%is_uniform, &
                                                Edge=self%target_edges(:, iEdge), &
                                                Dval=Dval, &
                                                perm=Domain%RCM_inv_perm)
            end do
        else
            select case (mode)
            case (0)
                call Calc_Time_Coefficients(current_time, self%time_points, timeCoe, idx)
                Dval = (self%values(idx) * (1.0d0 - timeCoe) + self%values(idx + 1) * timeCoe)
            case (1)
                !! Newton-Raphson step
                Dval = 0.0d0
            case (2)
                !! initial condition
                Dval = self%values(1)
            end select

            do iEdge = 1, self%num_target_edges
                call apply_Dense_Dirichlet_base(b=b, &
                                                isUniform=self%is_uniform, &
                                                Edge=self%target_edges(:, iEdge), &
                                                Dval=Dval, &
                                                perm=Domain%RCM_inv_perm)
            end do
        end if
        end if

    end subroutine apply_CRS_Thermal_Dirichlet

    subroutine apply_CRS_Dirichlet_base(A, b, isUniform, Edge, Dval, perm)
        implicit none
        type(Type_CRS), intent(inout), optional :: A
        real(real64), intent(inout) :: b(:)
        logical(logical32), intent(in) :: isUniform
        integer(int32), intent(in) :: Edge(2)
        real(real64), intent(in) :: Dval
        integer(int32), intent(in) :: perm(:)

        integer(int32) :: i, ind, ps, pe
        integer(int32) :: p1, p2

        if (isUniform) then
            p1 = perm(Edge(1))
            p2 = perm(Edge(2))

            if (present(A)) then
                call A%Find(p1, p1, ind)
                ps = A%Ptr(p1)
                pe = A%Ptr(p1 + 1) - 1
                A%val(ps:pe) = 0.0d0
                A%val(ind) = 1.0d0

                call A%Find(p2, p2, ind)
                ps = A%Ptr(p2)
                pe = A%Ptr(p2 + 1) - 1
                A%val(ps:pe) = 0.0d0
                A%val(ind) = 1.0d0
            end if

            b(p1) = Dval
            b(p2) = Dval
        end if

    end subroutine apply_CRS_Dirichlet_base

    subroutine apply_Dense_Dirichlet_base(A, b, isUniform, Edge, Dval, perm)
        implicit none
        real(real64), intent(inout), optional :: A(:, :)
        real(real64), intent(inout) :: b(:)
        logical(logical32), intent(in) :: isUniform
        integer(int32), intent(in) :: Edge(2)
        real(real64), intent(in) :: Dval
        integer(int32), intent(in) :: perm(:)

        integer(int32) :: i, ind, ps, pe
        integer(int32) :: p1, p2

        if (isUniform) then
            ! ! --- ここからデバッグ用コード ---
            ! print *, 'Debug: Edge = ', Edge(1), Edge(2)
            ! print *, 'Debug: size(perm) = ', size(perm)
            ! if (present(A)) then
            !     print *, 'Debug: shape(A) = ', shape(A)
            ! end if
            ! print *, 'Debug: size(b) = ', size(b)
            ! ! --- ここまでデバッグ用コード ---
            p1 = perm(Edge(1))
            p2 = perm(Edge(2))

            if (present(A)) then
                A(p1, :) = 0.0d0
                A(p1, p1) = 1.0d0

                A(p2, :) = 0.0d0
                A(p2, p2) = 1.0d0
            end if

            b(p1) = Dval
            b(p2) = Dval
        end if

    end subroutine apply_Dense_Dirichlet_base

end submodule Condition_Boundary_Dirichlet
