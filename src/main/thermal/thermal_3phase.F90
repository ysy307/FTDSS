submodule(Main_Thermal) Main_Thermal_3Phase
    implicit none
contains
    module function construct_type_thermal_3phase_2d(input, coordinate, domain) result(structure)
        implicit none
        class(abst_thermal), allocatable :: structure
        type(type_input), intent(inout) :: input
        type(type_dp_3d), intent(inout), pointer :: coordinate
        type(type_domain), intent(inout) :: domain

        integer(int32) :: i
        integer(int32) :: nNode

        integer(int32) :: ierr

        if (allocated(structure)) deallocate (structure)
        allocate (Type_Thermal_3Phase_2D :: structure)

        nNode = domain%get_num_nodes()

        call structure%KT_star_0%initialize(domain)

        structure%KT_l = structure%KT_star_0%copy()
        structure%KT_old = structure%KT_star_0%copy()
        structure%CT_l = structure%KT_star_0%copy()
        structure%order = input%basic%solver_settings%bdf_order

        allocate (structure%CT_old(structure%order))
        do i = 1, structure%order
            structure%CT_old(i) = structure%KT_star_0%copy()
        end do

        call allocate_array(structure%FT, nNode)
        call allocate_array(structure%FT_old, nNode)
        call allocate_array(structure%PHIT, nNode)
        call allocate_array(structure%PHIT_old, nNode)

        call structure%T%initialize(nNode, structure%order)

        !---------------------------------------------------------------------------------------------------------------------------
        ! 線形求解ソルバーの設定
        !---------------------------------------------------------------------------------------------------------------------------
        ! if (input%Solver_Thermal%useSolver == 1) then
        !     structure%Solver = Solver_CRS_LU_Constructor(N=nNode, &
        !                                                  MAXFCT=1, &
        !                                                  MNUM=1, &
        !                                                  MTYPE=1, &
        !                                                  PHASE=13, &
        !                                                  NRHS=1, &
        !                                                  MSGVLV=0, &
        !                                                  a=structure%KT_star_0)
        ! else if (input%Solver_Thermal%useSolver == 2) then
        !     if (input%Solver_Thermal%useSolverType == 4) then
        !         structure%Solver = Solver_CRS_BiCGSTAB_Constructor(N=nNode, &
        !                                                            tol=input%Solver_Thermal%tolerance, &
        !                                                            maxiter=input%Solver_Thermal%maxIteration, &
        !                                                            Preconditioner=input%Solver_Thermal%usePreconditionerType)
        !     end if
        ! end if
        !---------------------------------------------------------------------------------------------------------------------------

    end function construct_type_thermal_3phase_2d

    ! module subroutine Type_Thermal_3Phase_2D_Update(self, NodeBelonging, arr_phi)
    !     implicit none
    !     class(Type_Thermal_3Phase_2D), intent(inout) :: self
    !     type(Belonging), intent(inout), optional :: NodeBelonging(:)
    !     real(real64), intent(inout) :: arr_phi(:)

    !     call self%Ice(1)%f%Update_Ice(NodeBelonging=NodeBelonging, &
    !                                   arr_T=self%T%pre(:), &
    !                                   arr_phi=arr_phi(:), &
    !                                   Density=self%DEN, &
    !                                   arr_Cp=self%HTC%value(:, 1), &
    !                                   arr_Qw=self%Qw%pre(:), &
    !                                   arr_Qice=self%Qice%pre(:), &
    !                                   arr_Si=self%Si)

    !     call self%THC%Update(NodeBelonging, 1.0d0 - arr_phi(:), self%Qw%pre, self%Qice%pre)
    !     call self%SPH%Update(NodeBelonging, 1.0d0 - arr_phi(:), self%Qw%pre, self%Qice%pre)
    !     call self%DEN%Update(NodeBelonging, 1.0d0 - arr_phi(:), self%Qw%pre, self%Qice%pre)
    !     call self%HTC%Update(NodeBelonging=NodeBelonging, &
    !                          arr_phi1=1.0d0 - arr_phi(:), &
    !                          arr_phi2=self%Qw%pre, &
    !                          arr_phi3=self%Qice%pre, &
    !                          Ice=self%Ice(1)%f, &
    !                          Temperature=self%T%pre(:), &
    !                          Density=self%DEN)
    ! end subroutine Type_Thermal_3Phase_2D_Update
    module subroutine assemble_type_thermal_3phase_2d(self, domain, property, porosity, dt, step, iter)
        implicit none
        class(type_thermal_3phase_2d), intent(inout) :: self
        type(type_domain), intent(inout) :: domain
        type(type_proereties_manager), intent(inout) :: property
        real(real64), intent(in) :: porosity(:)
        real(real64), intent(in) :: dt
        integer(int32), intent(in) :: step
        integer(int32), intent(in) :: iter

        ! if (step >= 2) then
        !     self%CT_old(2)%val(:) = self%CT_old(1)%val(:)
        !     self%CT_old(1)%val(:) = self%CT_l%val(:)
        ! end if

        self%CT_l%val(:) = 0.0d0
        self%KT_l%val(:) = 0.0d0
        self%KT_star_0%val(:) = 0.0d0
        ! ! if (step == 1) then

        ! ! end if
        !---------------------------------------------------------------------------------------------------------------------------
        ! 各剛性行列の組み立て
        !---------------------------------------------------------------------------------------------------------------------------
        !!!-----------------------------------------------------------------------
        ! (A, domain, Temperature, porosity, Propeties)
        call Assemble_Mass_Heat_1_Parallel(self%CT_l, domain, self%T%new, porosity, property)
        ! call Assemble_Mass_Heat_1(self%CT_l, domain, self%T%new, porosity, property)
        !
        call Assemble_Diffusion_Heat_1_Parallel(self%KT_l, domain, self%T%new, porosity, property)
        ! call Assemble_Diffusion_Heat_1(self%KT_l, domain, self%T%new, porosity, property)
        ! モジュール変数へ反映

!$      t_total = t_total + time_total
!$      t_interp = t_interp + time_interp
!$      t_get_prop = t_get_prop + time_get_prop
!$      t_integration = t_integration + time_integration
!$      t_find_index = t_find_index + time_find_index
!$      t_add_val = t_add_val + time_add_val
        ! !!!-----------------------------------------------------------------------
         !!!-----------------------------------------------------------------------

        !---------------------------------------------------------------------------------------------------------------------------
        ! if (step == 1) then
        self%KT_star_0 = dt * self%KT_l + self%CT_l

        !     if (iter == 1) then
        ! self%CT_old(1)%val(:) = self%CT_l%val(:)
        !         self%KT_old%val(:) = self%KT_l%val(:)
        !         self%PHIT(:) = 0.0d0
        ! self%PHIT_old(:) = -self%CT_old(1) * self%T%old(:, 1)
        !     end if
        ! print *, size(self%T%old(:, 1))
        self%PHIT(:) = self%CT_l * self%T%old(:, 1)
        ! stop
        ! self%PHIT(:) = -self%CT_old(1) * self%T%old(:, 1)
        ! self%PHIT(:) = dt * (self%KT_l * self%T%pre(:)) + self%CT_l * self%T%pre(:) + self%PHIT_old(:)
        ! else
        !     self%KT_star_0%val(:) = 2.0d0 * dt * self%KT_l%val(:) + 3.0d0 * self%CT_l%val(:)
        !     if (iter == 1) then
        !         self%PHIT_old(:) = -4.0d0 * (self%CT_old(1) * self%T%old(:, 1)) + self%CT_old(2) * self%T%old(:, 2)
        !     end if
        !     self%PHIT(:) = 2.0d0 * dt * (self%KT_l * self%T%pre(:)) + 3.0d0 * (self%CT_l * self%T%pre(:)) + self%PHIT_old(:)
        ! end if
        ! self%PHIT(:) = self%PHIT_old(:)
    end subroutine assemble_type_thermal_3phase_2d

end submodule Main_Thermal_3Phase
