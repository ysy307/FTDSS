submodule(Main_Thermal) Main_Thermal_3Phase
    implicit none
contains
    module function Type_Thermal_3Phase_2D_Construct(Input, Coordinate, Domain) result(Structure)
        implicit none
        class(Abstract_Thermal), allocatable :: Structure
        type(Type_Input), intent(inout) :: Input
        type(type_dp_3d), intent(inout), pointer :: Coordinate
        type(type_domain), intent(inout) :: Domain

        integer(int32) :: i
        integer(int32) :: nNode

        integer(int32) :: ierr

        if (allocated(Structure)) deallocate (Structure)
        allocate (Type_Thermal_3Phase_2D :: Structure)

        nNode = Domain%get_num_nodes()

        call Structure%KT_star_0%initialize(Domain)

        Structure%KT_l = Structure%KT_star_0%Copy()
        Structure%KT_old = Structure%KT_star_0%Copy()
        Structure%CT_l = Structure%KT_star_0%Copy()
        Structure%Order = Input%Basic%Order

        allocate (Structure%CT_old(Input%Basic%Order))
        do i = 1, Input%Basic%Order
            Structure%CT_old(i) = Structure%KT_star_0%Copy()
        end do

        call allocate_array(Structure%FT, nNode)
        call allocate_array(Structure%FT_old, nNode)
        call allocate_array(Structure%PHIT, nNode)
        call allocate_array(Structure%PHIT_old, nNode)

        call Structure%T%initialize(nNode, Input%Basic%Order)

        !---------------------------------------------------------------------------------------------------------------------------
        ! 線形求解ソルバーの設定
        !---------------------------------------------------------------------------------------------------------------------------
        if (Input%Solver_Thermal%useSolver == 1) then
            Structure%Solver = Solver_CRS_LU_Constructor(N=nNode, &
                                                         MAXFCT=1, &
                                                         MNUM=1, &
                                                         MTYPE=1, &
                                                         PHASE=13, &
                                                         NRHS=1, &
                                                         MSGVLV=0, &
                                                         a=Structure%KT_star_0)
        else if (Input%Solver_Thermal%useSolver == 2) then
            if (Input%Solver_Thermal%useSolverType == 4) then
                Structure%Solver = Solver_CRS_BiCGSTAB_Constructor(N=nNode, &
                                                                   tol=Input%Solver_Thermal%tolerance, &
                                                                   maxiter=Input%Solver_Thermal%maxIteration, &
                                                                   Preconditioner=Input%Solver_Thermal%usePreconditionerType)
            end if
        end if
        !---------------------------------------------------------------------------------------------------------------------------

    end function Type_Thermal_3Phase_2D_Construct

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

    module subroutine Type_Thermal_3Phase_2D_Assemble(self, Domain, Property, Porosity, dt, step, iter)
        implicit none
        class(Type_Thermal_3Phase_2D), intent(inout) :: self
        type(type_domain), intent(inout) :: Domain
        type(Proereties_Model_t), intent(inout) :: Property
        real(real64), intent(in) :: Porosity(:)
        real(real64), intent(in) :: dt
        integer(int32), intent(in) :: step
        integer(int32), intent(in) :: iter

        ! if (step >= 2) then
        !     self%CT_old(2)%Val(:) = self%CT_old(1)%Val(:)
        !     self%CT_old(1)%Val(:) = self%CT_l%Val(:)
        ! end if

        self%CT_l%Val(:) = 0.0d0
        self%KT_l%Val(:) = 0.0d0
        self%KT_star_0%Val(:) = 0.0d0
        ! ! if (step == 1) then

        ! ! end if
        !---------------------------------------------------------------------------------------------------------------------------
        ! 各剛性行列の組み立て
        !---------------------------------------------------------------------------------------------------------------------------
        !!!-----------------------------------------------------------------------
        ! (A, Domain, Temperature, Porosity, Propeties)
        call Assemble_Mass_Heat_1_Parallel(self%CT_l, Domain, self%T%new, Porosity, Property)
        ! call Assemble_Mass_Heat_1(self%CT_l, Domain, self%T%new, Porosity, Property)
        !
        call Assemble_Diffusion_Heat_1_Parallel(self%KT_l, Domain, self%T%new, Porosity, Property)
        ! call Assemble_Diffusion_Heat_1(self%KT_l, Domain, self%T%new, Porosity, Property)
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
        ! self%CT_old(1)%Val(:) = self%CT_l%Val(:)
        !         self%KT_old%Val(:) = self%KT_l%Val(:)
        !         self%PHIT(:) = 0.0d0
        ! self%PHIT_old(:) = -self%CT_old(1) * self%T%old(:, 1)
        !     end if
        ! print *, size(self%T%old(:, 1))
        self%PHIT(:) = self%CT_l * self%T%old(:, 1)
        ! stop
        ! self%PHIT(:) = -self%CT_old(1) * self%T%old(:, 1)
        ! self%PHIT(:) = dt * (self%KT_l * self%T%pre(:)) + self%CT_l * self%T%pre(:) + self%PHIT_old(:)
        ! else
        !     self%KT_star_0%Val(:) = 2.0d0 * dt * self%KT_l%Val(:) + 3.0d0 * self%CT_l%Val(:)
        !     if (iter == 1) then
        !         self%PHIT_old(:) = -4.0d0 * (self%CT_old(1) * self%T%old(:, 1)) + self%CT_old(2) * self%T%old(:, 2)
        !     end if
        !     self%PHIT(:) = 2.0d0 * dt * (self%KT_l * self%T%pre(:)) + 3.0d0 * (self%CT_l * self%T%pre(:)) + self%PHIT_old(:)
        ! end if
        ! self%PHIT(:) = self%PHIT_old(:)
    end subroutine Type_Thermal_3Phase_2D_Assemble

end submodule Main_Thermal_3Phase
