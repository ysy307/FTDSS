module Condition_FixInitialCondition
    use, intrinsic :: iso_fortran_env, only : int32, real64
    use :: Types, g => GravityAcceleration
	use :: Allocate, only : Duplicate_CRS
	use :: Allocate_Structure
	use :: error
    use :: Calculate_Update
    use :: Condition_FixBoundaryCondition
    use :: Matrix_Assemble, only : Assemble_GM_Heat_IC, Assemble_GM_Water_IC
    use :: Solver_Solve
    implicit none
    private

    public :: Fix_InitialCondition

    contains

    subroutine Fix_InitialCondition(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver

        if (Solver%isHeat) then
            if (Solver%IC%Heat%type == 1) then
                call Fix_InitialCondtion_Constant_Heat(Solver)
            else if (Solver%IC%Heat%type == 2) then
                call Fix_InitialCondition_Gradient_Heat(Solver)
                Solver%IC%Heat%isSet = .false.
            end if
            if (Solver%Flags%isStdOut) write(*,'(a)') "Set initial value of Temperature."
        end if
        if (Solver%isWater) then
            if (Solver%IC%Water%type == 1) then
                call Fix_InitialCondtion_Constant_Water(Solver)
            else if (Solver%IC%Water%type == 2) then
                call Fix_InitialCondition_Gradient_Water(Solver)
                Solver%IC%Water%isSet = .false.
            end if
            if (Solver%Flags%isStdOut) write(*,'(a)') "Set initial value of Pressure."
        end if
        if (Solver%isStress) then
            ! To be implemented
            ! if (Solver%IC%Stress%type == 1) then
            !     call Fix_InitialCondtion_Constant(Solver)
            ! else if (Solver%IC%Stress%type == 2) then
            !     call Fix_InitialCondition_Gradient(Solver)
            ! end if
            ! if (Solver%Flags%isStdOut) write(*,'(a)') "Set initial value of Stress."
        end if

        call Fix_InitialCondition_FreezingRate(Solver)

    end subroutine Fix_InitialCondition

    subroutine Fix_InitialCondtion_Constant_Heat(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver
        integer(int32)                  :: iN, iBC
        integer(int32)                  :: ntBC, nBC

        do iN = 1, Solver%N%node
            Solver%T%pre(iN) = Solver%IC%Heat%Value
        end do
        ! 初期温度場への境界条件の設定
        do iBC = 1, Solver%BC%numNode
            ntBC = Solver%BC%Heat%Type(Solver%BC%Heat%TypeKey(iBC))
            if (10 <= ntBC .and. ntBC <= 19) then
                Solver%T%pre(Solver%BC%Heat%Node(iBC)) = Solver%BC%Heat%Value(Solver%BC%Heat%TypeKey(iBC))
            end if
        end do
    end subroutine Fix_InitialCondtion_Constant_Heat

    subroutine Fix_InitialCondtion_Constant_Water(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver
        integer(int32)                  :: iN, iBC
        integer(int32)                  :: ntBC, nBC

        do iN = 1, Solver%N%node
            Solver%P%pre(iN) = Solver%IC%Water%Value
        end do
        ! 初期圧力場への境界条件の設定
        do iBC = 1, Solver%BC%numNode
            ntBC = Solver%BC%Water%Type(Solver%BC%Water%TypeKey(iBC))
            if (20 <= ntBC .and. ntBC <= 29) then
                Solver%P%pre(Solver%BC%Water%Node(iBC)) = Solver%BC%Water%Value(Solver%BC%Water%TypeKey(iBC))
            end if
        end do
    end subroutine Fix_InitialCondtion_Constant_Water

    subroutine Fix_InitialCondition_Gradient_Heat(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver
        type(ILS),allocatable :: ILEQ
        integer(int32) :: ierr

        ILEQ = ILS(Solver, Solver%Heat%LHS_A)

        call Update_Parameters_Heat(Solver)
        call Assemble_GM_Heat_IC(Solver)
        call Fix_BoundaryConditions(Solver, Temperature)

        
        call ILEQ%BiCGStab(Solver, Solver%Heat%LHS_A, Solver%Heat%Rhs, Solver%T%pre, ierr)
        call ILEQ%Chkerr(ierr, 0.0d0)
        deallocate(ILEQ)
    end subroutine Fix_InitialCondition_Gradient_Heat

    subroutine Fix_InitialCondition_Gradient_Water(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver
        type(DLS) ::DLEQ

        Solver%Si%old(:) = Solver%Si%pre(:)
        Solver%Si%pre(:) = 0.0d0
        call Update_Parameters_Water(Solver)
        Solver%Si%pre(:) = Solver%Si%old(:)
        call Assemble_GM_Water_IC(Solver)
        call Fix_BoundaryConditions(Solver, Pressure)

        DLEQ = DLS(Solver)
        call DLEQ%LU(Solver%Water%RA, Solver%Water%Rhs, Solver%P%pre)

    end subroutine Fix_InitialCondition_Gradient_Water

    subroutine Fix_InitialCondition_FreezingRate(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver
        integer(int32)                  :: iN, iBC

        real(real64)                    :: Tf
        ! GCC model
        real(real64)                    :: Qs, Qr, alpha, n, m, Lf
        real(real64), parameter         :: TtoK = 273.15d0
        ! Power model
        real(real64)                    :: a

        if (Solver%Flags%isTRM) then
            do iN = 1, Solver%N%node
                if (Solver%T%pre(iN) < 0.0d0) then
                    Solver%Si%pre(iN) = 1.0d0
                else
                    Solver%Si%pre(iN) = 0.0d0
                end if
            end do
        else if (Solver%Flags%isGCC) then
            Qs    = Solver%Heat%Latent%GCC%thetaS
            Qr    = Solver%Heat%Latent%GCC%thetaR
            alpha = Solver%Heat%Latent%GCC%alpha
            n     = Solver%Heat%Latent%GCC%n
            m     = Solver%Heat%Latent%GCC%m
            Lf    = Solver%Heat%Latent%Lf
            Tf   = Solver%Heat%Latent%GCC%Tf
            do iN = 1, Solver%N%node
                if (Solver%T%pre(iN) < Tf) then
                    Solver%Si%pre(iN) = ((Qs- Qr) * (1.0d0 - (1.0d0 + abs(alpha * Lf * log((Solver%T%pre(iN) + TtoK ) /TtoK ) / g) &
                                         ** n) ** (-m))) / Qs
                else
                    Solver%Si%pre(iN) = 0.0d0
                end if
            end do
        else if (Solver%Flags%isPower) then
            Tf = Solver%Heat%Latent%Power%Tf
            a  = Solver%Heat%Latent%Power%a
            do iN = 1, Solver%N%node
                if (Solver%T%pre(iN) < Tf) then
                    Solver%Si%pre(iN) = 1.0d0 - (1.0d0 - Solver%T%pre(iN) + Tf) ** a
                else
                    Solver%Si%pre(iN) = 0.0d0
                end if
            end do
        end if

        if (Solver%Flags%isStdOut) write(*,'(a)') "Set initial value of Freezing rate."
    end subroutine Fix_InitialCondition_FreezingRate
end module Condition_FixInitialCondition