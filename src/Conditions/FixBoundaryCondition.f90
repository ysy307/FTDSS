module Condition_FixBoundaryCondition
    use, intrinsic :: iso_fortran_env, only : int32, real64
    use :: Types, Tmp => Temperature, Prs => Pressure, Sts => Stress
    use :: Matrix_FindInd
    implicit none
    private

    public :: Fix_BoundaryConditions

    contains

    subroutine Fix_BoundaryConditions(Solver, inTarget)
        implicit none
        type(SolverInfo), intent(inout) :: Solver
        integer(int32), intent(in) :: inTarget

        integer(int32) :: iBC, i
        integer(int32) :: iBCType
        integer(int32) :: p1, p2

        do iBC = 1, Solver%BC%numNode
            if (inTarget == Tmp) then
                iBCType = Solver%BC%Heat%Type(Solver%BC%Heat%TypeKey(iBC))
                if (10 <= iBCType .and. iBCType <= 19) then
                    call Fix_DirichletBoundaryCondition_Temperature(Solver, iBC)
                    ! call Fix_DirichletBoundaryCondition_Temperature_F(Solver, iBC)
                end if
            else if (inTarget == Prs) then
                iBCType = Solver%BC%Water%Type(Solver%BC%Water%TypeKey(iBC))
                if (10 <= iBCType .and. iBCType <= 19) then
                    ! call Fix_DirichletBoundaryCondition_Water(Solver, iBC)
                    call Fix_DirichletBoundaryCondition_Water_F(Solver, iBC)
                end if
            end if

            ! stop
            
            ! Solver%Water%Variables%wFlux%y(Solver%BC%Water%TypeKey(iBC)) = 0.0d0
            ! if (Solver%BC%Water%TypeKey(iBC) == 4) Solver%Water%Variables%wFlux%x(Solver%BC%Water%TypeKey(iBC)) = 0.0d0
        end do
        if (inTarget == Tmp .and. Solver%BC%numEdges > 0) then
            do i = 1, Solver%BC%numEdges
                p1 = Solver%BC%Heat%Edges%x(i)
                p2 = Solver%BC%Heat%Edges%y(i)
                
                Solver%Heat%Rhs(p1) = Solver%Heat%Rhs(p1) - (2.0d0 * Solver%Heat%Variables%TFlux%x(p1)+Solver%Heat%Variables%TFlux%x(p2)) * Solver%BC%Heat%EdgesDistance(i) / 6.0d0
                Solver%Heat%Rhs(p2) = Solver%Heat%Rhs(p2) - (Solver%Heat%Variables%TFlux%x(p1)+2.0d0 * Solver%Heat%Variables%TFlux%x(p2)) * Solver%BC%Heat%EdgesDistance(i) / 6.0d0
                ! print*,Solver%Heat%Rhs(p1),Solver%Heat%Rhs(p2)
                ! print*,Solver%Heat%Variables%TFlux%x(p1),Solver%Heat%Variables%TFlux%x(p2)
                ! print*,(Solver%N%vCood%x(Solver%BC%Heat%Edges%y(i)) - Solver%N%vCood%x(Solver%BC%Heat%Edges%x(i))),(Solver%N%vCood%y(Solver%BC%Heat%Edges%y(i)) - Solver%N%vCood%y(Solver%BC%Heat%Edges%x(i)))
            end do
            ! print*, Solver%BC%Heat%EdgesDistance(:)

            ! print *, "Water"
            ! stop

        end if
        
    end subroutine Fix_BoundaryConditions

    subroutine Fix_DirichletBoundaryCondition_Temperature(Solver, iBC)
		implicit none
        type(SolverInfo), intent(inout) :: Solver
        integer(int32),   intent(in)    :: iBC
		integer(int32)                  :: iCol, ind, ps, pe

        iCol                         = Solver%BC%Heat%Node(iBC)
        call Find_CRS_Index(Solver%Heat%LHS_A, iCol, iCol, ind)

        ps                           = Solver%Heat%LHS_A%Ptr(iCol-1)
        pe                           = Solver%Heat%LHS_A%Ptr(iCol) - 1

        Solver%Heat%LHS_A%val(ps:pe) = 0.0d0
        Solver%Heat%LHS_A%val(ind)   = 1.0d0
        Solver%Heat%Rhs(iCol)        = Solver%BC%Heat%Value(Solver%BC%Heat%TypeKey(iBC))
	end subroutine Fix_DirichletBoundaryCondition_Temperature

    subroutine Fix_DirichletBoundaryCondition_Temperature_F(Solver, iBC)
		implicit none
        type(SolverInfo), intent(inout) :: Solver
        integer(int32),   intent(in)    :: iBC
		integer(int32)                  :: iCol, ind, ps, pe
       

        iCol                            = Solver%BC%Heat%Node(iBC)
        ! print*,iCol,iBC
        Solver%Heat%RA(iCol, :)        = 0.0d0
        Solver%Heat%RA(iCol, iCol)     = 1.0d0
        Solver%Heat%Rhs(iCol)          = Solver%BC%Heat%Value(Solver%BC%Heat%TypeKey(iBC))
	end subroutine Fix_DirichletBoundaryCondition_Temperature_F

    subroutine Fix_DirichletBoundaryCondition_Water(Solver, iBC)
		implicit none
        type(SolverInfo), intent(inout) :: Solver
        integer(int32),   intent(in)    :: iBC
		integer(int32)                  :: iCol, ind, ps, pe

        iCol                          = Solver%BC%Water%Node(iBC)
        call Find_CRS_Index(Solver%Water%LHS_A, iCol, iCol, ind)

        ps                            = Solver%Water%LHS_A%Ptr(iCol-1)
        pe                            = Solver%Water%LHS_A%Ptr(iCol) - 1

        Solver%Water%LHS_A%val(ps:pe) = 0.0d0
        Solver%Water%LHS_A%val(ind)   = 1.0d0
        Solver%Water%Rhs(iCol)        = Solver%BC%Water%Value(Solver%BC%Water%TypeKey(iBC))
	end subroutine Fix_DirichletBoundaryCondition_Water

    subroutine Fix_DirichletBoundaryCondition_Water_F(Solver, iBC)
		implicit none
        type(SolverInfo), intent(inout) :: Solver
        integer(int32),   intent(in)    :: iBC
		integer(int32)                  :: iCol, ind, ps, pe

        iCol                            = Solver%BC%Water%Node(iBC)
        Solver%Water%RA(iCol, :)        = 0.0d0
        Solver%Water%RA(iCol, iCol)     = 1.0d0
        Solver%Water%Rhs(iCol)          = Solver%BC%Water%Value(Solver%BC%Water%TypeKey(iBC))
	end subroutine Fix_DirichletBoundaryCondition_Water_F

end module Condition_FixBoundaryCondition