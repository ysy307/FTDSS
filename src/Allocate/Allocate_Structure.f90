module Allocate_Structure
    use :: error
    use :: Types
    use :: Allocate
    use, intrinsic :: iso_fortran_env, only : int32, real64
    implicit none
    private

    public :: Allocate_DF
    public :: Allocate_DP2d
    public :: Allocate_BCinfo
    public :: Allocate_Solver

    contains

    subroutine Allocate_DF(ar_DF, n)
		implicit none
		type(DF), intent(inout)    :: ar_DF
		integer(int32), intent(in) :: n

		call Allocate_Vector(ar_DF%old, n)
		call Allocate_Vector(ar_DF%pre, n)
		call Allocate_Vector(ar_DF%new, n)
		call Allocate_Vector(ar_DF%dif, n)
		call Allocate_Vector(ar_DF%div, n)
		call Allocate_Vector(ar_DF%tmp, n)

	end subroutine Allocate_DF

	subroutine Allocate_DP2d(ar_DP2d, n)
		implicit none
		type(DP2d), intent(inout)  :: ar_DP2d
		integer(int32), intent(in) :: n

		call Allocate_Vector(ar_DP2d%x, n)
		call Allocate_Vector(ar_DP2d%y, n)

	end subroutine Allocate_DP2d
    subroutine Allocate_INT2d(ar_INT2d, n)
		implicit none
		type(INT2d), intent(inout)  :: ar_INT2d
		integer(int32), intent(in) :: n

		call Allocate_Vector(ar_INT2d%x, n)
		call Allocate_Vector(ar_INT2d%y, n)

	end subroutine Allocate_INT2d

	subroutine Allocate_BCinfo(BCinfo, nNode, nType, nEdge)
		implicit none
		type(BoudaryConditionInfo), intent(inout) :: BCinfo
		integer(int32), intent(in)                :: nNode, nType
		integer(int32), intent(in), optional      :: nEdge

		call Allocate_Vector(BCinfo%Node,    nNode)
		call Allocate_Vector(BCinfo%TypeKey, nNode)
		call Allocate_Vector(BCinfo%Type,    nType)
		call Allocate_Vector(BCinfo%Value,   nType)
        if (present(nEdge)) then
            ! print*, nEdge
		call Allocate_INT2d(BCinfo%Edges,  nEdge)
		call Allocate_Vector(BCinfo%EdgesDirection,   nEdge)
		call Allocate_Vector(BCinfo%EdgesDistance,   nEdge)
        end if

	end subroutine Allocate_BCinfo

    subroutine Allocate_Solver(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver

        !* Allocate Geometry2d
        call Allocate_Matrix(Solver%N%pElement, Solver%N%shape, Solver%N%element)
        call Allocate_Vector(Solver%N%vCood%x,  Solver%N%node)
        call Allocate_Vector(Solver%N%vCood%y,  Solver%N%node)
        call Allocate_Vector(Solver%N%eArea,    Solver%N%element)
        call Allocate_Matrix(Solver%N%Basis%a,  Solver%N%ShCoe, Solver%N%element)
        call Allocate_Matrix(Solver%N%Basis%b,  Solver%N%ShCoe, Solver%N%element)
        call Allocate_Matrix(Solver%N%Basis%c,  Solver%N%ShCoe, Solver%N%element)
        if (Solver%N%ShCoe == 4) call Allocate_Matrix(Solver%N%Basis%d, Solver%N%ShCoe, Solver%N%element)

        call Allocate_Vector(Solver%mWater%old, Solver%N%node)
        call Allocate_Vector(Solver%mWater%pre, Solver%N%node)
        call Allocate_Vector(Solver%mIce%old,   Solver%N%node)
        call Allocate_Vector(Solver%mIce%pre,   Solver%N%node)
        call Allocate_Vector(Solver%mIce%dif,   Solver%N%node)

        call Allocate_DF(Solver%Si, Solver%N%node)

        if (Solver%isHeat) then
            call Allocate_DF(Solver%Heat%Variables%Cs,        Solver%N%node)
            call Allocate_DF(Solver%Heat%Variables%Cp,        Solver%N%node)
            call Allocate_DF(Solver%Heat%Variables%lambda,    Solver%N%node)
            call Allocate_DF(Solver%Heat%Variables%rho,       Solver%N%node)
            call Allocate_DF(Solver%Heat%Variables%Ca,        Solver%N%node)

            call Allocate_DF(Solver%T,                        Solver%N%node)
            call Allocate_Vector(Solver%Heat%Rhs,             Solver%N%node)
            call Allocate_Vector(Solver%Heat%Variables%Phase, Solver%N%node)
            call Allocate_Matrix(Solver%Heat%RA,              Solver%N%node, Solver%N%node)
            call Allocate_DP2d(Solver%Heat%Variables%Tgrad,   Solver%N%node)
            call Allocate_DP2d(Solver%Heat%Variables%TFlux,   Solver%N%node)
        end if
        if (Solver%isWater) then
            call Allocate_DF  (Solver%Water%Variables%Klh,   Solver%N%node)
            call Allocate_DP2d(Solver%Water%Variables%wFlux, Solver%N%node)
            call Allocate_DP2d(Solver%Water%Variables%hGrad, Solver%N%node)

            call Allocate_DF  (Solver%P,                     Solver%N%node)
            call Allocate_Vector(Solver%Water%Rhs,           Solver%N%node)
            call Allocate_Matrix(Solver%Water%RA,            Solver%N%node, Solver%N%node)
        end if


    end subroutine Allocate_Solver

end module Allocate_Structure