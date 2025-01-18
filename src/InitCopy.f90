module Solver_InitCopy
    use, intrinsic :: iso_fortran_env, only : int32, real64
    use :: Types
    implicit none
    private

    public :: Init_Copy_Temperature
    public :: Init_Copy_Pressure


    contains

    subroutine Init_Copy_Temperature(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver

        Solver%T%old(:)                 = Solver%T%pre(:)
        Solver%Si%old(:)                = Solver%Si%pre(:)
        Solver%mWater%old(:)            = Solver%mWater%pre(:)
        Solver%mIce%old(:)              = Solver%mIce%pre(:)
        Solver%Heat%Variables%Ca%old(:) = Solver%Heat%Variables%Ca%pre(:)

    end subroutine Init_Copy_Temperature

    subroutine Init_Copy_Pressure(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver

        Solver%P%old(:)                   = Solver%P%pre(:)
        Solver%Water%Variables%Klh%old(:) = Solver%Water%Variables%Klh%pre(:)

    end subroutine Init_Copy_Pressure

end module Solver_InitCopy