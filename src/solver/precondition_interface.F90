!> Defines abstract and concrete preconditioner types.
module solver_preconditioner
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
    use :: module_field
    implicit none
    private

    public :: abst_preconditioner
    public :: create_preconditioner

    !
    ! === 抽象前処理型 ===
    !
    !> Abstract base type for all preconditioners.
    type, abstract :: abst_preconditioner
        private
    contains
        !> Applies the preconditioner M^-1 to a vector r, returning z. (z = M^-1 * r)
        procedure(abst_apply_preconditioner), pass(self), public, deferred :: apply
    end type abst_preconditioner

    abstract interface
        subroutine abst_apply_preconditioner(self, r, z)
            import :: abst_preconditioner, type_residual_vector
            implicit none
            class(abst_preconditioner), intent(in) :: self
            type(type_residual_vector), intent(in) :: r
            type(type_residual_vector), intent(inout) :: z
        end subroutine abst_apply_preconditioner
    end interface

    !
    ! === 前処理なし (Identity) ===
    !
    !> Identity preconditioner (z = r).
    type, extends(abst_preconditioner) :: type_preconditioner_none
    contains
        procedure :: apply => apply_none
    end type type_preconditioner_none

    ! !
    ! ! === ファクトリインターフェース ===
    ! !
    ! interface create_preconditioner
    !     !> Factory function to create a preconditioner instance based on type.
    !     module function create_preconditioner(precond_type) result(pc)
    !         implicit none
    !         integer(int32), intent(in) :: precond_type
    !         class(abst_preconditioner), allocatable :: pc
    !     end function create_preconditioner
    ! end interface

contains

    ! --- 'None' (Identity) 実装 ---
    subroutine apply_none(self, r, z)
        implicit none
        class(type_preconditioner_none), intent(in) :: self
        type(type_residual_vector), intent(in) :: r
        type(type_residual_vector), intent(inout) :: z

        ! z = I * r
    end subroutine apply_none

    ! ! --- ファクトリ関数の実装 ---
    function create_preconditioner(precond_type) result(pc)
        implicit none
        integer(int32), intent(in) :: precond_type
        class(abst_preconditioner), allocatable :: pc

    end function create_preconditioner

end module solver_preconditioner
