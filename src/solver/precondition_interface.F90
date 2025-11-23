!> Defines abstract and concrete preconditioner types.
module solver_preconditioner
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_linalg
    implicit none
    private

    public :: abst_preconditioner
    public :: type_preconditioner_none

    public :: type_preconditioner_settings

    public :: create_preconditioner

    type :: type_preconditioner_settings
        integer(int32) :: id
    end type type_preconditioner_settings

    !
    ! === 抽象前処理型 ===
    !
    !> Abstract base type for all preconditioners.
    type, abstract :: abst_preconditioner
        private
        character(:), allocatable :: name
        integer(int32) :: id
        integer(int32) :: status
    contains
        !> Initializes the preconditioner with given info.
        procedure(abst_preconditioner_initialize), pass(self), public, deferred :: initialize
        !> Sets up the preconditioner (e.g., computes factors).
        procedure(abst_preconditioner_setup), pass(self), public, deferred :: setup
        !> Applies the preconditioner M^-1 to a vector r, returning z. (z = M^-1 * r)
        procedure(abst_preconditioner_apply), pass(self), public, deferred :: apply
        !> Destructs the preconditioner instance.
        procedure(abst_preconditioner_destroy), pass(self), public, deferred :: destroy
    end type abst_preconditioner

    abstract interface
        subroutine abst_preconditioner_initialize(self, info)
            import :: abst_preconditioner, type_preconditioner_settings, int32
            implicit none
            class(abst_preconditioner), intent(inout) :: self
            type(type_preconditioner_settings), intent(in) :: info
        end subroutine abst_preconditioner_initialize

        subroutine abst_preconditioner_setup(self, A)
            import :: abst_preconditioner, abst_matrix, int32
            implicit none
            class(abst_preconditioner), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
        end subroutine abst_preconditioner_setup

        subroutine abst_preconditioner_apply(self, r, z)
            import :: abst_preconditioner, type_vector_dp
            implicit none
            class(abst_preconditioner), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine abst_preconditioner_apply

        subroutine abst_preconditioner_destroy(self)
            import :: abst_preconditioner
            implicit none
            class(abst_preconditioner), intent(inout) :: self
        end subroutine abst_preconditioner_destroy
    end interface

    !
    ! === 前処理なし (Identity) ===
    !
    type, extends(abst_preconditioner) :: type_preconditioner_none
    contains
        procedure :: initialize => initialize_preconditioner_none
        procedure :: setup => setup_preconditioner_none
        procedure :: apply => apply_preconditioner_none
        procedure :: destroy => destroy_preconditioner_none
    end type type_preconditioner_none

    interface
        module subroutine initialize_preconditioner_none(self, info)
            implicit none
            class(type_preconditioner_none), intent(inout) :: self
            type(type_preconditioner_settings), intent(in) :: info
        end subroutine initialize_preconditioner_none

        module subroutine setup_preconditioner_none(self, A)
            implicit none
            class(type_preconditioner_none), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
        end subroutine setup_preconditioner_none

        module subroutine apply_preconditioner_none(self, r, z)
            implicit none
            class(type_preconditioner_none), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_preconditioner_none

        module subroutine destroy_preconditioner_none(self)
            implicit none
            class(type_preconditioner_none), intent(inout) :: self
        end subroutine destroy_preconditioner_none
    end interface

contains

    ! --- ファクトリ関数の実装 ---
    subroutine create_preconditioner(pc, info, ierr)
        implicit none
        class(abst_preconditioner), allocatable, intent(inout) :: pc
        type(type_preconditioner_settings), intent(in) :: info
        integer(int32), intent(inout) :: ierr

        if (allocated(pc)) then
            deallocate (pc)
        end if

        select case (info%id)
        case (SOLVER_PRECONDITION_NONE)
            allocate (type_preconditioner_none :: pc)
            call pc%initialize(info)
            ierr = pc%status
        case (SOLVER_PRECONDITION_JACOBI)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_PRECONDITION_ILU)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_PRECONDITION_SSOR)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_PRECONDITION_HYBRID)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_PRECONDITION_IS)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_PRECONDITION_SAINV)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_PRECONDITION_SAAMG)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_PRECONDITION_ILUC)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_PRECONDITION_ILUT)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        end select

    end subroutine create_preconditioner

end module solver_preconditioner
