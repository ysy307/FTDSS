module core_constants_parallel
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Node types for parallel computing
    !-------------------------------------------------------------------------------------------------------------------------------
    !> Internal node: exists only inside a partition
    integer(int32), parameter, public :: NODE_INTERNAL = 1
    !> Boundary node: exists on partition boundaries, shared among partitions
    integer(int32), parameter, public :: NODE_BORDER = 2
    !> Halo node: ghost node, used for communication but not owned
    integer(int32), parameter, public :: NODE_HALO = 3

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Communication flags for parallel data exchange
    !-------------------------------------------------------------------------------------------------------------------------------
    !> owner rank or internal node (no communication needed)
    integer(int32), parameter, public :: ROLE_OWNER = 0
    !> Other ranks that need to receive data from owner
    integer(int32), parameter, public :: ROLE_RECEIVER = 1
    !> Ranks not involved for this node
    integer(int32), parameter, public :: ROLE_NONE = -1

    ! Communication operations
    integer(int32), parameter, public :: OP_UPDATE = 1
    integer(int32), parameter, public :: OP_ASSEMBLE = 2

end module core_constants_parallel
