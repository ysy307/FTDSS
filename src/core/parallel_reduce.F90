!> Global reductions over the nodes this rank owns.
!>
!> A node on a partition boundary is stored by every rank whose cells touch it.
!> Reducing over every local entry therefore counts such a node once per rank,
!> which makes a "global" norm depend on how the mesh was partitioned. Every
!> reduction here skips the entries this rank does not own, so a multi-rank run
!> measures the same quantity a single-rank run does.
!>
!> The ownership mask comes from the mesh and is installed once at start-up.
!> Until it is, and in a serial build, every reduction is the identity on the
!> local array, which is the correct answer for one rank.
module core_parallel_reduce
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: core_parallel_mpi
    implicit none
    private

    public :: set_node_ownership
    public :: has_node_ownership
    public :: owned_node_count
    public :: ownership_mask
    public :: reduce_sum
    public :: reduce_max
    public :: reduce_all
    public :: reduce_any
    public :: reduce_sum_nodal
    public :: reduce_sum_squares_nodal

    logical, allocatable :: node_owned(:)
    integer(int32) :: num_owned_global = 0

contains

    !> Install the ownership mask. One entry per local node, in the same
    !> numbering every nodal array uses.
    subroutine set_node_ownership(is_owned)
        implicit none
        logical, intent(in) :: is_owned(:)

        integer(int32) :: num_owned_local

        if (allocated(node_owned)) deallocate (node_owned)
        allocate (node_owned(size(is_owned)))
        node_owned = is_owned

        num_owned_local = count(node_owned)
        num_owned_global = int(reduce_sum(real(num_owned_local, real64)) + 0.5d0, int32)
    end subroutine set_node_ownership

    logical function has_node_ownership()
        implicit none
        has_node_ownership = allocated(node_owned)
    end function has_node_ownership

    !> Number of nodes across all ranks, counting a shared node once.
    integer(int32) function owned_node_count(num_local_nodes)
        implicit none
        integer(int32), intent(in) :: num_local_nodes

        if (allocated(node_owned)) then
            if (size(node_owned) == num_local_nodes) then
                owned_node_count = num_owned_global
                return
            end if
        end if
        owned_node_count = int(reduce_sum(real(num_local_nodes, real64)) + 0.5d0, int32)
    end function owned_node_count

    !> Ownership of an array of n entries. A nodal array maps one to one; a
    !> degree-of-freedom array carries num_dofs_per_node consecutive entries per
    !> node, which is the layout the whole code uses, so the node's flag repeats
    !> across its dofs. All true when the mask is unavailable or the length fits
    !> neither shape, which is the serial answer.
    function ownership_mask(n) result(mask)
        implicit none
        integer(int32), intent(in) :: n
        logical, allocatable :: mask(:)

        integer(int32) :: num_local_nodes, dofs_per_node, node, k, index

        allocate (mask(max(n, 0)))
        mask = .true.
        if (.not. allocated(node_owned)) return
        if (n <= 0) return

        num_local_nodes = size(node_owned)
        if (num_local_nodes <= 0) return

        if (n == num_local_nodes) then
            mask = node_owned
            return
        end if

        if (mod(n, num_local_nodes) /= 0) return
        dofs_per_node = n / num_local_nodes
        index = 0
        do node = 1, num_local_nodes
            do k = 1, dofs_per_node
                index = index + 1
                mask(index) = node_owned(node)
            end do
        end do
    end function ownership_mask

    real(real64) function reduce_sum(value)
        implicit none
        real(real64), intent(in) :: value
#ifdef _MPI
        integer(int32) :: ierr
        real(real64) :: send_buffer, recv_buffer

        send_buffer = value
        recv_buffer = value
        call MPI_Allreduce(send_buffer, recv_buffer, 1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, ierr)
        reduce_sum = recv_buffer
#else
        reduce_sum = value
#endif
    end function reduce_sum

    real(real64) function reduce_max(value)
        implicit none
        real(real64), intent(in) :: value
#ifdef _MPI
        integer(int32) :: ierr
        real(real64) :: send_buffer, recv_buffer

        send_buffer = value
        recv_buffer = value
        call MPI_Allreduce(send_buffer, recv_buffer, 1, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, ierr)
        reduce_max = recv_buffer
#else
        reduce_max = value
#endif
    end function reduce_max

    !> True only where it is true on every rank.
    logical function reduce_all(flag)
        implicit none
        logical, intent(in) :: flag
#ifdef _MPI
        integer(int32) :: ierr
        logical :: send_buffer, recv_buffer

        send_buffer = flag
        recv_buffer = flag
        call MPI_Allreduce(send_buffer, recv_buffer, 1, MPI_LOGICAL, MPI_LAND, MPI_COMM_WORLD, ierr)
        reduce_all = recv_buffer
#else
        reduce_all = flag
#endif
    end function reduce_all

    !> True where it is true on any rank.
    logical function reduce_any(flag)
        implicit none
        logical, intent(in) :: flag
#ifdef _MPI
        integer(int32) :: ierr
        logical :: send_buffer, recv_buffer

        send_buffer = flag
        recv_buffer = flag
        call MPI_Allreduce(send_buffer, recv_buffer, 1, MPI_LOGICAL, MPI_LOR, MPI_COMM_WORLD, ierr)
        reduce_any = recv_buffer
#else
        reduce_any = flag
#endif
    end function reduce_any

    !> Sum of a nodal array over every rank, each shared node counted once.
    real(real64) function reduce_sum_nodal(x)
        implicit none
        real(real64), intent(in) :: x(:)

        logical, allocatable :: mask(:)

        mask = ownership_mask(size(x))
        reduce_sum_nodal = reduce_sum(sum(x, mask=mask))
    end function reduce_sum_nodal

    !> Sum of squares of a nodal array over every rank, each shared node counted
    !> once. The building block of every global nodal norm.
    real(real64) function reduce_sum_squares_nodal(x)
        implicit none
        real(real64), intent(in) :: x(:)

        logical, allocatable :: mask(:)

        mask = ownership_mask(size(x))
        reduce_sum_squares_nodal = reduce_sum(sum(x**2, mask=mask))
    end function reduce_sum_squares_nodal

end module core_parallel_reduce
