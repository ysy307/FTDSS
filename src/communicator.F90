!> @brief Module for managing halo region data communication between parallel processes.
!> @details
!> This module provides a communicator for data exchange (update) and
!> aggregation (assemble) at process boundaries (halos) in a domain-decomposed mesh.
!> Achieves efficient scalar and vector data exchange via MPI asynchronous communication.
module numerical_parallel_communicator
    use, intrinsic :: iso_fortran_env
    use :: core_parallel_mpi
    use :: module_core
    use :: module_input, only:type_input
    use :: core_findings, only:binary_find

    implicit none
    private

    ! Communication operation type parameters
    integer(int32), private, parameter :: OP_UPDATE = 1 !< Overwrite operation
    integer(int32), private, parameter :: OP_ASSEMBLE = 2 !< Accumulate operation

    public :: type_halo_communicator

    !> @brief Data type for managing halo communication
    type :: type_halo_communicator
        private
        ! -- MPI info --
        integer(int32) :: my_rank = -1
        integer(int32) :: num_procs = -1
        type(MPI_Comm) :: comm = MPI_COMM_NULL

        ! -- Communication partner info --
        integer(int32) :: num_partners = 0
        integer(int32), allocatable :: partners(:)

        ! -- Communication schedule (scalar) --
        integer(int32), allocatable :: send_counts(:)
        integer(int32), allocatable :: send_displs(:)
        integer(int32), allocatable :: recv_counts(:)
        integer(int32), allocatable :: recv_displs(:)

        ! -- Communication schedule (vector) --
        integer(int32), allocatable :: send_counts_vector(:)
        integer(int32), allocatable :: send_displs_vector(:)
        integer(int32), allocatable :: recv_counts_vector(:)
        integer(int32), allocatable :: recv_displs_vector(:)

        ! -- Data indices --
        integer(int32), allocatable :: send_indices(:) !< Indices of local nodes to send
        integer(int32), allocatable :: recv_indices(:) !< Indices of halo nodes to receive

        ! -- Send/receive buffers --
        real(real64), allocatable :: send_buf(:)
        real(real64), allocatable :: recv_buf(:)
        type(MPI_Request), allocatable :: requests(:)
        type(MPI_Status), allocatable :: statuses(:)

        ! -- GID lookup data --
        integer(int64), allocatable :: sorted_local_gids(:) !< Sorted GIDs of local border nodes
        integer(int32), allocatable :: sorted_local_lids(:) !< LIDs corresponding to the above GIDs

        logical :: is_initialized = .false.

    contains
        procedure, pass(self) :: initialize => initialize_halo_communicator
        procedure, pass(self) :: display => display_communicator_state
        final :: destroy_halo_communicator

        procedure, pass(self) :: update_scalar
        procedure, pass(self) :: update_vector
        procedure, pass(self) :: assemble_scalar
        procedure, pass(self) :: assemble_vector

        generic, public :: update => update_scalar, update_vector
        generic, public :: assemble => assemble_scalar, assemble_vector

        ! -- private methods --
        procedure, private, pass(self) :: build_communication_schedule
        procedure, private, pass(self) :: setup_local_sorted_nodes
        procedure, private, pass(self) :: exchange_communication_plan
        procedure, private, pass(self) :: exchange_gids_and_build_indices

        procedure, private, pass(self) :: exchange_and_operate_scalar_impl
        procedure, private, pass(self) :: exchange_and_operate_vector_impl
        procedure, private, pass(self) :: ensure_buffers_ready
    end type type_halo_communicator

    interface swap
        procedure swap_i32
        procedure swap_i64
    end interface

contains

    !> @brief Initialize the communicator.
    subroutine initialize_halo_communicator(self, input, comm_in)
        class(type_halo_communicator), intent(inout) :: self
        class(type_input), intent(in) :: input
        type(MPI_Comm), intent(in) :: comm_in
        integer(int32) :: ierr

        if (self%is_initialized) return

        self%comm = comm_in
        call MPI_Comm_rank(self%comm, self%my_rank, ierr)
        call handle_mpi_error(ierr, "MPI_Comm_rank in initialize")
        call MPI_Comm_size(self%comm, self%num_procs, ierr)
        call handle_mpi_error(ierr, "MPI_Comm_size in initialize")

        call self%build_communication_schedule(input)

        self%is_initialized = .true.
    end subroutine initialize_halo_communicator

    !> @brief Destroy the communicator and free memory.
    subroutine destroy_halo_communicator(self)
        type(type_halo_communicator), intent(inout) :: self

        call deallocate_array(self%partners)
        call deallocate_array(self%send_counts)
        call deallocate_array(self%recv_counts)
        call deallocate_array(self%send_displs)
        call deallocate_array(self%recv_displs)
        call deallocate_array(self%send_indices)
        call deallocate_array(self%recv_indices)
        call deallocate_array(self%send_buf)
        call deallocate_array(self%recv_buf)
        if (allocated(self%requests)) deallocate (self%requests)
        if (allocated(self%statuses)) deallocate (self%statuses)
        call deallocate_array(self%send_counts_vector)
        call deallocate_array(self%send_displs_vector)
        call deallocate_array(self%recv_counts_vector)
        call deallocate_array(self%recv_displs_vector)
        call deallocate_array(self%sorted_local_gids)
        call deallocate_array(self%sorted_local_lids)

        self%comm = MPI_COMM_NULL
        self%is_initialized = .false.
    end subroutine destroy_halo_communicator

    !> @brief Update (overwrite) scalar data.
    subroutine update_scalar(self, data_array)
        class(type_halo_communicator), intent(inout) :: self
        real(real64), intent(inout) :: data_array(:)
        call self%exchange_and_operate_scalar_impl(COMM_OPS%UPDATE, data_array)
    end subroutine update_scalar

    !> @brief Assemble (accumulate) scalar data.
    subroutine assemble_scalar(self, data_array)
        class(type_halo_communicator), intent(inout) :: self
        real(real64), intent(inout) :: data_array(:)
        call self%exchange_and_operate_scalar_impl(COMM_OPS%ASSEMBLE, data_array)
    end subroutine assemble_scalar

    !> @brief Update (overwrite) vector data.
    subroutine update_vector(self, data_array, num_components)
        class(type_halo_communicator), intent(inout) :: self
        real(real64), intent(inout) :: data_array(:, :)
        integer(int32), intent(in) :: num_components
        call self%exchange_and_operate_vector_impl(COMM_OPS%UPDATE,data_array, num_components)
    end subroutine update_vector

    !> @brief Assemble (accumulate) vector data.
    subroutine assemble_vector(self, data_array, num_components)
        class(type_halo_communicator), intent(inout) :: self
        real(real64), intent(inout) :: data_array(:, :)
        integer(int32), intent(in) :: num_components
        call self%exchange_and_operate_vector_impl(COMM_OPS%ASSEMBLE, data_array, num_components)
    end subroutine assemble_vector

    !> @brief Internal implementation for scalar data exchange and operation.
    subroutine exchange_and_operate_scalar_impl(self, op, data_array)
        class(type_halo_communicator), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        real(real64), intent(inout) :: data_array(:)
        integer(int32) :: i, ierr, total_send_nodes, total_recv_nodes

        if (.not. self%is_initialized .or. self%comm == MPI_COMM_NULL .or. self%num_partners == 0) return

        total_send_nodes = sum(self%send_counts)
        total_recv_nodes = sum(self%recv_counts)
        call self%ensure_buffers_ready(total_send_nodes, total_recv_nodes)

        if (total_send_nodes > 0) then
            self%send_buf(1:total_send_nodes) = data_array(self%send_indices(1:total_send_nodes))
        end if

        if (.not. allocated(self%requests)) allocate (self%requests(self%num_partners * 2))
        if (.not. allocated(self%statuses)) allocate (self%statuses(self%num_partners * 2))

        do i = 1, self%num_partners
            call MPI_Irecv(self%recv_buf(self%recv_displs(i) + 1), self%recv_counts(i), MPI_DOUBLE_PRECISION, &
                           self%partners(i), 0, self%comm, self%requests(i), ierr)
            call handle_mpi_error(ierr, "MPI_Irecv loop")
        end do

        do i = 1, self%num_partners
            call MPI_Isend(self%send_buf(self%send_displs(i) + 1), self%send_counts(i), MPI_DOUBLE_PRECISION, &
                           self%partners(i), 0, self%comm, self%requests(self%num_partners + i), ierr)
            call handle_mpi_error(ierr, "MPI_Isend loop")
        end do

        call MPI_Waitall(self%num_partners * 2, self%requests, self%statuses, ierr)
        call handle_mpi_error(ierr, "MPI_Waitall for scalar exchange")

        if (total_recv_nodes > 0) then
            select case (op%ID)
            case (COMM_OPS%UPDATE%ID)
                data_array(self%recv_indices(1:total_recv_nodes)) = self%recv_buf(1:total_recv_nodes)
            case (COMM_OPS%ASSEMBLE%ID)
                data_array(self%recv_indices(1:total_recv_nodes)) = data_array(self%recv_indices(1:total_recv_nodes)) + &
                                                                    self%recv_buf(1:total_recv_nodes)
            end select
        end if
    end subroutine exchange_and_operate_scalar_impl

    !> @brief Internal implementation for vector data exchange and operation.
    subroutine exchange_and_operate_vector_impl(self, op, data_array, num_components)
        class(type_halo_communicator), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        real(real64), intent(inout) :: data_array(:, :)
        integer(int32), intent(in) :: num_components
        integer(int32) :: i, total_send_nodes, total_recv_nodes, total_send_values, total_recv_values, ierr

        if (.not. self%is_initialized .or. self%comm == MPI_COMM_NULL .or. self%num_partners == 0 .or. num_components <= 0) return

        total_send_nodes = sum(self%send_counts)
        total_recv_nodes = sum(self%recv_counts)
        total_send_values = total_send_nodes * num_components
        total_recv_values = total_recv_nodes * num_components

        call self%ensure_buffers_ready(total_send_values, total_recv_values, num_components)

        if (total_send_nodes > 0) then
            do i = 1, total_send_nodes
                self%send_buf((i - 1) * num_components + 1:i * num_components) = data_array(:, self%send_indices(i))
            end do
        end if

        if (.not. allocated(self%requests)) allocate (self%requests(self%num_partners * 2))
        if (.not. allocated(self%statuses)) allocate (self%statuses(self%num_partners * 2))

        do i = 1, self%num_partners
            call MPI_Irecv(self%recv_buf(self%recv_displs_vector(i) + 1), self%recv_counts_vector(i), MPI_DOUBLE_PRECISION, &
                           self%partners(i), 1, self%comm, self%requests(i), ierr)
            call handle_mpi_error(ierr, "MPI_Irecv loop for vector")
        end do

        do i = 1, self%num_partners
            call MPI_Isend(self%send_buf(self%send_displs_vector(i) + 1), self%send_counts_vector(i), MPI_DOUBLE_PRECISION, &
                           self%partners(i), 1, self%comm, self%requests(self%num_partners + i), ierr)
            call handle_mpi_error(ierr, "MPI_Isend loop for vector")
        end do

        call MPI_Waitall(self%num_partners * 2, self%requests, self%statuses, ierr)
        call handle_mpi_error(ierr, "MPI_Waitall for vector exchange")

        if (total_recv_nodes > 0) then
            select case (op%ID)
            case (COMM_OPS%UPDATE%ID)
                do i = 1, total_recv_nodes
                    data_array(:, self%recv_indices(i)) = self%recv_buf((i - 1) * num_components + 1:i * num_components)
                end do
            case (COMM_OPS%ASSEMBLE%ID)
                do i = 1, total_recv_nodes
                    data_array(:, self%recv_indices(i)) = data_array(:, self%recv_indices(i)) + &
                                                          self%recv_buf((i - 1) * num_components + 1:i * num_components)
                end do
            end select
        end if
    end subroutine exchange_and_operate_vector_impl

    !--------------------------------------------------------------------------
    ! Communication schedule construction subroutines
    !--------------------------------------------------------------------------

    !> @brief Build the communication schedule (main routine).
    subroutine build_communication_schedule(self, input)
        class(type_halo_communicator), intent(inout) :: self
        class(type_input), intent(in) :: input
        integer(int32) :: num_halo_nodes
        integer(int64), allocatable :: halo_gids(:)
        integer(int32), allocatable :: halo_owners(:), halo_lids(:)
        integer(int32), allocatable :: send_counts_per_proc(:), recv_counts_per_proc(:)
        integer(int32) :: i, current_pos

        associate (vtk => input%geometry%vtk)
            ! 1. Extract GIDs and LIDs of border nodes owned by this process, sorted by GID
            call self%setup_local_sorted_nodes(vtk)

            ! 2. Extract halo node info (owner, LID, GID)
            num_halo_nodes = count(vtk%node_type == COMM_NODE_TYPES%HALO%ID)
            allocate (halo_owners(num_halo_nodes), halo_lids(num_halo_nodes), halo_gids(num_halo_nodes))
            current_pos = 0
            do i = 1, vtk%num_points
                if (vtk%node_type(i) == COMM_NODE_TYPES%HALO%ID) then
                    current_pos = current_pos + 1
                    halo_owners(current_pos) = vtk%owner_rank(1, i)
                    halo_lids(current_pos) = i
                    halo_gids(current_pos) = vtk%global_node_ids(i)
                end if
            end do
            if (num_halo_nodes > 1) then
                call quicksort_rank_lid_gid_triplets(halo_owners, halo_lids, halo_gids, 1, num_halo_nodes)
            end if
            self%recv_indices = halo_lids

            ! 3. Exchange per-process request counts via Alltoall to build communication plan
            call self%exchange_communication_plan(halo_owners, send_counts_per_proc, recv_counts_per_proc)

            ! 4. Exchange GIDs and finalize send/recv indices
            call self%exchange_gids_and_build_indices(halo_gids, send_counts_per_proc, recv_counts_per_proc)

            deallocate (halo_gids, halo_owners, send_counts_per_proc, recv_counts_per_proc)
        end associate
    end subroutine build_communication_schedule

    !> @brief Extract local border nodes and store them sorted by GID.
    subroutine setup_local_sorted_nodes(self, vtk)
        class(type_halo_communicator), intent(inout) :: self
        class(type_vtk), intent(in) :: vtk
        integer(int32) :: num_border_nodes, i, current_pos

        num_border_nodes = count(vtk%node_type == COMM_NODE_TYPES%BORDER%ID)
        allocate (self%sorted_local_gids(num_border_nodes), self%sorted_local_lids(num_border_nodes))

        current_pos = 0
        do i = 1, vtk%num_points
            if (vtk%node_type(i) == COMM_NODE_TYPES%BORDER%ID) then
                current_pos = current_pos + 1
                self%sorted_local_gids(current_pos) = vtk%global_node_ids(i)
                self%sorted_local_lids(current_pos) = i
            end if
        end do

        if (num_border_nodes > 1) then
            call quicksort_gid_lid_pairs(self%sorted_local_gids, self%sorted_local_lids, 1, num_border_nodes)
        end if
    end subroutine setup_local_sorted_nodes

    !> @brief Exchange node counts between processes and determine communication partners.
    subroutine exchange_communication_plan(self, halo_owners, send_counts_per_proc, recv_counts_per_proc)
        class(type_halo_communicator), intent(inout) :: self
        integer(int32), intent(in) :: halo_owners(:)
        integer(int32), intent(inout), allocatable :: send_counts_per_proc(:), recv_counts_per_proc(:)
        integer(int32) :: i, ierr, current_pos, num_all_partners
        integer(int32), allocatable :: all_partner_ranks(:)

        allocate (send_counts_per_proc(0:self%num_procs - 1), recv_counts_per_proc(0:self%num_procs - 1))
        send_counts_per_proc = 0
        recv_counts_per_proc = 0

        do i = 1, size(halo_owners)
            send_counts_per_proc(halo_owners(i)) = send_counts_per_proc(halo_owners(i)) + 1
        end do

        call MPI_Alltoall(send_counts_per_proc, 1, MPI_INT32_T, recv_counts_per_proc, 1, MPI_INT32_T, self%comm, ierr)
        call handle_mpi_error(ierr, "MPI_Alltoall for request counts")

        num_all_partners = count(send_counts_per_proc > 0 .or. recv_counts_per_proc > 0)
        allocate (all_partner_ranks(num_all_partners))
        current_pos = 0
        do i = 0, self%num_procs - 1
            if (send_counts_per_proc(i) > 0 .or. recv_counts_per_proc(i) > 0) then
                current_pos = current_pos + 1
                all_partner_ranks(current_pos) = i
            end if
        end do

        self%num_partners = num_all_partners
        allocate (self%partners(self%num_partners))
        self%partners = all_partner_ranks

        if (self%num_partners > 0) then
            allocate (self%send_counts(self%num_partners), self%recv_counts(self%num_partners))
            allocate (self%send_displs(self%num_partners), self%recv_displs(self%num_partners))
            do i = 1, self%num_partners
                self%send_counts(i) = send_counts_per_proc(self%partners(i))
                self%recv_counts(i) = recv_counts_per_proc(self%partners(i))
            end do
            self%send_displs(1) = 0
            self%recv_displs(1) = 0
            do i = 2, self%num_partners
                self%send_displs(i) = self%send_displs(i - 1) + self%send_counts(i - 1)
                self%recv_displs(i) = self%recv_displs(i - 1) + self%recv_counts(i - 1)
            end do
        end if
        deallocate (all_partner_ranks)
    end subroutine exchange_communication_plan

    !> @brief Send requested GIDs and receive needed GIDs to build send indices.
    subroutine exchange_gids_and_build_indices(self, halo_gids, send_counts_per_proc, recv_counts_per_proc)
        class(type_halo_communicator), intent(inout) :: self
        integer(int64), intent(in) :: halo_gids(:)
        integer(int32), intent(in) :: send_counts_per_proc(:), recv_counts_per_proc(:)
        integer(int32) :: i, ierr, found_idx
        integer(int32), allocatable :: send_displs_per_proc(:), recv_displs_per_proc(:)
        integer(int64), allocatable :: gids_others_need_from_me(:)

        allocate (send_displs_per_proc(0:self%num_procs - 1), recv_displs_per_proc(0:self%num_procs - 1))
        send_displs_per_proc(0) = 0
        recv_displs_per_proc(0) = 0
        do i = 1, self%num_procs - 1
            send_displs_per_proc(i) = send_displs_per_proc(i - 1) + send_counts_per_proc(i)
            recv_displs_per_proc(i) = recv_displs_per_proc(i - 1) + recv_counts_per_proc(i)
        end do

        allocate (gids_others_need_from_me(sum(recv_counts_per_proc)))
        call MPI_Alltoallv(halo_gids, send_counts_per_proc, send_displs_per_proc, MPI_INT64_T, &
                           gids_others_need_from_me, recv_counts_per_proc, recv_displs_per_proc, MPI_INT64_T, &
                           self%comm, ierr)
        call handle_mpi_error(ierr, "MPI_Alltoallv for GIDs")

        deallocate (send_displs_per_proc, recv_displs_per_proc)

        allocate (self%send_indices(size(gids_others_need_from_me)))
        do i = 1, size(gids_others_need_from_me)
            found_idx = int(binary_find(gids_others_need_from_me(i), self%sorted_local_gids), int32)
            if (found_idx > 0) then
                self%send_indices(i) = self%sorted_local_lids(found_idx)
            else
                write (*, '(A, I0, A, I0)') 'FATAL: Could not find LID for a requested GID. Rank=', self%my_rank, &
                    ', GID=', gids_others_need_from_me(i)
                call MPI_Abort(MPI_COMM_WORLD, 1)
            end if
        end do
        deallocate (gids_others_need_from_me)
    end subroutine exchange_gids_and_build_indices

    !> @brief Allocate or reallocate send/recv buffers as needed.
    subroutine ensure_buffers_ready(self, required_send_size, required_recv_size, num_components)
        class(type_halo_communicator), intent(inout) :: self
        integer(int32), intent(in) :: required_send_size, required_recv_size
        integer(int32), intent(in), optional :: num_components

        if (.not. allocated(self%send_buf) .or. size(self%send_buf) < required_send_size) then
            call deallocate_array(self%send_buf)
            allocate (self%send_buf(max(0, required_send_size)))
        end if

        if (.not. allocated(self%recv_buf) .or. size(self%recv_buf) < required_recv_size) then
            call deallocate_array(self%recv_buf)
            allocate (self%recv_buf(max(0, required_recv_size)))
        end if

        if (present(num_components) .and. self%num_partners > 0) then
            if (.not. allocated(self%send_counts_vector)) then
                allocate (self%send_counts_vector(self%num_partners))
                allocate (self%send_displs_vector(self%num_partners))
                allocate (self%recv_counts_vector(self%num_partners))
                allocate (self%recv_displs_vector(self%num_partners))
            end if
            self%send_counts_vector = self%send_counts * num_components
            self%send_displs_vector = self%send_displs * num_components
            self%recv_counts_vector = self%recv_counts * num_components
            self%recv_displs_vector = self%recv_displs * num_components
        end if
    end subroutine ensure_buffers_ready

    !> @brief Display communicator state to stdout (for debugging).
    subroutine display_communicator_state(self)
        class(type_halo_communicator), intent(in) :: self
        integer(int32) :: i, j, ierr

        call MPI_Barrier(self%comm, ierr)
        if (self%my_rank == 0) then
            write (*, '(A)') '--- Halo Communicator State ---'
        end if

        do i = 0, self%num_procs - 1
            call MPI_Barrier(self%comm, ierr)
            if (self%my_rank == i) then
                write (*, '(A, I0, A)') "[Rank ", self%my_rank, "]"
                write (*, '(4X, A, I0)') "Number of partners: ", self%num_partners
                if (self%num_partners > 0) then
                    write (*, '(4X, A)') "Partner | Send Count | Recv Count"
                    write (*, '(4X, A)') "---------------------------------"
                    do j = 1, self%num_partners
                        write (*, '(4X, I7, " | ", I10, " | ", I10)') &
                            self%partners(j), self%send_counts(j), self%recv_counts(j)
                    end do
                end if
            end if
        end do
        call MPI_Barrier(self%comm, ierr)
    end subroutine display_communicator_state

    !> @brief Handle MPI errors: print message and abort.
    subroutine handle_mpi_error(ierr, msg)
        integer(int32), intent(in) :: ierr
        character(len=*), intent(in) :: msg
        integer(int32) :: err_len, rank, mpi_ierr
        character(len=MPI_MAX_ERROR_STRING) :: err_str
        if (ierr == MPI_SUCCESS) return
        call MPI_Comm_rank(MPI_COMM_WORLD, rank, mpi_ierr)
        call MPI_Error_string(ierr, err_str, err_len)
        write (*, '(A,I0,A,A,A,A)') "MPI ERROR (rank ", rank, "): ", trim(msg), " - ", trim(err_str(1:err_len))
        call MPI_Abort(MPI_COMM_WORLD, ierr)
    end subroutine handle_mpi_error

    !--------------------------------------------------------------------------
    ! Sorting utilities
    !--------------------------------------------------------------------------

    !> @brief Non-recursive quicksort of GID-LID pairs by GID.
    subroutine quicksort_gid_lid_pairs(gids, lids, low, high)
        integer(int64), intent(inout) :: gids(:)
        integer(int32), intent(inout) :: lids(:)
        integer(int32), intent(in) :: low, high
        integer(int32), parameter :: MAX_DEPTH = 128
        integer(int32) :: stack_low(MAX_DEPTH), stack_high(MAX_DEPTH)
        integer(int32) :: sp, l, h, p_idx

        if (low >= high) return
        sp = 1
        stack_low(sp) = low
        stack_high(sp) = high

        do while (sp > 0)
            l = stack_low(sp)
            h = stack_high(sp)
            sp = sp - 1
            if (l >= h) cycle
            p_idx = partition_gid_lid(gids, lids, l, h)
            ! Push smaller partition first to limit stack depth
            if ((p_idx - l) > (h - p_idx)) then
                if (l < p_idx - 1) then
                    sp = sp + 1; if (sp > MAX_DEPTH) call handle_fatal_sort_error("gid_lid")
                    stack_low(sp) = l; stack_high(sp) = p_idx - 1
                end if
                if (p_idx + 1 < h) then
                    sp = sp + 1; if (sp > MAX_DEPTH) call handle_fatal_sort_error("gid_lid")
                    stack_low(sp) = p_idx + 1; stack_high(sp) = h
                end if
            else
                if (p_idx + 1 < h) then
                    sp = sp + 1; if (sp > MAX_DEPTH) call handle_fatal_sort_error("gid_lid")
                    stack_low(sp) = p_idx + 1; stack_high(sp) = h
                end if
                if (l < p_idx - 1) then
                    sp = sp + 1; if (sp > MAX_DEPTH) call handle_fatal_sort_error("gid_lid")
                    stack_low(sp) = l; stack_high(sp) = p_idx - 1
                end if
            end if
        end do
    end subroutine quicksort_gid_lid_pairs

    !> @brief Partition function for GID-LID quicksort.
    function partition_gid_lid(gids, lids, low, high) result(p_idx)
        integer(int64), intent(inout) :: gids(:)
        integer(int32), intent(inout) :: lids(:)
        integer(int32), intent(in) :: low, high
        integer(int32) :: p_idx, i, j
        integer(int64) :: pivot_gid

        pivot_gid = gids(high)
        i = low - 1
        do j = low, high - 1
            if (gids(j) <= pivot_gid) then
                i = i + 1
                call swap(gids(i), gids(j))
                call swap(lids(i), lids(j))
            end if
        end do
        call swap(gids(i + 1), gids(high))
        call swap(lids(i + 1), lids(high))
        p_idx = i + 1
    end function partition_gid_lid

    !> @brief Non-recursive quicksort of rank-LID-GID triplets by rank.
    subroutine quicksort_rank_lid_gid_triplets(ranks, lids, gids, low, high)
        integer(int32), intent(inout) :: ranks(:), lids(:)
        integer(int64), intent(inout) :: gids(:)
        integer(int32), intent(in) :: low, high
        integer(int32), parameter :: MAX_DEPTH = 128
        integer(int32) :: stack_low(MAX_DEPTH), stack_high(MAX_DEPTH)
        integer(int32) :: sp, l, h, p_idx

        if (low >= high) return
        sp = 1
        stack_low(sp) = low
        stack_high(sp) = high
        do while (sp > 0)
            l = stack_low(sp)
            h = stack_high(sp)
            sp = sp - 1
            if (l >= h) cycle
            p_idx = partition_rank_lid_gid(ranks, lids, gids, l, h)
            if ((p_idx - l) > (h - p_idx)) then
                if (l < p_idx - 1) then
                    sp = sp + 1; if (sp > MAX_DEPTH) call handle_fatal_sort_error("rank_lid_gid")
                    stack_low(sp) = l; stack_high(sp) = p_idx - 1
                end if
                if (p_idx + 1 < h) then
                    sp = sp + 1; if (sp > MAX_DEPTH) call handle_fatal_sort_error("rank_lid_gid")
                    stack_low(sp) = p_idx + 1; stack_high(sp) = h
                end if
            else
                if (p_idx + 1 < h) then
                    sp = sp + 1; if (sp > MAX_DEPTH) call handle_fatal_sort_error("rank_lid_gid")
                    stack_low(sp) = p_idx + 1; stack_high(sp) = h
                end if
                if (l < p_idx - 1) then
                    sp = sp + 1; if (sp > MAX_DEPTH) call handle_fatal_sort_error("rank_lid_gid")
                    stack_low(sp) = l; stack_high(sp) = p_idx - 1
                end if
            end if
        end do
    end subroutine quicksort_rank_lid_gid_triplets

    !> @brief Partition function for rank-LID-GID quicksort.
    function partition_rank_lid_gid(ranks, lids, gids, low, high) result(p_idx)
        integer(int32), intent(inout) :: ranks(:), lids(:)
        integer(int64), intent(inout) :: gids(:)
        integer(int32), intent(in) :: low, high
        integer(int32) :: p_idx, i, j, pivot_rank

        pivot_rank = ranks(high)
        i = low - 1
        do j = low, high - 1
            if (ranks(j) <= pivot_rank) then
                i = i + 1
                call swap(ranks(i), ranks(j))
                call swap(lids(i), lids(j))
                call swap(gids(i), gids(j))
            end if
        end do
        call swap(ranks(i + 1), ranks(high))
        call swap(lids(i + 1), lids(high))
        call swap(gids(i + 1), gids(high))
        p_idx = i + 1
    end function partition_rank_lid_gid

    !> @brief Abort on sort internal stack overflow.
    subroutine handle_fatal_sort_error(msg)
        character(len=*), intent(in) :: msg
        write (*, '(A,A,A)') "FATAL: Quicksort internal stack for '", trim(msg), "' overflowed."
        call MPI_Abort(MPI_COMM_WORLD, -1)
    end subroutine handle_fatal_sort_error

    subroutine swap_i32(a, b)
        integer(int32), intent(inout) :: a, b
        integer(int32) :: tmp
        tmp = a
        a = b
        b = tmp
    end subroutine swap_i32

    subroutine swap_i64(a, b)
        integer(int64), intent(inout) :: a, b
        integer(int64) :: tmp
        tmp = a
        a = b
        b = tmp
    end subroutine swap_i64

end module numerical_parallel_communicator
