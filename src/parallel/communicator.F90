!>
!> Module for managing halo communication in parallel computing environments.
!>
module parallel_communicator
    use, intrinsic :: iso_fortran_env
    use :: mpi_f08
    use :: stdlib_sorting, only:sort
    use :: module_input, only:type_input
    use :: module_core
    implicit none
    private
    public :: type_halo_communicator

    !>
    !> Generic interface for updating halo data for different data types/ranks.
    !>
    ! interface update_halo
    !     module procedure update_halo_scalar
    ! end interface

    !>
    !> Manages all data structures and operations for halo communication.
    !>
    type :: type_halo_communicator
        private
        integer(int32) :: my_rank = -1
        integer(int32) :: num_procs = -1
        integer(int32) :: num_partners = 0
        integer(int32), allocatable :: partners(:)

        ! Send-related data
        integer(int32), allocatable :: send_counts(:), send_displs(:)
        integer(int32), allocatable :: send_indices(:)

        ! Receive-related data
        integer(int32), allocatable :: recv_counts(:), recv_displs(:)
        integer(int32), allocatable :: recv_indices(:)

    contains
        procedure, pass(self), public :: initialize => initialize_halo_communicator
        procedure, pass(self), private :: setup_communication_partners
        procedure, pass(self), private :: compute_communication_indices
    end type type_halo_communicator

contains

    !>
    !> Initializes the halo communicator with the given input configuration.
    !>
    subroutine initialize_halo_communicator(self, input)
        implicit none
        class(type_halo_communicator), intent(inout) :: self
        class(type_input), intent(in) :: input

        integer(int32) :: rank_tmp, size_tmp

        call MPI_Comm_rank(MPI_COMM_WORLD, rank_tmp)
        call MPI_Comm_size(MPI_COMM_WORLD, size_tmp)
        self%my_rank = rank_tmp
        self%num_procs = size_tmp

        ! Exit if the mesh data for communication is not allocated (e.g., serial run).
        if (.not. allocated(input%geometry%vtk%communication_partners)) then
            self%num_partners = 0
            return
        end if

        call self%setup_communication_partners(input)
        if (self%num_partners == 0) return

        call self%compute_communication_indices(input)
    end subroutine initialize_halo_communicator

    !>
    !> Identifies unique communication partners based on the pre-processed mesh data.
    !> This routine correctly interprets the communication_partners array.
    !>
    subroutine setup_communication_partners(self, input)
        implicit none
        !> Sets up the list of unique communication partners for halo exchange.
        class(type_halo_communicator), intent(inout) :: self
        !> Input configuration containing mesh and communication data.
        class(type_input), intent(in) :: input

        integer(int32) :: i, p
        integer(int32), allocatable :: unique_partners(:)
        ! Use a temporary, dynamically sized array to collect all partners
        integer(int32), allocatable :: collected_partners(:)

        allocate (collected_partners(0))

        ! Scan all local nodes to find ranks to communicate with.
        do i = 1, input%geometry%vtk%num_points
            select case (input%geometry%vtk%node_type(i))
            case (NODE_BORDER)
                ! If I am a border node (owner), I send data to receivers.
                do p = 0, self%num_procs
                    if (input%geometry%vtk%communication_partners(p + 1, i) == ROLE_RECEIVER) then
                        collected_partners = [collected_partners, p]
                    end if
                end do
            case (NODE_HALO)
                ! If I am a halo node (receiver), I receive data from the owner.
                do p = 0, self%num_procs
                    if (input%geometry%vtk%communication_partners(p + 1, i) == ROLE_OWNER) then
                        collected_partners = [collected_partners, p]
                        exit ! There is only one owner per node.
                    end if
                end do
            end select
        end do

        if (size(collected_partners) == 0) then
            self%num_partners = 0
            call deallocate_array(collected_partners)
            return
        end if

        ! Find the unique set of partners.
        call sort(collected_partners)
        call unique(collected_partners, unique_partners)
        call deallocate_array(collected_partners)

        self%num_partners = size(unique_partners)
        call allocate_array(self%partners, self%num_partners)
        self%partners = unique_partners

    end subroutine setup_communication_partners

    !>
    !> Computes send/receive counts, displacements, and indices for halo communication.
    !>
    subroutine compute_communication_indices(self, input)
        implicit none
        !> Computes the indices and counts for sending and receiving halo data.
        class(type_halo_communicator), intent(inout) :: self
        !> Input configuration containing mesh and communication data.
        class(type_input), intent(in) :: input

        integer(int32) :: i, p, partner_idx, total_sends, total_recvs
        integer(int32), allocatable :: temp_send_counters(:), temp_recv_counters(:)

        ! Step 1: Count send and receive nodes for each partner.
        call allocate_array(self%send_counts, self%num_partners)
        call allocate_array(self%recv_counts, self%num_partners)
        self%send_counts = 0
        self%recv_counts = 0

        do i = 1, input%geometry%vtk%num_points
            select case (input%geometry%vtk%node_type(i))
            case (NODE_BORDER)
                ! I am the owner of this node, so I send data.
                do p = 0, self%num_procs - 1
                    if (input%geometry%vtk%communication_partners(p + 1, i) == ROLE_RECEIVER) then
                        partner_idx = binary_find(int(p, int32), self%partners)
                        if (partner_idx > 0) then
                            self%send_counts(partner_idx) = self%send_counts(partner_idx) + 1
                        end if
                    end if
                end do
            case (NODE_HALO)
                ! I am not the owner of this node, so I receive data.
                do p = 0, self%num_procs - 1
                    if (input%geometry%vtk%communication_partners(p + 1, i) == ROLE_OWNER) then
                        partner_idx = binary_find(int(p, int32), self%partners)
                        if (partner_idx > 0) then
                            self%recv_counts(partner_idx) = self%recv_counts(partner_idx) + 1
                        end if
                        exit ! Only one owner.
                    end if
                end do
            end select
        end do

        ! Step 2: Compute displacements for MPI_Alltoallv.
        call allocate_array(self%send_displs, self%num_partners)
        call allocate_array(self%recv_displs, self%num_partners)
        self%send_displs = 0
        self%recv_displs = 0

        do i = 2, self%num_partners
            self%send_displs(i) = self%send_displs(i - 1) + self%send_counts(i - 1)
            self%recv_displs(i) = self%recv_displs(i - 1) + self%recv_counts(i - 1)
        end do

        ! Step 3: Build the send_indices and recv_indices arrays.
        total_sends = sum(self%send_counts)
        total_recvs = sum(self%recv_counts)
        call allocate_array(self%send_indices, total_sends)
        call allocate_array(self%recv_indices, total_recvs)

        ! Use temporary counters to fill the index arrays at the correct positions.
        call allocate_array(temp_send_counters, self%num_partners)
        call allocate_array(temp_recv_counters, self%num_partners)
        temp_send_counters = 0
        temp_recv_counters = 0

        do i = 1, input%geometry%vtk%num_points
            select case (input%geometry%vtk%node_type(i))
            case (NODE_BORDER)
                do p = 0, self%num_procs - 1
                    if (input%geometry%vtk%communication_partners(p + 1, i) == ROLE_RECEIVER) then
                        partner_idx = binary_find(int(p, int32), self%partners)
                        if (partner_idx > 0) then
                            self%send_indices(self%send_displs(partner_idx) + temp_send_counters(partner_idx) + 1) = i
                            temp_send_counters(partner_idx) = temp_send_counters(partner_idx) + 1
                        end if
                    end if
                end do
            case (NODE_HALO)
                do p = 0, self%num_procs - 1
                    if (input%geometry%vtk%communication_partners(p + 1, i) == ROLE_OWNER) then
                        partner_idx = binary_find(int(p, int32), self%partners)
                        if (partner_idx > 0) then
                            self%recv_indices(self%recv_displs(partner_idx) + temp_recv_counters(partner_idx) + 1) = i
                            temp_recv_counters(partner_idx) = temp_recv_counters(partner_idx) + 1
                        end if
                        exit ! Only one owner.
                    end if
                end do
            end select
        end do

        call deallocate_array(temp_send_counters)
        call deallocate_array(temp_recv_counters)

        print *, "DEBUG: Halo communicator initialized on rank ", self%my_rank
        print *, "  Number of partners: ", self%num_partners
        print *, "  Partners: ", self%partners
        print *, "  Send counts: ", self%send_counts
        print *, "  Send displs: ", self%send_displs
        print *, "  Recv counts: ", self%recv_counts
        print *, "  Recv displs: ", self%recv_displs
        print *, "  Total sends: ", total_sends
        print *, "  Total recvs: ", total_recvs
    end subroutine compute_communication_indices

    !>
    !> Updates halo data for a scalar field (rank-1 array of real64).
    !>
    ! subroutine update_halo_scalar(self, data_array)
    !     class(type_halo_communicator), intent(in) :: self
    !     real(real64), intent(inout) :: data_array(:)
    !     real(real64), allocatable :: send_buffer(:), recv_buffer(:)
    !     integer(int32) :: ierror, total_sends, total_recvs

    !     if (self%num_partners == 0) return

    !     ! Pack data to be sent.
    !     total_sends = sum(self%send_counts)
    !     call allocate_array(send_buffer, total_sends)
    !     send_buffer = data_array(self%send_indices)

    !     ! Allocate buffer for incoming data.
    !     total_recvs = sum(self%recv_counts)
    !     call allocate_array(recv_buffer, total_recvs)

    !     ! Perform MPI communication.
    !     call MPI_Alltoallv(send_buffer, self%send_counts, self%send_displs, MPI_DOUBLE_PRECISION, &
    !                        recv_buffer, self%recv_counts, self%recv_displs, MPI_DOUBLE_PRECISION, &
    !                        MPI_COMM_WORLD, ierror)

    !     ! Unpack received data into the main data array.
    !     data_array(self%recv_indices) = recv_buffer

    !     call deallocate_array(send_buffer, recv_buffer)
    ! end subroutine update_halo_scalar

end module parallel_communicator
