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

    type :: type_halo_communicator
        private
        integer :: my_rank = -1
        integer :: num_procs = -1
        integer :: num_partners = 0
        integer, allocatable :: partners(:)
        integer, allocatable :: send_counts(:), send_displs(:)
        integer, allocatable :: recv_counts(:), recv_displs(:)
        integer, allocatable :: send_indices(:)
        integer, allocatable :: recv_indices(:)
        type(MPI_Datatype), allocatable :: send_types(:), recv_types(:)
        type(MPI_Request), allocatable :: requests(:)
    contains
        procedure, pass(self), public :: initialize => initialize_halo_communicator

        procedure, pass(self), public :: setup_basic_info
        ! procedure, pass(self), public :: update => update_halo_communicator
        ! final :: finalize_communicator
    end type type_halo_communicator

contains

    !>
    !> Initializes the halo communicator with the given input configuration.
    !>
    subroutine initialize_halo_communicator(self, input)
        implicit none
        !> Halo communicator object to be initialized.
        class(type_halo_communicator), intent(inout) :: self
        !> Input configuration object containing geometry and VTK information.
        class(type_input), intent(in) :: input

        ! 基本情報の設定
        call self%setup_basic_info(input)
        if (self%num_partners == 0) return

        ! 送信先インデックスとカウント計算
        ! call compute_send_indices(self, input)

        ! パートナー間の受信カウントを取得
        ! call exchange_counts(self, ierror)
        ! call check_mpi(ierror, self%comm)

        ! 受信インデックスを取得
        ! call exchange_global_ids(self, input, ierror)
        ! call check_mpi(ierror, self%comm)

        ! ! MPI データ型の作成
        ! call create_halo_datatypes(self, ierror)
        ! call check_mpi(ierror, self%comm)
    end subroutine initialize_halo_communicator

    !>
    !> Sets up basic information for the halo communicator, including rank, number of processes,
    !> and communication partners.
    !>
    subroutine setup_basic_info(self, input)
        implicit none
        !> Halo communicator object to be initialized.
        class(type_halo_communicator), intent(inout) :: self
        !> Input configuration object containing geometry and VTK information.
        class(type_input), intent(in) :: input

        integer(int32), allocatable :: all_partners_with_duplicates(:)
        integer(int32), allocatable :: all_partners(:)

        self%my_rank = input%geometry%vtk%my_rank
        self%num_procs = input%geometry%vtk%num_procs

        if (.not. allocated(input%geometry%vtk%communication_partners)) then
            self%num_partners = 0
            return
        end if

        all_partners_with_duplicates = pack(input%geometry%vtk%communication_partners, &
                                            input%geometry%vtk%communication_partners /= -1 .and. &
                                            input%geometry%vtk%communication_partners /= self%my_rank)
        call sort(all_partners_with_duplicates)
        call unique(all_partners_with_duplicates, all_partners)

        self%num_partners = size(all_partners)
        if (self%num_partners == 0) return

        call allocate_array(self%partners, self%num_partners)
        self%partners = all_partners

        print *, "Rank ", self%my_rank, " has ", self%num_partners, " partners: ", self%partners
    end subroutine setup_basic_info

end module parallel_communicator
