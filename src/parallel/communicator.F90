!>
!> @brief Halo (ゴーストセル) 通信を管理するモジュール
!> @details
!> MPIを用いた並列計算において、領域境界に配置されたノード (ハロ) の
!> データ交換を効率的に行うためのデータ構造と手続きを提供します.
!>
!> ---
!> @usage
!> 1. `type(type_halo_communicator)` の変数を宣言します.
!>    `type(type_halo_communicator) :: halo_comm`
!> 2. `initialize` を呼び出し、メッシュ情報から通信スケジュールを構築します.
!>    `call halo_comm%initialize(input)`
!> 3. `display` (任意) を呼び出して、構築されたスケジュールをデバッグします.
!>    `call halo_comm%display()`
!> 4. `update_halo` を呼び出して、スカラー場やベクトル場のハロデータを更新します.
!>    `call update_halo(halo_comm, temperature_field)`
!>    `call update_halo(halo_comm, velocity_field, num_components=3)`
!> ---
!> @note
!> MPI通信には `MPI_Neighbor_alltoallv` を使用しています.
!> 最高のパフォーマンスを得るには、事前に `MPI_Dist_graph_create_adjacent` などで
!> トポロジ情報を持つコミュニケータを作成し、それを渡すことが推奨されます.
!>
module parallel_communicator
    use, intrinsic :: iso_fortran_env
    use :: mpi_f08
    use :: module_input, only:type_input
    use :: module_core, only:NODE_BORDER, NODE_HALO, ROLE_OWNER, ROLE_RECEIVER
    implicit none
    private

    public :: type_halo_communicator
    public :: update_halo

    !>
    !> @brief 異なるデータ型・ランクの配列に対応するハロ更新の汎用インターフェース
    !>
    interface update_halo
        module procedure update_halo_scalar
        module procedure update_halo_vector
    end interface

    !>
    !> @brief ハロ交換に必要な全てのデータ構造と操作をカプセル化する型
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
        procedure, pass(self), public :: display => display_communicator_state
        procedure, pass(self), private :: build_communication_schedule
    end type type_halo_communicator

    !>
    !> @brief 動的に拡張可能な整数リストを管理する型 (64-bit版も追加)
    !>
    type :: dynamic_int32_list
        integer(int32), allocatable :: data(:)
    end type dynamic_int32_list

    type :: dynamic_int64_list
        integer(int64), allocatable :: data(:)
    end type dynamic_int64_list

contains

    !>
    !> @brief Halo communicatorを初期化し、通信スケジュールを構築する
    !> @param[inout] self Halo communicatorオブジェクト
    !> @param[in]    input メッシュ情報を含む入力データオブジェクト
    !>
    subroutine initialize_halo_communicator(self, input)
        class(type_halo_communicator), intent(inout) :: self
        class(type_input), intent(in) :: input
        integer(int32) :: rank_tmp, size_tmp

        call MPI_Comm_rank(MPI_COMM_WORLD, rank_tmp)
        call MPI_Comm_size(MPI_COMM_WORLD, size_tmp)
        self%my_rank = rank_tmp
        self%num_procs = size_tmp

        ! ! メッシュに分散情報(communication_partners)がなければ何もしない
        ! if (.not. associated(input%geometry%vtk%communication_partners)) then
        !     self%num_partners = 0
        !     return
        ! end if
        ! ! グローバルIDがなければエラー (Two-Pass方式に必須)
        ! if (.not. associated(input%geometry%vtk%global_node_id)) then
        !     call handle_error(99, "global_node_id is required for building communication schedule.")
        ! end if

        call self%build_communication_schedule(input)
    end subroutine initialize_halo_communicator

    !>
    !> @brief スカラー場 (1次元配列) のハロデータを更新する
    !>
    subroutine update_halo_scalar(self, data_array)
        class(type_halo_communicator), intent(in) :: self
        real(real64), intent(inout) :: data_array(:)
        real(real64), allocatable :: send_buffer(:), recv_buffer(:)
        integer(int32) :: total_sends, total_recvs, stat
        integer :: ierror

        if (self%num_partners == 0) return

        total_sends = sum(self%send_counts)
        total_recvs = sum(self%recv_counts)
        allocate (send_buffer(total_sends), stat=stat)
        call handle_error(stat, "allocating send_buffer in scalar update")
        allocate (recv_buffer(total_recvs), stat=stat)
        call handle_error(stat, "allocating recv_buffer in scalar update")

        send_buffer = data_array(self%send_indices)

        call MPI_Neighbor_alltoallv(send_buffer, self%send_counts, self%send_displs, MPI_DOUBLE_PRECISION, &
                                    recv_buffer, self%recv_counts, self%recv_displs, MPI_DOUBLE_PRECISION, &
                                    MPI_COMM_WORLD, ierror)
        call handle_error(ierror, "MPI_Neighbor_alltoallv in scalar update")

        data_array(self%recv_indices) = recv_buffer

        deallocate (send_buffer, recv_buffer)
    end subroutine update_halo_scalar

    !>
    !> @brief ベクトル場 (2次元配列) のハロデータを更新する
    !>
    subroutine update_halo_vector(self, data_array, num_components)
        class(type_halo_communicator), intent(in) :: self
        real(real64), intent(inout) :: data_array(:, :)
        integer(int32), intent(in) :: num_components
        real(real64), allocatable :: send_buffer(:), recv_buffer(:)
        integer(int32), allocatable :: send_counts_vec(:), send_displs_vec(:)
        integer(int32), allocatable :: recv_counts_vec(:), recv_displs_vec(:)
        integer(int32) :: total_sends, total_recvs, i, stat
        integer :: ierror

        if (self%num_partners == 0) return

        total_sends = sum(self%send_counts)
        total_recvs = sum(self%recv_counts)
        allocate (send_buffer(total_sends * num_components), stat=stat)
        call handle_error(stat, "allocating send_buffer in vector update")
        allocate (recv_buffer(total_recvs * num_components), stat=stat)
        call handle_error(stat, "allocating recv_buffer in vector update")

        do i = 1, total_sends
            send_buffer((i - 1) * num_components + 1:i * num_components) = data_array(:, self%send_indices(i))
        end do

        allocate (send_counts_vec(self%num_partners), send_displs_vec(self%num_partners), &
                  recv_counts_vec(self%num_partners), recv_displs_vec(self%num_partners), stat=stat)
        call handle_error(stat, "allocating vector count/displ arrays")
        send_counts_vec = self%send_counts * num_components
        send_displs_vec = self%send_displs * num_components
        recv_counts_vec = self%recv_counts * num_components
        recv_displs_vec = self%recv_displs * num_components

        call MPI_Neighbor_alltoallv(send_buffer, send_counts_vec, send_displs_vec, MPI_DOUBLE_PRECISION, &
                                    recv_buffer, recv_counts_vec, recv_displs_vec, MPI_DOUBLE_PRECISION, &
                                    MPI_COMM_WORLD, ierror)
        call handle_error(ierror, "MPI_Neighbor_alltoallv in vector update")

        do i = 1, total_recvs
            data_array(:, self%recv_indices(i)) = recv_buffer((i - 1) * num_components + 1:i * num_components)
        end do

        deallocate (send_buffer, recv_buffer)
        deallocate (send_counts_vec, send_displs_vec, recv_counts_vec, recv_displs_vec)
    end subroutine update_halo_vector

    !>
    !> @brief Communicatorの内部状態をMarkdown形式でデバッグ出力する
    !>
    subroutine display_communicator_state(self)
        class(type_halo_communicator), intent(in) :: self
        integer :: i, p, rank_to_print, start_idx, end_idx

        call MPI_Barrier(MPI_COMM_WORLD)
        do rank_to_print = 0, self%num_procs - 1
            if (self%my_rank == rank_to_print) then
                write (*, '(A)') '---'
                write (*, '("### Halo Communicator State [Rank ", I0, "/", I0, "]")') self%my_rank, self%num_procs
                write (*, *)
                if (self%num_partners == 0) then
                    write (*, '(A)') 'This rank has no communication partners.'
                else
                    write (*, '("**Summary:** ", I0, " communication partner(s).")') self%num_partners
                    write (*, '(A)') '| Partner Rank | Send Count | Recv Count | Send Displ | Recv Displ |'
                    write (*, '(A)') '|--------------|------------|------------|------------|------------|'
                    do i = 1, self%num_partners
                        write (*, '("| ", I12, " | ", I10, " | ", I10, " | ", I10, " | ", I10, " |")') &
                            self%partners(i), self%send_counts(i), self%recv_counts(i), &
                            self%send_displs(i), self%recv_displs(i)
                    end do
                    write (*, *)
                    write (*, '(A)') '#### Detailed Send/Receive Node Indices'
                    do i = 1, self%num_partners
                        p = self%partners(i)
                        write (*, '(" - **To/From Rank `", I0, "`**:")') p
                        start_idx = self%send_displs(i) + 1
                        end_idx = self%send_displs(i) + self%send_counts(i)
                        if (start_idx <= end_idx) then
                            write (*, '("   - `send_indices`: ", 15(I0, ", "))') self%send_indices(start_idx:end_idx)
                        end if
                        start_idx = self%recv_displs(i) + 1
                        end_idx = self%recv_displs(i) + self%recv_counts(i)
                        if (start_idx <= end_idx) then
                            write (*, '("   - `recv_indices`: ", 15(I0, ", "))') self%recv_indices(start_idx:end_idx)
                        end if
                    end do
                end if
            end if
            call MPI_Barrier(MPI_COMM_WORLD)
        end do
    end subroutine display_communicator_state

    ! ======================================================================
    ! PRIVATE SUBROUTINES
    ! ======================================================================

    !>
    !> @brief Two-Pass方式で堅牢な通信スケジュールを構築する
    !> @details
    !> Pass 1: 各プロセスが必要とするノードのグローバルIDをオーナープロセスに要求する.
    !> Pass 2: 要求を受け取ったプロセスが、対応するローカルノードのインデックスを送信リストに加える.
    !> これにより、送受信の数と順序の整合性を保証する.
    !>
    subroutine build_communication_schedule(self, input)
        class(type_halo_communicator), intent(inout) :: self
        class(type_input), intent(in) :: input

        integer(int32) :: i, p, stat, ierror, total_send_reqs, total_recv_reqs
        integer(int32), allocatable :: send_counts_req(:), recv_counts_req(:)
        integer(int32), allocatable :: send_displs_req(:), recv_displs_req(:)
        integer(int64), allocatable :: send_buffer_gid(:), recv_buffer_gid(:)
        logical, allocatable :: is_partner_flag(:)

        type(dynamic_int64_list), allocatable :: requests_to_send(:) !< [GID] to send to each proc
        type(dynamic_int32_list), allocatable :: recv_indices_map(:) !< [LID] to map received data
        type(dynamic_int32_list), allocatable :: temp_send_indices(:)

        associate (vtk => input%geometry%vtk)

            ! === Pass 1: 要求フェーズ (どのデータが欲しいかを通知) ===

            ! 1-1. 要求リストの作成 (自プロセス視点)
            !      requests_to_send(p): プロセスpに要求するGIDs
            !      recv_indices_map(p): プロセスpから受信したデータを格納するLIDs
            allocate (requests_to_send(0:self%num_procs - 1), stat=stat)
            call handle_error(stat, "allocating requests_to_send")
            allocate (recv_indices_map(0:self%num_procs - 1), stat=stat)
            call handle_error(stat, "allocating recv_indices_map")

            do i = 1, vtk%num_points
                if (vtk%node_type(i) == NODE_HALO) then
                    do p = 0, self%num_procs - 1
                        if (vtk%communication_partners(p + 1, i) == ROLE_OWNER) then
                            requests_to_send(p)%data = [requests_to_send(p)%data, int(vtk%global_node_ids(i), kind=int64)]
                            recv_indices_map(p)%data = [recv_indices_map(p)%data, i]
                            exit ! Owner is unique for a halo node
                        end if
                    end do
                end if
            end do

            ! 1-2. 要求数を全プロセスで交換
            allocate (send_counts_req(self%num_procs), recv_counts_req(self%num_procs), stat=stat)
            call handle_error(stat, "allocating req count arrays")
            do p = 1, self%num_procs
                send_counts_req(p) = size(requests_to_send(p - 1)%data)
            end do
            call MPI_Alltoall(send_counts_req, 1, MPI_INTEGER, recv_counts_req, 1, MPI_INTEGER, MPI_COMM_WORLD, ierror)
            call handle_error(ierror, "MPI_Alltoall for request counts")

            ! 1-3. 要求内容 (GIDs) を全プロセスで交換
            total_send_reqs = sum(send_counts_req)
            total_recv_reqs = sum(recv_counts_req)
            allocate (send_buffer_gid(total_send_reqs), recv_buffer_gid(total_recv_reqs), stat=stat)
            call handle_error(stat, "allocating GID buffers")
            allocate (send_displs_req(self%num_procs), recv_displs_req(self%num_procs), stat=stat)
            call handle_error(stat, "allocating req displ arrays")

            send_displs_req = 0; recv_displs_req = 0
            if (self%num_procs > 1) then
                do i = 2, self%num_procs
                    send_displs_req(i) = send_displs_req(i - 1) + send_counts_req(i - 1)
                    recv_displs_req(i) = recv_displs_req(i - 1) + recv_counts_req(i - 1)
                end do
            end if

            do p = 0, self%num_procs - 1
                if (send_counts_req(p + 1) > 0) then
                    send_buffer_gid(send_displs_req(p + 1) + 1:send_displs_req(p + 1) + send_counts_req(p + 1)) = requests_to_send(p)%data
                end if
            end do

            call MPI_Alltoallv(send_buffer_gid, send_counts_req, send_displs_req, MPI_INT64_T, &
                               recv_buffer_gid, recv_counts_req, recv_displs_req, MPI_INT64_T, &
                               MPI_COMM_WORLD, ierror)
            call handle_error(ierror, "MPI_Alltoallv for request GIDs")

            deallocate (requests_to_send)

            ! === Pass 2: 応答フェーズ (要求されたデータを準備) ===

            ! 2-1. GIDからLIDへの高速逆引きマップを作成 (Owned nodes only)
            ! Note: For extreme performance, use a hash map or sorted list + binary search.
            !       A simple linear search is used here for simplicity.
            allocate (temp_send_indices(0:self%num_procs - 1), stat=stat)
            call handle_error(stat, "allocating temp_send_indices")

            do i = 1, total_recv_reqs
                block
                    integer(int64) :: target_gid
                    integer :: owner_rank, lid, found_lid
                    target_gid = recv_buffer_gid(i)
                    found_lid = -1
                    ! find which process requested this GID
                    do owner_rank = self%num_procs - 1, 0, -1
                        if (i > recv_displs_req(owner_rank + 1)) then
                            exit
                        end if
                    end do
                    ! find corresponding LID in my domain
                    do lid = 1, vtk%num_points
                        if (int(vtk%global_node_ids(lid), kind=int64) == target_gid) then
                            if (vtk%node_type(lid) == NODE_BORDER .or. vtk%node_type(lid) == NODE_HALO) then
                                found_lid = lid
                                exit
                            end if
                        end if
                    end do
                    if (found_lid > 0) then
                        temp_send_indices(owner_rank)%data = [temp_send_indices(owner_rank)%data, found_lid]
                    else
                        call handle_error(101, "Could not find requested GID in local domain.")
                    end if
                end block
            end do
            deallocate (recv_buffer_gid)

            ! === 最終処理: Communicatorのデータ構造を構築 ===

            allocate (is_partner_flag(0:self%num_procs - 1), stat=stat)
            is_partner_flag = .false.
            do p = 0, self%num_procs - 1
                if (send_counts_req(p + 1) > 0 .or. recv_counts_req(p + 1) > 0) then
                    is_partner_flag(p) = .true.
                end if
            end do

            self%num_partners = count(is_partner_flag)
            if (self%num_partners == 0) return

            allocate (self%partners(self%num_partners), stat=stat)
            self%partners = pack([(p, p=0, self%num_procs - 1)], is_partner_flag)

            allocate (self%send_counts(self%num_partners), self%recv_counts(self%num_partners), &
                      self%send_displs(self%num_partners), self%recv_displs(self%num_partners), stat=stat)
            call handle_error(stat, "allocating final count/displ arrays")

            do i = 1, self%num_partners
                p = self%partners(i)
                self%send_counts(i) = size(temp_send_indices(p)%data) ! What I will send to p
                self%recv_counts(i) = size(recv_indices_map(p)%data) ! What I will receive from p
            end do

            self%send_displs = 0; self%recv_displs = 0
            if (self%num_partners > 1) then
                do i = 2, self%num_partners
                    self%send_displs(i) = self%send_displs(i - 1) + self%send_counts(i - 1)
                    self%recv_displs(i) = self%recv_displs(i - 1) + self%recv_counts(i - 1)
                end do
            end if

            allocate (self%send_indices(sum(self%send_counts)), stat=stat)
            call handle_error(stat, "allocating send_indices")
            allocate (self%recv_indices(sum(self%recv_counts)), stat=stat)
            call handle_error(stat, "allocating recv_indices")

            do i = 1, self%num_partners
                p = self%partners(i)
                if (self%send_counts(i) > 0) then
                    self%send_indices(self%send_displs(i) + 1:self%send_displs(i) + self%send_counts(i)) = temp_send_indices(p)%data
                end if
                if (self%recv_counts(i) > 0) then
                    self%recv_indices(self%recv_displs(i) + 1:self%recv_displs(i) + self%recv_counts(i)) = recv_indices_map(p)%data
                end if
            end do

        end associate
    end subroutine build_communication_schedule

    subroutine handle_error(error_code, message)
        integer, intent(in) :: error_code
        character(len=*), intent(in) :: message
        integer :: my_rank

        if (error_code == 0) return

        call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
        if (my_rank == 0) then
            print *, "================================================================================"
            print *, "FATAL ERROR in parallel_communicator:"
            print *, "  Message: ", trim(message)
            print *, "  Error Code: ", error_code
            print *, "Aborting execution."
            print *, "================================================================================"
        end if
        call MPI_Abort(MPI_COMM_WORLD, error_code)
    end subroutine handle_error

end module parallel_communicator
