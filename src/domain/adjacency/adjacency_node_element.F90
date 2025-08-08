module domain_adjacency_adjacency_node_element
    use, intrinsic :: iso_fortran_env, only: int32
    use :: domain_element, only:holder_elements, abst_element
    implicit none
    private

    ! --- Public Data Types ---
    public :: type_map_node_to_element

    ! --------------------------------------------------------------------------
    ! private: 内部で使うリストの定義
    ! --------------------------------------------------------------------------
    type :: type_element_list
        integer(int32), allocatable :: ids(:)
    end type type_element_list

    ! --------------------------------------------------------------------------
    ! Data Structure: type_map_node_to_element
    !
    ! マップデータと、それを作成・取得するプロシージャをカプセル化する。
    ! --------------------------------------------------------------------------
    type :: type_map_node_to_element
        private
        ! マップデータ本体
        type(type_element_list), allocatable :: map_data(:)
    contains
        ! マップを構築するプロシージャ
        procedure, pass(self) :: initialize => initialize_map
        ! 特定のノードの隣接リストを取得するプロシージャ
        procedure, pass(self) :: get_list => get_neighbor_list
        ! マップ全体を解放するプロシージャ
        procedure, pass(self) :: destroy => destroy_map
    end type type_map_node_to_element

contains

    ! --------------------------------------------------------------------------
    ! Subroutine: initialize_map
    !
    ! マップを構築する。algorithm引数で挙動を選択できる。
    ! --------------------------------------------------------------------------
    subroutine initialize_map(self, num_nodes, elements, algorithm)
        class(type_map_node_to_element), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        type(holder_elements), intent(in) :: elements(:)
        character(len=*), intent(in), optional :: algorithm

        character(len=10) :: chosen_algorithm

        ! 既存のマップがあれば解放
        call self%destroy()

        ! アルゴリズムの選択 (指定がなければ 'fast' をデフォルトに)
        if (present(algorithm)) then
            chosen_algorithm = trim(adjustl(algorithm))
        else
            chosen_algorithm = 'fast'
        end if

        select case (chosen_algorithm)
        case ('fast')
            call initialize_fast(self, num_nodes, elements)
        case ('simple')
            call initialize_simple(self, num_nodes, elements)
        case default
            ! error handling
        end select
    end subroutine initialize_map

    ! --------------------------------------------------------------------------
    ! Function: get_neighbor_list
    !
    ! 特定のノードIDに対応する隣接要素リスト(配列)へのポインタを返す。
    ! --------------------------------------------------------------------------
    function get_neighbor_list(self, node_id) result(id_list)
        class(type_map_node_to_element), intent(inout), target :: self
        integer(int32), intent(in) :: node_id
        integer(int32), pointer :: id_list(:)

        nullify (id_list)
        if (node_id < 1 .or. node_id > size(self%map_data)) return

        if (allocated(self%map_data(node_id)%ids)) then
            id_list => self%map_data(node_id)%ids
        end if
    end function get_neighbor_list

    ! --------------------------------------------------------------------------
    ! Subroutine: destroy_map
    !
    ! マップが確保したメモリを全て解放する。
    ! --------------------------------------------------------------------------
    subroutine destroy_map(self)
        class(type_map_node_to_element), intent(inout) :: self
        integer(int32) :: i
        if (.not. allocated(self%map_data)) return
        do i = 1, size(self%map_data)
            if (allocated(self%map_data(i)%ids)) then
                deallocate (self%map_data(i)%ids)
            end if
        end do
        deallocate (self%map_data)
    end subroutine destroy_map

    ! ==========================================================================
    ! private: 内部実装サブルーチン
    ! ==========================================================================

    ! --- 「高速版」アルゴリズム (3パス方式) ---
    subroutine initialize_fast(self, num_nodes, elements)
        class(type_map_node_to_element), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        type(holder_elements), intent(in) :: elements(:)

        integer(int32) :: i, ielem, inode_local, node_id, num_elements
        integer(int32), allocatable :: node_element_counts(:)
        integer(int32), allocatable :: current_indices(:)

        num_elements = size(elements)
        allocate (self%map_data(num_nodes))

        ! Phase 1: Count
        allocate (node_element_counts(num_nodes))
        node_element_counts = 0
        do ielem = 1, num_elements
            do inode_local = 1, elements(ielem)%e%get_num_nodes()
                node_id = elements(ielem)%e%get_connectivity(inode_local)
                if (node_id > 0 .and. node_id <= num_nodes) then
                    node_element_counts(node_id) = node_element_counts(node_id) + 1
                end if
            end do
        end do

        ! Phase 2: Allocate
        do i = 1, num_nodes
            if (node_element_counts(i) > 0) then
                allocate (self%map_data(i)%ids(node_element_counts(i)))
            end if
        end do
        deallocate (node_element_counts)

        ! Phase 3: Fill
        allocate (current_indices(num_nodes))
        current_indices = 1
        do ielem = 1, num_elements
            do inode_local = 1, elements(ielem)%e%get_num_nodes()
                node_id = elements(ielem)%e%get_connectivity(inode_local)
                if (node_id > 0 .and. node_id <= num_nodes) then
                    self%map_data(node_id)%ids(current_indices(node_id)) = ielem
                    current_indices(node_id) = current_indices(node_id) + 1
                end if
            end do
        end do
        deallocate (current_indices)
    end subroutine initialize_fast

    ! --- 「シンプル版」アルゴリズム (毎回再確保) ---
    subroutine initialize_simple(self, num_nodes, elements)
        class(type_map_node_to_element), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        type(holder_elements), intent(in) :: elements(:)

        integer(int32) :: ielem, inode_local, node_id, num_elements, current_size
        integer(int32), allocatable :: temp_list(:)

        num_elements = size(elements)
        allocate (self%map_data(num_nodes))

        do ielem = 1, num_elements
            do inode_local = 1, elements(ielem)%e%get_num_nodes()
                node_id = elements(ielem)%e%get_connectivity(inode_local)
                if (node_id > 0 .and. node_id <= num_nodes) then
                    if (allocated(self%map_data(node_id)%ids)) then
                        current_size = size(self%map_data(node_id)%ids)
                        allocate (temp_list(current_size))
                        temp_list = self%map_data(node_id)%ids
                        deallocate (self%map_data(node_id)%ids)
                        allocate (self%map_data(node_id)%ids(current_size + 1))
                        self%map_data(node_id)%ids(1:current_size) = temp_list
                        self%map_data(node_id)%ids(current_size + 1) = ielem
                        deallocate (temp_list)
                    else
                        allocate (self%map_data(node_id)%ids(1))
                        self%map_data(node_id)%ids(1) = ielem
                    end if
                end if
            end do
        end do
    end subroutine initialize_simple

end module domain_adjacency_adjacency_node_element
