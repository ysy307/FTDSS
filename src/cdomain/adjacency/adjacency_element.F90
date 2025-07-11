module domain_adjacency_adjacency_element
    use, intrinsic :: iso_fortran_env, only: int32
    use :: module_core, only:allocate_array, deallocate_array
    use :: domain_element, only:abst_element, holder_elements
    implicit none
    private

    public :: type_crs_adjacency_element

    !============================================================
    ! CRS形式の隣接行列を格納する型
    !============================================================
    type :: type_crs_adjacency_element
        private
        integer(int32) :: nnz = 0
        integer(int32) :: num_row = 0
        integer(int32), allocatable :: ptr(:)
        integer(int32), allocatable :: ind(:)
    contains
        procedure, pass(self), public :: initialize => initialize_crs_adjacency
        procedure, pass(self), public :: get_num_elements => get_num_elements_impl
        procedure, pass(self), public :: get_degree => get_degree_impl
        ! ★★★ 新しいゲッターを追加 ★★★
        procedure, pass(self), public :: get_neighbors => get_neighbors_impl
    end type type_crs_adjacency_element

contains

    !============================================================
    ! メインの初期化サブルーチン
    !============================================================
    subroutine initialize_crs_adjacency(self, elements)
        class(type_crs_adjacency_element), intent(inout) :: self
        type(holder_elements), intent(in) :: elements(:)

        integer(int32) :: num_elements
        integer(int32) :: i, j
        integer(int32) :: pair_count, capacity
        integer(int32), allocatable :: coo_row(:), coo_col(:), temp_row(:), temp_col(:)

        num_elements = size(elements)
        self%num_row = num_elements
        if (num_elements <= 1) return

        ! --- Step 1 & 2: 隣接ペアを全探索で探し、一時的なCOO形式で格納 ---
        pair_count = 0
        capacity = num_elements * 5
        call allocate_array(coo_row, length=capacity)
        call allocate_array(coo_col, length=capacity)

        do i = 1, num_elements
            do j = i + 1, num_elements
                if (are_elements_adjacent(elements(i)%e, elements(j)%e)) then
                    if ((pair_count + 2) > capacity) then
                        capacity = capacity * 2
                        call allocate_array(temp_row, length=capacity)
                        call allocate_array(temp_col, length=capacity)
                        temp_row(1:pair_count) = coo_row(1:pair_count)
                        temp_col(1:pair_count) = coo_col(1:pair_count)

                        call deallocate_array(coo_row)
                        call deallocate_array(coo_col)

                        call move_alloc(temp_row, coo_row)
                        call move_alloc(temp_col, coo_col)
                    end if

                    pair_count = pair_count + 1
                    coo_row(pair_count) = i
                    coo_col(pair_count) = j

                    pair_count = pair_count + 1
                    coo_row(pair_count) = j
                    coo_col(pair_count) = i
                end if
            end do
        end do

        self%nnz = pair_count

        ! --- Step 3: COO形式からCRS形式へ変換 ---
        call allocate_array(self%ptr, length=self%num_row + 1_int32)
        call allocate_array(self%ind, length=self%nnz)
        self%ptr = 0

        ! Pass 1: 各行の非ゼロ要素数(次数)をカウント
        do i = 1, self%nnz
            self%ptr(coo_row(i) + 1) = self%ptr(coo_row(i) + 1) + 1
        end do

        ! Pass 2: 累積和を計算して、ptr配列を完成させる
        self%ptr(1) = 1
        do i = 2, self%num_row + 1
            self%ptr(i) = self%ptr(i) + self%ptr(i - 1)
        end do

        ! Pass 3: ind配列を構築する
        call allocate_array(temp_row, length=self%num_row)
        temp_row(:) = self%ptr(1:self%num_row)

        do i = 1, self%nnz
            j = temp_row(coo_row(i))
            self%ind(j) = coo_col(i)
            temp_row(coo_row(i)) = j + 1
        end do

        call deallocate_array(temp_row)

        call deallocate_array(coo_row)
        call deallocate_array(coo_col)

    end subroutine initialize_crs_adjacency

    !============================================================
    ! 2つの要素が隣接しているか判定する private logical 関数
    ! (この関数は変更なし)
    !============================================================
    function are_elements_adjacent(elem1, elem2) result(is_adjacent)
        class(abst_element), intent(in) :: elem1, elem2
        logical :: is_adjacent
        integer(int32) :: i, j
        integer(int32) :: num_nodes1, num_nodes2
        integer(int32), allocatable :: conn1(:), conn2(:)

        is_adjacent = .false.
        num_nodes1 = elem1%get_num_nodes()
        num_nodes2 = elem2%get_num_nodes()

        conn1 = elem1%connectivity
        conn2 = elem2%connectivity

        do i = 1, num_nodes1
            do j = 1, num_nodes2
                if (conn1(i) == conn2(j)) then
                    is_adjacent = .true.
                    return
                end if
            end do
        end do
    end function are_elements_adjacent

    !============================================================
    ! Getter: 要素数を返す
    !============================================================
    function get_num_elements_impl(self) result(num_row)
        implicit none
        class(type_crs_adjacency_element), intent(in) :: self
        integer(int32) :: num_row

        num_row = self%num_row
    end function get_num_elements_impl

    !============================================================
    ! Getter: 指定した要素の次数を返す
    !============================================================
    function get_degree_impl(self, i) result(degree)
        implicit none
        class(type_crs_adjacency_element), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32) :: degree

        if (i < 1 .or. i > self%num_row) then
            degree = 0
        else
            degree = self%ptr(i + 1) - self%ptr(i)
        end if
    end function get_degree_impl

    !============================================================
    ! ★★★ Getter: 指定した要素の隣接要素リストを返す (新規追加) ★★★
    !============================================================
    function get_neighbors_impl(self, i) result(neighbors)
        implicit none
        class(type_crs_adjacency_element), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), allocatable :: neighbors(:)

        integer(int32) :: start_p, end_p, num_neighbors

        if (i < 1 .or. i > self%num_row) then
            ! 範囲外の場合はサイズ0の配列を返す
            call allocate_array(neighbors, length=0_int32)
            return
        end if

        start_p = self%ptr(i)
        end_p = self%ptr(i + 1) - 1
        num_neighbors = end_p - start_p + 1

        if (num_neighbors > 0) then
            call allocate_array(neighbors, length=num_neighbors)
            neighbors = self%ind(start_p:end_p)
        else
            call allocate_array(neighbors, length=0_int32)
        end if
    end function get_neighbors_impl

end module domain_adjacency_adjacency_element
