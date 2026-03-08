!>
!> Implements the procedures for a Degree-of-Freedom (DOF) based Compressed
!> Row Storage (bsr) sparse matrix.
!>
submodule(types_algebra_matrix) algebra_matrix_bsr
    implicit none

contains

    !>
    !> Initializes the DOF-level bsr matrix structure.
    !>
    module subroutine initialize_type_matrix_bsr(self, num_nodes, row, col, row_blocks, col_blocks)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in), optional :: row(:)
        integer(int32), intent(in), optional :: col(:)
        integer(int32), intent(in), optional :: row_blocks
        integer(int32), intent(in), optional :: col_blocks

        ! Argument validation
        if (.not. present(row) .or. .not. present(col)) then
            print *, "Error: row (node_ptr) and col (node_ind) must be provided."
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        if (.not. present(row_blocks) .or. .not. present(col_blocks)) then
            print *, "Error: row_blocks and col_blocks must be provided for bsr matrix."
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        if (size(row) /= num_nodes + 1) then
            print *, "Error: The size of row (node_ptr) array must be num_nodes + 1."
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        self%num_nodes = num_nodes
        self%num_rows = num_nodes
        self%num_ptrs = num_nodes + 1
        self%num_blocks = size(col)
        self%num_block_rows = row_blocks
        self%num_block_cols = col_blocks
        self%nnz = self%num_block_rows * self%num_block_cols * self%num_blocks

        ! Allocate arrays
        call allocate_array(self%ptr, source=row)
        call allocate_array(self%ind, source=col)
        ! Allocate val as (rows, cols, blocks) to improve memory access patterns in block operations
        call allocate_array(self%val, self%num_block_rows, self%num_block_cols, self%num_blocks)

        call self%zero()

        self%is_initialized_matrix = .true.
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine initialize_type_matrix_bsr

    !>
    !> Deallocates all internal arrays.
    !>
    module subroutine destroy_bsr(self)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self

        call deallocate_array(self%ptr)
        call deallocate_array(self%ind)
        call deallocate_array(self%val)

        self%num_nodes = 0
        self%num_rows = 0
        self%num_ptrs = 0
        self%nnz = 0

        self%is_initialized_matrix = .false.
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine destroy_bsr

    !>
    !> Returns matrix info.
    !>
    module subroutine get_info_bsr(self, info)
        implicit none
        class(type_matrix_bsr), intent(in) :: self
        type(type_matrix_info), intent(inout) :: info

        info%num_nodes = self%num_nodes
        info%num_rows = self%num_rows
        info%num_ptrs = self%num_ptrs
        info%num_blocks = self%num_blocks
        info%num_block_rows = self%num_block_rows
        info%num_block_cols = self%num_block_cols
        info%nnz = self%nnz
    end subroutine get_info_bsr

    module subroutine get_diagonal_bsr(self, diagonal)
        implicit none
        class(type_matrix_bsr), intent(in) :: self
        type(type_vector_dp), intent(inout) :: diagonal

        integer(int32) :: i, row_start, row_end, j, k, m

        !$omp parallel do private(i, row_start, row_end, j, k, m)
        do i = 1, self%num_ptrs - 1
            row_start = self%ptr(i)
            row_end = self%ptr(i + 1) - 1
            do j = row_start, row_end
                if (self%ind(j) == i) then
                    do k = 1, self%num_block_rows
                        do m = 1, self%num_block_cols
                            if (k == m) then
                                ! Access val(row_in_block, col_in_block, block_index)
                                call diagonal%set(VECTOR_OPS%INS, (i - 1) * self%num_block_rows + k, self%val(k, m, j))
                            end if
                        end do
                    end do
                    exit
                end if
            end do
        end do
        !$omp end parallel do

    end subroutine get_diagonal_bsr

    module subroutine get_diagonal_block_bsr(self, target_block, diagonal_block)
        implicit none
        class(type_matrix_bsr), intent(in) :: self
        integer(int32), intent(in) :: target_block
        real(real64), intent(inout) :: diagonal_block(:, :)

        integer(int32) :: row_start, row_end, j, k, m
        ! Initialize diagonal_block to zero
        diagonal_block = 0.0d0
        row_start = self%ptr(target_block)
        row_end = self%ptr(target_block + 1) - 1
        do j = row_start, row_end
            if (self%ind(j) == target_block) then
                do k = 1, self%num_block_rows
                    do m = 1, self%num_block_cols
                        diagonal_block(k, m) = self%val(k, m, j)
                    end do
                end do
                exit
            end if
        end do
    end subroutine get_diagonal_block_bsr

    !> Getters for internal arrays
    module function get_ptr_bsr(self) result(ptr)
        implicit none
        class(type_matrix_bsr), intent(in), target :: self
        integer(int32), dimension(:), pointer :: ptr
        ptr => self%ptr
    end function get_ptr_bsr

    module function get_ind_bsr(self) result(ind)
        implicit none
        class(type_matrix_bsr), intent(in), target :: self
        integer(int32), dimension(:), pointer :: ind
        ind => self%ind
    end function get_ind_bsr

    module function get_val_bsr(self) result(val)
        implicit none
        class(type_matrix_bsr), intent(in), target :: self
        real(real64), dimension(:, :, :), pointer :: val
        val => self%val
    end function get_val_bsr

    !>
    !> Sets the value of a specific entry (Not fully implemented for BSR yet, use block version).
    !>
    module subroutine set_value_bsr(self, op, row, col, value)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: row
        integer(int32), intent(in) :: col
        real(real64), intent(in) :: value

        self%status = MATRIX_STATUS%NOT_IMPLEMENTED
    end subroutine set_value_bsr

    !>
    !> Sets the value of a specific entry in the sparse matrix (Block level).
    !>
    module subroutine set_value_block_bsr(self, op, row, col, row_block, col_block, value)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: row ! Block row index (Node index)
        integer(int32), intent(in) :: col ! Block col index (Node index)
        integer(int32), intent(in) :: row_block ! Local row index within block
        integer(int32), intent(in) :: col_block ! Local col index within block
        real(real64), intent(in) :: value

        integer(int32) :: index

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

#ifdef USE_DEBUG
        if (.not. value_in_range(row_block, 1, self%num_block_rows) .or. &
            .not. value_in_range(col_block, 1, self%num_block_cols)) then
            self%status = MATRIX_STATUS%OUT_OF_MEMORY
            return
        end if
#endif

        index = self%find(row, col)

#ifdef USE_DEBUG
        if (index < 0) then
            self%status = MATRIX_STATUS%OUT_OF_MEMORY
            return
        end if
#endif

        if (op == MATRIX_OPS%INS) then
            ! Access val(rKow_in_block, col_in_block, block_index)
            self%val(row_block, col_block, index) = value
        else if (op == MATRIX_OPS%ADD) then
            self%val(row_block, col_block, index) = self%val(row_block, col_block, index) + value
        else
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end if

    end subroutine set_value_block_bsr

    !>
    !> Sets all non-zero entries in a specific row to a single scalar value.
    !>
    module subroutine set_row_bsr(self, op, row, value, row_block)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: row
        real(real64), intent(in) :: value
        integer(int32), intent(in), optional :: row_block

        integer(int32) :: is, ie, k, col_node
        integer(int32) :: r_start, r_end, r

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        ! 範囲チェック
        if (.not. value_in_range(row, 1, self%num_rows)) then
            self%status = MATRIX_STATUS%OUT_OF_MEMORY
            return
        end if

        is = self%ptr(row)
        ie = self%ptr(row + 1) - 1

        if (present(row_block)) then
            if (row_block < 1 .or. row_block > self%num_block_rows) then
                self%status = MATRIX_STATUS%OUT_OF_MEMORY
                return
            end if
            r_start = row_block
            r_end = row_block
        else
            r_start = 1
            r_end = self%num_block_rows
        end if

        if (op == MATRIX_OPS%INS) then
            do k = is, ie
                col_node = self%ind(k)
                ! Access val(row_in_block, col_in_block, block_index)
                self%val(r_start:r_end, :, k) = value
            end do
        else if (op == MATRIX_OPS%ADD) then
            do k = is, ie
                if (self%ind(k) == row) then
                    do r = r_start, r_end
                        self%val(r, :, k) = self%val(r, :, k) + value
                    end do
                    exit
                end if
            end do
        else
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end if

        self%status = MATRIX_STATUS%SUCCESS
    end subroutine set_row_bsr

    !>
    !> Sets all stored non-zero values.
    !>
    module subroutine set_all_bsr(self, op, value)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        real(real64), intent(in) :: value

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        if (op == MATRIX_OPS%INS) then
            self%val(:, :, :) = value
        else if (op == MATRIX_OPS%ADD) then
            self%val(:, :, :) = self%val(:, :, :) + value
        else
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end if

        self%status = MATRIX_STATUS%SUCCESS
    end subroutine set_all_bsr

    module subroutine scale_bsr(self, op, alpha)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        type(type_vector_dp), intent(in) :: alpha

        real(real64), dimension(:), pointer :: alpha_data

        integer(int32) :: i, j, rb, cb, col
        integer(int32) :: row_start, row_end
        integer(int32) :: row_dof, col_dof
        integer(int32) :: nrequired
        integer(int32) :: bnr, bnc ! ブロックサイズを保持する一時変数

        if (.not. self%is_initialized()) then
            self%status = MATRIX_STATUS%NOT_INITIALIZED
            return
        end if

        ! ポインタの関連付け
        alpha_data => alpha%get_data()

        !--------------------------------------------------------
        ! 変数名の可読性と安全性のためのエイリアス
        ! self%num_block_rows が「ブロックサイズ(bnr)」であることを前提としています
        !--------------------------------------------------------
        bnr = self%num_block_rows
        bnc = self%num_block_cols

        ! 正方ブロックチェック (Symmetric Scalingの場合に必須)
        if (op == MATRIX_OPS%SCALE_SYMM_DIAG .and. bnr /= bnc) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        ! ベクトルサイズチェック
        ! num_nodes が「全ブロック数」あるいは「全ノード数」を指すと仮定
        nrequired = bnr * self%num_nodes

        if (size(alpha_data) /= nrequired) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        !-------------------------------------------
        ! Symmetric Scaling: A <- D^{-1/2} A D^{-1/2}
        !-------------------------------------------
        if (op == MATRIX_OPS%SCALE_SYMM_DIAG) then
            ! private変数に bnr, bnc は不要(read only sharedで良い)
            !$omp parallel do default(shared) private(i,j,rb,cb,col,row_start,row_end,row_dof,col_dof)
            do i = 1, self%num_ptrs - 1
                row_start = self%ptr(i)
                row_end = self%ptr(i + 1) - 1

                do j = row_start, row_end
                    col = self%ind(j)

                    ! ブロック内の計算
                    ! ループ順序: rb(行)を一番内側にするとメモリアクセスが連続になる
                    ! ただし，alpha_dataのアクセス回数を減らす最適化も考えられる
                    do cb = 1, bnc
                        ! 列側のスケーリング係数はこのcbループ内で不変
                        ! col_dof は (ブロックColID - 1)*BlockSize + LocalColID
                        col_dof = (col - 1) * bnr + cb

                        do rb = 1, bnr
                            row_dof = (i - 1) * bnr + rb

                            ! val(rb, cb, j) -> Fortranは第1次元(rb)が連続
                            self%val(rb, cb, j) = self%val(rb, cb, j) * &
                                                  (alpha_data(row_dof) * alpha_data(col_dof))
                        end do
                    end do
                end do
            end do
            !$omp end parallel do
            self%status = MATRIX_STATUS%SUCCESS

            !-------------------------------------------
            ! Jacobi Scaling: A <- D^{-1} A
            !-------------------------------------------
        else if (op == MATRIX_OPS%SCALE_JACOBI) then

            !$omp parallel do default(shared) private(i,j,rb,cb,row_start,row_end,row_dof)
            do i = 1, self%num_ptrs - 1
                row_start = self%ptr(i)
                row_end = self%ptr(i + 1) - 1

                do j = row_start, row_end
                    ! Jacobiの場合，列(cb)方向には同じ行スケーリング係数がかかるため
                    ! cbを外側，rbを内側にして連続アクセスを最大化
                    do cb = 1, bnc
                        do rb = 1, bnr
                            row_dof = (i - 1) * bnr + rb

                            self%val(rb, cb, j) = self%val(rb, cb, j) * alpha_data(row_dof)
                        end do
                    end do
                end do
            end do
            !$omp end parallel do
            self%status = MATRIX_STATUS%SUCCESS

        else
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end if

    end subroutine scale_bsr
    !>
    !> Zero out the matrix.
    !>
    module subroutine zero_all_bsr(self)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self

        self%val(:, :, :) = 0.0d0
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine zero_all_bsr

    !> Sets all values in a specified row of the bsr matrix to zero.
    module subroutine zero_row_bsr(self, row, row_block)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self
        integer(int32), intent(in) :: row
        integer(int32), intent(in), optional :: row_block

        integer(int32) :: is, ie, k, r_start, r_end, r
        ! 範囲チェック
        if (.not. value_in_range(row, 1, self%num_rows)) then
            self%status = MATRIX_STATUS%OUT_OF_MEMORY
            return
        end if

        is = self%ptr(row)
        ie = self%ptr(row + 1) - 1

        if (present(row_block)) then
            if (row_block < 1 .or. row_block > self%num_block_rows) then
                self%status = MATRIX_STATUS%OUT_OF_MEMORY
                return
            end if
            r_start = row_block
            r_end = row_block
        else
            r_start = 1
            r_end = self%num_block_rows
        end if

        do k = is, ie
            ! Access val(row_in_block, col_in_block, block_index)
            do r = r_start, r_end
                self%val(r, :, k) = 0.0d0
            end do
        end do

        self%status = MATRIX_STATUS%SUCCESS
    end subroutine zero_row_bsr

    !>
    !> Find index.
    !>
    pure module function find_bsr(self, row, col) result(index)
        implicit none
        class(type_matrix_bsr), intent(in) :: self
        integer(int32), intent(in) :: row, col
        integer(int32) :: index
        integer(int32) :: ptr_start, ptr_end

        index = 0

        if (.not. value_in_range(row, 1, self%num_nodes) .or. &
            .not. value_in_range(col, 1, self%num_nodes)) then
            index = -1
            return
        end if

        ptr_start = self%ptr(row)
        ptr_end = self%ptr(row + 1) - 1

        index = binary_find(col, self%ind, ptr_start, ptr_end)
    end function find_bsr

    !>
    !> Display.
    !>
    module subroutine display_bsr(self, unit_in)
        implicit none
        class(type_matrix_bsr), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in
        integer(int32) :: i, r, row_start, row_end, rb, cb
        integer(int32) :: unit

        unit = optval(unit_in, output_unit)

        if (.not. self%is_initialized()) then
            write (unit, '(a)') "Matrix is not initialized."
            return
        end if

        write (unit, '(a,i0,2x,a,i0,a)') "bsr Matrix (Nodes= ", self%num_nodes, ", nnz= ", self%nnz, ")"
        do r = 1, self%num_nodes
            row_start = self%ptr(r)
            row_end = self%ptr(r + 1) - 1
            do i = row_start, row_end
                do rb = 1, self%num_block_rows
                    do cb = 1, self%num_block_cols
                        ! Access val(row_in_block, col_in_block, block_index)
                        write (unit, '(a,i0,a,i0,a, i0,a,i0,a, es16.8e3)') &
                            "Node(", r, ",", self%ind(i), ") Block(", rb, ",", cb, "): ", &
                            self%val(rb, cb, i)
                    end do
                end do
            end do
        end do
    end subroutine display_bsr

end submodule algebra_matrix_bsr
