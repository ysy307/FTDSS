submodule(core_types_matrix) core_types_matrix_coo
    use :: core_allocate, only:allocate_array
    use :: core_deallocate, only:deallocate_array
    implicit none

contains

    module subroutine initialize_type_coo(self, num_nodes, num_dofs, row, col)
        implicit none
        class(type_coo), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes, num_dofs
        integer(int32), intent(in), optional :: row(:) ! 節点レベル行インデックス(ソート済を仮定)
        integer(int32), intent(in), optional :: col(:) ! 節点レベル列インデックス

        integer(int32) :: idx, nloc, i
        integer(int32) :: idof, jdof
        integer(int32) :: r_node, c_node

        if (.not. present(row) .or. .not. present(col)) then
            print *, "Error: row and col must be provided for COO initialization."
            stop
        end if

        nloc = size(row)

        ! --- 行列サイズと nnz の計算 ---
        self%num_nodes = num_nodes
        self%num_dofs = num_dofs
        self%num_row = num_nodes * num_dofs
        self%num_col = num_nodes * num_dofs
        self%nnz = nloc * num_dofs * num_dofs

        ! --- 配列のメモリ確保 ---
        if (self%nnz > 0) then
            call allocate_array(self%row, self%nnz)
            call allocate_array(self%col, self%nnz)
            call allocate_array(self%val, self%nnz)
        else
            return
        end if

        ! --- [修正点] 自由度ブロック展開 (行・列でソート済になるようにループを構成) ---
        ! 入力のrow(:)が行番号でソート済み、かつ同じ行番号内ではcol(:)が
        ! 列番号でソート済みであると仮定することで、効率的にソート済みCOOを生成する.
        idx = 0
        do i = 1, nloc
            r_node = row(i)
            c_node = col(i)
            do idof = 1, num_dofs
                do jdof = 1, num_dofs
                    idx = idx + 1
                    self%row(idx) = (idof - 1) * num_nodes + r_node
                    self%col(idx) = (jdof - 1) * num_nodes + c_node
                end do
            end do
        end do

        ! 値をゼロで初期化
        self%val = 0.0d0

    end subroutine initialize_type_coo

    ! find_coo は非常に低速であることに注意。
    ! 頻繁な要素アクセスにはCRS形式が推奨される。
    module pure function find_coo(self, row_dof, col_dof, row, col) result(index)
        implicit none
        class(type_coo), intent(in) :: self
        integer(int32), intent(in) :: row_dof, col_dof, row, col
        integer(int32) :: index, i
        integer(int32) :: actual_row, actual_col

        actual_row = (row_dof - 1) * self%num_nodes + row
        actual_col = (col_dof - 1) * self%num_nodes + col

        index = 0
        ! O(nnz)の線形探索。大規模問題では深刻なボトルネックになる。
        do i = 1, self%nnz
            if (self%row(i) == actual_row .and. self%col(i) == actual_col) then
                index = i
                return
            end if
        end do
    end function find_coo

    module subroutine gemv_coo(self, alpha, x, beta, y)
        ! y := alpha*A*x + beta*y
        implicit none
        class(type_coo), intent(in) :: self
        real(real64), intent(in) :: alpha
        real(real64), intent(in) :: x(:)
        real(real64), intent(in) :: beta
        real(real64), intent(inout) :: y(:)
        integer(int32) :: i

        ! --- [修正点] 並列化のバグを修正 ---
        ! 1. 先に y 全体を beta でスケールする (betaの誤った複数回適用を防ぐ)
        if (beta == 0.0d0) then
            y = 0.0d0
        else
            y = beta * y
        end if

        ! 2. 各非ゼロ要素の寄与を加算する
        !    yの同じ要素への複数スレッドからの書き込みは atomic で保護する
        !$omp parallel do
        do i = 1, self%nnz
            !$omp atomic update
            y(self%row(i)) = y(self%row(i)) + alpha * self%val(i) * x(self%col(i))
        end do
        !$omp end parallel do

    end subroutine gemv_coo

    ! --- 以下、変更なし ---

    module pure function get_nnz_coo(self) result(nnz)
        implicit none
        class(type_coo), intent(in) :: self
        integer(int32) :: nnz
        nnz = self%nnz
    end function get_nnz_coo

    module pure function get_num_row_coo(self) result(num_row)
        implicit none
        class(type_coo), intent(in) :: self
        integer(int32) :: num_row
        num_row = self%num_row
    end function get_num_row_coo

    module pure function get_num_col_coo(self) result(num_col)
        implicit none
        class(type_coo), intent(in) :: self
        integer(int32) :: num_col
        num_col = self%num_col
    end function get_num_col_coo

    module function get_row_coo(self) result(row)
        implicit none
        class(type_coo), intent(in), target :: self
        integer(int32), dimension(:), pointer :: row
        row => self%row
    end function get_row_coo

    module function get_col_coo(self) result(col)
        implicit none
        class(type_coo), intent(in), target :: self
        integer(int32), dimension(:), pointer :: col
        col => self%col
    end function get_col_coo

    module function get_val_coo(self) result(val)
        implicit none
        class(type_coo), intent(in), target :: self
        real(real64), dimension(:), pointer :: val
        val => self%val
    end function get_val_coo

    module subroutine set_coo(self, row_dof, col_dof, row, col, value)
        implicit none
        class(type_coo), intent(inout) :: self
        integer(int32), intent(in) :: row_dof, col_dof, row, col
        real(real64), intent(in) :: value
        integer(int32) :: index
        index = self%find(row_dof, col_dof, row, col)
#ifdef USE_DEBUG
        if (index > 0) then
#endif
            self%val(index) = value
#ifdef USE_DEBUG
        else
            print *, "Warning(set_coo): Element not in sparsity pattern.", row, col
        end if
#endif
    end subroutine set_coo

    module subroutine set_row_coo(self, row_dof, row, value)
        implicit none
        class(type_coo), intent(inout) :: self
        integer(int32), intent(in) :: row_dof, row
        real(real64), intent(in) :: value
        integer(int32) :: i, actual_row
        actual_row = (row_dof - 1) * self%num_nodes + row
        do i = 1, self%nnz
            if (self%row(i) == actual_row) then
                self%val(i) = value
            end if
        end do
    end subroutine set_row_coo

    module subroutine set_all_coo(self, value)
        implicit none
        class(type_coo), intent(inout) :: self
        real(real64), intent(in) :: value
        self%val = value
    end subroutine set_all_coo

    module subroutine zero_coo(self)
        implicit none
        class(type_coo), intent(inout) :: self
        self%val = 0.0d0
    end subroutine zero_coo

    module subroutine add_coo(self, row_dof, col_dof, row, col, value)
        implicit none
        class(type_coo), intent(inout) :: self
        integer(int32), intent(in) :: row_dof, col_dof, row, col
        real(real64), intent(in) :: value
        integer(int32) :: index
        index = self%find(row_dof, col_dof, row, col)
#ifdef USE_DEBUG
        if (index > 0) then
#endif
            self%val(index) = self%val(index) + value
#ifdef USE_DEBUG
        else
            print *, "Warning(add_coo): Element not in sparsity pattern.", row, col
        end if
#endif
    end subroutine add_coo

    module subroutine add_matrix_coo(self, alpha, B, C)
        implicit none
        class(type_coo), intent(in) :: self
        real(real64), intent(in) :: alpha
        class(abst_matrix), intent(in) :: B
        class(abst_matrix), intent(inout) :: C
        select type (B_coo => B)
        type is (type_coo)
            select type (C_coo => C)
            type is (type_coo)
                if (self%nnz /= B_coo%nnz .or. self%nnz /= C_coo%nnz) then
                    print *, "ERROR(add_matrix_coo): In this simplified version, NNZ must be identical."
                    stop
                end if
                C_coo%val = alpha * self%val + B_coo%val
            end select
        end select
    end subroutine add_matrix_coo

    module subroutine display_coo(self)
        implicit none
        class(type_coo), intent(in) :: self
        integer(int32) :: i
        print *, "COO Matrix (max_dims=", self%num_row, "x", self%num_col, ", nnz=", self%nnz, ")"
        do i = 1, self%nnz
            write (*, '(2(i8, ", "), es16.8)') self%row(i), self%col(i), self%val(i)
        end do
    end subroutine display_coo

    module subroutine destroy_coo(self)
        implicit none
        class(type_coo), intent(inout) :: self
        call deallocate_array(self%row)
        call deallocate_array(self%col)
        call deallocate_array(self%val)
        self%nnz = 0
    end subroutine destroy_coo

end submodule core_types_matrix_coo
