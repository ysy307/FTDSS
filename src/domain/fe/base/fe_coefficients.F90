submodule(domain_base_fe) domain_base_fe_coefficients
    implicit none
contains

    !>
    !> Computes shape functions, their global gradients, and the Jacobian determinant.
    !> Optimized to avoid redundant Jacobian calculations.
    !>
    module subroutine calc_shape_function_abst_fe(self, r, node_coords, psi, dpsi_dx, inverse_jacobian, determinant_jacobian)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)

        real(real64), intent(inout), optional :: psi(:)
        real(real64), intent(inout), optional :: dpsi_dx(:, :)
        real(real64), intent(inout), optional :: inverse_jacobian(:, :)
        real(real64), intent(inout), optional :: determinant_jacobian

        integer(int32) :: i, j, dim
        integer(int32) :: ierr
        ! スタック確保のため固定長にするが、最大次元(3)を確保して dim で制御する
        real(real64) :: local_J(3, 3)
        real(real64) :: local_inv_J(3, 3)
        real(real64) :: local_det_J
        real(real64) :: dpsi_dxi(3)
        logical :: need_jacobian, need_inverse

        dim = self%dimension

        ! 1. Evaluate Shape Functions (Requires no Jacobian)
        if (present(psi)) then
            psi(:) = 0.0d0 ! 初期化推奨
            do i = 1, self%num_nodes
                call self%calc_psi(i, r, psi(i))
            end do
        end if

        ! フラグ判定: ヤコビアン計算が必要か？
        need_jacobian = present(determinant_jacobian) .or. &
                        present(inverse_jacobian) .or. &
                        present(dpsi_dx)

        if (.not. need_jacobian) return

        ! --- A. Compute Jacobian Matrix (ONCE) ---
        ! local_J は (3,3) だが、計算は (dim, dim) の範囲で行われることを期待
        call self%calc_jacobian(r, node_coords, local_J)

        ! --- B. Compute Determinant ---
        if (present(determinant_jacobian)) then
            call matrix_determinant(local_J(1:dim, 1:dim), local_det_J, ierr)
            determinant_jacobian = local_det_J
        end if

        ! フラグ判定: 逆行列が必要か？ (dpsi_dx計算にも必要)
        need_inverse = present(inverse_jacobian) .or. present(dpsi_dx)

        if (.not. need_inverse) return

        ! --- C. Compute Inverse Jacobian ---
        ! local_J の内容を壊さないよう、matrix_inverse の仕様に合わせて注意
        ! ここでは local_J(1:dim, 1:dim) を入力として逆行列を計算
        local_inv_J(1:dim, 1:dim) = local_J(1:dim, 1:dim)
        call matrix_inverse(local_inv_J(1:dim, 1:dim), ierr)

        if (present(inverse_jacobian)) then
            inverse_jacobian(1:dim, 1:dim) = local_inv_J(1:dim, 1:dim)
        end if

        ! --- D. Compute Global Gradients (dpsi_dx) ---
        if (present(dpsi_dx)) then
            dpsi_dx(:, :) = 0.0d0

            do i = 1, self%num_nodes
                ! ローカル勾配 dpsi/dxi を取得
                do j = 1, dim
                    call self%calc_dpsi(i, j, r, dpsi_dxi(j))
                end do

                ! 座標変換: dpsi/dx = J^(-T) * dpsi/dxi
                ! 行列ベクトル積として実装 (Unrolled for performance)
                if (dim == 1) then
                    dpsi_dx(1, i) = dpsi_dxi(1) * local_inv_J(1, 1)
                else if (dim == 2) then
                    dpsi_dx(1, i) = dpsi_dxi(1) * local_inv_J(1, 1) + dpsi_dxi(2) * local_inv_J(2, 1)
                    dpsi_dx(2, i) = dpsi_dxi(1) * local_inv_J(1, 2) + dpsi_dxi(2) * local_inv_J(2, 2)
                else if (dim == 3) then
                    dpsi_dx(1, i) = vector_dot(dpsi_dxi(1:3), local_inv_J(1:3, 1))
                    dpsi_dx(2, i) = vector_dot(dpsi_dxi(1:3), local_inv_J(1:3, 2))
                    dpsi_dx(3, i) = vector_dot(dpsi_dxi(1:3), local_inv_J(1:3, 3))
                end if
            end do
        end if

    end subroutine calc_shape_function_abst_fe

    ! 以下のルーチンは単体呼び出し用として残すが、メインループからは上記を使うべき
    ! (実装内容は元のままでも機能的には問題ないが、同様にスライス処理を入れると安全)

    module subroutine calc_inverse_jacobian_abst_fe(self, r, node_coords, inverse_jacobian)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: inverse_jacobian(:, :)

        integer(int32) :: ierr, dim
        ! スタックで確保
        real(real64) :: local_J(3, 3)

        dim = self%dimension
        call self%calc_jacobian(r, node_coords, local_J)

        ! スライスして渡す
        inverse_jacobian(1:dim, 1:dim) = local_J(1:dim, 1:dim)
        call matrix_inverse(inverse_jacobian(1:dim, 1:dim), ierr)

    end subroutine calc_inverse_jacobian_abst_fe

    module subroutine calc_dpsi_dx_abst_fe(self, r, node_coords, dpsi_dx)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: dpsi_dx(:, :)

        integer(int32) :: i, j, dim
        real(real64) :: inverse_jacobian(3, 3)
        real(real64) :: dpsi_dxi(3)
        integer(int32) :: ierr

        dim = self%dimension

        ! ここでも calc_inverse_jacobian_abst_fe を呼ぶより、
        ! ここで閉じた計算をしたほうが変数の受け渡しが安全
        call self%calc_jacobian(r, node_coords, inverse_jacobian)
        ! スライスして逆行列化 (In-place置換を想定)
        call matrix_inverse(inverse_jacobian(1:dim, 1:dim), ierr)

        do i = 1, self%num_nodes
            do j = 1, dim
                call self%calc_dpsi(i, j, r, dpsi_dxi(j))
            end do

            ! Transform
            if (dim == 1) then
                dpsi_dx(1, i) = dpsi_dxi(1) * inverse_jacobian(1, 1)
            else if (dim == 2) then
                dpsi_dx(1, i) = dpsi_dxi(1) * inverse_jacobian(1, 1) + dpsi_dxi(2) * inverse_jacobian(2, 1)
                dpsi_dx(2, i) = dpsi_dxi(1) * inverse_jacobian(1, 2) + dpsi_dxi(2) * inverse_jacobian(2, 2)
            else if (dim == 3) then
                dpsi_dx(1, i) = vector_dot(dpsi_dxi(1:3), inverse_jacobian(1:3, 1))
                dpsi_dx(2, i) = vector_dot(dpsi_dxi(1:3), inverse_jacobian(1:3, 2))
                dpsi_dx(3, i) = vector_dot(dpsi_dxi(1:3), inverse_jacobian(1:3, 3))
            end if
        end do

    end subroutine calc_dpsi_dx_abst_fe

    module subroutine calc_jacobian_determinant_abst_fe(self, r, node_coords, determinant_jacobian)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: determinant_jacobian

        real(real64) :: jacobian(3, 3)
        integer(int32) :: ierr, dim

        dim = self%dimension
        call self%calc_jacobian(r, node_coords, jacobian)
        ! dimでスライス
        call matrix_determinant(jacobian(1:dim, 1:dim), determinant_jacobian, ierr)

    end subroutine calc_jacobian_determinant_abst_fe

end submodule domain_base_fe_coefficients
