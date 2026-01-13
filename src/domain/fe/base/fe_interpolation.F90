submodule(domain_base_fe) domain_base_fe_interpolation
    implicit none
contains

    module subroutine lerp_1d_abst_fe(self, r, values, lerped_value)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: values(:)
        real(real64), intent(inout) :: lerped_value
        integer(int32) :: i
        real(real64) :: psi_i

        lerped_value = 0.0d0
        do i = 1, self%num_nodes
            call self%calc_psi(i, r, psi_i)
            lerped_value = lerped_value + psi_i * values(i)
        end do
    end subroutine lerp_1d_abst_fe

    module subroutine lerp_2d_abst_fe(self, r, values, lerped_values)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: values(:, :)
        real(real64), intent(inout) :: lerped_values(:)
        integer(int32) :: i
        real(real64) :: psi_i

        lerped_values(:) = 0.0d0
        do i = 1, self%num_nodes
            call self%calc_psi(i, r, psi_i)
            lerped_values(:) = lerped_values(:) + psi_i * values(:, i)
        end do
    end subroutine lerp_2d_abst_fe

    module subroutine lerp_3d_abst_fe(self, r, values, lerped_values)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: values(:, :, :)
        real(real64), intent(inout) :: lerped_values(:, :)
        integer(int32) :: i
        real(real64) :: psi_i

        lerped_values = 0.0d0
        do i = 1, self%num_nodes
            call self%calc_psi(i, r, psi_i)
            lerped_values(:, :) = lerped_values(:, :) + psi_i * values(:, :, i)
        end do
    end subroutine lerp_3d_abst_fe

    !---------------------------------------------------------------------------
    !> 物理座標系における値の勾配(nabla u)を計算する
    !> 内部で dpsi_dx (形状関数の物理勾配) を呼び出して線形結合をとる
    !>
    !> @param[in]    self          要素オブジェクト
    !> @param[in]    r             局所座標 (xi, eta, zeta)
    !> @param[in]    values        節点値配列 (u_i)
    !> @param[in]    node_coords   節点座標配列 (x, y, z)
    !> @param[in]    plane_axis    2次元の場合の軸指定 (1:XY面, 2:XZ面). 3次元は無視.
    !> @param[inout] dlerped_value 計算された勾配ベクトル (du/dx, du/dy, du/dz)
    !---------------------------------------------------------------------------
    module subroutine dlerp_abst_fe(self, r, values, node_coords, plane_axis, dlerped_value)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: values(:)
        real(real64), intent(in) :: node_coords(:, :)
        integer(int32), intent(in) :: plane_axis
        type(type_coordinate_dp), intent(inout) :: dlerped_value

        ! 形状関数の物理勾配を格納する一時配列 (num_nodes x 3)
        ! スタック領域(Automatic Array)を使用
        real(real64) :: shape_grads(self%num_nodes, 3)

        ! 1. 初期化
        call dlerped_value%reset()
        shape_grads(:, :) = 0.0d0

        ! 2. 形状関数の物理勾配 (dN_i/dx, dN_i/dy, dN_i/dz) を取得
        !    ここで逆ヤコビアンの計算等はすべて行われる
        call self%calc_dpsi_dx(r, node_coords, shape_grads)

        ! 3. 勾配の計算: grad u = sum( u_i * grad N_i )
        if (self%dimension == 3) then
            ! --- 3次元 (XYZ) ---
            dlerped_value%x = vector_dot(values, shape_grads(:, 1))
            dlerped_value%y = vector_dot(values, shape_grads(:, 2))
            dlerped_value%z = vector_dot(values, shape_grads(:, 3))

        else if (self%dimension == 2) then
            ! --- 2次元 (XY or XZ) ---
            ! 第1成分は常に x
            dlerped_value%x = vector_dot(values, shape_grads(:, 1))

            if (plane_axis == 2) then
                ! XZ面の場合: 第2成分を z にマッピング
                dlerped_value%y = 0.0d0
                dlerped_value%z = vector_dot(values, shape_grads(:, 2))
            else
                ! XY面の場合: 第2成分を y にマッピング (default)
                dlerped_value%y = vector_dot(values, shape_grads(:, 2))
                dlerped_value%z = 0.0d0
            end if

        else if (self%dimension == 1) then
            dlerped_value%x = vector_dot(values, shape_grads(:, 1))
        end if

    end subroutine dlerp_abst_fe

end submodule domain_base_fe_interpolation
