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
    !> @param[in]    values        節点値配列 (u_i) [num_nodes]
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

        ! 形状関数の物理勾配を格納する一時配列 (3 x num_nodes)
        ! calc_dpsi_dx は dim=2 の場合 (1:2, :) に値を埋めることに注意
        real(real64) :: shape_grads(self%dimension, self%num_nodes)
        real(real64) :: dlerp_array(self%dimension)
        integer(int32) :: ierr

        ! 1. 初期化
        call dlerped_value%reset()
        shape_grads(:, :) = 0.0d0
        dlerp_array(:) = 0.0d0

        ! 2. 形状関数の物理勾配 (dN_i/dx, dN_i/dy, dN_i/dz) を取得
        call self%calc_shape_function(r, node_coords, dpsi_dx=shape_grads)

        call matvec(shape_grads, values, dlerp_array, ierr)

        ! 3. 勾配の計算: grad u = sum( u_i * grad N_i )
        ! shape_grads の第1次元は常に 1〜dim に詰められていることを考慮してマッピングする
        if (self%dimension == 3) then
            ! --- 3次元 (XYZ) ---
            ! shape_grads(1:3, :) に順に x, y, z 成分が入っている
            dlerped_value%x = dlerp_array(1)
            dlerped_value%y = dlerp_array(2)
            dlerped_value%z = dlerp_array(3)
        else if (self%dimension == 2) then
            ! --- 2次元 (XY or XZ) ---
            ! shape_grads(1, :) -> 第1成分 (x)
            ! shape_grads(2, :) -> 第2成分 (y or z)

            ! 第1成分は常に x
            dlerped_value%x = dlerp_array(1)

            if (plane_axis == 2) then
                dlerped_value%y = 0.0d0
                dlerped_value%z = dlerp_array(2)
            else
                dlerped_value%y = dlerp_array(2)
                dlerped_value%z = 0.0d0
            end if
        else if (self%dimension == 1) then
            dlerped_value%x = dlerp_array(1)
            dlerped_value%y = 0.0d0
            dlerped_value%z = 0.0d0
        end if

    end subroutine dlerp_abst_fe
end submodule domain_base_fe_interpolation
