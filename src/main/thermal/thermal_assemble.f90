module thermal_thermal_assemble
    use, intrinsic :: iso_fortran_env, only: int32, real64
!$  use omp_lib
    use :: module_core, only:type_state, type_dp_vector_3d, assignment(=), type_variable, allocate_array, deallocate_array
    use :: module_domain, only:type_domain, abst_element
    use :: module_properties, only:type_properties_manager
    use :: module_matrix, only:type_crs, type_dense, gemv, add
    use :: module_control

    implicit none
    private

    public :: abst_assemble_global_thermal
    public :: thermal_assemble_system_linear_1, thermal_assemble_system_linear_1_parallel

    abstract interface
        subroutine abst_assemble_global_thermal(J, R, domain, temperature, porosity, properties, controls, actual_order)
            import :: type_crs, type_domain, type_properties_manager, type_variable, type_controls, int32, real64
            implicit none
            type(type_crs), intent(inout) :: J
            real(real64), intent(inout) :: R(:)
            type(type_domain), intent(inout), target :: domain
            type(type_variable), intent(in) :: temperature
            type(type_variable), intent(in) :: porosity
            type(type_properties_manager), intent(in) :: properties
            type(type_controls), intent(in) :: controls
            integer(int32), intent(in) :: actual_order
        end subroutine abst_assemble_global_thermal
    end interface
contains

    subroutine process_element_thermal_linear_1(J, R, element, temperature, porosity, properties, controls, actual_order)
        implicit none
        ! --- 引数 ---
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        class(abst_element), intent(in), pointer :: element
        type(type_variable), intent(in) :: temperature
        type(type_variable), intent(in) :: porosity
        type(type_properties_manager), intent(in) :: properties
        type(type_controls), intent(in) :: controls
        integer(int32), intent(in) :: actual_order

        ! --- ローカル変数 ---
        integer(int32) :: index, num_nodes, num_gauss, i_material, il, jl, iG, iO
        real(real64) :: weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j
        real(real64) :: val
        real(real64) :: dt_n
        real(real64) :: coefficients(0:actual_order)

        ! --- スタック上のワークスペース (自動配列) ---
        real(real64) :: CT_e(element%get_num_nodes(), element%get_num_nodes())
        real(real64) :: KT_e(element%get_num_nodes(), element%get_num_nodes())
        real(real64) :: J_e(element%get_num_nodes(), element%get_num_nodes())
        real(real64) :: R_e(element%get_num_nodes())
        real(real64) :: T_hist_e(element%get_num_nodes())

        ! --- ガウスポイントでの物理量 (自動配列) ---
        type(type_state) :: state(element%get_num_gauss())
        real(real64) :: Ca(element%get_num_gauss()), lambda(element%get_num_gauss())

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 0: 初期化とサイズの取得
        !---------------------------------------------------------------------------------------------------------------------------
        num_nodes = element%get_num_nodes()
        num_gauss = element%get_num_gauss()
        i_material = element%get_group()
        if (.not. controls%is_target(calc_thermal, i_material)) return

        CT_e(:, :) = 0.0d0
        KT_e(:, :) = 0.0d0

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 1: 全ガウスポイントの物理量を一括計算
        !---------------------------------------------------------------------------------------------------------------------------
        do iG = 1, num_gauss
            state(iG)%temperature = element%interpolate(element%gauss(iG), temperature%pre) !&
            state(iG)%porosity    = element%interpolate(element%gauss(iG), porosity%pre) !&
        end do
        call properties%calc_thermal(i_material, state, lambda, Ca)

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 2: 要素行列 CT_e と KT_e を計算
        !---------------------------------------------------------------------------------------------------------------------------
        do iG = 1, num_gauss
            weight = element%weight(iG)
            detJ = element%jacobian_det(element%gauss(iG))
            do il = 1, num_nodes
                dNdx_i = (element%jacobian(2, 2, element%gauss(iG)) * element%dpsi_dxi(il, element%gauss(iG)) - &
                          element%jacobian(2, 1, element%gauss(iG)) * element%dpsi_deta(il, element%gauss(iG))) / detJ
                dNdy_i = (-element%jacobian(1, 2, element%gauss(iG)) * element%dpsi_dxi(il, element%gauss(iG)) + &
                          element%jacobian(1, 1, element%gauss(iG)) * element%dpsi_deta(il, element%gauss(iG))) / detJ
                do jl = 1, num_nodes
                    dNdx_j = (element%jacobian(2, 2, element%gauss(iG)) * element%dpsi_dxi(jl, element%gauss(iG)) - &
                              element%jacobian(2, 1, element%gauss(iG)) * element%dpsi_deta(jl, element%gauss(iG))) / detJ
                    dNdy_j = (-element%jacobian(1, 2, element%gauss(iG)) * element%dpsi_dxi(jl, element%gauss(iG)) + &
                              element%jacobian(1, 1, element%gauss(iG)) * element%dpsi_deta(jl, element%gauss(iG))) / detJ

                    CT_e(il, jl) = CT_e(il, jl) + element%psi(il, element%gauss(iG)) * & !&
                                                  element%psi(jl, element%gauss(iG)) * Ca(iG) * weight * detJ
                    KT_e(il, jl) = KT_e(il, jl) + (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * lambda(iG) * weight * detJ
                end do
            end do
        end do

        dt_n = controls%time%get_dt()
        call controls%time%get_time_coefficients(actual_order, coefficients)

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 3: 最終的な LHS(J_e) と RHS(R_e) を構築
        !---------------------------------------------------------------------------------------------------------------------------
        ! --- 3a. LHS行列 J_e の構築 (物理的に正しい式) ---
        J_e(:, :) = 0.0d0
        do jl = 1, num_nodes
            do il = 1, num_nodes
                J_e(il, jl) = coefficients(0) * CT_e(il, jl) + dt_n * KT_e(il, jl)
            end do
        end do

        T_hist_e(:) = 0.0d0
        do il = 1, num_nodes
            do iO = 1, actual_order
                T_hist_e(il) = T_hist_e(il) + coefficients(iO) * temperature%old(element%get_connectivity(il), iO)
            end do
        end do

        do il = 1, num_nodes
            val = 0.0d0
            do jl = 1, num_nodes
                val = val + CT_e(il, jl) * T_hist_e(jl)
            end do
            R_e(il) = -val
        end do

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 4: 全体行列・ベクトルへのアセンブル (数学的に正しい標準手順)
        !---------------------------------------------------------------------------------------------------------------------------
        do il = 1, num_nodes
            R(element%get_connectivity(il)) = R(element%get_connectivity(il)) + R_e(il)
            do jl = 1, num_nodes
                call J%add(element%get_connectivity(il), element%get_connectivity(jl), J_e(il, jl))
            end do
        end do

    end subroutine process_element_thermal_linear_1

    subroutine process_element_thermal_linear_1_ns(J, R, element, temperature, porosity, properties, controls, actual_order)
        implicit none
        ! --- arguments ---
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        class(abst_element), intent(in), pointer :: element
        type(type_variable), intent(in) :: temperature
        type(type_variable), intent(in) :: porosity
        type(type_properties_manager), intent(in) :: properties
        type(type_controls), intent(in) :: controls
        integer(int32), intent(in) :: actual_order

        ! --- Local variables ---
        integer(int32) :: index, num_nodes, num_gauss, i_material, il, jl, iG, iO
        real(real64) :: weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j
        real(real64) :: val
        real(real64) :: dt_n
        real(real64), allocatable :: coefficients(:)

        ! --- Workspace variables ---
        type(type_dense) :: CT_e
        type(type_dense) :: KT_e
        type(type_dense) :: J_e
        real(real64), allocatable :: R_e(:)
        real(real64), allocatable :: T_hist_e(:)

        ! --- Physical quantities at Gauss points ---
        type(type_state), allocatable :: state(:)
        real(real64), allocatable :: Ca(:), lambda(:)

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 0: Initialize and obtain sizes
        !---------------------------------------------------------------------------------------------------------------------------
        num_nodes = element%get_num_nodes()
        num_gauss = element%get_num_gauss()
        i_material = element%get_group()
        if (.not. controls%is_target(calc_thermal, i_material)) return

        call allocate_array(coefficients, bounds=[0:actual_order])
        call CT_e%initialize_local(num_nodes)
        call KT_e%initialize_local(num_nodes)
        call J_e%initialize_local(num_nodes)
        call allocate_array(R_e, num_nodes)
        call allocate_array(T_hist_e, num_nodes)
        allocate (state(num_gauss))
        call allocate_array(Ca, num_gauss)
        call allocate_array(lambda, num_gauss)

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 1: Compute the physical quantities at all Gauss points
        !---------------------------------------------------------------------------------------------------------------------------
        do iG = 1, num_gauss
            state(iG)%temperature = element%interpolate(element%gauss(iG), temperature%pre) !&
            state(iG)%porosity    = element%interpolate(element%gauss(iG), porosity%pre) !&
        end do
        call properties%calc_thermal(i_material, state, lambda, Ca)

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 2: Compute the element matrices CT_e and KT_e
        !---------------------------------------------------------------------------------------------------------------------------
        do iG = 1, num_gauss
            weight = element%weight(iG)
            detJ = element%jacobian_det(element%gauss(iG))
            do il = 1, num_nodes
                dNdx_i = (element%jacobian(2, 2, element%gauss(iG)) * element%dpsi_dxi(il, element%gauss(iG)) - &
                          element%jacobian(2, 1, element%gauss(iG)) * element%dpsi_deta(il, element%gauss(iG))) / detJ
                dNdy_i = (-element%jacobian(1, 2, element%gauss(iG)) * element%dpsi_dxi(il, element%gauss(iG)) + &
                          element%jacobian(1, 1, element%gauss(iG)) * element%dpsi_deta(il, element%gauss(iG))) / detJ
                do jl = 1, num_nodes
                    dNdx_j = (element%jacobian(2, 2, element%gauss(iG)) * element%dpsi_dxi(jl, element%gauss(iG)) - &
                              element%jacobian(2, 1, element%gauss(iG)) * element%dpsi_deta(jl, element%gauss(iG))) / detJ
                    dNdy_j = (-element%jacobian(1, 2, element%gauss(iG)) * element%dpsi_dxi(jl, element%gauss(iG)) + &
                              element%jacobian(1, 1, element%gauss(iG)) * element%dpsi_deta(jl, element%gauss(iG))) / detJ

                    val = element%psi(il, element%gauss(iG)) * element%psi(jl, element%gauss(iG)) * Ca(iG) * weight * detJ
                    call CT_e%add(il, jl, val)
                    val = (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * lambda(iG) * weight * detJ
                    call KT_e%add(il, jl, val)
                end do
            end do
        end do

        dt_n = controls%time%get_dt()
        call controls%time%get_time_coefficients(actual_order, coefficients)

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 3: Build the final local matrix (J_e) and vector (R_e)
        !---------------------------------------------------------------------------------------------------------------------------
        T_hist_e(:) = 0.0d0
        do il = 1, num_nodes
            do iO = 1, actual_order
                T_hist_e(il) = T_hist_e(il) + coefficients(iO) * temperature%old(element%get_connectivity(il), iO)
            end do
        end do

        call add(coefficients(0) / dt_n, CT_e, KT_e, J_e)
        call gemv(-1.0d0 / dt_n, CT_e, T_hist_e, 0.0d0, R_e)

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 4: Assemble the global matrix and vector
        !---------------------------------------------------------------------------------------------------------------------------
        do il = 1, num_nodes
            R(element%get_connectivity(il)) = R(element%get_connectivity(il)) + R_e(il)
            do jl = 1, num_nodes
                call J%add(element%get_connectivity(il), element%get_connectivity(jl), J_e%val(il, jl))
            end do
        end do

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 5: Finalization
        !---------------------------------------------------------------------------------------------------------------------------
        call deallocate_array(coefficients)
        call deallocate_array(Ca)
        call deallocate_array(lambda)
        call deallocate_array(T_hist_e)
        call deallocate_array(R_e)
        call J_e%destroy()
        call KT_e%destroy()
        call CT_e%destroy()
        deallocate (state)

    end subroutine process_element_thermal_linear_1_ns

    subroutine thermal_assemble_system_linear_1(J, R, domain, temperature, porosity, properties, controls, actual_order)
        implicit none
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        type(type_domain), intent(inout), target :: domain
        type(type_variable), intent(in) :: temperature
        type(type_variable), intent(in) :: porosity
        type(type_properties_manager), intent(in) :: properties
        type(type_controls), intent(in) :: controls
        integer(int32), intent(in) :: actual_order

        class(abst_element), pointer :: element
        integer(int32) :: iE, num_elements

        num_elements = domain%get_num_elements()
        call J%set_all(0.0d0)
        R(:) = 0.0d0

        do iE = 1, num_elements
            element => domain%Elements(iE)%e
            call process_element_thermal_linear_1(J, R, element, temperature, porosity, properties, controls, actual_order)
        end do
    end subroutine thermal_assemble_system_linear_1

    subroutine thermal_assemble_system_linear_1_parallel(J, R, domain, temperature, porosity, properties, controls, actual_order)
        implicit none
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        type(type_domain), intent(inout), target :: domain
        type(type_variable), intent(in) :: temperature
        type(type_variable), intent(in) :: porosity
        type(type_properties_manager), intent(in) :: properties
        type(type_controls), intent(in) :: controls
        integer(int32), intent(in) :: actual_order

        integer(int32) :: c, ie_idx
        class(abst_element), pointer :: element

        call J%set_all(0.0d0)
        R(:) = 0.0d0

        !$omp parallel private(c, ie_idx, element)
        do c = 1, domain%colors%num_colors
            !$omp do
            do ie_idx = 1, domain%colors%colored(c)%num_elements
                element => domain%Elements(domain%colors%colored(c)%Elements(ie_idx))%e
                call process_element_thermal_linear_1(J, R, element, temperature, porosity, properties, controls, actual_order)
            end do
            !$omp end do
        end do
        !$omp end parallel
    end subroutine thermal_assemble_system_linear_1_parallel

end module thermal_thermal_assemble
