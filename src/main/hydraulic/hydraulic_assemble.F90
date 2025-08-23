module hydraulic_hydraulic_assemble
    use, intrinsic :: iso_fortran_env, only: int32, real64
!$  use omp_lib
    use :: module_core, only:type_state, type_dp_vector_3d, assignment(=), type_variable, allocate_array, deallocate_array
    use :: module_domain, only:type_domain, abst_element
    use :: module_properties, only:type_properties_manager
    use :: module_matrix, only:type_crs, type_dense, gemv, add
    use :: module_control, only:type_time

    implicit none
    private

    public :: abst_assemble_global_hydraulic

    public :: hydraulic_assemble_system_linear_1, hydraulic_assemble_system_linear_1_parallel

    abstract interface
        subroutine abst_assemble_global_hydraulic(J, R, domain, pressure, temperature, ice, porosity, &
                                                  properties, time, actual_order)
            import :: type_crs, type_domain, type_properties_manager, type_variable, type_time, int32, real64
            implicit none
            type(type_crs), intent(inout) :: J
            real(real64), intent(inout) :: R(:)
            type(type_domain), intent(inout), target :: domain
            type(type_variable), intent(in) :: pressure
            type(type_variable), intent(in) :: temperature
            type(type_variable), intent(in) :: ice
            type(type_variable), intent(in) :: porosity
            type(type_properties_manager), intent(in) :: properties
            type(type_time), intent(in) :: time
            integer(int32), intent(in) :: actual_order
        end subroutine abst_assemble_global_hydraulic
    end interface
contains

    subroutine process_element_hydraulic_linear_1(J, R, element, pressure, temperature, ice, porosity, &
                                                  properties, time, actual_order)
        implicit none
        ! --- 引数 ---
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        class(abst_element), intent(in), pointer :: element
        type(type_variable), intent(in) :: pressure
        type(type_variable), intent(in) :: temperature
        type(type_variable), intent(in) :: ice
        type(type_variable), intent(in) :: porosity
        type(type_properties_manager), intent(in) :: properties
        type(type_time), intent(in) :: time
        integer(int32), intent(in) :: actual_order

        ! --- ローカル変数 ---
        integer(int32) :: index, num_nodes, num_gauss, i_material, il, jl, iG, iO
        real(real64) :: weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j
        real(real64) :: val, zeta
        real(real64) :: dt

        ! --- スタック上のワークスペース (自動配列) ---
        real(real64) :: CH_e(element%get_num_nodes(), element%get_num_nodes())
        real(real64) :: KH_e(element%get_num_nodes(), element%get_num_nodes())
        real(real64) :: J_e(element%get_num_nodes(), element%get_num_nodes())
        real(real64) :: R_e(element%get_num_nodes())

        ! --- ガウスポイントでの物理量 (自動配列) ---
        type(type_state) :: state(element%get_num_gauss())
        real(real64) :: kflh(element%get_num_gauss())
        real(real64) :: dot_ice(element%get_num_gauss())

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 0: 初期化とサイズの取得
        !---------------------------------------------------------------------------------------------------------------------------
        num_nodes = element%get_num_nodes()
        num_gauss = element%get_num_gauss()
        i_material = element%get_group()

        CH_e(:, :) = 0.0d0
        KH_e(:, :) = 0.0d0

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 1: 全ガウスポイントの物理量を一括計算
        !---------------------------------------------------------------------------------------------------------------------------
        do iG = 1, num_gauss
            state(iG)%temperature = element%interpolate(element%gauss(iG), temperature%pre) !&
            state(iG)%pressure    = element%interpolate(element%gauss(iG), pressure%pre) !&
            state(iG)%porosity    = element%interpolate(element%gauss(iG), porosity%pre) !&
            state(iG)%ice         = element%interpolate(element%gauss(iG), ice%pre) !&
        end do
        call properties%calc_hydraulic(i_material, state, kflh)

        dt = time%get_dt()
        dot_ice(:) = 0.0d0
        do il = 1, num_nodes
            dot_ice(il) = ice%dif(element%get_connectivity(il)) / dt
        end do

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 2: 要素行列 CH_e と KH_e を計算
        !---------------------------------------------------------------------------------------------------------------------------
        do iG = 1, num_gauss
            weight = element%weight(iG)
            detJ = element%jacobian_det(element%gauss(iG))
            zeta = state(iG)%density_ice / state(iG)%density_water - 1.0d0
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

                    CH_e(il, jl) = CH_e(il, jl) + element%psi(il, element%gauss(iG)) * & !&
                                                  element%psi(jl, element%gauss(iG)) * zeta * weight * detJ
                    KH_e(il, jl) = KH_e(il, jl) + (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * kflh(iG) * weight * detJ
                end do
            end do
        end do

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 3: 最終的な LHS(J_e) と RHS(R_e) を構築
        !---------------------------------------------------------------------------------------------------------------------------
        ! --- 3a. LHS行列 J_e の構築 (物理的に正しい式) ---
        J_e(:, :) = 0.0d0
        do jl = 1, num_nodes
            do il = 1, num_nodes
                J_e(il, jl) = KH_e(il, jl)
            end do
        end do

        do il = 1, num_nodes
            val = 0.0d0
            do jl = 1, num_nodes
                val = val + CH_e(il, jl) * dot_ice(jl)
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

    end subroutine process_element_hydraulic_linear_1

    subroutine process_element_hydraulic_linear_1_ns(J, R, element, pressure, temperature, ice, porosity, &
                                                     properties, time, actual_order)
        implicit none
        ! --- arguments ---
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        class(abst_element), intent(in), pointer :: element
        type(type_variable), intent(in) :: pressure
        type(type_variable), intent(in) :: temperature
        type(type_variable), intent(in) :: ice
        type(type_variable), intent(in) :: porosity
        type(type_properties_manager), intent(in) :: properties
        type(type_time), intent(in) :: time
        integer(int32), intent(in) :: actual_order

        ! --- Local variables ---
        integer(int32) :: index, num_nodes, num_gauss, i_material, il, jl, iG, iO
        real(real64) :: weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j
        real(real64) :: val, zeta
        real(real64) :: dt

        ! --- Workspace variables ---
        type(type_dense) :: CH_e
        type(type_dense) :: KH_e
        type(type_dense) :: J_e
        real(real64), allocatable :: R_e(:)

        ! --- Physical quantities at Gauss points ---
        type(type_state), allocatable :: state(:)
        real(real64), allocatable :: kflh(:)
        real(real64), allocatable :: dot_ice(:)

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 0: Initialize and obtain sizes
        !---------------------------------------------------------------------------------------------------------------------------
        num_nodes = element%get_num_nodes()
        num_gauss = element%get_num_gauss()
        i_material = element%get_group()

        call CH_e%initialize_local(num_nodes)
        call KH_e%initialize_local(num_nodes)
        call J_e%initialize_local(num_nodes)
        call allocate_array(R_e, num_nodes)
        allocate (state(num_gauss))
        call allocate_array(kflh, num_gauss)
        call allocate_array(dot_ice, num_nodes)

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 1: Compute the physical quantities at all Gauss points
        !---------------------------------------------------------------------------------------------------------------------------
        do iG = 1, num_gauss
            state(iG)%temperature = element%interpolate(element%gauss(iG), temperature%pre) !&
            state(iG)%pressure    = element%interpolate(element%gauss(iG), pressure%pre) !&
            state(iG)%porosity    = element%interpolate(element%gauss(iG), porosity%pre) !&
            state(iG)%ice         = element%interpolate(element%gauss(iG), ice%pre) !&
        end do
        call properties%calc_hydraulic(i_material, state, kflh)

        dt = time%get_dt()
        dot_ice(:) = 0.0d0
        do il = 1, num_nodes
            dot_ice(il) = ice%dif(element%get_connectivity(il)) / dt
        end do

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 2: Compute the element matrices CH_e and KH_e
        !---------------------------------------------------------------------------------------------------------------------------
        do iG = 1, num_gauss
            weight = element%weight(iG)
            detJ = element%jacobian_det(element%gauss(iG))
            zeta = state(iG)%density_ice / state(iG)%density_water - 1.0d0
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

                    val = element%psi(il, element%gauss(iG)) * element%psi(jl, element%gauss(iG)) * zeta * weight * detJ
                    call CH_e%add(il, jl, val)
                    val = (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * kflh(iG) * weight * detJ
                    call KH_e%add(il, jl, val)
                end do
            end do
        end do

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 3: Build the final local matrix (J_e) and vector (R_e)
        !---------------------------------------------------------------------------------------------------------------------------
        call add(0.0d0, KH_e, CH_e, J_e)
        call gemv(-1.0d0, CH_e, dot_ice, 0.0d0, R_e)

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
        call deallocate_array(kflh)
        call deallocate_array(R_e)
        call J_e%destroy()
        call KH_e%destroy()
        call CH_e%destroy()
        deallocate (state)

    end subroutine process_element_hydraulic_linear_1_ns

    subroutine hydraulic_assemble_system_linear_1(J, R, domain, pressure, temperature, porosity, ice, &
                                                  properties, time, actual_order)
        implicit none
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        type(type_domain), intent(inout), target :: domain
        type(type_variable), intent(in) :: pressure
        type(type_variable), intent(in) :: temperature
        type(type_variable), intent(in) :: ice
        type(type_variable), intent(in) :: porosity
        type(type_properties_manager), intent(in) :: properties
        type(type_time), intent(in) :: time
        integer(int32), intent(in) :: actual_order

        class(abst_element), pointer :: element
        integer(int32) :: iE, num_elements

        num_elements = domain%get_num_elements()
        call J%set_all(0.0d0)
        R(:) = 0.0d0

        do iE = 1, num_elements
            element => domain%Elements(iE)%e
            call process_element_hydraulic_linear_1(J, R, element, pressure, temperature, porosity, ice, &
                                                    properties, time, actual_order)
        end do
    end subroutine hydraulic_assemble_system_linear_1

    subroutine hydraulic_assemble_system_linear_1_parallel(J, R, domain, pressure, temperature, porosity, ice, &
                                                           properties, time, actual_order)
        implicit none
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        type(type_domain), intent(inout), target :: domain
        type(type_variable), intent(in) :: pressure
        type(type_variable), intent(in) :: temperature
        type(type_variable), intent(in) :: ice
        type(type_variable), intent(in) :: porosity
        type(type_properties_manager), intent(in) :: properties
        type(type_time), intent(in) :: time
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
                call process_element_hydraulic_linear_1(J, R, element, pressure, temperature, porosity, ice, &
                                                        properties, time, actual_order)
            end do
            !$omp end do
        end do
        !$omp end parallel
    end subroutine hydraulic_assemble_system_linear_1_parallel
end module hydraulic_hydraulic_assemble
