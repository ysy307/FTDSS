module module_ftdss
    use, intrinsic :: iso_fortran_env
    use :: mpi_f08
    use :: stdlib_logger
    use :: module_core
    use :: module_input, only:type_input
    use :: module_control, only:type_controls
    ! use :: module_output, only:type_output
    use :: module_domain, only:type_domain, abst_fe
    ! use :: module_properties, only:type_properties_manager
    ! use :: module_boundary, only:type_bc
    use :: module_initial, only:type_ic_manager
    use :: module_field, only:type_jacobian_matrix, type_residual_vector

    use :: module_thermal, only:type_thermal
    use :: module_hydraulic, only:type_hydraulic
    implicit none

    type :: type_ftdss
        type(type_domain) :: domain

        type(type_variable) :: porosity
        type(type_variable) :: temperature
        type(type_variable) :: pressure

        type(type_coordinate_array_dp) :: water_flux
        type(type_coordinate_array_dp) :: vapor_flux

        type(type_variable) :: Qw
        type(type_variable) :: Qi
        type(type_variable) :: Qa
        type(type_variable) :: Qv

        type(type_jacobian_matrix) :: J
        type(type_residual_vector) :: R

        type(type_thermal) :: thermal
        type(type_hydraulic) :: hydraulic

        ! class(abst_thermal), allocatable :: thermal
        ! class(abst_hydraulic), allocatable :: hydraulic

        ! type(type_properties_manager) :: property
        ! type(type_bc) :: bc

        type(type_controls) :: controls
        ! type(type_output) :: output

    contains
        procedure, pass(self) :: initialize => initialize_type_ftdss
        procedure, pass(self) :: shift => shift_type_ftdss
        procedure, pass(self) :: calc_gradient => calc_gradient_ftdss
    end type type_ftdss

contains
    subroutine initialize_type_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        type(type_input) :: input
        type(type_ic_manager) :: ic

        integer(int32) :: max_bdf_order
        integer(int32), allocatable :: active_region_ids(:)
        integer(int32) :: ierr
        integer(int32) :: num_nodes
        character(len=10), allocatable :: profiler_labels(:)

        profiler_labels = [character(len=10) :: "IO", "Setup", "Assemble", "Solve", "Total"]
        call self%controls%profiler%initialize(profiler_labels)
        call self%controls%profiler%record(TIME_RECORD_START)
        call self%controls%profiler%start("Total")
        call self%controls%profiler%start("IO")

        call setup_handler()

        call input%initialize()
        call self%controls%initialize(input)
        call ic%initialize(input)

        if (input%output_settings%standard_output%print_progress) then
            call global_logger%configure(level=information_level, &
                                         time_stamp=.true., &
                                         max_width=0)
        else
            call global_logger%configure(level=warning_level, &
                                         time_stamp=.true., &
                                         max_width=0)
        end if

        !---------------------------------------------------------------------------------------------------------------------------
        !
        !---------------------------------------------------------------------------------------------------------------------------
        num_nodes = input%geometry%vtk%num_points
        call self%domain%initialize(input, self%controls)

        max_bdf_order = input%basic%solver_settings%bdf_order
        call self%porosity%initialize(num_nodes, max_bdf_order)
        call ic%apply(IC_TARGET_POROSITY, self%domain, self%porosity)

        if (self%controls%is_physics_active(PHYSICS_TYPE_THERMAL)) then
            call self%temperature%initialize(num_nodes, max_bdf_order)
            call ic%apply(IC_TARGET_THERMAL, self%domain, self%temperature)
        end if

        if (self%controls%is_physics_active(PHYSICS_TYPE_HYDRAULIC)) then
            call self%pressure%initialize(num_nodes, max_bdf_order)
            call ic%apply(IC_TARGET_HYDRAULIC, self%domain, self%pressure)
        end if

        call self%Qw%initialize(num_nodes, max_bdf_order)
        call self%Qi%initialize(num_nodes, max_bdf_order)
        call self%Qa%initialize(num_nodes, max_bdf_order)
        call self%Qv%initialize(num_nodes, max_bdf_order)

        call input%geometry%vtk%get_active_region_info(active_region_ids, target_dim=self%domain%get_computation_dimension())

        call self%thermal%initialize(input, active_region_ids)
        call self%hydraulic%initialize(input, active_region_ids)

        ! self%thermal = type_thermal_crs(input, self%coordinate, self%domain)

        ! call self%property%initialize(input, ierr)

        ! call self%output%initialize(input, self%domain, self%coordinate)

        ! call self%output%output_coloring(self%domain)

        call self%controls%profiler%stop("IO")
        call global_logger%log_information(message="FTDSS module initialized successfully.")
    end subroutine initialize_type_ftdss

    !>
    !> 節点上の物理量勾配を計算する（L2射影 / Lumped Mass法）
    !> Private成分へのアクセスをGetter経由に修正
    !>
    subroutine calc_gradient_ftdss(self, values_vec, grad)
        implicit none
        class(type_ftdss), intent(inout) :: self
        real(real64), intent(in) :: values_vec(:)
        type(type_coordinate_array_dp), intent(inout) :: grad

        class(abst_fe), pointer :: fe
        integer(int32), dimension(:), pointer :: p_conn

        ! 要素データ用配列
        real(real64), allocatable :: elem_u(:)
        real(real64), allocatable :: node_coords(:, :)
        real(real64), allocatable :: psi(:)
        real(real64), allocatable :: dpsi_dx(:, :)

        ! [追加] FE情報キャッシュ用（Getterで取得するため）
        real(real64), allocatable :: fe_weights(:)
        type(type_coordinate_dp), allocatable :: fe_gauss_pts(:)

        real(real64), allocatable :: nodal_vol(:)

        real(real64) :: det_j
        real(real64) :: gauss_grad(3)
        real(real64) :: w_vol, shape_weight
        type(type_coordinate_dp) :: r

        integer(int32) :: num_elements, num_total_nodes, dim
        integer(int32) :: n_nodes_elem, n_gauss
        integer(int32) :: i, p, k, d, global_nid

        num_elements = self%domain%get_num_elements()
        num_total_nodes = self%domain%get_num_nodes()
        dim = self%domain%get_computation_dimension()

        ! 初期化
        call grad%zero()

        if (allocated(nodal_vol)) deallocate (nodal_vol)
        allocate (nodal_vol(num_total_nodes))
        nodal_vol(:) = 0.0d0

        !-----------------------------------------------------------------------
        ! 全要素ループ
        !-----------------------------------------------------------------------
        do i = 1, num_elements
            call self%domain%get_element(i, fe)
            call self%domain%get_connectivity(i, p_conn)

            call fe%get_num_nodes(n_nodes_elem)
            call fe%get_num_gauss(n_gauss)

            ! -----------------------------------------------------------
            ! [修正] Getterを使ってガウス点と重みの配列を取得する
            ! -----------------------------------------------------------
            ! ※ get_weight, get_gauss は allocatable 配列を受け取り、そこに値をセットする仕様
            call fe%get_weight(fe_weights)
            call fe%get_gauss(fe_gauss_pts)

            ! 作業配列確保
            if (allocated(elem_u)) deallocate (elem_u)
            if (allocated(node_coords)) deallocate (node_coords)
            if (allocated(psi)) deallocate (psi)
            if (allocated(dpsi_dx)) deallocate (dpsi_dx)

            allocate (elem_u(n_nodes_elem))
            allocate (node_coords(dim, n_nodes_elem))
            allocate (psi(n_nodes_elem))
            allocate (dpsi_dx(n_nodes_elem, dim))

            ! 要素データの収集
            elem_u(:) = values_vec(p_conn(:))
            do k = 1, n_nodes_elem
                node_coords(:, k) = self%domain%nodes%coordinates(:, p_conn(k))
            end do

            !-------------------------------------------------------------------
            ! ガウス積分点ループ
            !-------------------------------------------------------------------
            do p = 1, n_gauss
                ! [修正] キャッシュした配列から値を取得
                r = fe_gauss_pts(p)

                call fe%calc_shape_data(r, node_coords, p_conn, psi, dpsi_dx, det_j)

                ! [修正] キャッシュした配列から値を取得
                w_vol = fe_weights(p) * det_j

                ! 勾配計算
                gauss_grad = 0.0d0
                do d = 1, dim
                    gauss_grad(d) = dot_product(elem_u, dpsi_dx(:, d))
                end do

                ! 節点への分配
                do k = 1, n_nodes_elem
                    global_nid = p_conn(k)
                    shape_weight = psi(k) * w_vol

                    nodal_vol(global_nid) = nodal_vol(global_nid) + shape_weight

                    if (allocated(grad%x)) grad%x(global_nid) = grad%x(global_nid) + shape_weight * gauss_grad(1)
                    if (dim >= 2) then
                        if (allocated(grad%y)) grad%y(global_nid) = grad%y(global_nid) + shape_weight * gauss_grad(2)
                    end if
                    if (dim >= 3) then
                        if (allocated(grad%z)) grad%z(global_nid) = grad%z(global_nid) + shape_weight * gauss_grad(3)
                    end if
                end do
            end do
        end do

        ! 正規化
        do k = 1, num_total_nodes
            if (nodal_vol(k) > epsilon(1.0d0)) then
                if (allocated(grad%x)) grad%x(k) = grad%x(k) / nodal_vol(k)
                if (allocated(grad%y)) grad%y(k) = grad%y(k) / nodal_vol(k)
                if (allocated(grad%z)) grad%z(k) = grad%z(k) / nodal_vol(k)
            else
                if (allocated(grad%x)) grad%x(k) = 0.0d0
                if (allocated(grad%y)) grad%y(k) = 0.0d0
                if (allocated(grad%z)) grad%z(k) = 0.0d0
            end if
        end do

        ! 後始末
        if (allocated(elem_u)) deallocate (elem_u)
        if (allocated(node_coords)) deallocate (node_coords)
        if (allocated(psi)) deallocate (psi)
        if (allocated(dpsi_dx)) deallocate (dpsi_dx)
        if (allocated(nodal_vol)) deallocate (nodal_vol)
        ! キャッシュの解放
        if (allocated(fe_weights)) deallocate (fe_weights)
        if (allocated(fe_gauss_pts)) deallocate (fe_gauss_pts)

    end subroutine calc_gradient_ftdss

    subroutine shift_type_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        ! call self%phi%shift()
        ! if (self%controls%calculate_thermal) then
        !     call self%T%shift()
        !     call self%thermal%shift()
        ! end if

    end subroutine shift_type_ftdss

end module module_ftdss
