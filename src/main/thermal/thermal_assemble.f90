module thermal_thermal_assemble
    use, intrinsic :: iso_fortran_env, only: int32, real64
!$  use omp_lib
    use :: module_core, only:type_state, type_dp_vector_3d, assignment(=), type_variable, allocate_array, deallocate_array, type_crs, type_dense
    use :: module_domain, only:type_domain, abst_mesh
    use :: module_properties, only:type_properties_manager
    use :: module_calculate, only:gemv, add
    use :: module_control

    implicit none
    private

    public :: abst_assemble_global_thermal
    public :: thermal_assemble_system_linear_1, thermal_assemble_system_linear_1_parallel

    ! --- ワークスペース型 ---
    type :: type_workspace_thermal_assemble
        private
        integer(int32) :: type_id = -1
        integer(int32) :: num_nodes = 0
        integer(int32) :: num_gauss = 0
        integer(int32) :: order = 0

        ! 計算結果/履歴
        real(real64), allocatable :: coefficients(:)
        type(type_dense) :: CT_e
        type(type_dense) :: KT_e
        type(type_dense) :: J_e
        real(real64), allocatable :: R_e(:)
        real(real64), allocatable :: T_hist_e(:)

        ! 物理量/状態変数
        type(type_state), allocatable :: state(:)
        real(real64), allocatable :: Ca(:)
        real(real64), allocatable :: lambda(:)

        ! ガウス点計算用ワークスペース
        real(real64), allocatable :: psi_g(:)
        real(real64), allocatable :: dpsi_xi_g(:)
        real(real64), allocatable :: dpsi_eta_g(:)
        real(real64), allocatable :: dNdx(:)
        real(real64), allocatable :: dNdy(:)

        integer(int32), pointer :: p_conn(:) => null()
        real(real64), pointer :: p_weight(:) => null()
        type(type_dp_vector_3d), pointer :: p_gauss(:) => null()
    contains
        procedure :: initialize => initialize_workspace_thermal_assemble
        procedure :: destroy => destroy_workspace_thermal_assemble
    end type type_workspace_thermal_assemble

    abstract interface
        subroutine abst_assemble_global_thermal(J, R, domain, temperature, porosity, properties, controls, actual_order)
            import :: type_crs, type_domain, type_properties_manager, type_variable, type_controls, int32, real64
            implicit none
            type(type_crs), intent(inout) :: J
            real(real64), intent(inout) :: R(:)
            type(type_domain), intent(inout), target :: domain
            type(type_variable), intent(in) :: temperature, porosity
            type(type_properties_manager), intent(in) :: properties
            type(type_controls), intent(in) :: controls
            integer(int32), intent(in) :: actual_order
        end subroutine abst_assemble_global_thermal
    end interface

contains

    subroutine initialize_workspace_thermal_assemble(self, mesh, actual_order)
        implicit none
        class(type_workspace_thermal_assemble), intent(inout) :: self
        class(abst_mesh), intent(in), pointer :: mesh
        integer(int32), intent(in) :: actual_order
        integer(int32) :: new_type_id, new_num_nodes, new_num_gauss

        new_type_id = mesh%get_type()
        if (self%type_id /= new_type_id) then
            new_num_nodes = mesh%get_num_nodes()
            new_num_gauss = mesh%get_num_gauss()

            call self%CT_e%destroy()
            call self%KT_e%destroy()
            call self%J_e%destroy()
            call deallocate_array(self%R_e)
            call deallocate_array(self%T_hist_e)
            if (allocated(self%state)) deallocate (self%state)
            call deallocate_array(self%Ca)
            call deallocate_array(self%lambda)
            call deallocate_array(self%psi_g)
            call deallocate_array(self%dpsi_xi_g)
            call deallocate_array(self%dpsi_eta_g)
            call deallocate_array(self%dNdx)
            call deallocate_array(self%dNdy)

            call self%CT_e%initialize_local(new_num_nodes)
            call self%KT_e%initialize_local(new_num_nodes)
            call self%J_e%initialize_local(new_num_nodes)
            call allocate_array(self%R_e, new_num_nodes)
            call allocate_array(self%T_hist_e, new_num_nodes)
            allocate (self%state(new_num_gauss))
            call allocate_array(self%Ca, new_num_gauss)
            call allocate_array(self%lambda, new_num_gauss)
            call allocate_array(self%psi_g, new_num_nodes)
            call allocate_array(self%dpsi_xi_g, new_num_nodes)
            call allocate_array(self%dpsi_eta_g, new_num_nodes)
            call allocate_array(self%dNdx, new_num_nodes)
            call allocate_array(self%dNdy, new_num_nodes)

            self%type_id = new_type_id
            self%num_nodes = new_num_nodes
            self%num_gauss = new_num_gauss
        end if

        if (self%order /= actual_order) then
            call deallocate_array(self%coefficients)
            call allocate_array(self%coefficients, bounds=[0:actual_order])
            self%order = actual_order
        end if
    end subroutine initialize_workspace_thermal_assemble

    subroutine destroy_workspace_thermal_assemble(self)
        implicit none
        class(type_workspace_thermal_assemble), intent(inout) :: self

        call deallocate_array(self%coefficients)
        call self%CT_e%destroy()
        call self%KT_e%destroy()
        call self%J_e%destroy()
        call deallocate_array(self%R_e)
        call deallocate_array(self%T_hist_e)
        if (allocated(self%state)) deallocate (self%state)
        call deallocate_array(self%Ca)
        call deallocate_array(self%lambda)
        call deallocate_array(self%psi_g)
        call deallocate_array(self%dpsi_xi_g)
        call deallocate_array(self%dpsi_eta_g)
        call deallocate_array(self%dNdx)
        call deallocate_array(self%dNdy)

        self%type_id = -1
        self%num_nodes = 0
        self%num_gauss = 0
        self%order = 0
    end subroutine destroy_workspace_thermal_assemble

    subroutine process_mesh_thermal_linear_1(J, R, mesh, temperature, porosity, properties, controls, actual_order, workspace)
        implicit none
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        class(abst_mesh), intent(in), pointer :: mesh
        type(type_variable), intent(in) :: temperature, porosity
        type(type_properties_manager), intent(in) :: properties
        type(type_controls), intent(in) :: controls
        integer(int32), intent(in) :: actual_order
        type(type_workspace_thermal_assemble), intent(inout) :: workspace

        integer(int32) :: num_nodes, num_gauss, i_material
        integer(int32) :: il, jl, iG, iO, i
        real(real64) :: weight_detJ, val, dt
        real(real64) :: detJ

        i_material = mesh%get_group()
        if (.not. controls%is_target(calc_thermal, i_material)) return

        call workspace%initialize(mesh, actual_order)
        call workspace%CT_e%zero()
        call workspace%KT_e%zero()
        workspace%R_e(:) = 0.0d0

        num_nodes = workspace%num_nodes
        num_gauss = workspace%num_gauss
        dt = controls%time%get_dt()

        call controls%time%get_time_coefficients(actual_order, workspace%coefficients)
        workspace%p_weight => mesh%get_weight()
        workspace%p_gauss => mesh%get_gauss()
        workspace%p_conn => mesh%get_connectivity()
        ! print *, p_conn

        ! --- 物理量計算（ガウス点ごと） ---
        do iG = 1, num_gauss
            workspace%state(iG)%temperature = mesh%lerp(workspace%p_gauss(iG), temperature%pre)
            workspace%state(iG)%porosity = mesh%lerp(workspace%p_gauss(iG), porosity%pre)
        end do
        call properties%calc_thermal(i_material, workspace%state, workspace%lambda, workspace%Ca)

        ! --- ガウス積分ループ：行列・ベクトル組み立て ---
        do iG = 1, num_gauss
            detJ = mesh%jacobian_det(workspace%p_gauss(iG))

            ! ★ 安定性向上のためのチェック
#ifdef USE_DEBUG
            if (detJ <= 1.0d-12) then
                ! 必要に応じて警告メッセージを出力
                cycle ! このガウス点の計算をスキップ
            end if
#endif

            do i = 1, num_nodes
                workspace%psi_g(i) = mesh%psi(i, workspace%p_gauss(iG))
                workspace%dpsi_xi_g(i) = mesh%dpsi(i, 1, workspace%p_gauss(iG))
                workspace%dpsi_eta_g(i) = mesh%dpsi(i, 2, workspace%p_gauss(iG))
                workspace%dNdx(i) = (mesh%jacobian(2, 2, workspace%p_gauss(iG)) * workspace%dpsi_xi_g(i) - mesh%jacobian(2, 1, workspace%p_gauss(iG)) * workspace%dpsi_eta_g(i)) / detJ
                workspace%dNdy(i) = (-mesh%jacobian(1, 2, workspace%p_gauss(iG)) * workspace%dpsi_xi_g(i) + mesh%jacobian(1, 1, workspace%p_gauss(iG)) * workspace%dpsi_eta_g(i)) / detJ
            end do

            weight_detJ = workspace%p_weight(iG) * detJ

            do il = 1, num_nodes
                do jl = 1, num_nodes
                    val = workspace%psi_g(il) * workspace%psi_g(jl) * workspace%Ca(iG) * weight_detJ
                    call workspace%CT_e%add(il, jl, val)
                    val = (workspace%dNdx(il) * workspace%dNdx(jl) + workspace%dNdy(il) * workspace%dNdy(jl)) * workspace%lambda(iG) * weight_detJ
                    call workspace%KT_e%add(il, jl, val)
                end do
            end do
        end do

        ! --- 履歴項 ---
        workspace%T_hist_e(:) = 0.0d0
        do il = 1, num_nodes
            do iO = 1, actual_order
                workspace%T_hist_e(il) = workspace%T_hist_e(il) + workspace%coefficients(iO) * temperature%old(workspace%p_conn(il), iO)
            end do
        end do

        call add(workspace%coefficients(0) / dt, workspace%CT_e, workspace%KT_e, workspace%J_e)
        call gemv(-1.0d0 / dt, workspace%CT_e, workspace%T_hist_e, 0.0d0, workspace%R_e)

        do il = 1, num_nodes
            R(workspace%p_conn(il)) = R(workspace%p_conn(il)) + workspace%R_e(il)
            do jl = 1, num_nodes
                call J%add(workspace%p_conn(il), workspace%p_conn(jl), workspace%J_e%val(il, jl))
            end do
        end do
    end subroutine process_mesh_thermal_linear_1

    subroutine thermal_assemble_system_linear_1(J, R, domain, temperature, porosity, properties, controls, actual_order)
        implicit none
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        type(type_domain), intent(inout), target :: domain
        type(type_variable), intent(in) :: temperature, porosity
        type(type_properties_manager), intent(in) :: properties
        type(type_controls), intent(in) :: controls
        integer(int32), intent(in) :: actual_order
        class(abst_mesh), pointer :: mesh
        integer(int32) :: iE, num_elements
        type(type_workspace_thermal_assemble) :: workspace

        num_elements = domain%get_num_elements()
        call J%zero()
        R(:) = 0.0d0

        do iE = 1, num_elements
            mesh => domain%elements(iE)%e
            call process_mesh_thermal_linear_1(J, R, mesh, temperature, porosity, properties, controls, actual_order, workspace)
        end do

        call workspace%destroy()
    end subroutine thermal_assemble_system_linear_1

    subroutine thermal_assemble_system_linear_1_parallel(J, R, domain, temperature, porosity, properties, controls, actual_order)
        implicit none
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        type(type_domain), intent(inout), target :: domain
        type(type_variable), intent(in) :: temperature, porosity
        type(type_properties_manager), intent(in) :: properties
        type(type_controls), intent(in) :: controls
        integer(int32), intent(in) :: actual_order
        integer(int32) :: c, ie_idx
        class(abst_mesh), pointer :: mesh
        type(type_workspace_thermal_assemble) :: workspace

        call J%zero()
        R(:) = 0.0d0

        !$omp parallel private(workspace, mesh, ie_idx, c)
        do c = 1, domain%colors%num_colors
            !$omp do
            do ie_idx = 1, domain%colors%colored(c)%num_elements
                mesh => domain%elements(domain%colors%colored(c)%elements(ie_idx))%e
                call process_mesh_thermal_linear_1(J, R, mesh, temperature, porosity, properties, controls, actual_order, workspace)
            end do
            !$omp end do
        end do

        call workspace%destroy()
        !$omp end parallel
    end subroutine thermal_assemble_system_linear_1_parallel

end module thermal_thermal_assemble
