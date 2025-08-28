module main_thermal_workspace
    use, intrinsic :: iso_fortran_env
    use :: module_core, only:type_state, type_dp_vector_3d, type_variable, allocate_array, deallocate_array, type_dense
    use :: module_domain, only:abst_mesh
    use :: module_control
    implicit none
    private

    public :: type_workspace_thermal_assemble

    type :: type_workspace_thermal_assemble
        integer(int32) :: type_id = -1
        integer(int32) :: num_nodes = 0
        integer(int32) :: num_gauss = 0
        integer(int32) :: order = 0

        ! 計算結果/履歴
        real(real64)              :: dt !&
        real(real64), allocatable :: coefficients(:) !&
        type(type_dense)          :: CT_e !&
        type(type_dense)          :: KT_e !&
        type(type_dense)          :: J_e !&
        real(real64), allocatable :: R_e(:) !&
        real(real64), allocatable :: T_hist_e(:) !&

        ! 物理量/状態変数
        type(type_state), allocatable :: state(:) !&
        real(real64),     allocatable :: Ca(:) !&
        real(real64),     allocatable :: lambda(:) !&

        ! ガウス点計算用ワークスペース
        real(real64) :: detJ
        real(real64) :: weight_detJ
        real(real64), allocatable :: psi(:)
        real(real64), allocatable :: dpsi_dxi(:)
        real(real64), allocatable :: dpsi_deta(:)
        real(real64), allocatable :: dpsi_dx(:)
        real(real64), allocatable :: dpsi_dy(:)

        ! メッシュ情報ポインタ
        integer(int32),          dimension(:), pointer :: p_conn   => null() !&
        real(real64),            dimension(:), pointer :: p_weight => null() !&
        type(type_dp_vector_3d), dimension(:), pointer :: p_gauss  => null() !&

        integer(int32), allocatable :: connectivity(:)
    contains
        procedure, pass(self) :: initialize => initialize_workspace_thermal_assemble
        procedure, pass(self) :: calc_gauss => calc_gauss_points_values
        procedure, pass(self) :: calc_history
        procedure, pass(self) :: destroy => destroy_workspace_thermal_assemble
    end type type_workspace_thermal_assemble

contains

    subroutine initialize_workspace_thermal_assemble(self, mesh, controls, actual_order)
        implicit none
        class(type_workspace_thermal_assemble), intent(inout) :: self
        class(abst_mesh), intent(in), pointer :: mesh
        class(type_controls), intent(in) :: controls
        integer(int32), intent(in) :: actual_order
        integer(int32) :: new_type_id, new_num_nodes, new_num_gauss
        integer(int32) :: i, j

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
            call deallocate_array(self%psi)
            call deallocate_array(self%dpsi_dxi)
            call deallocate_array(self%dpsi_deta)
            call deallocate_array(self%dpsi_dx)
            call deallocate_array(self%dpsi_dy)
            call deallocate_array(self%connectivity)

            call self%CT_e%initialize_local(new_num_nodes)
            call self%KT_e%initialize_local(new_num_nodes)
            call self%J_e%initialize_local(new_num_nodes)
            call allocate_array(self%R_e, new_num_nodes)
            call allocate_array(self%T_hist_e, new_num_nodes)
            allocate (self%state(new_num_gauss))
            call allocate_array(self%Ca, new_num_gauss)
            call allocate_array(self%lambda, new_num_gauss)
            call allocate_array(self%psi, new_num_nodes)
            call allocate_array(self%dpsi_dxi, new_num_nodes)
            call allocate_array(self%dpsi_deta, new_num_nodes)
            call allocate_array(self%dpsi_dx, new_num_nodes)
            call allocate_array(self%dpsi_dy, new_num_nodes)

            self%type_id = new_type_id
            self%num_nodes = new_num_nodes
            self%num_gauss = new_num_gauss
        end if

        if (self%order /= actual_order) then
            call deallocate_array(self%coefficients)
            call allocate_array(self%coefficients, bounds=[0:actual_order])
            self%order = actual_order
        end if

        call self%CT_e%zero()
        call self%KT_e%zero()
        self%R_e(:) = 0.0d0

        self%dt = controls%time%get_dt()
        call controls%time%get_time_coefficients(actual_order, self%coefficients)

        self%p_weight => mesh%get_weight_ptr() !&
        self%p_gauss  => mesh%get_gauss_ptr() !&
        self%p_conn   => mesh%get_connectivity_ptr() !&

        self%connectivity = mesh%get_connectivity()

    end subroutine initialize_workspace_thermal_assemble

    subroutine calc_gauss_points_values(self, mesh, iG)
        implicit none
        class(type_workspace_thermal_assemble), intent(inout) :: self
        class(abst_mesh), intent(in), pointer :: mesh
        integer(int32), intent(in) :: iG

        integer(int32) :: i

        self%detJ = mesh%jacobian_det(self%p_gauss(iG))

#ifdef USE_DEBUG
        if (self%detJ <= 1.0d-12) then
            cycle ! このガウス点の計算をスキップ
        end if
#endif

        do i = 1, self%num_nodes
            self%psi(i) = mesh%psi(i, self%p_gauss(iG))
            self%dpsi_dxi(i) = mesh%dpsi(i, 1, self%p_gauss(iG))
            self%dpsi_deta(i) = mesh%dpsi(i, 2, self%p_gauss(iG))
            self%dpsi_dx(i) = (mesh%jacobian(2, 2, self%p_gauss(iG)) * self%dpsi_dxi(i) &
                               - mesh%jacobian(2, 1, self%p_gauss(iG)) * self%dpsi_deta(i)) / self%detJ
            self%dpsi_dy(i) = (-mesh%jacobian(1, 2, self%p_gauss(iG)) * self%dpsi_dxi(i) &
                               + mesh%jacobian(1, 1, self%p_gauss(iG)) * self%dpsi_deta(i)) / self%detJ
        end do

        self%weight_detJ = self%p_weight(iG) * self%detJ

    end subroutine calc_gauss_points_values

    subroutine calc_history(self, temperature)
        implicit none
        class(type_workspace_thermal_assemble), intent(inout) :: self
        class(type_variable), intent(in) :: temperature

        integer(int32) :: i, j

        self%T_hist_e(:) = 0.0d0
        do i = 1, self%num_nodes
            do j = 1, self%order
                self%T_hist_e(i) = self%T_hist_e(i) + self%coefficients(j) * temperature%old(self%p_conn(i), j)
            end do
        end do

    end subroutine calc_history

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
        call deallocate_array(self%psi)
        call deallocate_array(self%dpsi_dxi)
        call deallocate_array(self%dpsi_deta)
        call deallocate_array(self%dpsi_dx)
        call deallocate_array(self%dpsi_dy)

        call deallocate_array(self%connectivity)

        self%type_id = -1
        self%num_nodes = 0
        self%num_gauss = 0
        self%order = 0

        self%p_conn => null()
        self%p_weight => null()
        self%p_gauss => null()
    end subroutine destroy_workspace_thermal_assemble

end module main_thermal_workspace
