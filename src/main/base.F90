module main_base
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_control
    use :: module_domain
    implicit none
    private

    public :: type_assemble_workspace

    type :: type_assemble_workspace
        logical, private :: is_initialized = .false.
        integer(int32), private :: fe_type = -1
        integer(int32) :: num_fe_nodes = -1
        integer(int32) :: num_fe_gauss = -1
        integer(int32) :: num_fe_dimension = -1
        class(abst_fe), pointer :: fe => null()
        real(real64), allocatable :: coordinates(:, :)
        type(type_state), allocatable :: state(:)
        type(type_state), allocatable :: state_gp(:)
        real(real64), allocatable :: T_node(:)
        real(real64), allocatable :: P_node(:)
        real(real64), allocatable :: phi_node(:)
        real(real64), allocatable :: work_node(:, :)
        real(real64), allocatable :: work_psi(:)
        real(real64), allocatable :: work_dpsi_dx(:, :)
        real(real64), allocatable :: work_vec(:)

        real(real64), allocatable :: work_C(:)
        real(real64), allocatable :: work_D(:, :, :)
        real(real64), allocatable :: work_V(:, :)
        real(real64), allocatable :: work_L(:)
        real(real64), allocatable :: work_d_dt(:)
        real(real64), allocatable :: work_matrix(:, :)

        integer(int32) :: material_id
        integer(int32) :: element_id

        logical, private :: associated_bdf = .false.
        integer(int32) :: bdf_order = -1
        real(real64), pointer, contiguous, dimension(:) :: bdf_coeffs => null()
    contains
        procedure, public, pass(self) :: initialize => initialize_type_assemble_workspace
        procedure, public, pass(self) :: destroy => destroy_type_assemble_workspace

        procedure, private, pass(self) :: set_basic => set_basic
        procedure, private, pass(self) :: set_bdf_info => set_bdf_info

        procedure, public, pass(self) :: lerp => lerp_states

        procedure, public, pass(self) :: compute_K1 => compute_K1_assemble_workspace
        procedure, private, pass(self) :: compute_K2_scalar_assemble_workspace
        procedure, private, pass(self) :: compute_K2_assemble_workspace
        generic :: compute_K2 => compute_K2_scalar_assemble_workspace, compute_K2_assemble_workspace
        procedure, public, pass(self) :: compute_K3 => compute_K3_assemble_workspace
        procedure, public, pass(self) :: compute_R1 => compute_R1_assemble_workspace
        procedure, public, pass(self) :: compute_R2 => compute_R2_assemble_workspace
    end type type_assemble_workspace

contains
    subroutine initialize_type_assemble_workspace(self, fe, target_material_id, target_element_id, controls)
        implicit none
        class(type_assemble_workspace), intent(inout) :: self
        class(abst_fe), intent(in), target :: fe
        integer(int32), intent(in) :: target_material_id
        integer(int32), intent(in) :: target_element_id
        type(type_controls), intent(in) :: controls

        integer(int32) :: fe_type

        if (.not. self%is_initialized) then
            self%fe => fe
            call self%set_basic()

            self%is_initialized = .true.
        else
            call fe%get_type(fe_type)
            if (fe_type /= self%fe_type) then
                call self%destroy()
                self%fe => fe
                call self%set_basic()
            end if
        end if

        self%material_id = target_material_id
        self%element_id = target_element_id

        if (.not. self%associated_bdf) then
            call self%set_bdf_info(controls)
        end if

    end subroutine initialize_type_assemble_workspace

    subroutine set_basic(self)
        implicit none
        class(type_assemble_workspace), intent(inout) :: self

        integer(int32) :: i

        call self%fe%get_type(self%fe_type)
        call self%fe%get_num_nodes(self%num_fe_nodes)
        call self%fe%get_num_gauss(self%num_fe_gauss)
        call self%fe%get_dimension(self%num_fe_dimension)

        allocate (self%state(self%num_fe_nodes))
        do i = 1, self%num_fe_nodes
            call self%state(i)%reset()
        end do
        allocate (self%state_gp(self%num_fe_gauss))
        do i = 1, self%num_fe_gauss
            call self%state_gp(i)%reset()
        end do

        call allocate_array(self%T_node, self%num_fe_nodes)
        call allocate_array(self%P_node, self%num_fe_nodes)
        call allocate_array(self%phi_node, self%num_fe_nodes)
        call allocate_array(self%work_node, self%bdf_order + 1, self%num_fe_nodes)

        call allocate_array(self%work_psi, self%num_fe_nodes)
        call allocate_array(self%work_dpsi_dx, self%num_fe_dimension, self%num_fe_nodes)
        call allocate_array(self%work_vec, self%num_fe_dimension)

        call allocate_array(self%work_C, self%num_fe_gauss)
        call allocate_array(self%work_D, self%num_fe_dimension, self%num_fe_dimension, self%num_fe_gauss)
        call allocate_array(self%work_V, self%num_fe_dimension, self%num_fe_gauss)
        call allocate_array(self%work_L, self%num_fe_gauss)
        call allocate_array(self%work_d_dt, self%num_fe_gauss)
        call allocate_array(self%work_matrix, self%num_fe_nodes, self%num_fe_nodes)

    end subroutine set_basic

    subroutine set_bdf_info(self, controls)
        implicit none
        class(type_assemble_workspace), intent(inout) :: self
        type(type_controls), intent(in) :: controls

        call controls%time%get_bdf_order(self%bdf_order)
        call controls%time%get_bdf_coeffs(self%bdf_coeffs)

        self%associated_bdf = .true.
    end subroutine set_bdf_info

    subroutine lerp_states(self)
        implicit none
        class(type_assemble_workspace), intent(inout) :: self

        integer(int32) :: i, j
        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: gp => null()
        real(real64) :: lerped_value
        type(type_coordinate_dp) :: dlerped_value
        real(real64), allocatable :: work(:)

        call self%fe%get_gauss(gp)

        ! Temperature and its gradient at Gauss points
        self%work_node(:, :) = 0.0d0
        do i = 1, self%num_fe_nodes
            call self%state(i)%get(temperature=self%T_node(i), &
                                   temperature_history=work)
            self%work_node(1:self%bdf_order + 1, i) = work(1:self%bdf_order + 1)
        end do
        do i = 1, self%num_fe_gauss
            call self%fe%lerp(gp(i), self%T_node, lerped_value)
            call self%state_gp(i)%temperature%set(lerped_value)
        end do
        do i = 1, self%bdf_order + 1
            do j = 1, self%num_fe_gauss
                call self%fe%lerp(gp(j), self%work_node(i, :), lerped_value)
                call self%state_gp(j)%temperature_history%set(i, lerped_value)
            end do
        end do
        do i = 1, self%num_fe_gauss
            call self%fe%dlerp(gp(i), self%T_node, dlerped_value)
            call self%state_gp(i)%grad_T%set(dlerped_value)
        end do

        ! Pressure and its gradient at Gauss points
        self%work_node(:, :) = 0.0d0
        do i = 1, self%num_fe_nodes
            call self%state(i)%get(pressure=self%P_node(i), &
                                   pressure_history=work)
            self%work_node(1:self%bdf_order + 1, i) = work(1:self%bdf_order + 1)
        end do
        do i = 1, self%num_fe_gauss
            call self%fe%lerp(gp(i), self%P_node, lerped_value)
            call self%state_gp(i)%pressure%set(lerped_value)
        end do
        do i = 1, self%bdf_order + 1
            do j = 1, self%num_fe_gauss
                call self%fe%lerp(gp(j), self%work_node(i, :), lerped_value)
                call self%state_gp(j)%pressure_history%set(i, lerped_value)
            end do
        end do
        do i = 1, self%num_fe_gauss
            call self%fe%dlerp(gp(i), self%P_node, dlerped_value)
            call self%state_gp(i)%grad_P%set(dlerped_value)
        end do

        ! Porosity at Gauss points
        self%work_node(:, :) = 0.0d0
        do i = 1, self%num_fe_nodes
            call self%state(i)%get(porosity=self%phi_node(i), &
                                   porosity_history=work)
            self%work_node(1:self%bdf_order + 1, i) = work(1:self%bdf_order + 1)
        end do
        do i = 1, self%num_fe_gauss
            call self%fe%lerp(gp(i), self%phi_node, lerped_value)
            call self%state_gp(i)%porosity%set(lerped_value)
        end do
        do i = 1, self%bdf_order + 1
            do j = 1, self%num_fe_gauss
                call self%fe%lerp(gp(j), self%work_node(i, :), lerped_value)
                call self%state_gp(j)%porosity_history%set(i, lerped_value)
            end do
        end do

    end subroutine lerp_states

    subroutine compute_K1_assemble_workspace(self, A_gp, local_matrix)
        implicit none
        class(type_assemble_workspace), intent(inout) :: self
        real(real64), intent(in) :: A_gp(:)
        real(real64), intent(inout) :: local_matrix(:, :)

        local_matrix(:, :) = 0.0d0

        self%work_psi(:) = 0.0d0
        call self%fe%compute_K1(self%coordinates, A_gp, local_matrix, self%work_psi)
    end subroutine compute_K1_assemble_workspace

    subroutine compute_K2_scalar_assemble_workspace(self, D_gp, local_matrix)
        implicit none
        class(type_assemble_workspace), intent(inout) :: self
        real(real64), intent(in) :: D_gp(:)
        real(real64), intent(inout) :: local_matrix(:, :)

        local_matrix(:, :) = 0.0d0
        self%work_psi(:) = 0.0d0
        self%work_dpsi_dx(:, :) = 0.0d0
        call self%fe%compute_K2(self%coordinates, D_gp, local_matrix, self%work_psi, self%work_dpsi_dx)
    end subroutine compute_K2_scalar_assemble_workspace

    subroutine compute_K2_assemble_workspace(self, D_gp, local_matrix)
        implicit none
        class(type_assemble_workspace), intent(inout) :: self
        real(real64), intent(in) :: D_gp(:, :, :)
        real(real64), intent(inout) :: local_matrix(:, :)

        local_matrix(:, :) = 0.0d0
        self%work_psi(:) = 0.0d0
        self%work_dpsi_dx(:, :) = 0.0d0
        self%work_vec(:) = 0.0d0
        call self%fe%compute_K2(self%coordinates, D_gp, local_matrix, self%work_psi, self%work_dpsi_dx, self%work_vec)
    end subroutine compute_K2_assemble_workspace

    subroutine compute_K3_assemble_workspace(self, V_gp, local_matrix)
        implicit none
        class(type_assemble_workspace), intent(inout) :: self
        real(real64), intent(in) :: V_gp(:, :)
        real(real64), intent(inout) :: local_matrix(:, :)

        local_matrix(:, :) = 0.0d0
        self%work_psi(:) = 0.0d0
        self%work_dpsi_dx(:, :) = 0.0d0
        call self%fe%compute_K3(self%coordinates, V_gp, local_matrix, self%work_psi, self%work_dpsi_dx)
    end subroutine compute_K3_assemble_workspace

    subroutine compute_R1_assemble_workspace(self, L_gp, local_vector)
        implicit none
        class(type_assemble_workspace), intent(inout) :: self
        real(real64), intent(in) :: L_gp(:)
        real(real64), intent(inout) :: local_vector(:)

        local_vector(:) = 0.0d0

        self%work_psi(:) = 0.0d0
        call self%fe%compute_R1(self%coordinates, L_gp, local_vector, self%work_psi)
    end subroutine compute_R1_assemble_workspace

    subroutine compute_R2_assemble_workspace(self, V_gp, local_vector)
        implicit none
        class(type_assemble_workspace), intent(inout) :: self
        real(real64), intent(in) :: V_gp(:, :)
        real(real64), intent(inout) :: local_vector(:)

        local_vector(:) = 0.0d0

        self%work_psi(:) = 0.0d0
        self%work_dpsi_dx(:, :) = 0.0d0
        call self%fe%compute_R2(self%coordinates, V_gp, local_vector, self%work_psi, self%work_dpsi_dx)
    end subroutine compute_R2_assemble_workspace

    subroutine destroy_type_assemble_workspace(self)
        implicit none
        class(type_assemble_workspace), intent(inout) :: self

        if (self%is_initialized) then
            self%fe_type = -1
            self%num_fe_nodes = -1
            self%num_fe_gauss = -1
            self%num_fe_dimension = -1
            if (allocated(self%state)) then
                deallocate (self%state)
            end if
            if (allocated(self%state_gp)) then
                deallocate (self%state_gp)
            end if
            call deallocate_array(self%T_node)
            call deallocate_array(self%P_node)
            call deallocate_array(self%phi_node)
            call deallocate_array(self%work_node)
            call deallocate_array(self%work_psi)
            call deallocate_array(self%work_dpsi_dx)
            call deallocate_array(self%work_vec)

            call deallocate_array(self%work_C)
            call deallocate_array(self%work_D)
            call deallocate_array(self%work_V)
            call deallocate_array(self%work_L)
            call deallocate_array(self%work_d_dt)
            call deallocate_array(self%work_matrix)

            nullify (self%fe)
            self%is_initialized = .false.

            self%material_id = -1
            self%element_id = -1

            self%associated_bdf = .false.
            self%bdf_order = -1
            nullify (self%bdf_coeffs)
        end if

    end subroutine destroy_type_assemble_workspace

end module main_base
