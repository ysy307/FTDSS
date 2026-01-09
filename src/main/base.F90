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
        type(type_state), allocatable :: state(:)
        type(type_state), allocatable :: state_gp(:)
        real(real64), allocatable :: work_psi(:)
        real(real64), allocatable :: work_dpsi_dx(:, :)
        real(real64), allocatable :: work_vec(:)

        real(real64), allocatable :: work_C(:)
        real(real64), allocatable :: work_D(:, :, :)
        real(real64), allocatable :: work_V(:, :)
        real(real64), allocatable :: work_L(:)

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

        call allocate_array(self%work_psi, self%num_fe_nodes)
        call allocate_array(self%work_dpsi_dx, self%num_fe_dimension, self%num_fe_nodes)
        call allocate_array(self%work_vec, self%num_fe_dimension)

        call allocate_array(self%work_C, self%num_fe_gauss)
        call allocate_array(self%work_D, self%num_fe_dimension, self%num_fe_dimension, self%num_fe_gauss)
        call allocate_array(self%work_V, self%num_fe_dimension, self%num_fe_gauss)
        call allocate_array(self%work_L, self%num_fe_gauss)

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

        integer(int32) :: i
        real(real64) :: work(self%num_fe_nodes)
        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: gp => null()
        real(real64) :: lerped_value
        type(type_coordinate_dp) :: dlerped_value

        call self%fe%get_gauss(gp)

        ! Temperature and its gradient at Gauss points
        do i = 1, self%num_fe_nodes
            call self%state(i)%temperature%get(work(i))
        end do
        do i = 1, self%num_fe_gauss
            call self%fe%lerp(gp(i), work, lerped_value)
            call self%state_gp(i)%temperature%set(lerped_value)
        end do
        do i = 1, self%num_fe_gauss
            call self%fe%dlerp(gp(i), work, dlerped_value)
            call self%state_gp(i)%grad_T%set(dlerped_value)
        end do

        ! Pressure and its gradient at Gauss points
        do i = 1, self%num_fe_nodes
            call self%state(i)%pressure%get(work(i))
        end do
        do i = 1, self%num_fe_gauss
            call self%fe%lerp(gp(i), work, lerped_value)
            call self%state_gp(i)%pressure%set(lerped_value)
        end do
        do i = 1, self%num_fe_gauss
            call self%fe%dlerp(gp(i), work, dlerped_value)
            call self%state_gp(i)%grad_P%set(dlerped_value)
        end do

        ! Porosity at Gauss points
        do i = 1, self%num_fe_nodes
            call self%state(i)%porosity%get(work(i))
        end do
        do i = 1, self%num_fe_gauss
            call self%fe%lerp(gp(i), work, lerped_value)
            call self%state_gp(i)%porosity%set(lerped_value)
        end do

    end subroutine lerp_states

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
            call deallocate_array(self%work_psi)
            call deallocate_array(self%work_dpsi_dx)
            call deallocate_array(self%work_vec)

            call deallocate_array(self%work_C)
            call deallocate_array(self%work_D)
            call deallocate_array(self%work_V)
            call deallocate_array(self%work_L)

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
