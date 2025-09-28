module main_thermal_assemble
    use, intrinsic :: iso_fortran_env, only: int32, real64
!$  use omp_lib
    use :: module_core, only:type_state, type_coordinate_dp, assignment(=), type_variable, allocate_array, deallocate_array, type_crs, type_dense
    use :: module_domain, only:type_domain, abst_mesh
    use :: module_field, only:type_jacobian_matrix, type_residual_vector
    use :: module_properties, only:type_properties_manager
    use :: module_control
    use :: main_thermal_workspace, only:type_workspace_thermal_assemble

    implicit none
    private

    public :: abst_assemble_global_thermal
    public :: thermal_assemble_system_linear_1, thermal_assemble_system_linear_1_parallel

    ! --- ワークスペース型 ---

    abstract interface
        subroutine abst_assemble_global_thermal(J, R, domain, temperature, porosity, properties, controls, actual_order)
            import :: type_jacobian_matrix, type_residual_vector, type_domain, type_properties_manager, type_variable, type_controls, int32, real64
            implicit none
            type(type_jacobian_matrix), intent(inout) :: J
            type(type_residual_vector), intent(inout) :: R
            type(type_domain), intent(inout), target :: domain
            type(type_variable), intent(in) :: temperature, porosity
            type(type_properties_manager), intent(in) :: properties
            type(type_controls), intent(in) :: controls
            integer(int32), intent(in) :: actual_order
        end subroutine abst_assemble_global_thermal
    end interface

contains

    subroutine process_mesh_thermal_linear_1(J, R, mesh, temperature, porosity, properties, controls, actual_order, workspace)
        implicit none
        type(type_jacobian_matrix), intent(inout) :: J
        type(type_residual_vector), intent(inout) :: R
        class(abst_mesh), intent(in), pointer :: mesh
        type(type_variable), intent(in) :: temperature, porosity
        type(type_properties_manager), intent(in) :: properties
        type(type_controls), intent(in) :: controls
        integer(int32), intent(in) :: actual_order
        type(type_workspace_thermal_assemble), intent(inout) :: workspace

        integer(int32) :: i_material
        integer(int32) :: il, jl, iG, iO, i
        real(real64) :: val

        i_material = mesh%get_group()
        if (.not. controls%is_target(calc_thermal, i_material)) return

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 1: Compute the physical quantities
        !---------------------------------------------------------------------------------------------------------------------------
        call workspace%initialize(mesh, controls, actual_order)
        do iG = 1, workspace%num_gauss
            workspace%state(iG)%temperature = mesh%lerp(workspace%p_gauss(iG), temperature%pre)
            workspace%state(iG)%porosity = mesh%lerp(workspace%p_gauss(iG), porosity%pre)
        end do
        call properties%calc_thermal(i_material, workspace%state, workspace%lambda, workspace%Ca)
        call workspace%calc_history(temperature)

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 2: Compute the element matrices CT_e and KT_e
        !---------------------------------------------------------------------------------------------------------------------------
        do iG = 1, workspace%num_gauss
            call workspace%calc_gauss(mesh, iG)
            do il = 1, workspace%num_nodes
                do jl = 1, workspace%num_nodes
                    val = workspace%psi(il) * workspace%psi(jl) * workspace%Ca(iG) * workspace%weight_detJ
                    call workspace%CT_e%add(il, jl, val)
                    val = (workspace%dpsi_dx(il) * workspace%dpsi_dx(jl) + workspace%dpsi_dy(il) * workspace%dpsi_dy(jl)) &
                          * workspace%lambda(iG) * workspace%weight_detJ
                    call workspace%KT_e%add(il, jl, val)
                end do
            end do
        end do

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 3: Build the final local matrix (J_e) and vector (R_e)
        !---------------------------------------------------------------------------------------------------------------------------
        call workspace%CT_e%add(workspace%coefficients(0) / workspace%dt, workspace%KT_e, workspace%J_e)
        call workspace%CT_e%gemv(-1.0d0 / workspace%dt, workspace%T_hist_e, 0.0d0, workspace%R_e)

        !---------------------------------------------------------------------------------------------------------------------------
        ! STEP 4: Assemble the global matrix and vector
        !---------------------------------------------------------------------------------------------------------------------------
        call J%add(workspace%connectivity, workspace%J_e)
        call R%add(workspace%connectivity, workspace%R_e)

    end subroutine process_mesh_thermal_linear_1

    subroutine thermal_assemble_system_linear_1(J, R, domain, temperature, porosity, properties, controls, actual_order)
        implicit none
        type(type_jacobian_matrix), intent(inout) :: J
        type(type_residual_vector), intent(inout) :: R
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
        call R%zero()

        do iE = 1, num_elements
            mesh => domain%elements(iE)%e
            call process_mesh_thermal_linear_1(J, R, mesh, temperature, porosity, properties, controls, actual_order, workspace)
        end do

        call workspace%destroy()
    end subroutine thermal_assemble_system_linear_1

    subroutine thermal_assemble_system_linear_1_parallel(J, R, domain, temperature, porosity, properties, controls, actual_order)
        implicit none
        type(type_jacobian_matrix), intent(inout) :: J
        type(type_residual_vector), intent(inout) :: R
        type(type_domain), intent(inout), target :: domain
        type(type_variable), intent(in) :: temperature, porosity
        type(type_properties_manager), intent(in) :: properties
        type(type_controls), intent(in) :: controls
        integer(int32), intent(in) :: actual_order
        integer(int32) :: c, ie_idx
        class(abst_mesh), pointer :: mesh
        type(type_workspace_thermal_assemble) :: workspace

        call J%zero()
        call R%zero()

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

end module main_thermal_assemble
