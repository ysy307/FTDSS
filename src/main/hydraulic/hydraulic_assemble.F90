! module hydraulic_hydraulic_assemble
! !$  use omp_lib
!     use, intrinsic :: iso_fortran_env, only: int32, real64
!     use :: module_core, only:type_state, type_coordinate_dp, assignment(=), type_variable, allocate_array, &
!         deallocate_array, type_crs, type_dense
!     use :: module_domain, only:type_domain, abst_mesh
!     use :: module_properties, only:type_properties_manager
!     use :: module_field
!     use :: module_control

!     implicit none
!     private

!     public :: abst_assemble_global_hydraulic

!     public :: hydraulic_assemble_system_linear_1, hydraulic_assemble_system_linear_1_parallel

!     abstract interface
!         subroutine abst_assemble_global_hydraulic(J, R, mesh, pressure, temperature, ice, porosity, &
!                                                   properties, controls, actual_order)
!             import :: type_crs, type_domain, type_properties_manager, type_variable, type_controls, int32, real64, type_jacobian_matrix, type_residual_vector, abst_mesh
!             implicit none
!             type(type_jacobian_matrix), intent(inout) :: J
!             type(type_residual_vector), intent(inout) :: R
!             class(abst_mesh), intent(in), pointer :: mesh
!             type(type_variable), intent(in) :: pressure
!             type(type_variable), intent(in) :: temperature
!             type(type_variable), intent(in) :: ice
!             type(type_variable), intent(in) :: porosity
!             type(type_properties_manager), intent(in) :: properties
!             type(type_controls), intent(in) :: controls
!             integer(int32), intent(in) :: actual_order
!         end subroutine abst_assemble_global_hydraulic
!     end interface
! contains

!     subroutine process_element_hydraulic_linear_1(J, R, mesh, pressure, temperature, ice, porosity, &
!                                                   properties, controls, actual_order)
!         implicit none
!         ! --- arguments ---
!         type(type_jacobian_matrix), intent(inout) :: J
!         type(type_residual_vector), intent(inout) :: R
!         class(abst_mesh), intent(in), pointer :: mesh
!         type(type_variable), intent(in) :: pressure
!         type(type_variable), intent(in) :: temperature
!         type(type_variable), intent(in) :: ice
!         type(type_variable), intent(in) :: porosity
!         type(type_properties_manager), intent(in) :: properties
!         type(type_controls), intent(in) :: controls
!         integer(int32), intent(in) :: actual_order

!         ! --- Local variables ---
!         integer(int32) :: index, num_nodes, num_gauss, material_id, il, jl, iG, iO
!         real(real64) :: weight, detJ
!         real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j
!         real(real64) :: val, zeta
!         real(real64) :: dt

!         ! --- Workspace variables ---
!         type(type_dense) :: CH_e
!         type(type_dense) :: KH_e
!         type(type_dense) :: J_e
!         real(real64), allocatable :: R_e(:)

!         ! --- Physical quantities at Gauss points ---
!         type(type_state), allocatable :: state(:)
!         real(real64), dimension(:), pointer :: p_weight => null()
!         type(type_coordinate_dp), dimension(:), pointer :: p_gauss => null()
!         real(real64), allocatable :: kflh(:)
!         real(real64), allocatable :: dot_ice(:)
!         integer(int32), dimension(:), pointer :: p_conn => null()

!         !---------------------------------------------------------------------------------------------------------------------------
!         ! STEP 0: Initialize and obtain sizes
!         !---------------------------------------------------------------------------------------------------------------------------
!         num_nodes   = mesh%get_num_nodes() !&
!         num_gauss   = mesh%get_num_gauss() !&
!         material_id = mesh%get_group() !&
!         if (.not. controls%is_target(calc_hydraulic, material_id)) return

!         call CH_e%initialize(num_nodes)
!         call KH_e%initialize(num_nodes)
!         call J_e%initialize(num_nodes)
!         call allocate_array(R_e, num_nodes)
!         allocate (state(num_gauss))
!         call allocate_array(kflh, num_gauss)
!         call allocate_array(dot_ice, num_nodes)

!         dt = controls%time%get_dt()
!         p_weight => mesh%get_weight_ptr()
!         p_gauss => mesh%get_gauss_ptr()
!         p_conn => mesh%get_connectivity_ptr()

!         !---------------------------------------------------------------------------------------------------------------------------
!         ! STEP 1: Compute the physical quantities at all Gauss points
!         !---------------------------------------------------------------------------------------------------------------------------
!         do iG = 1, num_gauss
!             state(iG)%temperature = mesh%lerp(p_gauss(iG), temperature%pre) !&
!             state(iG)%pressure    = mesh%lerp(p_gauss(iG), pressure%pre) !&
!             state(iG)%porosity    = mesh%lerp(p_gauss(iG), porosity%pre) !&
!         end do
!         call properties%calc_hydraulic(material_id, state, kflh)

!         do il = 1, num_nodes
!             dot_ice(il) = ice%dif(p_conn(il)) / dt
!         end do

!         !---------------------------------------------------------------------------------------------------------------------------
!         ! STEP 2: Compute the mesh matrices CH_e and KH_e
!         !---------------------------------------------------------------------------------------------------------------------------
!         do iG = 1, num_gauss
!             weight = p_weight(iG) !&
!             detJ   = mesh%jacobian_det(p_gauss(iG)) !&
!             zeta   = state(iG)%density_ice / state(iG)%density_water - 1.0d0 !&
!             do il = 1, num_nodes
!                 dNdx_i = ( mesh%jacobian(2, 2, p_gauss(iG)) * mesh%dpsi(il, 1, p_gauss(iG)) - & !&
!                            mesh%jacobian(2, 1, p_gauss(iG)) * mesh%dpsi(il, 2, p_gauss(iG))) / detJ !&
!                 dNdy_i = (-mesh%jacobian(1, 2, p_gauss(iG)) * mesh%dpsi(il, 1, p_gauss(iG)) + & !&
!                            mesh%jacobian(1, 1, p_gauss(iG)) * mesh%dpsi(il, 2, p_gauss(iG))) / detJ !&
!                 do jl = 1, num_nodes
!                     dNdx_j = ( mesh%jacobian(2, 2, p_gauss(iG)) * mesh%dpsi(jl, 1, p_gauss(iG)) - & !&
!                                mesh%jacobian(2, 1, p_gauss(iG)) * mesh%dpsi(jl, 2, p_gauss(iG))) / detJ !&
!                     dNdy_j = (-mesh%jacobian(1, 2, p_gauss(iG)) * mesh%dpsi(jl, 1, p_gauss(iG)) + & !&
!                                mesh%jacobian(1, 1, p_gauss(iG)) * mesh%dpsi(jl, 2, p_gauss(iG))) / detJ !&

!                     val = mesh%psi(il, p_gauss(iG)) * mesh%psi(jl, p_gauss(iG)) * zeta * weight * detJ
!                     call CH_e%add(il, jl, val)
!                     val = (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * kflh(iG) * weight * detJ
!                     call KH_e%add(il, jl, val)
!                 end do
!             end do
!         end do

!         !---------------------------------------------------------------------------------------------------------------------------
!         ! STEP 3: Build the final local matrix (J_e) and vector (R_e)
!         !---------------------------------------------------------------------------------------------------------------------------
!         call KH_e%add(0.0d0, CH_e, J_e)
!         call CH_e%gemv(-1.0d0, dot_ice, 0.0d0, R_e)

!         !---------------------------------------------------------------------------------------------------------------------------
!         ! STEP 4: Assemble the global matrix and vector
!         !---------------------------------------------------------------------------------------------------------------------------
!         call J%add(p_conn, J_e)
!         call R%add(p_conn, R_e)

!         !---------------------------------------------------------------------------------------------------------------------------
!         ! STEP 5: Finalization
!         !---------------------------------------------------------------------------------------------------------------------------
!         call deallocate_array(kflh)
!         call deallocate_array(R_e)
!         call J_e%destroy()
!         call KH_e%destroy()
!         call CH_e%destroy()
!         deallocate (state)

!     end subroutine process_element_hydraulic_linear_1

!     subroutine hydraulic_assemble_system_linear_1(J, R, domain, pressure, temperature, porosity, ice, &
!                                                   properties, controls, actual_order)
!         implicit none
!         type(type_jacobian_matrix), intent(inout) :: J
!         type(type_residual_vector), intent(inout) :: R
!         type(type_domain), intent(inout), target :: domain
!         type(type_variable), intent(in) :: pressure
!         type(type_variable), intent(in) :: temperature
!         type(type_variable), intent(in) :: ice
!         type(type_variable), intent(in) :: porosity
!         type(type_properties_manager), intent(in) :: properties
!         type(type_controls), intent(in) :: controls
!         integer(int32), intent(in) :: actual_order

!         class(abst_mesh), pointer :: element
!         integer(int32) :: iE, num_elements

!         num_elements = domain%get_num_elements()
!         call J%zero()
!         call R%zero()

!         do iE = 1, num_elements
!             element => domain%elements(iE)%e
!             call process_element_hydraulic_linear_1(J, R, element, pressure, temperature, porosity, ice, &
!                                                     properties, controls, actual_order)
!         end do
!     end subroutine hydraulic_assemble_system_linear_1

!     subroutine hydraulic_assemble_system_linear_1_parallel(J, R, domain, pressure, temperature, porosity, ice, &
!                                                            properties, controls, actual_order)
!         implicit none
!         type(type_jacobian_matrix), intent(inout) :: J
!         type(type_residual_vector), intent(inout) :: R
!         type(type_domain), intent(inout), target :: domain
!         type(type_variable), intent(in) :: pressure
!         type(type_variable), intent(in) :: temperature
!         type(type_variable), intent(in) :: ice
!         type(type_variable), intent(in) :: porosity
!         type(type_properties_manager), intent(in) :: properties
!         type(type_controls), intent(in) :: controls
!         integer(int32), intent(in) :: actual_order

!         integer(int32) :: c, ie_idx
!         class(abst_mesh), pointer :: element

!         call J%zero()
!         call R%zero()

!         !$omp parallel private(c, ie_idx, element)
!         do c = 1, domain%colors%num_colors
!             !$omp do
!             do ie_idx = 1, domain%colors%colored(c)%num_elements
!                 element => domain%elements(domain%colors%colored(c)%elements(ie_idx))%e
!                 call process_element_hydraulic_linear_1(J, R, element, pressure, temperature, porosity, ice, &
!                                                         properties, controls, actual_order)
!             end do
!             !$omp end do
!         end do
!         !$omp end parallel
!     end subroutine hydraulic_assemble_system_linear_1_parallel
! end module hydraulic_hydraulic_assemble

