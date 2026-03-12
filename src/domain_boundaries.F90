module domain_components_boundaries
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: module_core
    use :: domain_fe_manager, only:type_fe_manager
    implicit none
    private

    public :: type_boundaries_manager
    public :: type_boundary_patch

    !>
    !> Represents a single, unique boundary condition applied to a set of geometric entities.
    !>
    type :: type_boundary_patch
        !> The number of elements (sides) this boundary condition applies to.
        integer(int32) :: num_fe = 0
        !> Array of finite element type IDs for each element in this BC set.
        integer(int32), allocatable :: fe_types(:)
        !> Material ID for each element.
        integer(int32), allocatable :: fe_material_ids(:)
        !> Manager for FE type-specific operations (shape functions, etc.).
        type(type_fe_manager) :: fe_manager
        !> Connectivity data for the elements in this BC set.
        type(type_csr_index) :: connectivity
        !> Mesh entity ID this patch corresponds to (used to look up the BC strategy).
        integer(int32) :: entity_id = 0
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_type_boundary_patch
        procedure, public, pass(self) :: destroy => destroy_type_boundary_patch
        ! ---- Meta / Utility ----
        procedure, public, pass(self) :: display => display_boundary_patch
    end type type_boundary_patch

    !>
    !> Top-level manager for all boundary conditions across all physics types.
    !>
    type :: type_boundaries_manager
        !> The number of unique boundary conditions for this physics.
        integer(int32) :: num_bcs = 0
        !> Array of unique boundary condition sets.
        type(type_boundary_patch), allocatable :: bcs(:)
    contains
        ! ---- Lifecycle ----
        ! initialize, destroy, reset, etc.
        procedure, public, pass(self) :: initialize => initialize_boundary_manager
        procedure, public, pass(self) :: destroy => destroy_boundary_manager

        ! ---- Mutator ----
        ! set_XXX, increment_XXX, update_XXX, etc.

        ! ---- Algorithm / Operation ----
        ! compute_XXX, check_XXX, solve_XXX, etc.

        ! ---- Inquiry ----
        ! is_XXX, has_XXX, should_XXX, etc.

        ! ---- Getter ----
        procedure, public, pass(self) :: get_bc_patch => get_bc_patch_boundary_manager
        procedure, public, pass(self) :: get_num_bcs => get_num_bcs_boundary_manager

        ! ---- Meta / Utility ----
        ! display, to_string, etc.
        procedure, public, pass(self) :: display => display_boundary_manager
    end type type_boundaries_manager
contains

    !> Initialize a boundary patch with the given element data and FE manager
    subroutine initialize_type_boundary_patch(self, config_elements)
        implicit none
        class(type_boundary_patch), intent(inout) :: self
        type(type_config_elements), intent(in) :: config_elements

        self%num_fe = config_elements%num_elements
        self%entity_id = config_elements%entity_id
        call allocate_array(self%fe_types, source=config_elements%fe_types)
        call allocate_array(self%fe_material_ids, source=config_elements%fe_material_ids)

        if (allocated(config_elements%connectivity%row_ptr) .and. &
            allocated(config_elements%connectivity%col_ind)) then
            call self%connectivity%initialize(size(config_elements%connectivity%row_ptr), &
                                              size(config_elements%connectivity%col_ind))
            self%connectivity%row_ptr = config_elements%connectivity%row_ptr
            self%connectivity%col_ind = config_elements%connectivity%col_ind
        end if

        call self%fe_manager%initialize(config_elements%integration_order, self%num_fe, self%fe_types)

    end subroutine initialize_type_boundary_patch

    !> Destroy all data associated with this boundary patch and deallocate memory
    subroutine destroy_type_boundary_patch(self)
        implicit none
        class(type_boundary_patch), intent(inout) :: self

        call self%connectivity%destroy()
        call self%fe_manager%destroy()
        call deallocate_array(self%fe_types)
        call deallocate_array(self%fe_material_ids)
        self%num_fe = 0

    end subroutine destroy_type_boundary_patch

    subroutine display_boundary_patch(self, unit_in)
        implicit none
        class(type_boundary_patch), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit

        unit = optval(unit_in, output_unit)

        write (unit, *) "Boundary Patch:"
        write (unit, *) "  Number of Elements: ", self%num_fe
        write (unit, *) "  FE Types: ", self%fe_types
        write (unit, *) "  FE Material IDs: ", self%fe_material_ids
    end subroutine display_boundary_patch

    !> Initialize the boundary manager with input data and FE manager
    subroutine initialize_boundary_manager(self, config_elements)
        implicit none
        class(type_boundaries_manager), intent(inout) :: self
        type(type_config_elements), intent(in) :: config_elements(:)

        integer(int32) :: i

        self%num_bcs = size(config_elements)
        allocate (self%bcs(self%num_bcs))

        do i = 1, self%num_bcs
            call self%bcs(i)%initialize(config_elements(i))
        end do

    end subroutine initialize_boundary_manager

    !> Destroy all boundary condition data and deallocate memory
    subroutine destroy_boundary_manager(self)
        implicit none
        class(type_boundaries_manager), intent(inout) :: self

        integer(int32) :: i

        ! Destroy each boundary patch
        do i = 1, self%num_bcs
            call self%bcs(i)%destroy()
        end do

        ! Deallocate the boundary patches array
        deallocate (self%bcs)
        self%num_bcs = 0
    end subroutine destroy_boundary_manager

    !> Get the boundary patch for a given physics type and BC ID
    subroutine get_bc_patch_boundary_manager(self, bc_id, bc_patch)
        implicit none
        class(type_boundaries_manager), intent(in), target :: self
        integer(int32), intent(in) :: bc_id
        type(type_boundary_patch), intent(inout), pointer :: bc_patch

        if (value_in_range(bc_id, 1, self%num_bcs)) then
            bc_patch => self%bcs(bc_id)
        else
            error stop "Invalid BC ID in get_bc_patch_boundary_manager"
        end if
    end subroutine get_bc_patch_boundary_manager

    pure subroutine get_num_bcs_boundary_manager(self, num_bcs)
        implicit none
        class(type_boundaries_manager), intent(in) :: self
        integer(int32), intent(inout) :: num_bcs

        num_bcs = self%num_bcs
    end subroutine get_num_bcs_boundary_manager

    subroutine display_boundary_manager(self, unit_in)
        implicit none
        class(type_boundaries_manager), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit
        integer(int32) :: i

        unit = optval(unit_in, default=output_unit)

        write (unit, '(A)') '### Boundaries Manager'
        write (unit, '(A)')
        write (unit, '(A, I0)') '  - **Number of BC Sets**: ', self%num_bcs
        do i = 1, self%num_bcs
            call self%bcs(i)%display(unit_in=unit)
            write (unit, '(A)')
        end do
    end subroutine display_boundary_manager

end module domain_components_boundaries
