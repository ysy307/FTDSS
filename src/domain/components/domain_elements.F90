module components_domain_elements
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: module_core
    use :: domain_fe_manager, only:type_fe_manager
    use :: domain_multicoloring, only:type_coloring
    implicit none
    private

    public :: type_elements_manager

    !>
    !> Manages all data related to volume elements in the domain.
    !>
    type :: type_elements_manager
        !> Number of elements in this subdomain.
        integer(int32) :: num_elements = 0
        !> Finite element type ID for each element.
        integer(int32), allocatable :: fe_types(:)
        !> Material ID for each element.
        integer(int32), allocatable :: fe_material_ids(:)
        !> Manager for FE type-specific operations.
        type(type_fe_manager) :: fe_manager
        !> Connectivity data for all elements.
        type(type_csr_index) :: connectivity
        !> Coloring information for parallel element processing.
        type(type_coloring) :: colors
    contains
        ! ---- Lifecycle ----
        ! initialize, destroy, reset, etc.
        procedure, public, pass(self) :: initialize => initialize_elements_manager
        procedure, public, pass(self) :: destroy => destroy_elements_manager

        ! ---- Mutator ----
        ! set_XXX, increment_XXX, update_XXX, etc.

        ! ---- Algorithm / Operation ----
        ! compute_XXX, check_XXX, solve_XXX, etc.

        ! ---- Inquiry ----
        ! is_XXX, has_XXX, should_XXX, etc.

        ! ---- Getter ----
        ! get_XXX, etc.

        ! ---- Meta / Utility ----
        ! display, to_string, etc.
        procedure, public, pass(self) :: display => display_elements_manager

        ! ---- Operator ----
    end type type_elements_manager

contains
    !> Initializes the element manager by reading and organizing element data.
    subroutine initialize_elements_manager(self, config_elements, config_multicoloring)
        implicit none
        class(type_elements_manager), intent(inout) :: self
        type(type_config_elements), intent(in) :: config_elements
        type(type_config_multicoloring), intent(in) :: config_multicoloring

        self%num_elements = config_elements%num_elements
        call allocate_array(self%fe_types, source=config_elements%fe_types)
        call allocate_array(self%fe_material_ids, source=config_elements%fe_material_ids)

        call self%fe_manager%initialize(config_elements%integration_order, self%num_elements, self%fe_types)
        call self%colors%initialize(config_multicoloring)
    end subroutine initialize_elements_manager

    !> Deallocates all memory associated with the element manager.
    subroutine destroy_elements_manager(self)
        implicit none
        class(type_elements_manager), intent(inout) :: self

        call self%connectivity%destroy()
        call self%colors%destroy()
        call self%fe_manager%destroy()
        call deallocate_array(self%fe_types)
        call deallocate_array(self%fe_material_ids)
        self%num_elements = 0
    end subroutine destroy_elements_manager

    subroutine display_elements_manager(self, unit_in)
        implicit none
        class(type_elements_manager), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit

        unit = optval(unit_in, default=output_unit)

        write (unit, '(A)') '### Elements Manager'
        write (unit, '(A)')
        write (unit, '(A, I0)') '  - **Number of Elements**: ', self%num_elements
        call self%connectivity%display(unit_in=unit, title='Volume Elements')
        write (unit, '(A)')
    end subroutine display_elements_manager

end module components_domain_elements
