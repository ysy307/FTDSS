module types_config_elements
    use, intrinsic :: iso_fortran_env
    use :: core_memory
    use :: core_constants, only:type_constant_id
    use :: core_types_topology_connectivity, only:type_csr_index
    use :: core_types_geometry_coordinate_array, only:type_coordinate_array_dp
    use :: core_types_config_base, only:abst_config
    implicit none
    private

    public :: type_config_elements
    public :: type_config_multicoloring
    public :: type_config_colored_elements

    type, extends(abst_config) :: type_config_colored_elements
        integer(int32) :: num_elements = 0
        integer(int32), allocatable :: elements(:)
    contains
        procedure, public, pass(self) :: copy => copy_config_colored_elements
        procedure, public, pass(self) :: reset => reset_config_colored_elements
    end type type_config_colored_elements

    type, extends(abst_config) :: type_config_multicoloring
        integer(int32) :: num_colors = 0
        type(type_config_colored_elements), allocatable :: colored(:)
    contains
        procedure, public, pass(self) :: copy => copy_config_multicoloring
        procedure, public, pass(self) :: reset => reset_config_multicoloring
    end type type_config_multicoloring

    type, extends(abst_config) :: type_config_elements
        integer(int32) :: num_elements = 0
        integer(int32), allocatable :: fe_types(:)
        integer(int32), allocatable :: fe_material_ids(:)
        integer(int32) :: integration_order
        type(type_csr_index) :: connectivity
        !> Mesh entity ID this element group corresponds to (used for BC remapping).
        integer(int32) :: entity_id = 0
    contains
        procedure, public, pass(self) :: copy => copy_config_elements
        procedure, public, pass(self) :: reset => reset_config_elements
    end type type_config_elements

contains

    subroutine copy_config_elements(self, source)
        implicit none
        class(type_config_elements), intent(inout) :: self
        class(abst_config), intent(in) :: source

        call self%reset()
        select type (source)
        type is (type_config_elements)
            call self%set(self%num_elements, source%num_elements)
            call self%set(self%fe_types, source%fe_types)
            call self%set(self%fe_material_ids, source%fe_material_ids)
            call self%set(self%integration_order, source%integration_order)
            call self%set(self%connectivity, source%connectivity)
            call self%set(self%entity_id, source%entity_id)
        end select
    end subroutine copy_config_elements

    subroutine reset_config_elements(self)
        implicit none
        class(type_config_elements), intent(inout) :: self

        self%num_elements = 0
        call deallocate_array(self%fe_types)
        call deallocate_array(self%fe_material_ids)
        self%integration_order = 0
        call self%connectivity%destroy()
        self%entity_id = 0
    end subroutine reset_config_elements

    subroutine copy_config_colored_elements(self, source)
        implicit none
        class(type_config_colored_elements), intent(inout) :: self
        class(abst_config), intent(in) :: source

        call self%reset()
        select type (source)
        type is (type_config_colored_elements)
            call self%set(self%num_elements, source%num_elements)
            call self%set(self%elements, source%elements)
        end select
    end subroutine copy_config_colored_elements

    subroutine reset_config_colored_elements(self)
        implicit none
        class(type_config_colored_elements), intent(inout) :: self

        self%num_elements = 0
        call deallocate_array(self%elements)
    end subroutine reset_config_colored_elements

    subroutine copy_config_multicoloring(self, source)
        implicit none
        class(type_config_multicoloring), intent(inout) :: self
        class(abst_config), intent(in) :: source
        integer(int32) :: i

        call self%reset()
        select type (source)
        type is (type_config_multicoloring)
            call self%set(self%num_colors, source%num_colors)
            if (allocated(source%colored)) then
                allocate (self%colored(source%num_colors))
                do i = 1, source%num_colors
                    call self%colored(i)%copy(source%colored(i))
                end do
            end if
        end select
    end subroutine copy_config_multicoloring

    subroutine reset_config_multicoloring(self)
        implicit none
        class(type_config_multicoloring), intent(inout) :: self
        integer(int32) :: i

        if (allocated(self%colored)) then
            do i = 1, size(self%colored)
                call self%colored(i)%reset()
            end do
            deallocate (self%colored)
        end if
        self%num_colors = 0
    end subroutine reset_config_multicoloring

end module types_config_elements
