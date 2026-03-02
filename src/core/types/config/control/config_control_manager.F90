module types_config_control_manager
    use, intrinsic :: iso_fortran_env
    use :: core_memory, only:deallocate_array
    use :: core_constants, only:type_constant_id, type_constant_value
    use :: types_config_base, only:abst_config
    implicit none
    private

    public :: type_config_control_manager

    type, extends(abst_config) :: type_config_control_manager
        integer(int32), allocatable :: unique_material_ids(:)
        logical, allocatable :: compute_active(:)
        logical, allocatable :: physics_active(:, :)
        type(type_constant_id) :: coupling_mode = type_constant_id("", "", -1)
    contains
        procedure, public, pass(self) :: copy => copy_config_control_manager
        procedure, public, pass(self) :: reset => reset_config_control_manager
    end type type_config_control_manager

contains
    subroutine copy_config_control_manager(self, source)
        implicit none
        class(type_config_control_manager), intent(inout) :: self
        class(abst_config), intent(in) :: source

        call self%reset()

        select type (source)
        type is (type_config_control_manager)
            call self%set(self%unique_material_ids, source%unique_material_ids)
            call self%set(self%compute_active, source%compute_active)
            call self%set(self%physics_active, source%physics_active)
            call self%set(self%coupling_mode, source%coupling_mode)
        end select
    end subroutine copy_config_control_manager

    subroutine reset_config_control_manager(self)
        implicit none
        class(type_config_control_manager), intent(inout) :: self

        call deallocate_array(self%unique_material_ids)
        call deallocate_array(self%compute_active)
        call deallocate_array(self%physics_active)
        self%coupling_mode = type_constant_id("", "", -1)
    end subroutine reset_config_control_manager

end module types_config_control_manager
