module core_types_config_physics_base
    use, intrinsic :: iso_fortran_env
    use :: core_constants, only:ERROR_CODES
    use :: core_types_config_base, only:abst_config
    implicit none
    private

    public :: abst_config_physics_model
    public :: abst_config_physics_material

    type, abstract, extends(abst_config) :: abst_config_physics_model
        integer(int32) :: material_id = 0
    contains
        procedure, public, pass(self) :: copy => copy_config_physics_model
        procedure, public, pass(self) :: reset => reset_config_physics_model
    end type abst_config_physics_model

    type, abstract, extends(abst_config) :: abst_config_physics_material
        integer(int32) :: material_id = 0
    contains
        procedure, public, pass(self) :: copy => copy_config_physics_material
        procedure, public, pass(self) :: reset => reset_config_physics_material
    end type abst_config_physics_material

contains

    subroutine copy_config_physics_model(self, source)
        implicit none
        class(abst_config_physics_model), intent(inout) :: self
        class(abst_config), intent(in) :: source

        select type (source)
        type is (abst_config_physics_model)
            call self%set(self%material_id, source%material_id)
        end select
    end subroutine copy_config_physics_model

    subroutine reset_config_physics_model(self)
        implicit none
        class(abst_config_physics_model), intent(inout) :: self
        self%material_id = 0
    end subroutine reset_config_physics_model

    subroutine copy_config_physics_material(self, source)
        implicit none
        class(abst_config_physics_material), intent(inout) :: self
        class(abst_config), intent(in) :: source

        select type (source)
        type is (abst_config_physics_material)
            call self%set(self%material_id, source%material_id)
        end select
    end subroutine copy_config_physics_material

    subroutine reset_config_physics_material(self)
        implicit none
        class(abst_config_physics_material), intent(inout) :: self
        self%material_id = 0
    end subroutine reset_config_physics_material
end module core_types_config_physics_base
