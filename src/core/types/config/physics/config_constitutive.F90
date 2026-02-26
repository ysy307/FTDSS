module core_types_config_physics_constitutive
    use, intrinsic :: iso_fortran_env
    use :: core_constants, only:type_constant_id
    use :: core_types_config_base, only:abst_config
    use :: core_types_config_physics_base, only:abst_config_physics_material
    implicit none
    private

    public :: type_config_constitutive

    type, extends(abst_config_physics_material) :: type_config_constitutive
        real(real64) :: solid = 0.0d0
        real(real64) :: water = 0.0d0
        real(real64) :: ice = 0.0d0
        real(real64) :: vapor = 0.0d0
        real(real64), allocatable :: dispersivity(:)
        real(real64), allocatable :: params(:)
    contains
        procedure, pass(self), public :: copy => copy_config_constitutives
        procedure, pass(self), public :: reset => reset_config_constitutive
    end type type_config_constitutive

contains
    subroutine copy_config_constitutives(self, source)
        implicit none
        class(type_config_constitutive), intent(inout) :: self
        class(abst_config), intent(in) :: source

        call copy_config_physics_material(self, source)
        select type (source)
        type is (type_config_constitutive)
            call self%set(self%solid, source%solid)
            call self%set(self%water, source%water)
            call self%set(self%ice, source%ice)
            call self%set(self%vapor, source%vapor)
            call self%set(self%dispersivity, source%dispersivity)
            call self%set(self%params, source%params)
        end select
    end subroutine copy_config_constitutives

    subroutine reset_config_constitutive(self)
        implicit none
        class(type_config_constitutive), intent(inout) :: self

        call reset_config_physics_material(self)

        self%solid = 0.0d0
        self%water = 0.0d0
        self%ice = 0.0d0
        self%vapor = 0.0d0

        call deallocate_array(self%dispersivity)
        call deallocate_array(self%params)

    end subroutine reset_config_constitutive

end module core_types_config_physics_constitutive
