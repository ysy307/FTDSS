module types_config_conditions_initial
    use, intrinsic :: iso_fortran_env
    use :: core_memory, only:allocate_array, deallocate_array
    use :: core_constants, only:type_constant_id
    use :: core_types_config_base, only:abst_config
    implicit none
    private

    public :: type_config_ic

    type, extends(abst_config) :: type_config_ic
        logical :: active = .false.
        !> Physics type (e.g. heat transfer, moisture transfer)
        type(type_constant_id) :: physics_type = type_constant_id("", "", -1)
        !> Initial condition type
        type(type_constant_id) :: ic_kind = type_constant_id("", "", -1)
        !> Initial condition value (for uniform type)
        real(real64) :: value
    contains

        procedure, public, pass(self) :: copy => copy_config_ic
        procedure, public, pass(self) :: reset => reset_config_ic

    end type type_config_ic

contains

    subroutine copy_config_ic(self, source)
        implicit none
        class(type_config_ic), intent(inout) :: self
        class(abst_config), intent(in) :: source

        select type (source)
        type is (type_config_ic)
        
            call self%set(self%active, source%active)
            call self%set(self%physics_type, source%physics_type)
            call self%set(self%ic_kind, source%ic_kind)
            call self%set(self%value, source%value)
        class default
            call self%reset()
        end select
    end subroutine copy_config_ic

    subroutine reset_config_ic(self)
        implicit none
        class(type_config_ic), intent(inout) :: self

        self%active = .false.
        self%physics_type = type_constant_id("", "", -1)
        self%ic_kind = type_constant_id("", "", -1)
        self%value = 0.0d0

    end subroutine reset_config_ic

end module types_config_conditions_initial
