module types_config_output
    use, intrinsic :: iso_fortran_env
    use :: types_config_base, only:abst_config
    use :: types_config_observation, only:type_config_observation, type_config_observation_geometry
    implicit none
    private

    public :: type_config_output
    public :: type_config_observation
    public :: type_config_observation_geometry

    type, extends(abst_config) :: type_config_output
        logical, allocatable :: is_output_enabled(:)
    contains
        procedure, public, pass(self) :: copy => copy_config_output
        procedure, public, pass(self) :: reset => reset_config_output
    end type type_config_output

contains

    subroutine copy_config_output(self, source)
        implicit none
        class(type_config_output), intent(inout) :: self
        class(abst_config), intent(in) :: source

        select type (source)
        type is (type_config_output)
            call self%set(self%is_output_enabled, source%is_output_enabled)
        class default
            call self%reset()
        end select
    end subroutine copy_config_output

    subroutine reset_config_output(self)
        implicit none
        class(type_config_output), intent(inout) :: self

        call deallocate_array(self%is_output_enabled)

    end subroutine reset_config_output

end module types_config_output
