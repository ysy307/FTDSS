module types_config_observation
    use, intrinsic :: iso_fortran_env
    use :: core_constants
    use :: types_config_base, only:abst_config
    use :: types_geometry_coordinate, only:type_coordinate_dp
    implicit none

    public :: type_config_observation_geometry
    public :: type_config_observation

    type, extends(abst_config) :: type_config_observation_geometry
        type(type_constant_id) :: point_type = OUTPUT_OBSERVATION_TYPES%NONE
        integer(int32) :: node_id
        type(type_coordinate_dp) :: coordinate
        integer(int32) :: fe_id
        type(type_coordinate_dp) :: coordinate_normalized
        class(*), pointer :: fe
        integer(int32), allocatable :: connectivity(:)
    contains
        procedure, public, pass(self) :: copy => copy_config_observation_geometry
        procedure, public, pass(self) :: reset => reset_config_observation_geometry
    end type type_config_observation_geometry

    type, extends(abst_config) :: type_config_observation
        type(type_constant_id) :: point_type = OUTPUT_OBSERVATION_TYPES%NONE
        type(type_constant_id), allocatable :: output_variables(:)
        integer(int32) :: num_observations
        type(type_config_observation_geometry), allocatable :: observation_geometries(:)
    contains
        procedure, public, pass(self) :: copy => copy_config_observation
        procedure, public, pass(self) :: reset => reset_config_observation
    end type type_config_observation

contains

    subroutine copy_config_observation_geometry(self, source)
        implicit none
        class(type_config_observation_geometry), intent(inout) :: self
        class(abst_config), intent(in) :: source

        select type (source)
        type is (type_config_observation_geometry)
            call self%set(self%point_type, source%point_type)
            call self%set(self%node_id, source%node_id)
            call self%set(self%coordinate, source%coordinate)
            call self%set(self%fe_id, source%fe_id)
            call self%set(self%coordinate_normalized, source%coordinate_normalized)
            self%fe => source%fe
            call self%set(self%connectivity, source%connectivity)
        class default
            call self%reset()
        end select
    end subroutine copy_config_observation_geometry

    subroutine reset_config_observation_geometry(self)
        implicit none
        class(type_config_observation_geometry), intent(inout) :: self

        self%point_type = OUTPUT_OBSERVATION_TYPES%NONE
        self%node_id = 0
        self%coordinate = type_coordinate_dp(0.0d0, 0.0d0, 0.0d0)
        self%fe_id = 0
        self%coordinate_normalized = type_coordinate_dp(0.0d0, 0.0d0, 0.0d0)
        self%fe => null()
        call deallocate_array(self%connectivity)

    end subroutine reset_config_observation_geometry

    subroutine copy_config_observation(self, source)
        implicit none
        class(type_config_observation), intent(inout) :: self
        class(abst_config), intent(in) :: source

        integer(int32) :: i

        select type (source)
        type is (type_config_observation)
            call self%set(self%point_type, source%point_type)
            self%num_observations = source%num_observations

            call self%set(self%output_variables, source%output_variables)

            if (allocated(source%observation_geometries)) then
                if (allocated(self%observation_geometries)) then
                    call deallocate_array(self%observation_geometries)
                end if
                allocate (self%observation_geometries(size(source%observation_geometries)))
                do i = 1, size(source%observation_geometries)
                    call self%observation_geometries(i)%copy(source%observation_geometries(i))
                end do
            else
                if (allocated(self%observation_geometries)) then
                    call deallocate_array(self%observation_geometries)
                end if
            end if

        class default
            call self%reset()
        end select
    end subroutine copy_config_observation

    subroutine reset_config_observation(self)
        implicit none
        class(type_config_observation), intent(inout) :: self

        integer(int32) :: i

        self%point_type = OUTPUT_OBSERVATION_TYPES%NONE
        self%num_observations = 0

        call deallocate_array(self%output_variables)

        if (allocated(self%observation_geometries)) then
            do i = 1, size(self%observation_geometries)
                call self%observation_geometries(i)%reset()
            end do
            call deallocate_array(self%observation_geometries)
        end if

    end subroutine reset_config_observation

end module types_config_observation
