module types_config_nodes
    use, intrinsic :: iso_fortran_env
    use :: core_constants, only:type_constant_id
    use :: core_types_config_base, only:abst_config
    use :: core_memory, only:deallocate_array
    implicit none
    private

    public :: type_config_nodes

    type, extends(abst_config) :: type_config_nodes
        integer(int32) :: num_nodes
        real(real64), allocatable :: points(:, :)
        integer(int32), allocatable :: global_node_ids(:)
    contains
        procedure, public, pass(self) :: copy => copy_config_nodes
        procedure, public, pass(self) :: reset => reset_config_nodes
    end type type_config_nodes

contains

    subroutine copy_config_nodes(self, source)
        implicit none
        class(type_config_nodes), intent(inout) :: self
        class(abst_config), intent(in) :: source

        select type (source)
        type is (type_config_nodes)
            call self%set(self%num_nodes, source%num_nodes)
            call self%set(self%points, source%points)
            call self%set(self%global_node_ids, source%global_node_ids)
        class default
            call self%reset()
        end select
    end subroutine copy_config_nodes

    subroutine reset_config_nodes(self)
        implicit none
        class(type_config_nodes), intent(inout) :: self

        self%num_nodes = 0
        call deallocate_array(self%points)
        call deallocate_array(self%global_node_ids)

    end subroutine reset_config_nodes

end module types_config_nodes
