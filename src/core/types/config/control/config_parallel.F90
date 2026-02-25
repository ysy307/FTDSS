module core_types_config_control_parallel
    use, intrinsic :: iso_fortran_env
    use :: core_constants, only:type_constant_id, type_constant_value
    use :: core_types_config_base, only:abst_config
    implicit none
    private

    public :: type_config_parallel_openmp

    type, extends(abst_config) :: type_config_parallel_openmp
        logical :: is_parallel = .false.
        integer(int32) :: num_threads
        character(:), allocatable :: schedule
        integer(int32) :: max_active_levels
    contains
        procedure, public, pass(self) :: copy => copy_config_parallel_openmp
        procedure, public, pass(self) :: reset => reset_config_parallel_openmp
    end type type_config_parallel_openmp

contains
    subroutine copy_config_parallel_openmp(self, source)
        implicit none
        class(type_config_parallel_openmp), intent(inout) :: self
        class(abst_config), intent(in) :: source

        call self%reset()

        select type (source)
        type is (type_config_parallel_openmp)

            call self%set(self%is_parallel, source%is_parallel)
            call self%set(self%num_threads, source%num_threads)
            call self%set(self%schedule, source%schedule)
            call self%set(self%max_active_levels, source%max_active_levels)
        end select
    end subroutine copy_config_parallel_openmp

    subroutine reset_config_parallel_openmp(self)
        implicit none
        class(type_config_parallel_openmp), intent(inout) :: self

        self%is_parallel = .false.
        self%num_threads = 1
        self%schedule = "auto"
        self%max_active_levels = 1
    end subroutine reset_config_parallel_openmp

end module core_types_config_control_parallel
