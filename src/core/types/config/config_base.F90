module core_types_physics_config_base
    use, intrinsic :: iso_fortran_env
    use :: core_constants, only:type_constant_id
    implicit none
    private

    public :: abst_config

    type, abstract :: abst_config
    contains
        ! Generic interface for integer(int32) and real(real64) up to 3D
        generic :: set => &
            set_int32, set_int32_1d, set_int32_2d, set_int32_3d, &
            set_real64, set_real64_1d, set_real64_2d, set_real64_3d, &
            set_constant_id

        procedure, private, pass(self) :: set_int32
        procedure, private, pass(self) :: set_int32_1d
        procedure, private, pass(self) :: set_int32_2d
        procedure, private, pass(self) :: set_int32_3d
        procedure, private, pass(self) :: set_real64
        procedure, private, pass(self) :: set_real64_1d
        procedure, private, pass(self) :: set_real64_2d
        procedure, private, pass(self) :: set_real64_3d
        procedure, private, pass(self) :: set_constant_id

        procedure(abst_copy_config), public, pass(self), deferred :: copy
        procedure(abst_reset_config), public, pass(self), deferred :: reset
    end type abst_config

    abstract interface
        subroutine abst_copy_config(self, source)
            import :: abst_config
            implicit none
            class(abst_config), intent(inout) :: self
            class(abst_config), intent(in) :: source
        end subroutine abst_copy_config

        subroutine abst_reset_config(self)
            import :: abst_config
            implicit none
            class(abst_config), intent(inout) :: self
        end subroutine abst_reset_config
    end interface

contains

    subroutine set_int32(self, member, value)
        implicit none
        class(abst_config), intent(in) :: self
        integer(int32), intent(inout) :: member
        class(*), intent(in) :: value(..)

        select rank (v => value)
        rank (0)
            select type (v)
            type is (integer(int32))
                member = v
            end select
        end select
    end subroutine set_int32

    subroutine set_int32_1d(self, member, value)
        implicit none
        class(abst_config), intent(in) :: self
        integer(int32), allocatable, intent(inout) :: member(:)
        class(*), intent(in) :: value(..)

        select rank (v => value)
        rank (1)
            select type (v)
            type is (integer(int32))
                if (allocated(member)) deallocate (member)
                allocate (member, source=v)
            end select
        end select
    end subroutine set_int32_1d

    subroutine set_int32_2d(self, member, value)
        implicit none
        class(abst_config), intent(in) :: self
        integer(int32), allocatable, intent(inout) :: member(:, :)
        class(*), intent(in) :: value(..)

        select rank (v => value)
        rank (2)
            select type (v)
            type is (integer(int32))
                if (allocated(member)) deallocate (member)
                allocate (member, source=v)
            end select
        end select
    end subroutine set_int32_2d

    subroutine set_int32_3d(self, member, value)
        implicit none
        class(abst_config), intent(in) :: self
        integer(int32), allocatable, intent(inout) :: member(:, :, :)
        class(*), intent(in) :: value(..)
        select rank (v => value)

        rank (3)
            select type (v)
            type is (integer(int32))
                if (allocated(member)) deallocate (member)
                allocate (member, source=v)
            end select
        end select
    end subroutine set_int32_3d

    ! --- real(real64) implementations ---

    subroutine set_real64(self, member, value)
        implicit none
        class(abst_config), intent(in) :: self
        real(real64), intent(inout) :: member
        class(*), intent(in) :: value(..)

        select rank (v => value)
        rank (0)
            select type (v)
            type is (real(real64))
                member = v
            end select
        end select
    end subroutine set_real64

    subroutine set_real64_1d(self, member, value)
        implicit none
        class(abst_config), intent(in) :: self
        real(real64), allocatable, intent(inout) :: member(:)
        class(*), intent(in) :: value(..)

        select rank (v => value)
        rank (1)
            select type (v)
            type is (real(real64))
                if (allocated(member)) deallocate (member)
                allocate (member, source=v)
            end select
        end select
    end subroutine set_real64_1d

    subroutine set_real64_2d(self, member, value)
        implicit none
        class(abst_config), intent(in) :: self
        real(real64), allocatable, intent(inout) :: member(:, :)
        class(*), intent(in) :: value(..)

        select rank (v => value)
        rank (2)
            select type (v)
            type is (real(real64))
                if (allocated(member)) deallocate (member)
                allocate (member, source=v)
            end select
        end select
    end subroutine set_real64_2d

    subroutine set_real64_3d(self, member, value)
        implicit none
        class(abst_config), intent(in) :: self
        real(real64), allocatable, intent(inout) :: member(:, :, :)
        class(*), intent(in) :: value(..)

        select rank (v => value)
        rank (3)
            select type (v)
            type is (real(real64))
                if (allocated(member)) deallocate (member)
                allocate (member, source=v)
            end select
        end select
    end subroutine set_real64_3d

    subroutine set_constant_id(self, member, value)
        implicit none
        class(abst_config), intent(in) :: self
        type (type_constant_id), intent(inout) :: member
        class(*), intent(in) :: value(..)

        select rank (v => value)
        rank (0)
            select type (v)
            type is (type_constant_id)
                member = v
            end select
        end select
    end subroutine set_constant_id

end module core_types_physics_config_base
