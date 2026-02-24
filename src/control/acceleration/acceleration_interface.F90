module control_acceleration
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_linalg, only:vector_dot
    implicit none
    private

    public :: abst_acceleration
    public :: type_acceleration_aitken

    type, abstract :: abst_acceleration
    contains
        procedure(abst_initialize_acceleration), public, pass(self), deferred :: initialize
        procedure(abst_destory_acceleration), public, pass(self), deferred :: destory
        procedure(abst_compute_acceleration), public, pass(self), deferred :: compute
        procedure(abst_reset_acceleration), public, pass(self), deferred :: reset
    end type

    abstract interface
        subroutine abst_initialize_acceleration(self, config)
            import :: abst_acceleration, type_config_acceleration
            implicit none
            class(abst_acceleration), intent(inout) :: self
            type(type_config_acceleration), intent(in) :: config
        end subroutine abst_initialize_acceleration

        subroutine abst_destory_acceleration(self)
            import :: abst_acceleration
            class(abst_acceleration), intent(inout) :: self
        end subroutine abst_destory_acceleration

        subroutine abst_compute_acceleration(self, physics_type, iter, du, vec)
            import :: abst_acceleration, type_constant_id, int32, real64
            implicit none
            class(abst_acceleration), intent(inout) :: self
            type(type_constant_id), intent(in) :: physics_type
            integer(int32), intent(in) :: iter
            !> Increment vector
            real(real64), intent(in) :: du(:)
            !> Updated vector (u = u + ω*Δu)
            real(real64), intent(inout) :: vec(:)
        end subroutine abst_compute_acceleration

        subroutine abst_reset_acceleration(self)
            import :: abst_acceleration
            class(abst_acceleration), intent(inout) :: self
        end subroutine abst_reset_acceleration

    end interface

    type, extends(abst_acceleration) :: type_acceleration_aitken
        type(type_config_acceleration) :: config
        real(real64), private :: relaxation_factor(PHYSICS_TYPES%NUM_ID) = 0.5d0
        real(real64), private :: previous_relaxation_factor(PHYSICS_TYPES%NUM_ID) = 0.5d0
        real(real64), allocatable, private :: du_raw(:, :)
    contains
        procedure, public, pass(self) :: initialize => initialize_acceleration_aitken
        procedure, public, pass(self) :: destory => destory_acceleration_aitken
        procedure, public, pass(self) :: compute => compute_acceleration_aitken
        procedure, public, pass(self) :: reset => reset_acceleration_aitken
    end type type_acceleration_aitken

    interface
        module subroutine initialize_acceleration_aitken(self, config)
            implicit none
            class(type_acceleration_aitken), intent(inout) :: self
            type(type_config_acceleration), intent(in) :: config
        end subroutine initialize_acceleration_aitken

        module subroutine destory_acceleration_aitken(self)
            implicit none
            class(type_acceleration_aitken), intent(inout) :: self
        end subroutine destory_acceleration_aitken

        module subroutine compute_acceleration_aitken(self, physics_type, iter, du, vec)
            implicit none
            class(type_acceleration_aitken), intent(inout) :: self
            type(type_constant_id), intent(in) :: physics_type
            integer(int32), intent(in) :: iter
            !> du は前回の反復からの増分 Δu を表すベクトルを表します。
            real(real64), intent(in) :: du(:)
            !> vec は self%relaxation_factor を用いて更新されます (u = u + ω*Δu)。このサブルーチンは、反復ごとに呼び出され、
            !> Aitkenの加速法を適用してリラクゼーション係数 ω を動的に計算し、vec を更新します。
            real(real64), intent(inout) :: vec(:)
        end subroutine compute_acceleration_aitken

        module subroutine reset_acceleration_aitken(self)
            implicit none
            class(type_acceleration_aitken), intent(inout) :: self
        end subroutine reset_acceleration_aitken
    end interface

contains

end module control_acceleration
