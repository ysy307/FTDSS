module core_types_vector
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    public :: type_dp_vector_2d
    public :: type_dp_vector_3d

    public :: type_int_vector_2d
    public :: type_int_vector_3d

    public :: assignment(=)

    interface assignment(=)
        module procedure :: type_dp_vector_2d_assignment
        module procedure :: type_dp_vector_3d_assignment
        module procedure :: type_int_vector_2d_assignment
        module procedure :: type_int_vector_3d_assignment
    end interface assignment(=)

    type :: type_dp_vector_2d
        real(real64) :: x
        real(real64) :: y
    end type type_dp_vector_2d

    type :: type_dp_vector_3d
        real(real64) :: x
        real(real64) :: y
        real(real64) :: z
    end type type_dp_vector_3d

    type :: type_int_vector_2d
        integer(int32) :: x
        integer(int32) :: y
    end type type_int_vector_2d

    type :: type_int_vector_3d
        integer(int32) :: x
        integer(int32) :: y
        integer(int32) :: z
    end type type_int_vector_3d

contains

    ! Assignment for 2D double precision vector
    subroutine type_dp_vector_2d_assignment(a, b)
        implicit none
        class(type_dp_vector_2d), intent(inout) :: a
        class(type_dp_vector_2d), intent(in) :: b

        a%x = b%x
        a%y = b%y
    end subroutine type_dp_vector_2d_assignment

    ! Assignment for 3D double precision vector
    subroutine type_dp_vector_3d_assignment(a, b)
        implicit none
        class(type_dp_vector_3d), intent(inout) :: a
        class(type_dp_vector_3d), intent(in) :: b

        a%x = b%x
        a%y = b%y
        a%z = b%z
    end subroutine type_dp_vector_3d_assignment

    ! Assignment for 2D integer vector
    subroutine type_int_vector_2d_assignment(a, b)
        implicit none
        class(type_int_vector_2d), intent(inout) :: a
        class(type_int_vector_2d), intent(in) :: b

        a%x = b%x
        a%y = b%y
    end subroutine type_int_vector_2d_assignment

    ! Assignment for 3D integer vector
    subroutine type_int_vector_3d_assignment(a, b)
        implicit none
        class(type_int_vector_3d), intent(inout) :: a
        class(type_int_vector_3d), intent(in) :: b

        a%x = b%x
        a%y = b%y
        a%z = b%z
    end subroutine type_int_vector_3d_assignment

end module core_types_vector
