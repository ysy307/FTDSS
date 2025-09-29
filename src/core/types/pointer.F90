module core_types_pointer
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    private

    public :: type_dp_pointer
    public :: type_int_pointer
    public :: type_logical_pointer

    !--------------------------------------------------------------------------------------
    ! Pointer type for real numbers
    !  - This is used to manage the memory of coorinate values in a polymorphic way
    !  - The pointer is initialized to null and can be associated with coorinate values
    !--------------------------------------------------------------------------------------
    type :: type_dp_pointer
        real(real64), pointer :: val => null()
    end type type_dp_pointer

    type :: type_int_pointer
        integer(int32), pointer :: val => null()
    end type type_int_pointer

    type :: type_logical_pointer
        logical, pointer :: val => null()
    end type type_logical_pointer

end module core_types_pointer
