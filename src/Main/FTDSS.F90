module Main_FTDSS
    use, intrinsic :: iso_fortran_env

    use :: Core_BaseTypes
    use :: Main_Thermal
    implicit none

    type :: Type_FTDSS

        type(DP3d), pointer :: Coordinate
        type(Belonging), allocatable :: NodeBelonging(:)
        class(Abstract_Thermal), allocatable :: Thermal

    end type Type_FTDSS
contains

end module Main_FTDSS
