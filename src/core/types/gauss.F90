module core_types_gauss
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private

    public :: type_gauss_point_state

    type :: type_gauss_point_state
        real(real64) :: temperature
        real(real64) :: pressure
        real(real64) :: water_content
        real(real64) :: porosity
        ! ... 必要に応じて他の状態変数を追加 ...
    end type type_gauss_point_state

end module core_types_gauss
