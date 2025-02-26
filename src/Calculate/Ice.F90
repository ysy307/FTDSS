module Calculate_Ice
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Calculate_WRF
    use :: Calculate_GCC
    use :: Types, only:Variables
#ifdef _OPENMP
!$  use omp_lib
#endif
    implicit none

    type, abstract :: Abstract_Ice
        type(Variables) :: Qice
    end type

    type, extends(Abstract_Ice) :: Type_Ice_TRM
        real(real64) :: Lf !! Latent heat
        real(real64) :: Tf !! Freezing point
    end type

    type, extends(Abstract_Ice) :: Type_Ice_GCC
        class(Abstract_WRF), allocatable :: WRF
        class(Abstract_GCC), allocatable :: GCC
        type(Variables) :: D_Qice
    contains
        procedure, pass(self), public :: Set_WRF => Set_Type_Ice_GCC_WRF
        procedure, pass(self), public :: Set_GCC => Set_Type_Ice_GCC_GCC

        procedure, pass(self), private :: Calculate_Ice_GCC_NonSegregation_m
        procedure, pass(self), private :: Calculate_Ice_GCC_rhoW_scalar
        procedure, pass(self), private :: Calculate_Ice_GCC_rhoW_array
        generic, public :: Calculate_Ice => Calculate_Ice_GCC_NonSegregation_m, & !&
                                            Calculate_Ice_GCC_rhoW_Scalar, & !&
                                            Calculate_Ice_GCC_rhoW_Array

        procedure, pass(self), private :: Calculate_Ice_GCC_Derivative_Temperature_NonSegregation_m
        procedure, pass(self), private :: Calculate_Ice_GCC_Derivative_Temperature_rhoW_scalar
        procedure, pass(self), private :: Calculate_Ice_GCC_Derivative_Temperature_rhoW_array
        generic, public :: Calculate_Ice_Derivative =>  Calculate_Ice_GCC_Derivative_Temperature_NonSegregation_m, & !&
                                                        Calculate_Ice_GCC_Derivative_Temperature_rhoW_scalar, & !&
                                                        Calculate_Ice_GCC_Derivative_Temperature_rhoW_array
    end type Type_Ice_GCC

    type, extends(Abstract_Ice) :: Type_Ice_EXP
        real(real64) :: phi !! Porosity
        real(real64) :: Tf !! Freezing point
        real(real64) :: a !! power model parameter
        type(Variables) :: D_Qice
    contains
        procedure, pass(self) :: Calculate_Ice => Calculate_Ice_EXP
        procedure, pass(self) :: Calculate_Ice_Derivative => Calculate_Ice_EXP_Derivative_Temperature
    end type Type_Ice_EXP

    interface
        module subroutine Set_Type_Ice_GCC_WRF(self, ModelType)
            use, intrinsic :: iso_fortran_env, only: int32
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            integer(int32), intent(in) :: ModelType
        end subroutine Set_Type_Ice_GCC_WRF

        module subroutine Set_Type_Ice_GCC_GCC(self, isSegregation, c_unit)
            use, intrinsic :: iso_fortran_env, only: int32
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            logical(4), intent(in) :: isSegregation
            character(*), intent(in) :: c_unit
        end subroutine Set_Type_Ice_GCC_GCC

        module subroutine Calculate_Ice_GCC_NonSegregation_m(self, arr_Temperature)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)
        end subroutine Calculate_Ice_GCC_NonSegregation_m

        module subroutine Calculate_Ice_GCC_rhoW_scalar(self, arr_Temperature, rhoW, arr_Pw)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)
            real(real64), intent(in) :: rhoW
            real(real64), intent(in), optional :: arr_Pw(:)
        end subroutine Calculate_Ice_GCC_rhoW_scalar

        module subroutine Calculate_Ice_GCC_rhoW_array(self, arr_Temperature, arr_rhoW, arr_Pw)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)
            real(real64), intent(in) :: arr_rhoW(:)
            real(real64), intent(in), optional :: arr_Pw(:)
        end subroutine Calculate_Ice_GCC_rhoW_array

        module subroutine Calculate_Ice_GCC_Derivative_Temperature_NonSegregation_m(self, arr_Temperature)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)
        end subroutine Calculate_Ice_GCC_Derivative_Temperature_NonSegregation_m

        module subroutine Calculate_Ice_GCC_Derivative_Temperature_rhoW_scalar(self, arr_Temperature, rhoW, arr_Pw)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)
            real(real64), intent(in) :: rhoW
            real(real64), intent(in), optional :: arr_Pw(:)
        end subroutine Calculate_Ice_GCC_Derivative_Temperature_rhoW_scalar

        module subroutine Calculate_Ice_GCC_Derivative_Temperature_rhoW_array(self, arr_Temperature, arr_rhoW, arr_Pw)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)
            real(real64), intent(in) :: arr_rhoW(:)
            real(real64), intent(in), optional :: arr_Pw(:)
        end subroutine Calculate_Ice_GCC_Derivative_Temperature_rhoW_array

        module subroutine Calculate_Ice_EXP(self, arr_Temperature)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_EXP
            implicit none
            class(Type_Ice_EXP), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)
        end subroutine Calculate_Ice_EXP

        module subroutine Calculate_Ice_EXP_Derivative_Temperature(self, arr_Temperature)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_EXP
            implicit none
            class(Type_Ice_EXP), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)
        end subroutine Calculate_Ice_EXP_Derivative_Temperature
    end interface

end module Calculate_Ice
