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
        type(Variables) :: Qw
        type(Variables) :: Qice
        type(Variables) :: Si
    end type

    type, extends(Abstract_Ice) :: Type_Ice_TRM
        real(real64) :: Lf !! Latent heat
        real(real64) :: Tf !! Freezing point
    contains
        procedure, pass(self), private :: Update_Ice_TRM_scalar
        procedure, pass(self), private :: Update_Ice_TRM_array
        generic, public :: Update_Ice => Update_Ice_TRM_scalar, & !&
                                         Update_Ice_TRM_array !&
    end type

    type, extends(Abstract_Ice) :: Type_Ice_GCC
        class(Abstract_WRF), allocatable :: WRF
        class(Abstract_GCC), allocatable :: GCC
        type(Variables) :: D_Qice
    contains
        procedure, nopass, private :: Set_Type_Ice_GCC_WRF
        procedure, nopass, private :: Set_Type_Ice_GCC_WRF_minimum
        generic, private :: Set_WRF => Set_Type_Ice_GCC_WRF, & !&
                                       Set_Type_Ice_GCC_WRF_minimum !&
        procedure, nopass, private :: Set_Type_Ice_GCC_GCC
        procedure, nopass, private :: Set_Type_Ice_GCC_GCC_minimum
        generic, private :: Set_GCC => Set_Type_Ice_GCC_GCC, & !&
                                       Set_Type_Ice_GCC_GCC_minimum !&

        procedure, pass(self), private :: Calculate_Ice_GCC_NonSegregation_m
        procedure, pass(self), private :: Calculate_Ice_GCC_rhoW_scalar
        generic, public :: Calculate_Ice => Calculate_Ice_GCC_NonSegregation_m, & !&
                                            Calculate_Ice_GCC_rhoW_Scalar

        procedure, pass(self), private :: Calculate_Ice_GCC_Derivative_Temperature_NonSegregation_m
        procedure, pass(self), private :: Calculate_Ice_GCC_Derivative_Temperature_rhoW_scalar
        generic, public :: Calculate_Ice_Derivative => Calculate_Ice_GCC_Derivative_Temperature_NonSegregation_m, & !&
                                                       Calculate_Ice_GCC_Derivative_Temperature_rhoW_scalar

        procedure, pass(self), private :: Update_Ice_GCC_NonSegregation_m
        procedure, pass(self), private :: Update_Ice_GCC_rhoW_scalar
        procedure, pass(self), private :: Update_Ice_GCC_rhoW_array
        generic, public :: Update_Ice => Update_Ice_GCC_NonSegregation_m, & !&
                                            Update_Ice_GCC_rhoW_Scalar, & !&
                                            Update_Ice_GCC_rhoW_Array !&

        procedure, pass(self), private :: Update_Ice_GCC_Derivative_Temperature_NonSegregation_m
        procedure, pass(self), private :: Update_Ice_GCC_Derivative_Temperature_rhoW_scalar
        procedure, pass(self), private :: Update_Ice_GCC_Derivative_Temperature_rhoW_array
        generic, public :: Update_Ice_Derivative => Update_Ice_GCC_Derivative_Temperature_NonSegregation_m, & !&
                                                       Update_Ice_GCC_Derivative_Temperature_rhoW_scalar, & !&
                                                       Update_Ice_GCC_Derivative_Temperature_rhoW_array !&
    end type Type_Ice_GCC

    type, extends(Abstract_Ice) :: Type_Ice_EXP
        real(real64) :: phi !! Porosity
        real(real64) :: Tf !! Freezing point
        real(real64) :: a !! power model parameter
        type(Variables) :: D_Qice
    contains
        procedure, pass(self), public :: Calculate_Ice => Calculate_Ice_EXP
        procedure, pass(self), public :: Calculate_Ice_Derivative => Calculate_Ice_EXP_Derivative_Temperature
        procedure, pass(self), public :: Update_Ice => Update_Ice_EXP
        procedure, pass(self), public :: Update_Ice_Derivative => Update_Ice_EXP_Derivative_Temperature
    end type Type_Ice_EXP

    interface Type_Ice_TRM
        module procedure Construct_Type_Ice_TRM
        module procedure Construct_Type_Ice_TRM_minimum
    end interface

    interface Type_Ice_GCC
        module procedure Construct_Type_Ice_GCC
        module procedure Construct_Type_Ice_GCC_minimum
    end interface

    interface Type_Ice_EXP
        module procedure Construct_Type_Ice_EXP
        module procedure Construct_Type_Ice_EXP_minimum
    end interface

    interface
        module function Construct_Type_Ice_TRM(Lf, Tf, nsize) result(structure)
            use, intrinsic :: iso_fortran_env, only: real64
            implicit none
            real(real64), intent(in) :: Lf
            real(real64), intent(in) :: Tf
            integer(int32), intent(in) :: nsize
            class(Abstract_Ice), allocatable :: structure

        end function Construct_Type_Ice_TRM

        module function Construct_Type_Ice_TRM_minimum() result(structure)
            implicit none
            class(Abstract_Ice), allocatable :: structure

        end function Construct_Type_Ice_TRM_minimum

        module subroutine Update_Ice_TRM_array(self, arr_Temperature, arr_Si, arr_rhoW, arr_Cp)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_TRM
            implicit none
            class(Type_Ice_TRM), intent(inout) :: self
            type(Variables), intent(inout) :: arr_Temperature
            type(Variables), intent(inout) :: arr_Si
            real(real64), intent(in) :: arr_rhoW(:)
            real(real64), intent(in) :: arr_Cp(:)
        end subroutine Update_Ice_TRM_array

        module subroutine Update_Ice_TRM_scalar(self, arr_Temperature, arr_Si, rhoW, arr_Cp)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_TRM
            implicit none
            class(Type_Ice_TRM), intent(inout) :: self
            type(Variables), intent(inout) :: arr_Temperature
            type(Variables), intent(inout) :: arr_Si
            real(real64), intent(in) :: rhoW
            real(real64), intent(in) :: arr_Cp(:)
        end subroutine Update_Ice_TRM_scalar

        module function Construct_Type_Ice_GCC(ModelType, isSegregation, c_unit, nsize, thetaS, thetaR, alpha1, n1, w1, hcrit, alpha2, n2, Tf, Lf, rhoI) result(construct)
            implicit none
            integer(int32), intent(in) :: ModelType
            logical(4), intent(in) :: isSegregation
            character(*), intent(in) :: c_unit
            integer(int32), intent(in) :: nsize
            real(real64), intent(in) :: thetaS
            real(real64), intent(in) :: thetaR
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in), optional :: w1
            real(real64), intent(in), optional :: hcrit
            real(real64), intent(in), optional :: alpha2
            real(real64), intent(in), optional :: n2
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            real(real64), intent(in), optional :: rhoI

            class(Abstract_Ice), allocatable :: construct

        end function Construct_Type_Ice_GCC

        module function Construct_Type_Ice_GCC_minimum(ModelType, isSegregation, c_unit) result(construct)
            implicit none
            integer(int32), intent(in) :: ModelType
            logical(4), intent(in) :: isSegregation
            character(*), intent(in) :: c_unit

            class(Abstract_Ice), allocatable :: construct

        end function Construct_Type_Ice_GCC_minimum

        module function Set_Type_Ice_GCC_WRF(ModelType, thetaS, thetaR, alpha1, n1, w1, hcrit, alpha2, n2) result(structure_WRF)
            implicit none
            integer(int32), intent(in) :: ModelType
            real(real64), intent(in) :: thetaS
            real(real64), intent(in) :: thetaR
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in), optional :: w1
            real(real64), intent(in), optional :: hcrit
            real(real64), intent(in), optional :: alpha2
            real(real64), intent(in), optional :: n2
            class(Abstract_WRF), allocatable :: structure_WRF

        end function Set_Type_Ice_GCC_WRF

        module function Set_Type_Ice_GCC_WRF_minimum(ModelType) result(structure_WRF)
            implicit none
            integer(int32), intent(in) :: ModelType
            class(Abstract_WRF), allocatable :: structure_WRF

        end function Set_Type_Ice_GCC_WRF_minimum

        module function Set_Type_Ice_GCC_GCC(isSegregation, c_unit, Tf, Lf, rhoI) result(structure_GCC)
            implicit none
            logical(4), intent(in) :: isSegregation
            character(*), intent(in) :: c_unit
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            real(real64), intent(in), optional :: rhoI

            class(Abstract_GCC), allocatable :: structure_GCC

        end function Set_Type_Ice_GCC_GCC

        module function Set_Type_Ice_GCC_GCC_minimum(isSegregation, c_unit) result(structure_GCC)
            implicit none
            logical(4), intent(in) :: isSegregation
            character(*), intent(in) :: c_unit

            class(Abstract_GCC), allocatable :: structure_GCC

        end function Set_Type_Ice_GCC_GCC_minimum

        module function Calculate_Ice_GCC_NonSegregation_m(self, Temperature) result(Qice)
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in) :: Temperature
            real(real64) :: Qice

        end function Calculate_Ice_GCC_NonSegregation_m

        module function Calculate_Ice_GCC_rhoW_scalar(self, Temperature, rhoW, Pw) result(Qice)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in) :: Temperature
            real(real64), intent(in) :: rhoW
            real(real64), intent(in), optional :: Pw
            real(real64) :: Qice
        end function Calculate_Ice_GCC_rhoW_scalar

        module function Calculate_Ice_GCC_Derivative_Temperature_NonSegregation_m(self, Temperature) result(D_Qice)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in) :: Temperature
            real(real64) :: D_Qice
        end function Calculate_Ice_GCC_Derivative_Temperature_NonSegregation_m

        module function Calculate_Ice_GCC_Derivative_Temperature_rhoW_scalar(self, Temperature, rhoW, Pw) result(D_Qice)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in) :: Temperature
            real(real64), intent(in) :: rhoW
            real(real64), intent(in), optional :: Pw
            real(real64) :: D_Qice
        end function Calculate_Ice_GCC_Derivative_Temperature_rhoW_scalar

        module subroutine Update_Ice_GCC_NonSegregation_m(self, arr_Temperature)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)
        end subroutine Update_Ice_GCC_NonSegregation_m

        module subroutine Update_Ice_GCC_rhoW_scalar(self, arr_Temperature, rhoW, arr_Pw)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)
            real(real64), intent(in) :: rhoW
            real(real64), intent(in), optional :: arr_Pw(:)
        end subroutine Update_Ice_GCC_rhoW_scalar

        module subroutine Update_Ice_GCC_rhoW_array(self, arr_Temperature, arr_rhoW, arr_Pw)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)
            real(real64), intent(in) :: arr_rhoW(:)
            real(real64), intent(in), optional :: arr_Pw(:)
        end subroutine Update_Ice_GCC_rhoW_array

        module subroutine Update_Ice_GCC_Derivative_Temperature_NonSegregation_m(self, arr_Temperature)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)
        end subroutine Update_Ice_GCC_Derivative_Temperature_NonSegregation_m

        module subroutine Update_Ice_GCC_Derivative_Temperature_rhoW_scalar(self, arr_Temperature, rhoW, arr_Pw)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)
            real(real64), intent(in) :: rhoW
            real(real64), intent(in), optional :: arr_Pw(:)
        end subroutine Update_Ice_GCC_Derivative_Temperature_rhoW_scalar

        module subroutine Update_Ice_GCC_Derivative_Temperature_rhoW_array(self, arr_Temperature, arr_rhoW, arr_Pw)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_GCC
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)
            real(real64), intent(in) :: arr_rhoW(:)
            real(real64), intent(in), optional :: arr_Pw(:)
        end subroutine Update_Ice_GCC_Derivative_Temperature_rhoW_array

        module function Calculate_Ice_EXP(self, Temperature) result(Qice)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_EXP
            implicit none
            class(Type_Ice_EXP), intent(inout) :: self
            real(real64), intent(in) :: Temperature
            real(real64) :: Qice
        end function Calculate_Ice_EXP

        module function Calculate_Ice_EXP_Derivative_Temperature(self, Temperature) result(D_Qice)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_EXP
            implicit none
            class(Type_Ice_EXP), intent(inout) :: self
            real(real64), intent(in) :: Temperature
            real(real64) :: D_Qice
        end function Calculate_Ice_EXP_Derivative_Temperature

        module function Construct_Type_Ice_EXP(phi, Tf, a, nsize) result(self)
            use, intrinsic :: iso_fortran_env, only: real64
            implicit none
            real(real64), intent(in) :: phi
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: a
            integer(int32), intent(in) :: nsize
            class(Abstract_Ice), allocatable :: self

        end function Construct_Type_Ice_EXP

        module function Construct_Type_Ice_EXP_minimum() result(self)
            implicit none
            class(Abstract_Ice), allocatable :: self

        end function Construct_Type_Ice_EXP_minimum

        module subroutine Update_Ice_EXP(self, arr_Temperature)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_EXP
            implicit none
            class(Type_Ice_EXP), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)
        end subroutine Update_Ice_EXP

        module subroutine Update_Ice_EXP_Derivative_Temperature(self, arr_Temperature)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Type_Ice_EXP
            implicit none
            class(Type_Ice_EXP), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)
        end subroutine Update_Ice_EXP_Derivative_Temperature
    end interface

end module Calculate_Ice
