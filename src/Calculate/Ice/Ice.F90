module Calculate_Ice
    use, intrinsic :: iso_fortran_env
    use :: Core_BaseTypes
    use :: Core_Allocate, only:Allocate_Array
    use :: Inout_Input
    use :: Calculate_WRF
    use :: Calculate_GCC
    use :: Calculate_Density
#ifdef _OPENMP
    use omp_lib
#endif
    implicit none
    private

    public :: Abstract_Ice
    public :: Type_Ice_TRM
    public :: Type_Ice_GCC
    public :: Type_Ice_EXP
    public :: IceHolder

    type :: IceHolder
        class(Abstract_Ice), allocatable :: f
    end type IceHolder

    type, abstract :: Abstract_Ice
        integer(int32) :: nsize
    contains
        procedure(Abstract_Calculate_Ice),            pass(self), deferred :: Calculate_Ice !&
        procedure(Abstract_Calculate_Ice_Derivative), pass(self), deferred :: Calculate_Ice_Derivative !&
        procedure(Abstract_Update_Ice),               pass(self), deferred :: Update_Ice !&
        procedure(Abstract_Update_Ice_Derivative),    pass(self), deferred :: Update_Ice_Derivative !&
    end type

    type, extends(Abstract_Ice) :: Type_Ice_TRM
        real(real64) :: Lf
        real(real64) :: Tf
    contains
        procedure :: Calculate_Ice            => Calculate_Ice_TRM !&
        procedure :: Calculate_Ice_Derivative => Calculate_Ice_TRM_Derivative !&
        procedure :: Update_Ice               => Update_Ice_TRM !&
        procedure :: Update_Ice_Derivative    => Update_Ice_TRM_Derivative !&
    end type

    type, extends(Abstract_Ice) :: Type_Ice_GCC
        class(Abst_WRF), allocatable :: WRF
        class(Abst_GCC), allocatable :: GCC
    contains
        procedure :: Calculate_Ice            => Calculate_Ice_GCC !&
        procedure :: Calculate_Ice_Derivative => Calculate_Ice_GCC_Derivative !&
        procedure :: Update_Ice               => Update_Ice_GCC !&
        procedure :: Update_Ice_Derivative    => Update_Ice_GCC_Derivative !&
    end type Type_Ice_GCC

    type, extends(Abstract_Ice) :: Type_Ice_EXP
        real(real64) :: Lf
        real(real64) :: Tf
        real(real64) :: a
    contains
        procedure :: Calculate_Ice            => Calculate_Ice_EXP !&
        procedure :: Calculate_Ice_Derivative => Calculate_Ice_EXP_Derivative !&
        procedure :: Update_Ice               => Update_Ice_EXP !&
        procedure :: Update_Ice_Derivative    => Update_Ice_EXP_Derivative !&
    end type Type_Ice_EXP

    abstract interface
        function Abstract_Calculate_Ice(self, T, phi, Pw, rhoW, rhoI) result(Qice)
            import :: Abstract_Ice, real64
            implicit none
            class(Abstract_Ice), intent(inout) :: self
            real(real64), intent(in), optional :: T
            real(real64), intent(in), optional :: phi
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Qice

        end function Abstract_Calculate_Ice

        function Abstract_Calculate_Ice_Derivative(self, T, phi, Pw, rhoW, rhoI) result(D_Qice)
            import :: Abstract_Ice, real64
            implicit none
            class(Abstract_Ice), intent(inout) :: self
            real(real64), intent(in), optional :: T
            real(real64), intent(in), optional :: phi
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: D_Qice

        end function Abstract_Calculate_Ice_Derivative

        subroutine Abstract_Update_Ice(self, NodeBelonging, arr_T, arr_phi, arr_Pw, Density, arr_Cp, arr_Qw, arr_Qice, arr_Si)
            import :: Abstract_Ice, Belonging, Abstract_Density, real64, Variables
            implicit none
            class(Abstract_Ice), intent(inout) :: self
            type(Belonging), intent(inout), optional :: NodeBelonging(:)
            real(real64), intent(inout), optional :: arr_T(:)
            real(real64), intent(in), optional :: arr_phi(:)
            real(real64), intent(in), optional :: arr_Pw(:)
            class(Abstract_Density), intent(in), optional :: Density
            real(real64), intent(in) :: arr_Cp(:)
            real(real64), intent(inout), optional :: arr_Qw(:)
            real(real64), intent(inout), optional :: arr_Qice(:)
            type(Variables), intent(inout), optional :: arr_Si

        end subroutine Abstract_Update_Ice

        subroutine Abstract_Update_Ice_Derivative(self, NodeBelonging, arr_T, arr_phi, arr_Pw, Density, arr_Dice)
            import :: Abstract_Ice, Belonging, Abstract_Density, real64
            implicit none
            class(Abstract_Ice), intent(inout) :: self
            type(Belonging), intent(inout), optional :: NodeBelonging(:)
            real(real64), intent(in), optional :: arr_T(:)
            real(real64), intent(in), optional :: arr_phi(:)
            real(real64), intent(in), optional :: arr_Pw(:)
            class(Abstract_Density), intent(in), optional :: Density
            real(real64), intent(inout), optional :: arr_Dice(:)

        end subroutine Abstract_Update_Ice_Derivative
    end interface

    interface
        module function Construct_Type_Ice_TRM(Input, nsize) result(structure)
            implicit none
            type(Input_Region), intent(inout) :: Input
            integer(int32), intent(in) :: nsize
            class(Abstract_Ice), allocatable :: structure

        end function Construct_Type_Ice_TRM

        module function Calculate_Ice_TRM(self, T, phi, Pw, rhoW, rhoI) result(Qice)
            implicit none
            class(Type_Ice_TRM), intent(inout) :: self
            real(real64), intent(in), optional :: T
            real(real64), intent(in), optional :: phi
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Qice

        end function Calculate_Ice_TRM

        module function Calculate_Ice_TRM_Derivative(self, T, phi, Pw, rhoW, rhoI) result(D_Qice)
            implicit none
            class(Type_Ice_TRM), intent(inout) :: self
            real(real64), intent(in), optional :: T
            real(real64), intent(in), optional :: phi
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: D_Qice

        end function Calculate_Ice_TRM_Derivative

        module subroutine Update_Ice_TRM(self, NodeBelonging, arr_T, arr_phi, arr_Pw, Density, arr_Cp, arr_Qw, arr_Qice, arr_Si)
            implicit none
            class(Type_Ice_TRM), intent(inout) :: self
            type(Belonging), intent(inout), optional :: NodeBelonging(:)
            real(real64), intent(inout), optional :: arr_T(:)
            real(real64), intent(in), optional :: arr_phi(:)
            real(real64), intent(in), optional :: arr_Pw(:)
            class(Abstract_Density), intent(in), optional :: Density
            real(real64), intent(in) :: arr_Cp(:)
            real(real64), intent(inout), optional :: arr_Qw(:)
            real(real64), intent(inout), optional :: arr_Qice(:)
            type(Variables), intent(inout), optional :: arr_Si

        end subroutine Update_Ice_TRM

        module subroutine Update_Ice_TRM_Derivative(self, NodeBelonging, arr_T, arr_phi, arr_Pw, Density, arr_Dice)
            implicit none
            class(Type_Ice_TRM), intent(inout) :: self
            type(Belonging), intent(inout), optional :: NodeBelonging(:)
            real(real64), intent(in), optional :: arr_T(:)
            real(real64), intent(in), optional :: arr_phi(:)
            real(real64), intent(in), optional :: arr_Pw(:)
            class(Abstract_Density), intent(in), optional :: Density
            real(real64), intent(inout), optional :: arr_Dice(:)

        end subroutine Update_Ice_TRM_Derivative

    end interface

    interface
        module function Type_Ice_GCC_Construct(Input, nsize) result(Structure)
            implicit none
            type(Input_Region), intent(inout) :: Input
            integer(int32), intent(in) :: nsize
            class(Abstract_Ice), allocatable :: Structure

        end function Type_Ice_GCC_Construct

        module function Calculate_Ice_GCC(self, T, phi, Pw, rhoW, rhoI) result(Qice)
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in), optional :: T
            real(real64), intent(in), optional :: phi
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Qice

        end function Calculate_Ice_GCC

        module function Calculate_Ice_GCC_Derivative(self, T, phi, Pw, rhoW, rhoI) result(D_Qice)
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            real(real64), intent(in), optional :: T
            real(real64), intent(in), optional :: phi
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: D_Qice

        end function Calculate_Ice_GCC_Derivative

        module subroutine Update_Ice_GCC(self, NodeBelonging, arr_T, arr_phi, arr_Pw, Density, arr_Cp, arr_Qw, arr_Qice, arr_Si)
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            type(Belonging), intent(inout), optional :: NodeBelonging(:)
            real(real64), intent(inout), optional :: arr_T(:)
            real(real64), intent(in), optional :: arr_phi(:)
            real(real64), intent(in), optional :: arr_Pw(:)
            class(Abstract_Density), intent(in), optional :: Density
            real(real64), intent(in) :: arr_Cp(:)
            real(real64), intent(inout), optional :: arr_Qw(:)
            real(real64), intent(inout), optional :: arr_Qice(:)
            type(Variables), intent(inout), optional :: arr_Si

        end subroutine Update_Ice_GCC

        module subroutine Update_Ice_GCC_Derivative(self, NodeBelonging, arr_T, arr_phi, arr_Pw, Density, arr_Dice)
            implicit none
            class(Type_Ice_GCC), intent(inout) :: self
            type(Belonging), intent(inout), optional :: NodeBelonging(:)
            real(real64), intent(in), optional :: arr_T(:)
            real(real64), intent(in), optional :: arr_phi(:)
            real(real64), intent(in), optional :: arr_Pw(:)
            class(Abstract_Density), intent(in), optional :: Density
            real(real64), intent(inout), optional :: arr_Dice(:)

        end subroutine Update_Ice_GCC_Derivative
    end interface

    interface
        module function Type_Ice_EXP_Construct(Input, nsize) result(Structure)
            implicit none
            type(Input_Region), intent(inout) :: Input
            integer(int32), intent(in) :: nsize
            class(Abstract_Ice), allocatable :: Structure

        end function Type_Ice_EXP_Construct

        module function Calculate_Ice_EXP(self, T, phi, Pw, rhoW, rhoI) result(Qice)
            implicit none
            class(Type_Ice_EXP), intent(inout) :: self
            real(real64), intent(in), optional :: T
            real(real64), intent(in), optional :: phi
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Qice

        end function Calculate_Ice_EXP

        module function Calculate_Ice_EXP_Derivative(self, T, phi, Pw, rhoW, rhoI) result(D_Qice)
            implicit none
            class(Type_Ice_EXP), intent(inout) :: self
            real(real64), intent(in), optional :: T
            real(real64), intent(in), optional :: phi
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: D_Qice

        end function Calculate_Ice_EXP_Derivative

        module subroutine Update_Ice_EXP(self, NodeBelonging, arr_T, arr_phi, arr_Pw, Density, arr_Cp, arr_Qw, arr_Qice, arr_Si)
            implicit none
            class(Type_Ice_EXP), intent(inout) :: self
            type(Belonging), intent(inout), optional :: NodeBelonging(:)
            real(real64), intent(inout), optional :: arr_T(:)
            real(real64), intent(in), optional :: arr_phi(:)
            real(real64), intent(in), optional :: arr_Pw(:)
            class(Abstract_Density), intent(in), optional :: Density
            real(real64), intent(in) :: arr_Cp(:)
            real(real64), intent(inout), optional :: arr_Qw(:)
            real(real64), intent(inout), optional :: arr_Qice(:)
            type(Variables), intent(inout), optional :: arr_Si

        end subroutine Update_Ice_EXP

        module subroutine Update_Ice_EXP_Derivative(self, NodeBelonging, arr_T, arr_phi, arr_Pw, Density, arr_Dice)
            implicit none
            class(Type_Ice_EXP), intent(inout) :: self
            type(Belonging), intent(inout), optional :: NodeBelonging(:)
            real(real64), intent(in), optional :: arr_T(:)
            real(real64), intent(in), optional :: arr_phi(:)
            real(real64), intent(in), optional :: arr_Pw(:)
            class(Abstract_Density), intent(in), optional :: Density
            real(real64), intent(inout), optional :: arr_Dice(:)

        end subroutine Update_Ice_EXP_Derivative
    end interface

    interface Type_Ice_TRM
        module procedure Construct_Type_Ice_TRM
    end interface

    interface Type_Ice_GCC
        module procedure Type_Ice_GCC_Construct
    end interface

    interface Type_Ice_EXP
        module procedure Type_Ice_EXP_Construct
    end interface

end module Calculate_Ice
