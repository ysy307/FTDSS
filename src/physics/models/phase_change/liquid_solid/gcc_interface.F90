module physics_models_gcc
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_state
    implicit none
    private

    public :: holder_gccs
    public :: abst_gcc
    public :: type_gcc_non_segregation_m
    public :: type_gcc_non_segregation_pa
    public :: type_gcc_segregation_m
    public :: type_gcc_segregation_pa

    type :: holder_gccs
        class(abst_gcc), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_gccs
    end type holder_gccs

    type, abstract :: abst_gcc
        real(real64) :: Tf !! Freezing point
        real(real64) :: Lf !! Latent heat of fusion
        real(real64), private :: TtoK = 273.15d0 !! Conversion from Celsius to Kelvin
        real(real64), private :: g = 9.80665d0 !! Gravitational acceleration
    contains
        procedure(abst_gcc_calc), pass(self), deferred :: calc
        procedure(abst_gcc_calc), pass(self), deferred :: deriv
        procedure(abst_gcc_calc), pass(self), deferred :: deriv2
    end type abst_gcc

    type, extends(abst_gcc) :: type_gcc_non_segregation_m
    contains
        procedure, pass(self) :: calc => calc_gcc_nonseg_m
        procedure, pass(self) :: deriv => deriv_gcc_nonseg_m
        procedure, pass(self) :: deriv2 => deriv_2nd_gcc_nonseg_m
    end type type_gcc_non_segregation_m

    type, extends(abst_gcc) :: type_gcc_non_segregation_pa
    contains
        procedure, pass(self) :: calc => calc_gcc_nonseg_pa
        procedure, pass(self) :: deriv => deriv_gcc_nonseg_pa
        procedure, pass(self) :: deriv2 => deriv_2nd_gcc_nonseg_pa
    end type type_gcc_non_segregation_pa

    type, extends(abst_gcc) :: type_gcc_segregation_m
    contains
        procedure, pass(self) :: calc => calc_gcc_seg_m
        procedure, pass(self) :: deriv => deriv_gcc_seg_m
        procedure, pass(self) :: deriv2 => deriv_2nd_gcc_seg_m
    end type type_gcc_segregation_m

    type, extends(abst_gcc) :: type_gcc_segregation_pa
    contains
        procedure, pass(self) :: calc => calc_gcc_seg_pa
        procedure, pass(self) :: deriv => deriv_gcc_seg_pa
        procedure, pass(self) :: deriv2 => deriv_2nd_gcc_seg_pa
    end type type_gcc_segregation_pa

    abstract interface
        pure elemental function abst_gcc_calc(self, state) result(suction)
            import :: abst_gcc, type_state, real64
            implicit none
            class(abst_gcc), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: suction

        end function abst_gcc_calc

    end interface

    interface
        module subroutine initialize_holder_gccs(self, i_material)
            implicit none
            class(holder_gccs), intent(inout) :: self
            integer(int32), intent(in) :: i_material

        end subroutine initialize_holder_gccs
    end interface

    interface
        module function construct_type_gcc_nonseg_m(Tf, Lf) result(property)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(abst_gcc), allocatable :: property

        end function construct_type_gcc_nonseg_m

        module pure elemental function calc_gcc_nonseg_m(self, state) result(suction)
            implicit none
            class(type_gcc_non_segregation_m), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: suction

        end function calc_gcc_nonseg_m

        module pure elemental function deriv_gcc_nonseg_m(self, state) result(suction_derivative)
            implicit none
            class(type_gcc_non_segregation_m), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: suction_derivative

        end function deriv_gcc_nonseg_m

        module pure elemental function deriv_2nd_gcc_nonseg_m(self, state) result(suction_derivative)
            implicit none
            class(type_gcc_non_segregation_m), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: suction_derivative

        end function deriv_2nd_gcc_nonseg_m

    end interface

    interface
        module function construct_type_gcc_nonseg_pa(Tf, Lf) result(property)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(abst_gcc), allocatable :: property

        end function construct_type_gcc_nonseg_pa

        module pure elemental function calc_gcc_nonseg_pa(self, state) result(suction)
            implicit none
            class(type_gcc_non_segregation_pa), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: suction

        end function calc_gcc_nonseg_pa

        module pure elemental function deriv_gcc_nonseg_pa(self, state) result(suction_derivative)
            implicit none
            class(type_gcc_non_segregation_pa), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: suction_derivative

        end function deriv_gcc_nonseg_pa

        module pure elemental function deriv_2nd_gcc_nonseg_pa(self, state) result(suction_derivative)
            implicit none
            class(type_gcc_non_segregation_pa), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: suction_derivative

        end function deriv_2nd_gcc_nonseg_pa

    end interface

    interface
        module function construct_type_GCC_Seg_m(Tf, Lf) result(property)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(abst_gcc), allocatable :: property

        end function construct_type_GCC_Seg_m

        module pure elemental function calc_gcc_seg_m(self, state) result(suction)
            implicit none
            class(type_gcc_segregation_m), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: suction

        end function calc_gcc_seg_m

        module pure elemental function deriv_gcc_seg_m(self, state) result(suction_derivative)
            implicit none
            class(type_gcc_segregation_m), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: suction_derivative

        end function deriv_gcc_seg_m

        module pure elemental function deriv_2nd_gcc_seg_m(self, state) result(suction_derivative)
            implicit none
            class(type_gcc_segregation_m), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: suction_derivative

        end function deriv_2nd_gcc_seg_m

    end interface

    interface
        module function construct_type_gcc_seg_pa(Tf, Lf) result(property)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(abst_gcc), allocatable :: property

        end function construct_type_gcc_seg_pa

        module pure elemental function calc_gcc_seg_pa(self, state) result(suction)
            implicit none
            class(type_gcc_segregation_pa), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: suction

        end function calc_gcc_seg_pa

        module pure elemental function deriv_gcc_seg_pa(self, state) result(suction_derivative)
            implicit none
            class(type_gcc_segregation_pa), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: suction_derivative

        end function deriv_gcc_seg_pa

        module pure elemental function deriv_2nd_gcc_seg_pa(self, state) result(suction_derivative)
            implicit none
            class(type_gcc_segregation_pa), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64) :: suction_derivative

        end function deriv_2nd_gcc_seg_pa

    end interface

    interface type_gcc_non_segregation_m
        module procedure :: construct_type_gcc_nonseg_m
    end interface

    interface type_gcc_non_segregation_pa
        module procedure :: construct_type_gcc_nonseg_pa
    end interface

    interface type_gcc_segregation_m
        module procedure :: construct_type_gcc_seg_m
    end interface

    interface type_gcc_segregation_pa
        module procedure :: construct_type_gcc_seg_pa
    end interface

end module physics_models_gcc
