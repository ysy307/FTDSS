module Calculate_HeatCapacity
    use, intrinsic :: iso_fortran_env, only: int32, real64
!     use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    use :: Core_BaseTypes
    use :: Core_Allocate, only:Allocate_Array
    use :: Inout_Input
    use :: Calculate_Ice
    use :: Calculate_GCC
    use :: Calculate_Density
    implicit none
!     !----------------------------------------------------------------------------------------------------
!     ! Access settings
!     !----------------------------------------------------------------------------------------------------
    private
    !----------------------------------------------------------------------------------------------------
    ! Public abstract types
    !----------------------------------------------------------------------------------------------------
    public :: Abstract_HeatCapacity
!     public :: Abstract_HeatCapacity_Apparent
    !----------------------------------------------------------------------------------------------------
    ! Public types
    !----------------------------------------------------------------------------------------------------
!     public :: Type_HeatCapacity_1Phase
!     public :: Type_HeatCapacity_2Phase
    public :: Type_HeatCapacity_3Phase_Apparent
!     public :: Type_HeatCapacity_4Phase
    !----------------------------------------------------------------------------------------------------

    type, abstract :: Abstract_HeatCapacity
        integer(int32) :: nsize
        integer(int32) :: numRegion
        real(real64), allocatable :: value(:, :)
    contains
        procedure(Abstract_Calculate_HeatCapacity), pass(self), deferred :: Calculate
        procedure(Abstract_Update_HeatCapacity), pass(self), deferred :: Update
    end type Abstract_HeatCapacity

!     type, abstract, extends(Abstract_HeatCapacity) :: Abstract_HeatCapacity_Apparent
!         real(real64), allocatable :: Apparent(:) ! Apparent volumetric heat capacity
!         real(real64) :: Ca_max
!     contains
!         procedure(Abstract_Calculate_HeatCapacity_Apparent), pass(self), deferred :: Calculate_Ca
!     end type Abstract_HeatCapacity_Apparent

!     type, extends(Abstract_HeatCapacity) :: Type_HeatCapacity_1Phase
!         real(real64) :: Cp1 ! Material volumetric heat capacity
!     contains
!         procedure :: Calculate => Calculate_HeatCapacity_1Phase_Wrap

!         procedure :: Update => Update_HeatCapacity_1Phase
!     end type Type_HeatCapacity_1Phase

!     type, extends(Abstract_HeatCapacity) :: Type_HeatCapacity_2Phase
!         real(real64) :: Cp1 ! Material volumetric heat capacity
!         real(real64) :: Cp2 ! Material volumetric heat capacity
!     contains
!         procedure :: Calculate => Calculate_HeatCapacity_2Phase_Wrap
!         procedure, pass(self), private :: Update_Scalar => Update_HeatCapacity_2Phase_Scalar
!         procedure, pass(self), private :: Update_Array => Update_HeatCapacity_2Phase_Array
!         generic, public :: Update => Update_Scalar, Update_Array
!     end type Type_HeatCapacity_2Phase

!     type, extends(Abstract_HeatCapacity) :: Type_HeatCapacity_3Phase
!         real(real64) :: Cp_soil ! Soil volumetric heat capacity
!         real(real64) :: Cp_water ! Water volumetric heat capacity
!         real(real64) :: Cp_ice ! Ice volumetric heat capacity
!         real(real64) :: Cp_unfrozn ! Unfrozen volumetric heat capacity
!     contains
!         procedure :: Calculate => Calculate_HeatCapacity_3Phase_Wrap
!         procedure :: Calculate_Ca => Calculate_HeatCapacity_Apparent_3Phase_Wrap
!         procedure, pass(self), private :: Update_Scalar => Update_HeatCapacity_3Phase_Scalar
!         procedure, pass(self), private :: Update_Array => Update_HeatCapacity_3Phase_Array
!         generic, public :: Update => Update_Scalar, Update_Array
!         procedure, pass(self), private :: Update_Ca_Scalar => Update_HeatCapacity_Apparent_3Phase_Scalar
!         ! procedure, pass(self), private :: Update_Ca_Scalar_Revise => Update_HeatCapacity_Apparent_3Phase_Scalar_Revise
!         procedure, pass(self), private :: Update_Ca_Array => Update_HeatCapacity_Apparent_3Phase_Array
!         generic, public :: Update_Ca => Update_Ca_Scalar, Update_Ca_Array
!         ! generic, public :: Update_Ca_Revise => Update_Ca_Scalar_Revise
!     end type Type_HeatCapacity_3Phase

    type, extends(Abstract_HeatCapacity) :: Type_HeatCapacity_3Phase_Apparent
        real(real64), allocatable :: soil(:)
        real(real64), allocatable :: water(:)
        real(real64), allocatable :: ice(:)
    contains
        procedure :: Calculate => Calc_HTC_3Phase_Wrap
        procedure :: Update => Update_HTC_3Phase
    end type Type_HeatCapacity_3Phase_Apparent

!     type, extends(Abstract_HeatCapacity_Apparent) :: Type_HeatCapacity_4Phase
!         real(real64) :: Cp_soil ! Soil volumetric heat capacity
!         real(real64) :: Cp_water ! Water volumetric heat capacity
!         real(real64) :: Cp_ice ! Ice volumetric heat capacity
!         real(real64) :: Cp_air ! Air volumetric heat capacity
!     contains
!         procedure :: Calculate => Calculate_HeatCapacity_4Phase_Wrap
!         procedure :: Calculate_Ca => Calculate_HeatCapacity_Apparent_4Phase_Wrap
!         procedure, pass(self), private :: Update_Scalar => Update_HeatCapacity_4Phase_Scalar
!         procedure, pass(self), private :: Update_Array => Update_HeatCapacity_4Phase_Array
!         generic, public :: Update => Update_Scalar, Update_Array
!         procedure, pass(self), private :: Update_Ca_Scalar => Update_HeatCapacity_Apparent_4Phase_Scalar
!         procedure, pass(self), private :: Update_Ca_Array => Update_HeatCapacity_Apparent_4Phase_Array
!         generic, public :: Update_Ca => Update_Ca_Scalar, Update_Ca_Array
!     end type Type_HeatCapacity_4Phase

!     abstract interface
!         function Abstract_Calculate_HeatCapacity(self, phi1, phi2, phi3, phi4) result(Cp)
!             use, intrinsic :: iso_fortran_env, only: real64
!             import :: Abstract_HeatCapacity
!             implicit none
!             class(Abstract_HeatCapacity), intent(in) :: self
!             real(real64), intent(in), optional :: phi1 !! the ratio of material 1
!             real(real64), intent(in), optional :: phi2 !! the ratio of material 2
!             real(real64), intent(in), optional :: phi3 !! the ratio of material 3
!             real(real64), intent(in), optional :: phi4 !! the ratio of material 4
!             real(real64) :: Cp
!         end function Abstract_Calculate_HeatCapacity

!         function Abstract_Calculate_HeatCapacity_Apparent(self, structure_Ice, phi1, phi2, phi3, phi4, rho_ice, rho_water, Temperature, Pw) result(Ca)
!             use, intrinsic :: iso_fortran_env, only: real64
!             import :: Abstract_HeatCapacity_Apparent
!             import :: Abstract_Ice
!             implicit none
!             class(Abstract_HeatCapacity_Apparent), intent(in) :: self
!             class(Abstract_Ice), intent(inout) :: structure_Ice
!             real(real64), intent(in) :: phi1 !! the ratio of material 1
!             real(real64), intent(in) :: phi2 !! the ratio of material 2
!             real(real64), intent(in) :: phi3 !! the ratio of material 3
!             real(real64), intent(in), optional :: phi4 !! the ratio of material 4
!             real(real64), intent(in) :: rho_ice !! Density of ice
!             real(real64), intent(in), optional :: rho_water !! Density of water
!             real(real64), intent(in) :: Temperature !! Temperature
!             real(real64), intent(in), optional :: Pw !! Water pressure
!             real(real64) :: Ca

!         end function Abstract_Calculate_HeatCapacity_Apparent
!     end interface

    abstract interface
        function Abstract_Calculate_HeatCapacity(self, NodeBelonging, phi1, phi2, phi3, phi4, &
                                                 Ice, Temperature, Density, Pw) result(HeatCapacity)
            import :: Abstract_HeatCapacity, Abstract_Ice, Belonging, real64, Abstract_Density
            implicit none
            class(Abstract_HeatCapacity), intent(in) :: self
            type(Belonging), intent(inout) :: NodeBelonging
            real(real64), intent(in), optional :: phi1
            real(real64), intent(in), optional :: phi2
            real(real64), intent(in), optional :: phi3
            real(real64), intent(in), optional :: phi4
            class(Abstract_Ice), intent(inout), optional :: Ice
            real(real64), intent(in), optional :: Temperature
            class(Abstract_Density), intent(inout), optional :: Density
            real(real64), intent(in), optional :: Pw
            real(real64) :: HeatCapacity

        end function Abstract_Calculate_HeatCapacity

        subroutine Abstract_Update_HeatCapacity(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4, &
                                                Ice, Temperature, Density, Pw)
            import :: Abstract_HeatCapacity, Abstract_Ice, Belonging, real64, Abstract_Density
            implicit none
            class(Abstract_HeatCapacity), intent(inout) :: self
            type(Belonging), intent(inout) :: NodeBelonging(:)
            real(real64), intent(in), optional :: arr_phi1(:)
            real(real64), intent(in), optional :: arr_phi2(:)
            real(real64), intent(in), optional :: arr_phi3(:)
            real(real64), intent(in), optional :: arr_phi4(:)
            class(Abstract_Ice), intent(inout), optional :: Ice
            real(real64), intent(in), optional :: Temperature
            class(Abstract_Density), intent(inout), optional :: Density
            real(real64), intent(in), optional :: Pw

        end subroutine Abstract_Update_HeatCapacity

    end interface

    interface
        module function HTC_3Phase_Apparent_Construct(Input) result(Structure)
            implicit none
            class(Abstract_HeatCapacity), allocatable :: Structure
            type(Type_Input), intent(in) :: Input

        end function HTC_3Phase_Apparent_Construct

        module function Calc_HTC_3Phase_Wrap(self, NodeBelonging, phi1, phi2, phi3, phi4, &
                                             Ice, Temperature, Density, Pw) result(HeatCapacity)
            implicit none
            class(Type_HeatCapacity_3Phase_Apparent), intent(in) :: self
            type(Belonging), intent(inout) :: NodeBelonging
            real(real64), intent(in), optional :: phi1
            real(real64), intent(in), optional :: phi2
            real(real64), intent(in), optional :: phi3
            real(real64), intent(in), optional :: phi4
            class(Abstract_Ice), intent(inout), optional :: Ice
            real(real64), intent(in), optional :: Temperature
            class(Abstract_Density), intent(inout), optional :: Density
            real(real64), intent(in), optional :: Pw
            real(real64) :: HeatCapacity

        end function Calc_HTC_3Phase_Wrap

        module subroutine Update_HTC_3Phase(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4, &
                                            Ice, Temperature, Density, Pw)
            implicit none
            class(Type_HeatCapacity_3Phase_Apparent), intent(inout) :: self
            type(Belonging), intent(inout) :: NodeBelonging(:)
            real(real64), intent(in), optional :: arr_phi1(:)
            real(real64), intent(in), optional :: arr_phi2(:)
            real(real64), intent(in), optional :: arr_phi3(:)
            real(real64), intent(in), optional :: arr_phi4(:)
            class(Abstract_Ice), intent(inout), optional :: Ice
            real(real64), intent(in), optional :: Temperature
            class(Abstract_Density), intent(inout), optional :: Density
            real(real64), intent(in), optional :: Pw

        end subroutine Update_HTC_3Phase
    end interface

!     interface Type_HeatCapacity_1Phase
!         procedure :: Construct_HeatCapacity_1Phase
!     end interface

!     interface Type_HeatCapacity_2Phase
!         procedure :: Construct_HeatCapacity_2Phase
!     end interface

    interface Type_HeatCapacity_3Phase_Apparent
        module procedure :: HTC_3Phase_Apparent_Construct
    end interface

!     interface Type_HeatCapacity_4Phase
!         procedure :: Construct_HeatCapacity_4Phase
!     end interface

! contains

!     !----------------------------------------------------------------------------------------------------
!     ! Constructe each type of volumetric heat capacity
!     !----------------------------------------------------------------------------------------------------
!     function Construct_HeatCapacity_1Phase(Cp1, nsize) result(structure)
!         implicit none
!         real(real64), intent(in) :: Cp1 !! Volumetric heat capacity of matarial 1
!         integer(int32), intent(in) :: nsize !! Size of array
!         type(Type_HeatCapacity_1Phase) :: structure

!         structure%Cp1 = Cp1
!         structure%nsize = nsize

!         call Allocate_Array(structure%value, nsize)
!         structure%value(:) = 0.0d0

!     end function Construct_HeatCapacity_1Phase

!     function Construct_HeatCapacity_2Phase(Cp1, Cp2, nsize) result(structure)
!         implicit none
!         real(real64), intent(in) :: Cp1 !! Volumetric heat capacity of matarial 1
!         real(real64), intent(in) :: Cp2 !! Volumetric heat capacity of matarial 2
!         integer(int32), intent(in) :: nsize !! Size of array
!         type(Type_HeatCapacity_2Phase) :: structure

!         structure%Cp1 = Cp1
!         structure%Cp2 = Cp2
!         structure%nsize = nsize

!         call Allocate_Array(structure%value, nsize)
!         structure%value(:) = 0.0d0
!     end function Construct_HeatCapacity_2Phase

!     function Construct_HeatCapacity_3Phase(structure_Ice, Cp_soil, Cp_water, Cp_ice, rho_ice, rho_water, phi, nsize) result(structure)
!         implicit none
!         class(Abstract_Ice), intent(inout) :: structure_Ice
!         real(real64), intent(in) :: Cp_soil !! Volumetric heat capacity of soil
!         real(real64), intent(in) :: Cp_water !! Volumetric heat capacity of water
!         real(real64), intent(in) :: Cp_ice !! Volumetric heat capacity of ice
!         real(real64), intent(in) :: rho_ice !! Density of ice
!         real(real64), intent(in), optional :: rho_water !! Density of water
!         real(real64), intent(in) :: phi !! Density of water
!         integer(int32), intent(in) :: nsize !! Size of array
!         type(Type_HeatCapacity_3Phase) :: structure

!         structure%Cp_soil = Cp_soil
!         structure%Cp_water = Cp_water
!         structure%Cp_ice = Cp_ice
!         structure%nsize = nsize
!         structure%Cp_unfrozn = Cp_soil * (1.0d0 - phi) + Cp_water * phi

!         call Allocate_Array(structure%value, nsize)
!         call Allocate_Array(structure%Apparent, nsize)
!         structure%value(:) = 0.0d0
!         structure%Apparent(:) = 0.0d0

!         if (.not. present(rho_water)) then
!             call Find_Ca_maximum(structure, structure_Ice, rho_ice)
!         else
!             call Find_Ca_maximum(structure, structure_Ice, rho_ice, rho_water)
!         end if

!     end function Construct_HeatCapacity_3Phase

!     function Construct_HeatCapacity_4Phase(structure_Ice, Cp_soil, Cp_water, Cp_ice, Cp_air, rho_ice, rho_water, nsize) result(structure)
!         implicit none
!         real(real64), intent(in) :: Cp_soil !! Volumetric heat capacity of soil
!         real(real64), intent(in) :: Cp_water !! Volumetric heat capacity of water
!         real(real64), intent(in) :: Cp_ice !! Volumetric heat capacity of ice
!         real(real64), intent(in) :: Cp_air !! Volumetric heat capacity of air
!         class(Abstract_Ice), pointer, intent(in) :: structure_Ice
!         real(real64), intent(in) :: rho_ice !! Density of ice
!         real(real64), intent(in), optional :: rho_water !! Density of water
!         integer(int32), intent(in) :: nsize !! Size of array
!         type(Type_HeatCapacity_4Phase) :: structure

!         structure%Cp_soil = Cp_soil
!         structure%Cp_water = Cp_water
!         structure%Cp_ice = Cp_ice
!         structure%Cp_air = Cp_air
!         structure%nsize = nsize

!         call Allocate_Array(structure%value, nsize)
!         call Allocate_Array(structure%Apparent, nsize)
!         structure%value(:) = 0.0d0
!         structure%Apparent(:) = 0.0d0

!         ! if (.not. present(rho_water)) then
!         !     call Find_Ca_maximum(structure, rho_ice)
!         ! else
!         !     call Find_Ca_maximum(structure, rho_ice, rho_water)
!         ! end if

!     end function Construct_HeatCapacity_4Phase

!     !----------------------------------------------------------------------------------------------------
!     ! Calculate Volumetric heat capacity
!     !----------------------------------------------------------------------------------------------------
!     function Calculate_HeatCapacity_1Phase(Cp1) result(Cp)
!         implicit none
!         real(real64), intent(in) :: Cp1 !! Volumetric heat capacity of matarial 1
!         real(real64) :: Cp

!         Cp = Cp1
!     end function Calculate_HeatCapacity_1Phase

!     function Calculate_HeatCapacity_2Phase(Cp1, phi1, Cp2, phi2) result(Cp)
!         implicit none
!         real(real64), intent(in) :: Cp1 !! Volumetric heat capacity of matarial 1
!         real(real64), intent(in) :: phi1 !! the ratio of matarial 1
!         real(real64), intent(in) :: Cp2 !! Volumetric heat capacity of matarial 2
!         real(real64), intent(in) :: phi2 !! the ratio of matarial 2
!         real(real64) :: Cp

!         Cp = phi1 * Cp1 + phi2 * Cp2
!     end function Calculate_HeatCapacity_2Phase

!     function Calculate_HeatCapacity_3Phase(Cp_soil, phi_soil, Cp_water, phi_water, Cp_ice, phi_ice) result(Cp)
!         implicit none
!         real(real64), intent(in) :: Cp_soil !! Volumetric heat capacity of soil
!         real(real64), intent(in) :: phi_soil !! the ratio of soil
!         real(real64), intent(in) :: Cp_water !! Volumetric heat capacity of water
!         real(real64), intent(in) :: phi_water !! the ratio of water
!         real(real64), intent(in) :: Cp_ice !! Volumetric heat capacity of ice
!         real(real64), intent(in) :: phi_ice !! the ratio of ice
!         real(real64) :: Cp

!         Cp = phi_soil * Cp_soil + phi_water * Cp_water + phi_Ice * Cp_ice
!     end function Calculate_HeatCapacity_3Phase

!     function Calculate_HeatCapacity_4Phase(Cp_soil, phi_soil, Cp_water, phi_water, Cp_ice, phi_ice, Cp_air, phi_air) result(Cp)
!         implicit none
!         real(real64), intent(in) :: Cp_soil !! Volumetric heat capacity of soil
!         real(real64), intent(in) :: phi_soil !! the ratio of soil
!         real(real64), intent(in) :: Cp_water !! Volumetric heat capacity of water
!         real(real64), intent(in) :: phi_water !! the ratio of water
!         real(real64), intent(in) :: Cp_ice !! Volumetric heat capacity of ice
!         real(real64), intent(in) :: phi_ice !! the ratio of ice
!         real(real64), intent(in) :: Cp_air !! Volumetric heat capacity of air
!         real(real64), intent(in) :: phi_air !! the ratio of air
!         real(real64) :: Cp

!         Cp = phi_soil * Cp_soil + phi_water * Cp_water + phi_Ice * Cp_ice + phi_air * Cp_air
!     end function Calculate_HeatCapacity_4Phase

!     function Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, Cp_soil, phi_soil, Cp_water, phi_water, Cp_ice, phi_ice, rho_ice, rho_water, Temperature, Pw) result(Ca)
!         implicit none
!         class(Abstract_Ice), intent(inout) :: structure_Ice
!         real(real64), intent(in) :: Cp_soil !! Volumetric heat capacity of soil
!         real(real64), intent(in) :: phi_soil !! the ratio of soil
!         real(real64), intent(in) :: Cp_water !! Volumetric heat capacity of water
!         real(real64), intent(in) :: phi_water !! the ratio of water
!         real(real64), intent(in) :: Cp_ice !! Volumetric heat capacity of ice
!         real(real64), intent(in) :: phi_ice !! the ratio of ice
!         real(real64), intent(in) :: rho_ice !! Density of ice
!         real(real64), intent(in), optional :: rho_water !! Density of water
!         real(real64), intent(in) :: Temperature !! Temperature
!         real(real64), intent(in), optional :: Pw !! Water pressure
!         real(real64) :: Ca

!         real(real64) :: Lf
!         real(real64) :: Cp

!         Cp = Calculate_HeatCapacity_3Phase(Cp_soil, phi_soil, Cp_water, phi_water, Cp_ice, phi_ice)

!         select type (Ice => structure_Ice)
!         type is (Type_Ice_GCC)
!             Lf = Ice%GCC%Lf

!             select type (structure_GCC => Ice%GCC)
!             type is (Type_GCC_NonSegregation_m)
!                 Ca = Cp - Lf * rho_ice * Ice%Calculate_Ice_Derivative(Temperature)
!             type is (Type_GCC_NonSegregation_Pa)
!                 Ca = Cp - Lf * rho_ice * Ice%Calculate_Ice_Derivative(Temperature, rho_water)
!             type is (Type_GCC_Segregation_m)
!                 Ca = Cp - Lf * rho_ice * Ice%Calculate_Ice_Derivative(Temperature, Pw, rho_water)
!             type is (Type_GCC_Segregation_Pa)
!                 Ca = Cp - Lf * rho_ice * Ice%Calculate_Ice_Derivative(Temperature, Pw, rho_water)
!             end select
!         type is (Type_Ice_EXP)
!             Lf = Ice%Lf
!             Ca = Cp - Lf * rho_ice * Ice%Calculate_Ice_Derivative(Temperature)
!         end select

!     end function Calculate_HeatCapacity_Apparent_3Phase

!     function Calculate_HeatCapacity_Apparent_4Phase(structure_Ice, Cp_soil, phi_soil, Cp_water, phi_water, Cp_ice, phi_ice, Cp_air, phi_air, rho_ice, rho_water, Temperature, Pw) result(Ca)
!         implicit none
!         class(Abstract_Ice), intent(inout) :: structure_Ice
!         real(real64), intent(in) :: Cp_soil !! Volumetric heat capacity of soil
!         real(real64), intent(in) :: phi_soil !! the ratio of soil
!         real(real64), intent(in) :: Cp_water !! Volumetric heat capacity of water
!         real(real64), intent(in) :: phi_water !! the ratio of water
!         real(real64), intent(in) :: Cp_ice !! Volumetric heat capacity of ice
!         real(real64), intent(in) :: phi_ice !! the ratio of ice
!         real(real64), intent(in) :: Cp_air !! Volumetric heat capacity of air
!         real(real64), intent(in) :: phi_air !! the ratio of air
!         real(real64), intent(in) :: rho_ice !! Density of ice
!         real(real64), intent(in), optional :: rho_water !! Density of water
!         real(real64), intent(in) :: Temperature !! Temperature
!         real(real64), intent(in), optional :: Pw !! Water pressure
!         real(real64) :: Ca

!         real(real64) :: Lf
!         real(real64) :: Cp

!         Cp = Calculate_HeatCapacity_4Phase(Cp_soil, phi_soil, Cp_water, phi_water, Cp_ice, phi_ice, Cp_air, phi_air)

!         select type (Ice => structure_Ice)
!         type is (Type_Ice_GCC)
!             Lf = Ice%GCC%Lf

!             select type (structure_GCC => Ice%GCC)
!             type is (Type_GCC_NonSegregation_m)
!                 Ca = Cp - Lf * rho_ice * Ice%Calculate_Ice_Derivative(Temperature)
!             type is (Type_GCC_NonSegregation_Pa)
!                 Ca = Cp - Lf * rho_ice * Ice%Calculate_Ice_Derivative(Temperature, rho_water)
!             type is (Type_GCC_Segregation_m)
!                 Ca = Cp - Lf * rho_ice * Ice%Calculate_Ice_Derivative(Temperature, Pw, rho_water)
!             type is (Type_GCC_Segregation_Pa)
!                 Ca = Cp - Lf * rho_ice * Ice%Calculate_Ice_Derivative(Temperature, Pw, rho_water)
!             end select
!         type is (Type_Ice_EXP)
!             Lf = Ice%Lf
!             Ca = Cp - Lf * rho_ice * Ice%Calculate_Ice_Derivative(Temperature)
!         end select

!     end function Calculate_HeatCapacity_Apparent_4Phase

!     !----------------------------------------------------------------------------------------------------
!     ! Find maximum volumetric heat capacity
!     !----------------------------------------------------------------------------------------------------
!     subroutine Find_Ca_maximum(structure, structure_Ice, rho_ice, rho_water)
!         implicit none
!         class(Abstract_HeatCapacity_Apparent), intent(inout) :: Structure
!         class(Abstract_Ice), intent(inout) :: structure_Ice
!         real(real64), intent(in) :: rho_ice !! Density of ice
!         real(real64), intent(in), optional :: rho_water !! Density of water

!         real(real64) :: initial_phi
!         real(real64) :: T0, T1, T2, T3, Ca1, Ca2, tau
!         real(real64), parameter :: epsilon = 1.0d-15

!         select type (this => Structure)
!         type is (Type_HeatCapacity_3Phase)
!             select type (this_Ice => structure_Ice)
!             type is (Type_Ice_GCC)
!                 initial_phi = this_Ice%WRF%thetaS

!                 tau = (sqrt(5.d0) - 1.d0) / 2.d0 ! golden ratio
!                 T0 = 0.0d0 ! Upper limit
!                 T3 = -1.0d0 ! Lower limit
!                 T1 = T0 + (1.d0 - tau) * (T3 - T0)
!                 T2 = T0 + tau * (T3 - T0)
!                 select type (this_GCC => this_Ice%GCC)
!                 type is (Type_GCC_NonSegregation_m)
!                     Ca1 = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                  Cp_soil=this%Cp_soil, &
!                                                                  phi_soil=1.0d0 - initial_phi, &
!                                                                  Cp_water=this%Cp_water, &
!                                                                  phi_water=initial_phi, &
!                                                                  Cp_ice=this%Cp_ice, &
!                                                                  phi_ice=0.0d0, &
!                                                                  rho_ice=rho_ice, &
!                                                                  Temperature=T1)
!                     Ca2 = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                  Cp_soil=this%Cp_soil, &
!                                                                  phi_soil=1.0d0 - initial_phi, &
!                                                                  Cp_water=this%Cp_water, &
!                                                                  phi_water=initial_phi, &
!                                                                  Cp_ice=this%Cp_ice, &
!                                                                  phi_ice=0.0d0, &
!                                                                  rho_ice=rho_ice, &
!                                                                  Temperature=T2)

!                     do while (abs(T3 - T0) > epsilon)
!                         if (Ca2 > Ca1) then
!                             T0 = T1
!                             T1 = T2
!                             T2 = T0 + tau * (T3 - T0)
!                             Ca1 = Ca2
!                             Ca2 = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                          Cp_soil=this%Cp_soil, &
!                                                                          phi_soil=1.0d0 - initial_phi, &
!                                                                          Cp_water=this%Cp_water, &
!                                                                          phi_water=initial_phi, &
!                                                                          Cp_ice=this%Cp_ice, &
!                                                                          phi_ice=0.0d0, &
!                                                                          rho_ice=rho_ice, &
!                                                                          Temperature=T2)
!                         else
!                             T3 = T2
!                             T2 = T1
!                             T1 = T0 + (1.d0 - tau) * (T3 - T0)
!                             Ca2 = Ca1
!                             Ca1 = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                          Cp_soil=this%Cp_soil, &
!                                                                          phi_soil=1.0d0 - initial_phi, &
!                                                                          Cp_water=this%Cp_water, &
!                                                                          phi_water=initial_phi, &
!                                                                          Cp_ice=this%Cp_ice, &
!                                                                          phi_ice=0.0d0, &
!                                                                          rho_ice=rho_ice, &
!                                                                          Temperature=T1)
!                         end if
!                     end do
!                     this%Ca_max = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                          Cp_soil=this%Cp_soil, &
!                                                                          phi_soil=1.0d0 - initial_phi, &
!                                                                          Cp_water=this%Cp_water, &
!                                                                          phi_water=initial_phi, &
!                                                                          Cp_ice=this%Cp_ice, &
!                                                                          phi_ice=0.0d0, &
!                                                                          rho_ice=rho_ice, &
!                                                                          Temperature=(T1 + T2) / 2.d0)
!                 type is (Type_GCC_NonSegregation_Pa)
!                     Ca1 = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                  Cp_soil=this%Cp_soil, &
!                                                                  phi_soil=1.0d0 - initial_phi, &
!                                                                  Cp_water=this%Cp_water, &
!                                                                  phi_water=initial_phi, &
!                                                                  Cp_ice=this%Cp_ice, &
!                                                                  phi_ice=0.0d0, &
!                                                                  rho_ice=rho_ice, &
!                                                                  rho_water=rho_water, &
!                                                                  Temperature=T1)
!                     Ca2 = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                  Cp_soil=this%Cp_soil, &
!                                                                  phi_soil=1.0d0 - initial_phi, &
!                                                                  Cp_water=this%Cp_water, &
!                                                                  phi_water=initial_phi, &
!                                                                  Cp_ice=this%Cp_ice, &
!                                                                  phi_ice=0.0d0, &
!                                                                  rho_ice=rho_ice, &
!                                                                  rho_water=rho_water, &
!                                                                  Temperature=T2)

!                     do while (abs(T3 - T0) > epsilon)
!                         if (Ca2 > Ca1) then
!                             T0 = T1
!                             T1 = T2
!                             T2 = T0 + tau * (T3 - T0)
!                             Ca1 = Ca2
!                             Ca2 = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                          Cp_soil=this%Cp_soil, &
!                                                                          phi_soil=1.0d0 - initial_phi, &
!                                                                          Cp_water=this%Cp_water, &
!                                                                          phi_water=initial_phi, &
!                                                                          Cp_ice=this%Cp_ice, &
!                                                                          phi_ice=0.0d0, &
!                                                                          rho_ice=rho_ice, &
!                                                                          rho_water=rho_water, &
!                                                                          Temperature=T2)
!                         else
!                             T3 = T2
!                             T2 = T1
!                             T1 = T0 + (1.d0 - tau) * (T3 - T0)
!                             Ca2 = Ca1
!                             Ca1 = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                          Cp_soil=this%Cp_soil, &
!                                                                          phi_soil=1.0d0 - initial_phi, &
!                                                                          Cp_water=this%Cp_water, &
!                                                                          phi_water=initial_phi, &
!                                                                          Cp_ice=this%Cp_ice, &
!                                                                          phi_ice=0.0d0, &
!                                                                          rho_ice=rho_ice, &
!                                                                          rho_water=rho_water, &
!                                                                          Temperature=T1)
!                         end if
!                     end do
!                     this%Ca_max = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                          Cp_soil=this%Cp_soil, &
!                                                                          phi_soil=1.0d0 - initial_phi, &
!                                                                          Cp_water=this%Cp_water, &
!                                                                          phi_water=initial_phi, &
!                                                                          Cp_ice=this%Cp_ice, &
!                                                                          phi_ice=0.0d0, &
!                                                                          rho_ice=rho_ice, &
!                                                                          rho_water=rho_water, &
!                                                                          Temperature=(T1 + T2) / 2.d0)
!                 type is (Type_GCC_Segregation_m)
!                     Ca1 = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                  Cp_soil=this%Cp_soil, &
!                                                                  phi_soil=1.0d0 - initial_phi, &
!                                                                  Cp_water=this%Cp_water, &
!                                                                  phi_water=initial_phi, &
!                                                                  Cp_ice=this%Cp_ice, &
!                                                                  phi_ice=0.0d0, &
!                                                                  rho_ice=rho_ice, &
!                                                                  Temperature=T1)
!                     Ca2 = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                  Cp_soil=this%Cp_soil, &
!                                                                  phi_soil=1.0d0 - initial_phi, &
!                                                                  Cp_water=this%Cp_water, &
!                                                                  phi_water=initial_phi, &
!                                                                  Cp_ice=this%Cp_ice, &
!                                                                  phi_ice=0.0d0, &
!                                                                  rho_ice=rho_ice, &
!                                                                  Temperature=T2)
!                     do while (abs(T3 - T0) > epsilon)
!                         if (Ca2 > Ca1) then
!                             T0 = T1
!                             T1 = T2
!                             T2 = T0 + tau * (T3 - T0)
!                             Ca1 = Ca2
!                             Ca2 = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                          Cp_soil=this%Cp_soil, &
!                                                                          phi_soil=1.0d0 - initial_phi, &
!                                                                          Cp_water=this%Cp_water, &
!                                                                          phi_water=initial_phi, &
!                                                                          Cp_ice=this%Cp_ice, &
!                                                                          phi_ice=0.0d0, &
!                                                                          rho_ice=rho_ice, &
!                                                                          Temperature=T2)
!                         else
!                             T3 = T2
!                             T2 = T1
!                             T1 = T0 + (1.d0 - tau) * (T3 - T0)
!                             Ca2 = Ca1
!                             Ca1 = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                          Cp_soil=this%Cp_soil, &
!                                                                          phi_soil=1.0d0 - initial_phi, &
!                                                                          Cp_water=this%Cp_water, &
!                                                                          phi_water=initial_phi, &
!                                                                          Cp_ice=this%Cp_ice, &
!                                                                          phi_ice=0.0d0, &
!                                                                          rho_ice=rho_ice, &
!                                                                          Temperature=T1)
!                         end if
!                     end do
!                     this%Ca_max = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                          Cp_soil=this%Cp_soil, &
!                                                                          phi_soil=1.0d0 - initial_phi, &
!                                                                          Cp_water=this%Cp_water, &
!                                                                          phi_water=initial_phi, &
!                                                                          Cp_ice=this%Cp_ice, &
!                                                                          phi_ice=0.0d0, &
!                                                                          rho_ice=rho_ice, &
!                                                                          Temperature=(T1 + T2) / 2.d0)
!                 type is (Type_GCC_Segregation_Pa)
!                     Ca1 = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                  Cp_soil=this%Cp_soil, &
!                                                                  phi_soil=1.0d0 - initial_phi, &
!                                                                  Cp_water=this%Cp_water, &
!                                                                  phi_water=initial_phi, &
!                                                                  Cp_ice=this%Cp_ice, &
!                                                                  phi_ice=0.0d0, &
!                                                                  rho_ice=rho_ice, &
!                                                                  rho_water=rho_water, &
!                                                                  Temperature=T1)
!                     Ca2 = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                  Cp_soil=this%Cp_soil, &
!                                                                  phi_soil=1.0d0 - initial_phi, &
!                                                                  Cp_water=this%Cp_water, &
!                                                                  phi_water=initial_phi, &
!                                                                  Cp_ice=this%Cp_ice, &
!                                                                  phi_ice=0.0d0, &
!                                                                  rho_ice=rho_ice, &
!                                                                  rho_water=rho_water, &
!                                                                  Temperature=T2)
!                     do while (abs(T3 - T0) > epsilon)
!                         if (Ca2 > Ca1) then
!                             T0 = T1
!                             T1 = T2
!                             T2 = T0 + tau * (T3 - T0)
!                             Ca1 = Ca2
!                             Ca2 = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                          Cp_soil=this%Cp_soil, &
!                                                                          phi_soil=1.0d0 - initial_phi, &
!                                                                          Cp_water=this%Cp_water, &
!                                                                          phi_water=initial_phi, &
!                                                                          Cp_ice=this%Cp_ice, &
!                                                                          phi_ice=0.0d0, &
!                                                                          rho_ice=rho_ice, &
!                                                                          rho_water=rho_water, &
!                                                                          Temperature=T2)
!                         else
!                             T3 = T2
!                             T2 = T1
!                             T1 = T0 + (1.d0 - tau) * (T3 - T0)
!                             Ca2 = Ca1
!                             Ca1 = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                          Cp_soil=this%Cp_soil, &
!                                                                          phi_soil=1.0d0 - initial_phi, &
!                                                                          Cp_water=this%Cp_water, &
!                                                                          phi_water=initial_phi, &
!                                                                          Cp_ice=this%Cp_ice, &
!                                                                          phi_ice=0.0d0, &
!                                                                          rho_ice=rho_ice, &
!                                                                          rho_water=rho_water, &
!                                                                          Temperature=T1)
!                         end if
!                     end do
!                     this%Ca_max = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                          Cp_soil=this%Cp_soil, &
!                                                                          phi_soil=1.0d0 - initial_phi, &
!                                                                          Cp_water=this%Cp_water, &
!                                                                          phi_water=initial_phi, &
!                                                                          Cp_ice=this%Cp_ice, &
!                                                                          phi_ice=0.0d0, &
!                                                                          rho_ice=rho_ice, &
!                                                                          rho_water=rho_water, &
!                                                                          Temperature=(T1 + T2) / 2.d0)
!                 end select
!             type is (Type_Ice_EXP)
!                 initial_phi = this_Ice%phi
!                 this%Ca_max = Calculate_HeatCapacity_Apparent_3Phase(structure_Ice, &
!                                                                      Cp_soil=this%Cp_soil, &
!                                                                      phi_soil=1.0d0 - initial_phi, &
!                                                                      Cp_water=this%Cp_water, &
!                                                                      phi_water=initial_phi, &
!                                                                      Cp_ice=this%Cp_ice, &
!                                                                      phi_ice=0.0d0, &
!                                                                      rho_ice=rho_ice, &
!                                                                      Temperature=this_Ice%Tf)
!             end select
!         end select

!     end subroutine Find_Ca_maximum

!     !----------------------------------------------------------------------------------------------------
!     ! Wrapper of calculating volumetric heat capacity
!     !----------------------------------------------------------------------------------------------------
!     function Calculate_HeatCapacity_1Phase_Wrap(self, phi1, phi2, phi3, phi4) result(Cp)
!         implicit none
!         class(Type_HeatCapacity_1Phase), intent(in) :: self
!         real(real64), intent(in), optional :: phi1 !! the ratio of material 1
!         real(real64), intent(in), optional :: phi2 !! the ratio of material 2
!         real(real64), intent(in), optional :: phi3 !! the ratio of material 3
!         real(real64), intent(in), optional :: phi4 !! the ratio of material 4
!         real(real64) :: Cp

!         Cp = Calculate_HeatCapacity_1Phase(self%Cp1)
!     end function Calculate_HeatCapacity_1Phase_Wrap

!     function Calculate_HeatCapacity_2Phase_Wrap(self, phi1, phi2, phi3, phi4) result(Cp)
!         implicit none
!         class(Type_HeatCapacity_2Phase), intent(in) :: self
!         real(real64), intent(in), optional :: phi1 !! the ratio of material 1
!         real(real64), intent(in), optional :: phi2 !! the ratio of material 2
!         real(real64), intent(in), optional :: phi3 !! the ratio of material 3
!         real(real64), intent(in), optional :: phi4 !! the ratio of material 4
!         real(real64) :: Cp

!         if (.not. present(phi1) .or. &
!             .not. present(phi2) &
!             ) stop 'Error: Calculate_HeatCapacity_2Phase_Wrap'

!         Cp = Calculate_HeatCapacity_2Phase(self%Cp1, phi1, self%Cp2, phi2)
!     end function Calculate_HeatCapacity_2Phase_Wrap

!     function Calculate_HeatCapacity_3Phase_Wrap(self, phi1, phi2, phi3, phi4) result(Cp)
!         implicit none
!         class(Type_HeatCapacity_3Phase), intent(in) :: self
!         real(real64), intent(in), optional :: phi1 !! the ratio of material 1
!         real(real64), intent(in), optional :: phi2 !! the ratio of material 2
!         real(real64), intent(in), optional :: phi3 !! the ratio of material 3
!         real(real64), intent(in), optional :: phi4 !! the ratio of material 4
!         real(real64) :: Cp

!         if (.not. present(phi1) .or. &
!             .not. present(phi2) .or. &
!             .not. present(phi3) &
!             ) stop 'Error: Calculate_HeatCapacity_3Phase_Wrap'

!         Cp = Calculate_HeatCapacity_3Phase(self%Cp_soil, phi1, self%Cp_water, phi2, self%Cp_ice, phi3)
!     end function Calculate_HeatCapacity_3Phase_Wrap

!     function Calculate_HeatCapacity_4Phase_Wrap(self, phi1, phi2, phi3, phi4) result(Cp)
!         implicit none
!         class(Type_HeatCapacity_4Phase), intent(in) :: self
!         real(real64), intent(in), optional :: phi1 !! the ratio of material 1
!         real(real64), intent(in), optional :: phi2 !! the ratio of material 2
!         real(real64), intent(in), optional :: phi3 !! the ratio of material 3
!         real(real64), intent(in), optional :: phi4 !! the ratio of material 4
!         real(real64) :: Cp

!         if (.not. present(phi1) .or. &
!             .not. present(phi2) .or. &
!             .not. present(phi3) .or. &
!             .not. present(phi4) &
!             ) stop 'Error: Calculate_HeatCapacity_4Phase_Wrap'

!         Cp = Calculate_HeatCapacity_4Phase(self%Cp_soil, phi1, self%Cp_water, phi2, self%Cp_ice, phi3, self%Cp_air, phi4)
!     end function Calculate_HeatCapacity_4Phase_Wrap

!     !----------------------------------------------------------------------------------------------------
!     ! Wrapper of calculating Apparent volumetric heat capacity
!     !----------------------------------------------------------------------------------------------------
!     function Calculate_HeatCapacity_Apparent_3Phase_Wrap(self, structure_Ice, phi1, phi2, phi3, phi4, rho_ice, rho_water, Temperature, Pw) result(Ca)
!         implicit none
!         class(Type_HeatCapacity_3Phase), intent(in) :: self
!         class(Abstract_Ice), intent(inout) :: structure_Ice
!         real(real64), intent(in) :: phi1 !! the ratio of material 1
!         real(real64), intent(in) :: phi2 !! the ratio of material 2
!         real(real64), intent(in) :: phi3 !! the ratio of material 3
!         real(real64), intent(in), optional :: phi4 !! the ratio of material 4
!         real(real64), intent(in) :: rho_ice !! Density of ice
!         real(real64), intent(in), optional :: rho_water !! Density of water
!         real(real64), intent(in) :: Temperature !! Temperature
!         real(real64), intent(in), optional :: Pw !! Water pressure
!         real(real64) :: Ca

!         if (.not. present(rho_water) .and. .not. present(Pw)) then
!             Ca = Calculate_HeatCapacity_Apparent_3Phase(Cp_soil=self%Cp_soil, &
!                                                         phi_soil=phi1, &
!                                                         Cp_water=self%Cp_water, &
!                                                         phi_water=phi2, &
!                                                         Cp_ice=self%Cp_ice, &
!                                                         phi_ice=phi3, &
!                                                         structure_Ice=structure_Ice, &
!                                                         rho_ice=rho_ice, &
!                                                         Temperature=Temperature)
!         else if (present(rho_water) .and. .not. present(Pw)) then
!             Ca = Calculate_HeatCapacity_Apparent_3Phase(Cp_soil=self%Cp_soil, &
!                                                         phi_soil=phi1, &
!                                                         Cp_water=self%Cp_water, &
!                                                         phi_water=phi2, &
!                                                         Cp_ice=self%Cp_ice, &
!                                                         phi_ice=phi3, &
!                                                         structure_Ice=structure_Ice, &
!                                                         rho_ice=rho_ice, &
!                                                         rho_water=rho_water, &
!                                                         Temperature=Temperature)
!         else if (present(rho_water) .and. present(Pw)) then
!             Ca = Calculate_HeatCapacity_Apparent_3Phase(Cp_soil=self%Cp_soil, &
!                                                         phi_soil=phi1, &
!                                                         Cp_water=self%Cp_water, &
!                                                         phi_water=phi2, &
!                                                         Cp_ice=self%Cp_ice, &
!                                                         phi_ice=phi3, &
!                                                         structure_Ice=structure_Ice, &
!                                                         rho_ice=rho_ice, &
!                                                         rho_water=rho_water, &
!                                                         Temperature=Temperature, &
!                                                         Pw=Pw)
!         end if

!     end function Calculate_HeatCapacity_Apparent_3Phase_Wrap

!     function Calculate_HeatCapacity_Apparent_4Phase_Wrap(self, structure_Ice, phi1, phi2, phi3, phi4, rho_ice, rho_water, Temperature, Pw) result(Ca)
!         implicit none
!         class(Type_HeatCapacity_4Phase), intent(in) :: self
!         class(Abstract_Ice), intent(inout) :: structure_Ice
!         real(real64), intent(in) :: phi1 !! the ratio of material 1
!         real(real64), intent(in) :: phi2 !! the ratio of material 2
!         real(real64), intent(in) :: phi3 !! the ratio of material 3
!         real(real64), intent(in), optional :: phi4 !! the ratio of material 4
!         real(real64), intent(in) :: rho_ice !! Density of ice
!         real(real64), intent(in), optional :: rho_water !! Density of water
!         real(real64), intent(in) :: Temperature !! Temperature
!         real(real64), intent(in), optional :: Pw !! Water pressure
!         real(real64) :: Ca

!         if (.not. present(rho_water) .and. .not. present(Pw)) then
!             Ca = Calculate_HeatCapacity_Apparent_4Phase(Cp_soil=self%Cp_soil, &
!                                                         phi_soil=phi1, &
!                                                         Cp_water=self%Cp_water, &
!                                                         phi_water=phi2, &
!                                                         Cp_ice=self%Cp_ice, &
!                                                         phi_ice=phi3, &
!                                                         Cp_air=self%Cp_air, &
!                                                         phi_air=phi4, &
!                                                         structure_Ice=structure_Ice, &
!                                                         rho_ice=rho_ice, &
!                                                         Temperature=Temperature)
!         else if (present(rho_water) .and. .not. present(Pw)) then
!             Ca = Calculate_HeatCapacity_Apparent_4Phase(Cp_soil=self%Cp_soil, &
!                                                         phi_soil=phi1, &
!                                                         Cp_water=self%Cp_water, &
!                                                         phi_water=phi2, &
!                                                         Cp_ice=self%Cp_ice, &
!                                                         phi_ice=phi3, &
!                                                         Cp_air=self%Cp_air, &
!                                                         phi_air=phi4, &
!                                                         structure_Ice=structure_Ice, &
!                                                         rho_ice=rho_ice, &
!                                                         rho_water=rho_water, &
!                                                         Temperature=Temperature)
!         else if (present(rho_water) .and. present(Pw)) then
!             Ca = Calculate_HeatCapacity_Apparent_4Phase(Cp_soil=self%Cp_soil, &
!                                                         phi_soil=phi1, &
!                                                         Cp_water=self%Cp_water, &
!                                                         phi_water=phi2, &
!                                                         Cp_ice=self%Cp_ice, &
!                                                         phi_ice=phi3, &
!                                                         Cp_air=self%Cp_air, &
!                                                         phi_air=phi4, &
!                                                         structure_Ice=structure_Ice, &
!                                                         rho_ice=rho_ice, &
!                                                         rho_water=rho_water, &
!                                                         Temperature=Temperature, &
!                                                         Pw=Pw)
!         end if

!     end function Calculate_HeatCapacity_Apparent_4Phase_Wrap

!     !----------------------------------------------------------------------------------------------------
!     ! Update volumetric heat capacity
!     !----------------------------------------------------------------------------------------------------
!     subroutine Update_HeatCapacity_1Phase(self)
!         implicit none
!         class(Type_HeatCapacity_1Phase), intent(inout) :: self

!         self%value(:) = self%Cp1
!     end subroutine Update_HeatCapacity_1Phase

!     subroutine Update_HeatCapacity_2Phase_Scalar(self, phi1)
!         implicit none
!         class(Type_HeatCapacity_2Phase), intent(inout) :: self
!         real(real64), intent(in) :: phi1 !! the ratio of matarial 1

!         integer(int32) :: iN

!         !$omp parallel do private(iN)
!         do iN = 1, self%nsize
!             self%value(iN) = self%Cp1 * phi1 &
!                              + self%Cp2 * (1.0d0 - phi1)
!         end do
!         !$omp end parallel do
!     end subroutine Update_HeatCapacity_2Phase_Scalar

!     subroutine Update_HeatCapacity_2Phase_Array(self, arr_phi1)
!         implicit none
!         class(Type_HeatCapacity_2Phase), intent(inout) :: self
!         real(real64), intent(in) :: arr_phi1(:) !! the ratio of matarial 1

!         integer(int32) :: iN

!         !$omp parallel do private(iN)
!         do iN = 1, self%nsize
!             self%value(iN) = self%Cp1 * arr_phi1(iN) &
!                              + self%Cp2 * (1.0d0 - arr_phi1(iN))
!         end do
!         !$omp end parallel do
!     end subroutine Update_HeatCapacity_2Phase_Array

!     subroutine Update_HeatCapacity_3Phase_Scalar(self, phi_soil, arr_Qw, arr_Qice)
!         implicit none
!         class(Type_HeatCapacity_3Phase), intent(inout) :: self
!         real(real64), intent(in) :: phi_soil !! the ratio of soil
!         real(real64), intent(in) :: arr_Qw(:) !! the ratio of water
!         real(real64), intent(in) :: arr_Qice(:) !! the ratio of ice

!         integer(int32) :: iN

!         ! $omp parallel do private(iN)
!         do iN = 1, self%nsize
!             self%value(iN) = self%Cp_soil * (1.0d0 - phi_soil) &
!                              + self%Cp_water * arr_Qw(iN) &
!                              + self%Cp_ice * arr_Qice(iN)
!         end do
!         ! $omp end parallel do
!     end subroutine Update_HeatCapacity_3Phase_Scalar

!     subroutine Update_HeatCapacity_3Phase_Array(self, arr_phi_soil, arr_Qw, arr_Qice)
!         implicit none
!         class(Type_HeatCapacity_3Phase), intent(inout) :: self
!         real(real64), intent(in) :: arr_phi_soil(:) !! the ratio of soil
!         real(real64), intent(in) :: arr_Qw(:) !! the ratio of water
!         real(real64), intent(in) :: arr_Qice(:) !! the ratio of ice

!         integer(int32) :: iN

!         !$omp parallel do private(iN)
!         do iN = 1, self%nsize
!             self%value(iN) = self%Cp_soil * (1.0d0 - arr_phi_soil(iN)) &
!                              + self%Cp_water * arr_Qw(iN) &
!                              + self%Cp_ice * arr_Qice(iN)
!         end do
!         !$omp end parallel do
!     end subroutine Update_HeatCapacity_3Phase_Array

!     subroutine Update_HeatCapacity_4Phase_Scalar(self, phi_soil, arr_Qw, arr_Qice)
!         implicit none
!         class(Type_HeatCapacity_4Phase), intent(inout) :: self
!         real(real64), intent(in) :: phi_soil !! the ratio of soil
!         real(real64), intent(in) :: arr_Qw(:) !! the ratio of water
!         real(real64), intent(in) :: arr_Qice(:) !! the ratio of ice

!         integer(int32) :: iN

!         !$omp parallel do private(iN)
!         do iN = 1, self%nsize
!             self%value(iN) = self%Cp_soil * (1.0d0 - phi_soil) &
!                              + self%Cp_water * arr_Qw(iN) &
!                              + self%Cp_ice * arr_Qice(iN) &
!                              + self%Cp_air * (1.0d0 - phi_soil - arr_Qw(iN) - arr_Qice(iN))
!         end do
!         !$omp end parallel do
!     end subroutine Update_HeatCapacity_4Phase_Scalar

!     subroutine Update_HeatCapacity_4Phase_Array(self, arr_phi_soil, arr_Qw, arr_Qice)
!         implicit none
!         class(Type_HeatCapacity_4Phase), intent(inout) :: self
!         real(real64), intent(in) :: arr_phi_soil(:) !! the ratio of soil
!         real(real64), intent(in) :: arr_Qw(:) !! the ratio of water
!         real(real64), intent(in) :: arr_Qice(:) !! the ratio of ice

!         integer(int32) :: iN

!         !$omp parallel do private(iN)
!         do iN = 1, self%nsize
!             self%value(iN) = self%Cp_soil * (1.0d0 - arr_phi_soil(iN)) &
!                              + self%Cp_water * arr_Qw(iN) &
!                              + self%Cp_ice * arr_Qice(iN) &
!                              + self%Cp_air * (1.0d0 - arr_phi_soil(iN) - arr_Qw(iN) - arr_Qice(iN))
!         end do
!         !$omp end parallel do
!     end subroutine Update_HeatCapacity_4Phase_Array

!     subroutine Update_HeatCapacity_Apparent_3Phase_Scalar(self, structure_Ice, rho_ice, rho_water, arr_Temperature, arr_Pw)
!         implicit none
!         class(Type_HeatCapacity_3Phase), intent(inout) :: self
!         class(Abstract_Ice), intent(inout) :: structure_Ice
!         real(real64), intent(in) :: rho_ice !! Density of ice
!         real(real64), intent(in), optional :: rho_water !! Density of water
!         real(real64), intent(in) :: arr_Temperature(:) !! Temperature
!         real(real64), intent(in), optional :: arr_Pw(:) !! Water pressure

!         real(real64) :: Lf
!         integer(int32) :: iN

!         select type (Ice => structure_Ice)
!         type is (Type_Ice_GCC)
!             Lf = Ice%GCC%Lf

!             select type (structure_GCC => Ice%GCC)
!             type is (Type_GCC_NonSegregation_m)
!                 !$omp parallel do private(iN)
!                 do iN = 1, self%nsize
!                     self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN))
!                 end do
!                 !$omp end parallel do
!             type is (Type_GCC_NonSegregation_Pa)
!                 !$omp parallel do private(iN)
!                 do iN = 1, self%nsize
!                     self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN), rho_water)
!                 end do
!                 !$omp end parallel do
!             type is (Type_GCC_Segregation_m)
!                 !$omp parallel do private(iN)
!                 do iN = 1, self%nsize
!                     self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN), arr_Pw(iN), rho_water)
!                 end do
!                 !$omp end parallel do
!             type is (Type_GCC_Segregation_Pa)
!                 !$omp parallel do private(iN)
!                 do iN = 1, self%nsize
!                     self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN), arr_Pw(iN), rho_water)
!                 end do
!                 !$omp end parallel do
!             end select
!         type is (Type_Ice_EXP)
!             Lf = Ice%Lf
!             !$omp parallel do private(iN)
!             do iN = 1, self%nsize
!                 self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN))
!             end do
!             !$omp end parallel do
!         end select

!     end subroutine Update_HeatCapacity_Apparent_3Phase_Scalar

!     ! subroutine Update_HeatCapacity_Apparent_3Phase_Scalar_Revise(self, structure_Ice, rho_ice, rho_water, arr_Temperature, arr_Temperature_old, arr_Pw)
!     !     implicit none
!     !     class(Type_HeatCapacity_3Phase), intent(inout) :: self
!     !     class(Abstract_Ice), intent(inout) :: structure_Ice
!     !     real(real64), intent(in) :: rho_ice !! Density of ice
!     !     real(real64), intent(in), optional :: rho_water !! Density of water
!     !     real(real64), intent(in) :: arr_Temperature(:) !! Temperature
!     !     real(real64), intent(in) :: arr_Temperature_old(:) !! Temperature
!     !     real(real64), intent(in), optional :: arr_Pw(:) !! Water pressure

!     !     real(real64) :: Lf, Tp, To
!     !     real(real64) :: x0, x1, x2, Tnew, f0, f1, err
!     !     real(real64) :: eps, epsilon
!     !     integer(int32) :: max_iter = 1000
!     !     integer(int32) :: iN, iter

!     !     select type (Ice => structure_Ice)
!     !     type is (Type_Ice_GCC)
!     !         Lf = Ice%GCC%Lf

!     !         select type (structure_GCC => Ice%GCC)
!     !         type is (Type_GCC_NonSegregation_m)
!     !             !$omp parallel do private(iN, iter, x0, x1, x2, Tnew, f0, f1, err)
!     !             do iN = 1, self%nsize
!     !                 Tp = arr_Temperature(iN)
!     !                 To = arr_Temperature_old(iN)
!     !                 if (Tp >= 0.0d0 .and. To >= 0.0d0) then
!     !                     self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN))
!     !                 else if (Tp < 0.0d0 .and. To < 0.0d0) then
!     !                     self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN))
!     !                 else if (Tp < 0.0d0 .and. To >= 0.0d0) then

!     !                     x0 = 0.0d0
!     !                     x1 = arr_Temperature(iN)
!     !                     Tnew = arr_Temperature(iN)
!     !                     do iter = 1, max_iter
!     !                         f0 = self%Cp_unfrozn * (x0 - Tnew) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(x0)
!     !                         f1 = self%Cp_unfrozn * (x1 - Tnew) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(x1)
!     !                         if (abs(f1 - f0) < epsilon(1.0d0)) then
!     !                             print *, "Secant_method_GCC: f1 - f0 is too small"
!     !                             stop
!     !                         else
!     !                             err = f1 * (x1 - x0) / (f1 - f0)
!     !                             x2 = x1 - err
!     !                             if (abs(err) < eps) exit
!     !                             x0 = x1
!     !                             x1 = x2
!     !                         end if
!     !                     end do
!     !                     if (iter >= max_iter) then
!     !                         write (*, "(a)"), "Secant_method_GCC: iteration limit exceeded"
!     !                         stop
!     !                     end if
!     !                     if (ieee_is_nan(x2)) x2 = 0.0d0

!     !                     self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(x2)
!     !                 else
!     !                     self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN))
!     !                 end if

!     !             end do
!     !             !$omp end parallel do
!     !         type is (Type_GCC_NonSegregation_Pa)
!     !             !$omp parallel do private(iN)
!     !             do iN = 1, self%nsize
!     !                 self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN), rho_water)
!     !             end do
!     !             !$omp end parallel do
!     !         type is (Type_GCC_Segregation_m)
!     !             !$omp parallel do private(iN)
!     !             do iN = 1, self%nsize
!     !                 self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN), arr_Pw(iN), rho_water)
!     !             end do
!     !             !$omp end parallel do
!     !         type is (Type_GCC_Segregation_Pa)
!     !             !$omp parallel do private(iN)
!     !             do iN = 1, self%nsize
!     !                 self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN), arr_Pw(iN), rho_water)
!     !             end do
!     !             !$omp end parallel do
!     !         end select
!     !     type is (Type_Ice_EXP)
!     !         Lf = Ice%Lf
!     !         !$omp parallel do private(iN)
!     !         do iN = 1, self%nsize
!     !             self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN))
!     !         end do
!     !         !$omp end parallel do
!     !     end select

!     ! end subroutine Update_HeatCapacity_Apparent_3Phase_Scalar_Revise

!     subroutine Update_HeatCapacity_Apparent_3Phase_Array(self, structure_Ice, arr_rho_ice, arr_rho_water, arr_Temperature, arr_Pw)
!         implicit none
!         class(Type_HeatCapacity_3Phase), intent(inout) :: self
!         class(Abstract_Ice), intent(inout) :: structure_Ice
!         real(real64), intent(in) :: arr_rho_ice(:) !! Density of ice
!         real(real64), intent(in), optional :: arr_rho_water(:) !! Density of water
!         real(real64), intent(in) :: arr_Temperature(:) !! Temperature
!         real(real64), intent(in), optional :: arr_Pw(:) !! Water pressure

!         real(real64) :: Lf
!         integer(int32) :: iN

!         select type (Ice => structure_Ice)
!         type is (Type_Ice_GCC)
!             Lf = Ice%GCC%Lf

!             select type (structure_GCC => Ice%GCC)
!             type is (Type_GCC_NonSegregation_m)
!                 !$omp parallel do private(iN)
!                 do iN = 1, self%nsize
!                     self%Apparent(iN) = self%value(iN) - Lf * arr_rho_ice(iN) * Ice%Calculate_Ice_Derivative(arr_Temperature(iN))
!                 end do
!                 !$omp end parallel do
!             type is (Type_GCC_NonSegregation_Pa)
!                 !$omp parallel do private(iN)
!                 do iN = 1, self%nsize
!                     self%Apparent(iN) = self%value(iN) - Lf * arr_rho_ice(iN) * Ice%Calculate_Ice_Derivative(arr_Temperature(iN), arr_rho_water(iN))
!                 end do
!                 !$omp end parallel do
!             type is (Type_GCC_Segregation_m)
!                 !$omp parallel do private(iN)
!                 do iN = 1, self%nsize
!                     self%Apparent(iN) = self%value(iN) - Lf * arr_rho_ice(iN) * Ice%Calculate_Ice_Derivative(arr_Temperature(iN), arr_Pw(iN), arr_rho_water(iN))
!                 end do
!                 !$omp end parallel do
!             type is (Type_GCC_Segregation_Pa)
!                 !$omp parallel do private(iN)
!                 do iN = 1, self%nsize
!                     self%Apparent(iN) = self%value(iN) - Lf * arr_rho_ice(iN) * Ice%Calculate_Ice_Derivative(arr_Temperature(iN), arr_Pw(iN), arr_rho_water(iN))
!                 end do
!                 !$omp end parallel do
!             end select
!         type is (Type_Ice_EXP)
!             Lf = Ice%Lf
!             !$omp parallel do private(iN)
!             do iN = 1, self%nsize
!                 self%Apparent(iN) = self%value(iN) - Lf * arr_rho_ice(iN) * Ice%Calculate_Ice_Derivative(arr_Temperature(iN))
!             end do
!             !$omp end parallel do
!         end select

!     end subroutine Update_HeatCapacity_Apparent_3Phase_Array

!     subroutine Update_HeatCapacity_Apparent_4Phase_Scalar(self, structure_Ice, rho_ice, rho_water, arr_Temperature, arr_Pw)
!         implicit none
!         class(Type_HeatCapacity_4Phase), intent(inout) :: self
!         class(Abstract_Ice), intent(inout) :: structure_Ice
!         real(real64), intent(in) :: rho_ice !! Density of ice
!         real(real64), intent(in), optional :: rho_water !! Density of water
!         real(real64), intent(in) :: arr_Temperature(:) !! Temperature
!         real(real64), intent(in), optional :: arr_Pw(:) !! Water pressure

!         real(real64) :: Lf
!         integer(int32) :: iN

!         select type (Ice => structure_Ice)
!         type is (Type_Ice_GCC)
!             Lf = Ice%GCC%Lf

!             select type (structure_GCC => Ice%GCC)
!             type is (Type_GCC_NonSegregation_m)
!                 !$omp parallel do private(iN)
!                 do iN = 1, self%nsize
!                     self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN))
!                 end do
!                 !$omp end parallel do
!             type is (Type_GCC_NonSegregation_Pa)
!                 !$omp parallel do private(iN)
!                 do iN = 1, self%nsize
!                     self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN), rho_water)
!                 end do
!                 !$omp end parallel do
!             type is (Type_GCC_Segregation_m)
!                 !$omp parallel do private(iN)
!                 do iN = 1, self%nsize
!                     self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN), arr_Pw(iN), rho_water)
!                 end do
!                 !$omp end parallel do
!             type is (Type_GCC_Segregation_Pa)
!                 !$omp parallel do private(iN)
!                 do iN = 1, self%nsize
!                     self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN), arr_Pw(iN), rho_water)
!                 end do
!                 !$omp end parallel do
!             end select
!         type is (Type_Ice_EXP)
!             Lf = Ice%Lf
!             !$omp parallel do private(iN)
!             do iN = 1, self%nsize
!                 self%Apparent(iN) = self%value(iN) - Lf * rho_ice * Ice%Calculate_Ice_Derivative(arr_Temperature(iN))
!             end do
!             !$omp end parallel do
!         end select

!     end subroutine Update_HeatCapacity_Apparent_4Phase_Scalar

!     subroutine Update_HeatCapacity_Apparent_4Phase_Array(self, structure_Ice, arr_rho_ice, arr_rho_water, arr_Temperature, arr_Pw)
!         implicit none
!         class(Type_HeatCapacity_4Phase), intent(inout) :: self
!         class(Abstract_Ice), intent(inout) :: structure_Ice
!         real(real64), intent(in) :: arr_rho_ice(:) !! Density of ice
!         real(real64), intent(in), optional :: arr_rho_water(:) !! Density of water
!         real(real64), intent(in) :: arr_Temperature(:) !! Temperature
!         real(real64), intent(in), optional :: arr_Pw(:) !! Water pressure

!         real(real64) :: Lf
!         integer(int32) :: iN

!         select type (Ice => structure_Ice)
!         type is (Type_Ice_GCC)
!             Lf = Ice%GCC%Lf

!             select type (structure_GCC => Ice%GCC)
!             type is (Type_GCC_NonSegregation_m)
!                 !$omp parallel do private(iN)
!                 do iN = 1, self%nsize
!                     self%Apparent(iN) = self%value(iN) - Lf * arr_rho_ice(iN) * Ice%Calculate_Ice_Derivative(arr_Temperature(iN))
!                 end do
!                 !$omp end parallel do
!             type is (Type_GCC_NonSegregation_Pa)
!                 !$omp parallel do private(iN)
!                 do iN = 1, self%nsize
!                     self%Apparent(iN) = self%value(iN) - Lf * arr_rho_ice(iN) * Ice%Calculate_Ice_Derivative(arr_Temperature(iN), arr_rho_water(iN))
!                 end do
!                 !$omp end parallel do
!             type is (Type_GCC_Segregation_m)
!                 !$omp parallel do private(iN)
!                 do iN = 1, self%nsize
!                     self%Apparent(iN) = self%value(iN) - Lf * arr_rho_ice(iN) * Ice%Calculate_Ice_Derivative(arr_Temperature(iN), arr_Pw(iN), arr_rho_water(iN))
!                 end do
!                 !$omp end parallel do
!             type is (Type_GCC_Segregation_Pa)
!                 !$omp parallel do private(iN)
!                 do iN = 1, self%nsize
!                     self%Apparent(iN) = self%value(iN) - Lf * arr_rho_ice(iN) * Ice%Calculate_Ice_Derivative(arr_Temperature(iN), arr_Pw(iN), arr_rho_water(iN))
!                 end do
!                 !$omp end parallel do
!             end select
!         type is (Type_Ice_EXP)
!             Lf = Ice%Lf
!             !$omp parallel do private(iN)
!             do iN = 1, self%nsize
!                 self%Apparent(iN) = self%value(iN) - Lf * arr_rho_ice(iN) * Ice%Calculate_Ice_Derivative(arr_Temperature(iN))
!             end do
!             !$omp end parallel do
!         end select

!     end subroutine Update_HeatCapacity_Apparent_4Phase_Array

end module Calculate_HeatCapacity
