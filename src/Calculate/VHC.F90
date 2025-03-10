module Calculate_VHC
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Allocate_Allocate, only:Allocate_Array
    use :: Calculate_Ice
    implicit none

    type, abstract :: Abstract_VolumetricHeatCapacity
        real(real64), allocatable :: value(:) ! Volumetric heat capacit
    contains
        procedure(Abstract_Calculate_VolumetricHeatCapacity), pass(self), deferred :: Calculate
    end type Abstract_VolumetricHeatCapacity

    type, abstract, extends(Abstract_VolumetricHeatCapacity) :: Abstract_VolumetricHeatCapacity_Appearant
        real(real64), allocatable :: Apearant(:) ! Apearant volumetric heat capacity
        real(real64) :: Ca_max
        class(Abstract_Ice), pointer :: Ice
    end type Abstract_VolumetricHeatCapacity_Appearant

    type, extends(Abstract_VolumetricHeatCapacity) :: Type_VolumetricHeatCapacity_1Phase
        real(real64) :: Cp1 ! Material volumetric heat capacity
    contains
        procedure :: Calculate => Calculate_VolumetricHeatCapacity_1Phase_Wrapped
    end type Type_VolumetricHeatCapacity_1Phase

    type, extends(Abstract_VolumetricHeatCapacity) :: Type_VolumetricHeatCapacity_2Phase
        real(real64) :: Cp1 ! Material volumetric heat capacity
        real(real64) :: Cp2 ! Material volumetric heat capacity
    contains
        procedure :: Calculate => Calculate_VolumetricHeatCapacity_2Phase_Wrapped
    end type Type_VolumetricHeatCapacity_2Phase

    type, extends(Abstract_VolumetricHeatCapacity_Appearant) :: Type_VolumetricHeatCapacity_3Phase
        real(real64) :: Cp_soil ! Soil volumetric heat capacity
        real(real64) :: Cp_water ! Water volumetric heat capacity
        real(real64) :: Cp_ice ! Ice volumetric heat capacity
    contains
        procedure :: Calculate => Calculate_VolumetricHeatCapacity_3Phase_Wrapped
        procedure, pass(self) :: Calculate_Ca => Calculate_VolumetricHeatCapacity_Appearant_3Phase_Wrapped

    end type Type_VolumetricHeatCapacity_3Phase

    type, extends(Abstract_VolumetricHeatCapacity_Appearant) :: Type_VolumetricHeatCapacity_4Phase
        real(real64) :: Cp_soil ! Soil volumetric heat capacity
        real(real64) :: Cp_water ! Water volumetric heat capacity
        real(real64) :: Cp_ice ! Ice volumetric heat capacity
        real(real64) :: Cp_air ! Air volumetric heat capacity
    contains
        procedure :: Calculate => Calculate_VolumetricHeatCapacity_4Phase_Wrapped
    end type Type_VolumetricHeatCapacity_4Phase

    abstract interface
        function Abstract_Calculate_VolumetricHeatCapacity(self, phi1, phi2, phi3, phi4) result(Cp)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_VolumetricHeatCapacity
            implicit none
            class(Abstract_VolumetricHeatCapacity), intent(in) :: self
            real(real64), intent(in), optional :: phi1 !! the ratio of material 1
            real(real64), intent(in), optional :: phi2 !! the ratio of material 2
            real(real64), intent(in), optional :: phi3 !! the ratio of material 3
            real(real64), intent(in), optional :: phi4 !! the ratio of material 4
            real(real64) :: Cp
        end function Abstract_Calculate_VolumetricHeatCapacity

        ! function Abstract_Calculate_VolumetricHeatCapacity_Appearant_GCC(self, structure_GCC, Temperature) result(Ca)
        !     use, intrinsic :: iso_fortran_env, only: real64
        !     use :: Calculate_Ice, only:Type_Ice_GCC
        !     import :: Abstract_VolumetricHeatCapacity_Appearant
        !     implicit none
        !     class(Abstract_VolumetricHeatCapacity_Appearant), intent(in) :: self
        !     type(Type_Ice_GCC), intent(in) :: structure_GCC
        !     real(real64), intent(in) :: Temperature !! Temperature
        !     real(real64) :: Ca
        ! end function Abstract_Calculate_VolumetricHeatCapacity_Appearant_GCC
    end interface
    ! abstract interface
    !     function Calculate_VolumetricHeatCapacity_3Phase(self, phi_soil, phi_water) result(VHC)
    !         use, intrinsic :: iso_fortran_env, only: real64
    !         import :: Type_VolumetricHeatCapacity_3Phase
    !         class(Type_VolumetricHeatCapacity_3Phase), intent(in) :: self
    !         real(real64), intent(in) :: phi_soil !! the ratio of soil
    !         real(real64), intent(in) :: phi_water !! the ratio of water
    !         real(real64) :: VHC
    !     end function Calculate_VolumetricHeatCapacity_3Phase

    !     function Calculate_VolumetricHeatCapacity_4Phase(self, phi_soil, phi_water, phi_air) result(VHC)
    !         use, intrinsic :: iso_fortran_env, only: real64
    !         import :: Type_VolumetricHeatCapacity_3Phase
    !         class(Type_VolumetricHeatCapacity_3Phase), intent(in) :: self
    !         real(real64), intent(in) :: phi_soil !! the ratio of soil
    !         real(real64), intent(in) :: phi_water !! the ratio of water
    !         real(real64), intent(in) :: phi_air !! the ratio of air
    !         real(real64) :: VHC
    !     end function Calculate_VolumetricHeatCapacity_4Phase
    ! end interface

    interface Type_VolumetricHeatCapacity_1Phase
        procedure :: Construct_VolumetricHeatCapacity_1Phase
    end interface

    interface Type_VolumetricHeatCapacity_2Phase
        procedure :: Construct_VolumetricHeatCapacity_2Phase
    end interface

    interface Type_VolumetricHeatCapacity_3Phase
        procedure :: Construct_VolumetricHeatCapacity_3Phase
    end interface

    interface Type_VolumetricHeatCapacity_4Phase
        procedure :: Construct_VolumetricHeatCapacity_4Phase
    end interface

contains

    !----------------------------------------------------------------------------------------------------
    ! Constructe each type of volumetric heat capacity
    !----------------------------------------------------------------------------------------------------
    function Construct_VolumetricHeatCapacity_1Phase(Cp1, nsize) result(structure)
        implicit none
        real(real64), intent(in) :: Cp1 !! Volumetric heat capacity of matarial 1
        integer(int32), intent(in) :: nsize !! Size of array
        type(Type_VolumetricHeatCapacity_1Phase) :: structure

        structure%Cp1 = Cp1

        call Allocate_Array(structure%value, nsize)
        structure%value(:) = 0.0d0

    end function Construct_VolumetricHeatCapacity_1Phase

    function Construct_VolumetricHeatCapacity_2Phase(Cp1, Cp2, nsize) result(structure)
        implicit none
        real(real64), intent(in) :: Cp1 !! Volumetric heat capacity of matarial 1
        real(real64), intent(in) :: Cp2 !! Volumetric heat capacity of matarial 2
        integer(int32), intent(in) :: nsize !! Size of array
        type(Type_VolumetricHeatCapacity_2Phase) :: structure

        structure%Cp1 = Cp1
        structure%Cp2 = Cp2

        call Allocate_Array(structure%value, nsize)
        structure%value(:) = 0.0d0
    end function Construct_VolumetricHeatCapacity_2Phase

    function Construct_VolumetricHeatCapacity_3Phase(Cp_soil, Cp_water, Cp_ice, structure_Ice, rho_ice, rho_water, nsize) result(structure)
        implicit none
        real(real64), intent(in) :: Cp_soil !! Volumetric heat capacity of soil
        real(real64), intent(in) :: Cp_water !! Volumetric heat capacity of water
        real(real64), intent(in) :: Cp_ice !! Volumetric heat capacity of ice
        integer(int32), intent(in) :: nsize !! Size of array
        class(Abstract_Ice), pointer, intent(in) :: structure_Ice
        real(real64), intent(in) :: rho_ice !! Density of ice
        real(real64), intent(in), optional :: rho_water !! Density of water
        type(Type_VolumetricHeatCapacity_3Phase) :: structure

        structure%Cp_soil = Cp_soil
        structure%Cp_water = Cp_water
        structure%Cp_ice = Cp_ice

        call Allocate_Array(structure%value, nsize)
        call Allocate_Array(structure%Apearant, nsize)
        structure%value(:) = 0.0d0
        structure%Apearant(:) = 0.0d0

        structure%Ice => structure_Ice
        ! structure, initial_phi, rho_ice, rho_water, Temperature, Pw
        if (.not. present(rho_water)) then
            call Find_Ca_maximum(structure, rho_ice)
        else
            call Find_Ca_maximum(structure, rho_ice, rho_water)
        end if

    end function Construct_VolumetricHeatCapacity_3Phase

    function Construct_VolumetricHeatCapacity_4Phase(Cp_soil, Cp_water, Cp_ice, Cp_air, nsize) result(structure)
        implicit none
        real(real64), intent(in) :: Cp_soil !! Volumetric heat capacity of soil
        real(real64), intent(in) :: Cp_water !! Volumetric heat capacity of water
        real(real64), intent(in) :: Cp_ice !! Volumetric heat capacity of ice
        real(real64), intent(in) :: Cp_air !! Volumetric heat capacity of air
        integer(int32), intent(in) :: nsize !! Size of array
        type(Type_VolumetricHeatCapacity_4Phase) :: structure

        structure%Cp_soil = Cp_soil
        structure%Cp_water = Cp_water
        structure%Cp_ice = Cp_ice
        structure%Cp_air = Cp_air

        call Allocate_Array(structure%value, nsize)
        call Allocate_Array(structure%Apearant, nsize)
        structure%value(:) = 0.0d0
        structure%Apearant(:) = 0.0d0

    end function Construct_VolumetricHeatCapacity_4Phase

    !----------------------------------------------------------------------------------------------------
    ! Calculate Volumetric heat capacity
    !----------------------------------------------------------------------------------------------------
    function Calculate_VolumetricHeatCapacity_1Phase(Cp1) result(Cp)
        implicit none
        real(real64), intent(in) :: Cp1 !! Volumetric heat capacity of matarial 1
        real(real64) :: Cp

        Cp = Cp1
    end function Calculate_VolumetricHeatCapacity_1Phase

    function Calculate_VolumetricHeatCapacity_2Phase(Cp1, phi1, Cp2, phi2) result(Cp)
        implicit none
        real(real64), intent(in) :: Cp1 !! Volumetric heat capacity of matarial 1
        real(real64), intent(in) :: phi1 !! the ratio of matarial 1
        real(real64), intent(in) :: Cp2 !! Volumetric heat capacity of matarial 2
        real(real64), intent(in) :: phi2 !! the ratio of matarial 2
        real(real64) :: Cp

        Cp = phi1 * Cp1 + phi2 * Cp2
    end function Calculate_VolumetricHeatCapacity_2Phase

    function Calculate_VolumetricHeatCapacity_3Phase(Cp_soil, phi_soil, Cp_water, phi_water, Cp_ice, phi_ice) result(Cp)
        implicit none
        real(real64), intent(in) :: Cp_soil !! Volumetric heat capacity of soil
        real(real64), intent(in) :: phi_soil !! the ratio of soil
        real(real64), intent(in) :: Cp_water !! Volumetric heat capacity of water
        real(real64), intent(in) :: phi_water !! the ratio of water
        real(real64), intent(in) :: Cp_ice !! Volumetric heat capacity of ice
        real(real64), intent(in) :: phi_ice !! the ratio of ice
        real(real64) :: Cp

        Cp = phi_soil * Cp_soil + phi_water * Cp_water + phi_Ice * Cp_ice
    end function Calculate_VolumetricHeatCapacity_3Phase

    function Calculate_VolumetricHeatCapacity_4Phase(Cp_soil, phi_soil, Cp_water, phi_water, Cp_ice, phi_ice, Cp_air, phi_air) result(Cp)
        implicit none
        real(real64), intent(in) :: Cp_soil !! Volumetric heat capacity of soil
        real(real64), intent(in) :: phi_soil !! the ratio of soil
        real(real64), intent(in) :: Cp_water !! Volumetric heat capacity of water
        real(real64), intent(in) :: phi_water !! the ratio of water
        real(real64), intent(in) :: Cp_ice !! Volumetric heat capacity of ice
        real(real64), intent(in) :: phi_ice !! the ratio of ice
        real(real64), intent(in) :: Cp_air !! Volumetric heat capacity of air
        real(real64), intent(in) :: phi_air !! the ratio of air
        real(real64) :: Cp

        Cp = phi_soil * Cp_soil + phi_water * Cp_water + phi_Ice * Cp_ice + phi_air * Cp_air
    end function Calculate_VolumetricHeatCapacity_4Phase

    function Calculate_VolumetricHeatCapacity_Appearant_3Phase(Cp_soil, phi_soil, Cp_water, phi_water, Cp_ice, phi_ice, structure_Ice, rho_ice, rho_water, Temperature, Pw) result(Ca)
        implicit none
        real(real64), intent(in) :: Cp_soil !! Volumetric heat capacity of soil
        real(real64), intent(in) :: phi_soil !! the ratio of soil
        real(real64), intent(in) :: Cp_water !! Volumetric heat capacity of water
        real(real64), intent(in) :: phi_water !! the ratio of water
        real(real64), intent(in) :: Cp_ice !! Volumetric heat capacity of ice
        real(real64), intent(in) :: phi_ice !! the ratio of ice
        class(Abstract_Ice), intent(inout) :: structure_Ice
        real(real64), intent(in) :: rho_ice !! Density of ice
        real(real64), intent(in), optional :: rho_water !! Density of water
        real(real64), intent(in) :: Temperature !! Temperature
        real(real64), intent(in), optional :: Pw !! Water pressure
        real(real64) :: Ca

        real(real64) :: Lf

        select type (Ice => structure_Ice)
        type is (Type_Ice_GCC)
            Lf = Ice%GCC%Lf

            select type (structure_GCC => Ice%GCC)
            type is (Type_GCC_NonSegregation_m)
                Ca = phi_soil * Cp_soil + phi_water * Cp_water + phi_Ice * Cp_ice - Lf * rho_ice * Ice%Calculate_Ice_Derivative(Temperature)
            type is (Type_GCC_NonSegregation_Pa)
                Ca = phi_soil * Cp_soil + phi_water * Cp_water + phi_Ice * Cp_ice - Lf * rho_ice * Ice%Calculate_Ice_Derivative(Temperature, rho_water)
            type is (Type_GCC_Segregation_m)
                Ca = phi_soil * Cp_soil + phi_water * Cp_water + phi_Ice * Cp_ice - Lf * rho_ice * Ice%Calculate_Ice_Derivative(Temperature, Pw, rho_water)
            type is (Type_GCC_Segregation_Pa)
                Ca = phi_soil * Cp_soil + phi_water * Cp_water + phi_Ice * Cp_ice - Lf * rho_ice * Ice%Calculate_Ice_Derivative(Temperature, Pw, rho_water)
            end select
        type is (Type_Ice_EXP)
            Lf = Ice%Lf
            Ca = phi_soil * Cp_soil + phi_water * Cp_water + phi_Ice * Cp_ice - Lf * rho_ice * Ice%Calculate_Ice_Derivative(Temperature)
        end select

    end function Calculate_VolumetricHeatCapacity_Appearant_3Phase

    !----------------------------------------------------------------------------------------------------
    ! Find maximum Volumetric heat capacity
    !----------------------------------------------------------------------------------------------------

    subroutine Find_Ca_maximum(structure, rho_ice, rho_water)
        implicit none
        class(Abstract_VolumetricHeatCapacity_Appearant), intent(inout) :: Structure
        real(real64), intent(in) :: rho_ice !! Density of ice
        real(real64), intent(in), optional :: rho_water !! Density of water

        real(real64) :: initial_phi
        real(real64) :: T0, T1, T2, T3, Ca1, Ca2, tau
        real(real64), parameter :: epsilon = 1.0d-15

        select type (this => Structure)
        type is (Type_VolumetricHeatCapacity_3Phase)
            select type (this_Ice => this%Ice)
            type is (Type_Ice_GCC)
                initial_phi = this_Ice%WRF%thetaS

                tau = (sqrt(5.d0) - 1.d0) / 2.d0 ! golden ratio
                T0 = 0.0d0 ! Upper limit
                T3 = -1.0d0 ! Lower limit
                T1 = T0 + (1.d0 - tau) * (T3 - T0)
                T2 = T0 + tau * (T3 - T0)
                select type (this_GCC => this_Ice%GCC)
                type is (Type_GCC_NonSegregation_m)
                    Ca1 = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                            phi2=initial_phi, &
                                            phi3=0.0d0, &
                                            rho_ice=rho_ice, &
                                            Temperature=T1)
                    Ca2 = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                            phi2=initial_phi, &
                                            phi3=0.0d0, &
                                            rho_ice=rho_ice, &
                                            Temperature=T2)

                    do while (abs(T3 - T0) > epsilon)
                        if (Ca2 > Ca1) then
                            T0 = T1
                            T1 = T2
                            T2 = T0 + tau * (T3 - T0)
                            Ca1 = Ca2
                            Ca2 = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                                    phi2=initial_phi, &
                                                    phi3=0.0d0, &
                                                    rho_ice=rho_ice, &
                                                    Temperature=T2)
                        else
                            T3 = T2
                            T2 = T1
                            T1 = T0 + (1.d0 - tau) * (T3 - T0)
                            Ca2 = Ca1
                            Ca1 = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                                    phi2=initial_phi, &
                                                    phi3=0.0d0, &
                                                    rho_ice=rho_ice, &
                                                    Temperature=T1)
                        end if
                    end do

                    this%Ca_max = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                                    phi2=initial_phi, &
                                                    phi3=0.0d0, &
                                                    rho_ice=rho_ice, &
                                                    Temperature=(T1 + T2) / 2.d0)
                type is (Type_GCC_NonSegregation_Pa)
                    Ca1 = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                            phi2=initial_phi, &
                                            phi3=0.0d0, &
                                            rho_ice=rho_ice, &
                                            rho_water=rho_water, &
                                            Temperature=T1)
                    Ca2 = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                            phi2=initial_phi, &
                                            phi3=0.0d0, &
                                            rho_ice=rho_ice, &
                                            rho_water=rho_water, &
                                            Temperature=T2)

                    do while (abs(T3 - T0) > epsilon)
                        if (Ca2 > Ca1) then
                            T0 = T1
                            T1 = T2
                            T2 = T0 + tau * (T3 - T0)
                            Ca1 = Ca2
                            Ca2 = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                                    phi2=initial_phi, &
                                                    phi3=0.0d0, &
                                                    rho_ice=rho_ice, &
                                                    rho_water=rho_water, &
                                                    Temperature=T2)
                        else
                            T3 = T2
                            T2 = T1
                            T1 = T0 + (1.d0 - tau) * (T3 - T0)
                            Ca2 = Ca1
                            Ca1 = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                                    phi2=initial_phi, &
                                                    phi3=0.0d0, &
                                                    rho_ice=rho_ice, &
                                                    rho_water=rho_water, &
                                                    Temperature=T1)
                        end if
                    end do

                    this%Ca_max = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                                    phi2=initial_phi, &
                                                    phi3=0.0d0, &
                                                    rho_ice=rho_ice, &
                                                    rho_water=rho_water, &
                                                    Temperature=(T1 + T2) / 2.d0)
                type is (Type_GCC_Segregation_m)
                    Ca1 = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                            phi2=initial_phi, &
                                            phi3=0.0d0, &
                                            rho_ice=rho_ice, &
                                            rho_water=rho_water, &
                                            Temperature=T1)
                    Ca2 = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                            phi2=initial_phi, &
                                            phi3=0.0d0, &
                                            rho_ice=rho_ice, &
                                            rho_water=rho_water, &
                                            Temperature=T2)

                    do while (abs(T3 - T0) > epsilon)
                        if (Ca2 > Ca1) then
                            T0 = T1
                            T1 = T2
                            T2 = T0 + tau * (T3 - T0)
                            Ca1 = Ca2
                            Ca2 = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                                    phi2=initial_phi, &
                                                    phi3=0.0d0, &
                                                    rho_ice=rho_ice, &
                                                    rho_water=rho_water, &
                                                    Temperature=T2)
                        else
                            T3 = T2
                            T2 = T1
                            T1 = T0 + (1.d0 - tau) * (T3 - T0)
                            Ca2 = Ca1
                            Ca1 = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                                    phi2=initial_phi, &
                                                    phi3=0.0d0, &
                                                    rho_ice=rho_ice, &
                                                    rho_water=rho_water, &
                                                    Temperature=T1)
                        end if
                    end do

                    this%Ca_max = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                                    phi2=initial_phi, &
                                                    phi3=0.0d0, &
                                                    rho_ice=rho_ice, &
                                                    rho_water=rho_water, &
                                                    Temperature=(T1 + T2) / 2.d0)
                type is (Type_GCC_Segregation_Pa)
                    Ca1 = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                            phi2=initial_phi, &
                                            phi3=0.0d0, &
                                            rho_ice=rho_ice, &
                                            rho_water=rho_water, &
                                            Temperature=T1, &
                                            Pw=0.0d0)
                    Ca2 = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                            phi2=initial_phi, &
                                            phi3=0.0d0, &
                                            rho_ice=rho_ice, &
                                            rho_water=rho_water, &
                                            Temperature=T2, &
                                            Pw=0.0d0)

                    do while (abs(T3 - T0) > epsilon)
                        if (Ca2 > Ca1) then
                            T0 = T1
                            T1 = T2
                            T2 = T0 + tau * (T3 - T0)
                            Ca1 = Ca2
                            Ca2 = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                                    phi2=initial_phi, &
                                                    phi3=0.0d0, &
                                                    rho_ice=rho_ice, &
                                                    rho_water=rho_water, &
                                                    Temperature=T2, &
                                                    Pw=0.0d0)
                        else
                            T3 = T2
                            T2 = T1
                            T1 = T0 + (1.d0 - tau) * (T3 - T0)
                            Ca2 = Ca1
                            Ca1 = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                                    phi2=initial_phi, &
                                                    phi3=0.0d0, &
                                                    rho_ice=rho_ice, &
                                                    rho_water=rho_water, &
                                                    Temperature=T1, &
                                                    Pw=0.0d0)
                        end if
                    end do
                    this%Ca_max = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                                    phi2=initial_phi, &
                                                    phi3=0.0d0, &
                                                    rho_ice=rho_ice, &
                                                    rho_water=rho_water, &
                                                    Temperature=(T1 + T2) / 2.d0, &
                                                    Pw=0.0d0)
                end select
            type is (Type_Ice_EXP)
                initial_phi = this_Ice%phi
                this%Ca_max = this%Calculate_Ca(phi1=1.0d0 - initial_phi, &
                                                phi2=initial_phi, &
                                                phi3=0.0d0, &
                                                rho_ice=rho_ice, &
                                                Temperature=this_Ice%Tf)
            end select
        end select
    end subroutine Find_Ca_maximum

    !----------------------------------------------------------------------------------------------------
    ! Wrapper
    !----------------------------------------------------------------------------------------------------
    function Calculate_VolumetricHeatCapacity_1Phase_Wrapped(self, phi1, phi2, phi3, phi4) result(Cp)
        implicit none
        class(Type_VolumetricHeatCapacity_1Phase), intent(in) :: self
        real(real64), intent(in), optional :: phi1 !! the ratio of material 1
        real(real64), intent(in), optional :: phi2 !! the ratio of material 2
        real(real64), intent(in), optional :: phi3 !! the ratio of material 3
        real(real64), intent(in), optional :: phi4 !! the ratio of material 4
        real(real64) :: Cp

        if (.not. present(phi1)) stop 'Error: Calculate_VolumetricHeatCapacity_1Phase_Wrapped'

        Cp = Calculate_VolumetricHeatCapacity_1Phase(self%Cp1)
    end function Calculate_VolumetricHeatCapacity_1Phase_Wrapped

    function Calculate_VolumetricHeatCapacity_2Phase_Wrapped(self, phi1, phi2, phi3, phi4) result(Cp)
        implicit none
        class(Type_VolumetricHeatCapacity_2Phase), intent(in) :: self
        real(real64), intent(in), optional :: phi1 !! the ratio of material 1
        real(real64), intent(in), optional :: phi2 !! the ratio of material 2
        real(real64), intent(in), optional :: phi3 !! the ratio of material 3
        real(real64), intent(in), optional :: phi4 !! the ratio of material 4
        real(real64) :: Cp

        if (.not. present(phi1) .or. &
            .not. present(phi2) &
            ) stop 'Error: Calculate_VolumetricHeatCapacity_2Phase_Wrapped'

        Cp = Calculate_VolumetricHeatCapacity_2Phase(self%Cp1, phi1, self%Cp2, phi2)
    end function Calculate_VolumetricHeatCapacity_2Phase_Wrapped

    function Calculate_VolumetricHeatCapacity_3Phase_Wrapped(self, phi1, phi2, phi3, phi4) result(Cp)
        implicit none
        class(Type_VolumetricHeatCapacity_3Phase), intent(in) :: self
        real(real64), intent(in), optional :: phi1 !! the ratio of material 1
        real(real64), intent(in), optional :: phi2 !! the ratio of material 2
        real(real64), intent(in), optional :: phi3 !! the ratio of material 3
        real(real64), intent(in), optional :: phi4 !! the ratio of material 4
        real(real64) :: Cp

        if (.not. present(phi1) .or. &
            .not. present(phi2) .or. &
            .not. present(phi3) &
            ) stop 'Error: Calculate_VolumetricHeatCapacity_3Phase_Wrapped'

        Cp = Calculate_VolumetricHeatCapacity_3Phase(self%Cp_soil, phi1, self%Cp_water, phi2, self%Cp_ice, phi3)
    end function Calculate_VolumetricHeatCapacity_3Phase_Wrapped

    function Calculate_VolumetricHeatCapacity_4Phase_Wrapped(self, phi1, phi2, phi3, phi4) result(Cp)
        implicit none
        class(Type_VolumetricHeatCapacity_4Phase), intent(in) :: self
        real(real64), intent(in), optional :: phi1 !! the ratio of material 1
        real(real64), intent(in), optional :: phi2 !! the ratio of material 2
        real(real64), intent(in), optional :: phi3 !! the ratio of material 3
        real(real64), intent(in), optional :: phi4 !! the ratio of material 4
        real(real64) :: Cp

        if (.not. present(phi1) .or. &
            .not. present(phi2) .or. &
            .not. present(phi3) .or. &
            .not. present(phi4) &
            ) stop 'Error: Calculate_VolumetricHeatCapacity_4Phase_Wrapped'

        Cp = Calculate_VolumetricHeatCapacity_4Phase(self%Cp_soil, phi1, self%Cp_water, phi2, self%Cp_ice, phi3, self%Cp_air, phi4)
    end function Calculate_VolumetricHeatCapacity_4Phase_Wrapped

    function Calculate_VolumetricHeatCapacity_Appearant_3Phase_Wrapped(self, phi1, phi2, phi3, phi4, rho_ice, rho_water, Temperature, Pw) result(Ca)
        implicit none
        class(Type_VolumetricHeatCapacity_3Phase), intent(in) :: self
        real(real64), intent(in) :: phi1 !! the ratio of material 1
        real(real64), intent(in) :: phi2 !! the ratio of material 2
        real(real64), intent(in) :: phi3 !! the ratio of material 3
        real(real64), intent(in), optional :: phi4 !! the ratio of material 4
        real(real64), intent(in) :: rho_ice !! Density of ice
        real(real64), intent(in), optional :: rho_water !! Density of water
        real(real64), intent(in) :: Temperature !! Temperature
        real(real64), intent(in), optional :: Pw !! Water pressure
        real(real64) :: Ca

        if (.not. present(rho_water) .and. .not. present(Pw)) then
            Ca = Calculate_VolumetricHeatCapacity_Appearant_3Phase(Cp_soil=self%Cp_soil, &
                                                                   phi_soil=phi1, &
                                                                   Cp_water=self%Cp_water, &
                                                                   phi_water=phi2, &
                                                                   Cp_ice=self%Cp_ice, &
                                                                   phi_ice=phi3, &
                                                                   structure_Ice=self%Ice, &
                                                                   rho_ice=rho_ice, &
                                                                   Temperature=Temperature)
        else if (present(rho_water) .and. .not. present(Pw)) then
            Ca = Calculate_VolumetricHeatCapacity_Appearant_3Phase(Cp_soil=self%Cp_soil, &
                                                                   phi_soil=phi1, &
                                                                   Cp_water=self%Cp_water, &
                                                                   phi_water=phi2, &
                                                                   Cp_ice=self%Cp_ice, &
                                                                   phi_ice=phi3, &
                                                                   structure_Ice=self%Ice, &
                                                                   rho_ice=rho_ice, &
                                                                   rho_water=rho_water, &
                                                                   Temperature=Temperature)
        else if (present(rho_water) .and. present(Pw)) then
            Ca = Calculate_VolumetricHeatCapacity_Appearant_3Phase(Cp_soil=self%Cp_soil, &
                                                                   phi_soil=phi1, &
                                                                   Cp_water=self%Cp_water, &
                                                                   phi_water=phi2, &
                                                                   Cp_ice=self%Cp_ice, &
                                                                   phi_ice=phi3, &
                                                                   structure_Ice=self%Ice, &
                                                                   rho_ice=rho_ice, &
                                                                   rho_water=rho_water, &
                                                                   Temperature=Temperature, &
                                                                   Pw=Pw)
        end if

    end function Calculate_VolumetricHeatCapacity_Appearant_3Phase_Wrapped
    !----------------------------------------------------------------------------------------------------
    ! Calculate Volumetric heat capacity
    !----------------------------------------------------------------------------------------------------

    !----------------------------------------------------------------------------------------------------
    ! Update Volumetric heat capacity
    !----------------------------------------------------------------------------------------------------

end module Calculate_VHC
