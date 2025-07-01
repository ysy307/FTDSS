module Calculate_VolumetricHeatCapacity
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes, only:GaussPointState_t
    use :: Inout_Input, only:Type_Input
    use :: Calculate_Density, only:DENHolder
    implicit none
    private

    ! --- 公開する型定義 ---
    public :: VHCHolder
    public :: Abst_VHC
    public :: Type_VHC_3Phase

    ! --- ポリモーフィックなコンテナ ---
    type :: VHCHolder
        class(Abst_VHC), allocatable :: c
    contains
        procedure, pass(self) :: initialize => VHCHolder_initialize
    end type VHCHolder

    ! --- 密度の抽象基底クラス (インターフェースの契約) ---
    type, abstract :: Abst_VHC
        integer(int32) :: region_id
        real(real64) :: Material1 !! soil, rock, concrete
        real(real64) :: Material2 !! water
        real(real64) :: Material3 !! ice
        real(real64) :: Material4 !! gas
    contains
        procedure(Abst_Calc_VHC_GaussPoint), pass(self), deferred :: Calc_GaussPoint
    end type Abst_VHC

    ! --- 3相モデルの具象クラス ---
    type, extends(Abst_VHC) :: Type_VHC_3Phase
    contains
        ! Calcの具体的な実装としてCalc_VHC_3_Wrapをバインドする
        procedure :: Calc_GaussPoint => Calc_VHC_GaussPoint_3Phase
    end type Type_VHC_3Phase
    type, extends(Abst_VHC) :: Type_VHC_3Phase_Apparent
    contains
        ! Calcの具体的な実装としてCalc_VHC_3_Wrapをバインドする
        procedure :: Calc_GaussPoint => Calc_VHC_GaussPoint_3Phase_Apparent
    end type Type_VHC_3Phase_Apparent

    ! --- 手続きのインターフェース宣言 ---
    abstract interface
        function Abst_Calc_VHC_GaussPoint(self, state, DEN, LatentHeat, dQi_dT) result(VHC)
            import :: Abst_VHC, GaussPointState_t, real64, DENHolder
            implicit none
            class(Abst_VHC), intent(in) :: self
            type(GaussPointState_t), intent(in) :: state
            type(DENHolder), intent(in), optional :: DEN
            real(real64), intent(in), optional :: LatentHeat
            real(real64), intent(in), optional :: dQi_dT
            real(real64) :: VHC
        end function Abst_Calc_VHC_GaussPoint
    end interface

    interface
        module subroutine VHCHolder_initialize(self, iRegion, Input)
            implicit none
            class(VHCHolder), intent(inout) :: self
            integer(int32), intent(in) :: iRegion
            type(Type_Input), intent(in) :: Input
        end subroutine VHCHolder_initialize

        module function VHC_3_Construct(iRegion, Input) result(Structure)
            import :: Abst_VHC, Type_Input
            implicit none
            class(Abst_VHC), allocatable :: Structure
            integer(int32), intent(in) :: iRegion
            type(Type_Input), intent(in) :: Input
        end function VHC_3_Construct

        module function Calc_VHC_GaussPoint_3Phase(self, state, DEN, LatentHeat, dQi_dT) result(VHC)
            ! import :: Type_VHC_3Phase, GaussPointState_t
            implicit none
            class(Type_VHC_3Phase), intent(in) :: self
            type(GaussPointState_t), intent(in) :: state
            type(DENHolder), intent(in), optional :: DEN
            real(real64), intent(in), optional :: LatentHeat
            real(real64), intent(in), optional :: dQi_dT
            real(real64) :: VHC
        end function Calc_VHC_GaussPoint_3Phase

        module function VHC_3A_Construct(iRegion, Input) result(Structure)
            import :: Abst_VHC, Type_Input
            implicit none
            class(Abst_VHC), allocatable :: Structure
            integer(int32), intent(in) :: iRegion
            type(Type_Input), intent(in) :: Input
        end function VHC_3A_Construct

        module function Calc_VHC_GaussPoint_3Phase_Apparent(self, state, DEN, LatentHeat, dQi_dT) result(VHC)
            implicit none
            class(Type_VHC_3Phase_Apparent), intent(in) :: self
            type(GaussPointState_t), intent(in) :: state
            type(DENHolder), intent(in), optional :: DEN
            real(real64), intent(in), optional :: LatentHeat
            real(real64), intent(in), optional :: dQi_dT
            real(real64) :: VHC
        end function Calc_VHC_GaussPoint_3Phase_Apparent
    end interface

    interface

        module function Calc_VHC_3(VHC_soil, phi_soil, &
                                   VHC_water, phi_water, &
                                   VHC_ice, phi_ice) result(VHC)
            implicit none
            real(real64), intent(in) :: VHC_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: VHC_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: VHC_ice
            real(real64), intent(in) :: phi_ice
            real(real64) :: VHC
        end function Calc_VHC_3

        module function Calc_VHC_3A(VHC_soil, phi_soil, VHC_water, phi_water, &
                                    VHC_ice, phi_ice, Lf, DEN_ice, dQi_dT) result(VHC)
            implicit none
            real(real64), intent(in) :: VHC_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: VHC_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: VHC_ice
            real(real64), intent(in) :: phi_ice
            real(real64), intent(in) :: Lf
            real(real64), intent(in) :: DEN_ice
            real(real64), intent(in) :: dQi_dT
            real(real64) :: VHC

        end function Calc_VHC_3A
    end interface

    interface Type_VHC_3Phase
        module procedure VHC_3_Construct
    end interface

end module Calculate_VolumetricHeatCapacity
