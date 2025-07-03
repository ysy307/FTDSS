module Calculate_SpecificHeat
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes, only:GaussPointState_t
    use :: Inout_Input, only:Type_Input
    implicit none
    private

    ! --- 公開する型定義 ---
    public :: SPHHolder
    public :: Abst_SPH
    public :: Type_SPH_3Phase

    ! --- ポリモーフィックなコンテナ ---
    type :: SPHHolder
        class(Abst_SPH), allocatable :: c
    contains
        procedure, pass(self) :: initialize => SPHHolder_initialize
    end type SPHHolder

    ! --- 密度の抽象基底クラス (インターフェースの契約) ---
    type, abstract :: Abst_SPH
        integer(int32) :: region_id
        real(real64) :: Material1 !! soil, rock, concrete
        real(real64) :: Material2 !! water
        real(real64) :: Material3 !! ice
        real(real64) :: Material4 !! gas
    contains
        procedure(Abst_Calc_SPH_GaussPoint), pass(self), deferred :: Calc_GaussPoint
    end type Abst_SPH

    ! --- 3相モデルの具象クラス ---
    type, extends(Abst_SPH) :: Type_SPH_3Phase
    contains
        ! Calcの具体的な実装としてCalc_SPH_3_Wrapをバインドする
        procedure :: Calc_GaussPoint => Calc_SPH_GaussPoint_3Phase
    end type Type_SPH_3Phase

    ! --- 手続きのインターフェース宣言 ---
    abstract interface
        function Abst_Calc_SPH_GaussPoint(self, state) result(SpecificHeat)
            import :: Abst_SPH, GaussPointState_t, real64
            implicit none
            class(Abst_SPH), intent(in) :: self
            type(GaussPointState_t), intent(in) :: state
            real(real64) :: SpecificHeat
        end function Abst_Calc_SPH_GaussPoint
    end interface

    ! このモジュールで実装される手続きのインターフェース
    interface
        module subroutine SPHHolder_initialize(self, iRegion, Input)
            implicit none
            class(SPHHolder), intent(inout) :: self
            integer(int32), intent(in) :: iRegion
            type(Type_Input), intent(in) :: Input
        end subroutine SPHHolder_initialize

        module function SPH_3_Construct(iRegion, Input) result(Structure)
            import :: Abst_SPH, Type_Input
            implicit none
            class(Abst_SPH), allocatable :: Structure
            integer(int32), intent(in) :: iRegion
            type(Type_Input), intent(in) :: Input
        end function SPH_3_Construct

        module function Calc_SPH_GaussPoint_3Phase(self, state) result(SpecificHeat)
            import :: Type_SPH_3Phase, GaussPointState_t
            implicit none
            class(Type_SPH_3Phase), intent(in) :: self
            type(GaussPointState_t), intent(in) :: state
            real(real64) :: SpecificHeat
        end function Calc_SPH_GaussPoint_3Phase
    end interface

    interface

        module function Calc_SPH_3(SpecificHeat_soil, phi_soil, &
                                   SpecificHeat_water, phi_water, &
                                   SpecificHeat_ice, phi_ice) result(SpecificHeat)
            implicit none
            real(real64), intent(in) :: SpecificHeat_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: SpecificHeat_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: SpecificHeat_ice
            real(real64), intent(in) :: phi_ice
            real(real64) :: SpecificHeat
        end function Calc_SPH_3
    end interface

    interface Type_SPH_3Phase
        module procedure SPH_3_Construct
    end interface

end module Calculate_SpecificHeat
