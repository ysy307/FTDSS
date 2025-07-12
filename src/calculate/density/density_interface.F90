module calculate_density
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_gauss_point_state
    use :: Inout_Input, only:Type_Input
    implicit none
    private

    ! --- 公開する型定義 ---
    public :: DENHolder
    public :: Abst_DEN
    public :: Type_DEN_3Phase

    ! --- ポリモーフィックなコンテナ ---
    type :: DENHolder
        class(Abst_DEN), allocatable :: d
    contains
        procedure, pass(self) :: initialize => DENHolder_initialize
    end type DENHolder

    ! --- 密度の抽象基底クラス (インターフェースの契約) ---
    type, abstract :: Abst_DEN
        integer(int32) :: region_id
        real(real64) :: Material1 !! soil, rock, concrete
        real(real64) :: Material2 !! water
        real(real64) :: Material3 !! ice
        real(real64) :: Material4 !! gas
    contains
        procedure(Abst_Calc_DEN_GaussPoint), pass(self), deferred :: Calc_GaussPoint
    end type Abst_DEN

    ! --- 3相モデルの具象クラス ---
    type, extends(Abst_DEN) :: Type_DEN_3Phase
    contains
        ! Calcの具体的な実装としてCalc_DEN_3_Wrapをバインドする
        procedure :: Calc_GaussPoint => Calc_DEN_GaussPoint_3Phase
    end type Type_DEN_3Phase

    ! --- 手続きのインターフェース宣言 ---
    abstract interface
        function Abst_Calc_DEN_GaussPoint(self, state) result(Density)
            import :: Abst_DEN, type_gauss_point_state, real64
            implicit none
            class(Abst_DEN), intent(in) :: self
            type(type_gauss_point_state), intent(in) :: state
            real(real64) :: Density
        end function Abst_Calc_DEN_GaussPoint
    end interface

    ! このモジュールで実装される手続きのインターフェース
    interface
        module subroutine DENHolder_initialize(self, iRegion, Input)
            implicit none
            class(DENHolder), intent(inout) :: self
            integer(int32), intent(in) :: iRegion
            type(Type_Input), intent(in) :: Input
        end subroutine DENHolder_initialize

        module function DEN_3_Construct(iRegion, Input) result(Structure)
            import :: Abst_DEN, Type_Input
            implicit none
            class(Abst_DEN), allocatable :: Structure
            integer(int32), intent(in) :: iRegion
            type(Type_Input), intent(in) :: Input
        end function DEN_3_Construct

        module function Calc_DEN_GaussPoint_3Phase(self, state) result(density)
            import :: Type_DEN_3Phase, type_gauss_point_state
            implicit none
            class(Type_DEN_3Phase), intent(in) :: self
            type(type_gauss_point_state), intent(in) :: state
            real(real64) :: density
        end function Calc_DEN_GaussPoint_3Phase
    end interface

    interface

        module function Calc_DEN_3(density_soil, phi_soil, &
                                   density_water, phi_water, &
                                   density_ice, phi_ice) result(density)
            implicit none
            real(real64), intent(in) :: density_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: density_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: density_ice
            real(real64), intent(in) :: phi_ice
            real(real64) :: density
        end function Calc_DEN_3
    end interface

    interface Type_DEN_3Phase
        module procedure DEN_3_Construct
    end interface

end module calculate_density
