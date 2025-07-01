! src/Properties/material_manager.F90
module Properties_Thermal_Material_Manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use Calculate_ThermalConductivity, only: THCHolder, Type_THC_3Phase
    use Inout_Input, only: Type_Input
    implicit none
    private

    public :: ThermalMaterialManager_t

    type :: ThermalMaterialManager_t
        private
        ! あなたのTHCHolderを格納する配列
        type(THCHolder), allocatable :: THC(:)
        ! region_idを配列インデックスに変換するマッピング配列
        integer(int32), allocatable :: region_id_map(:)
    contains
        procedure :: initialize
        procedure :: get_thc_model
        ! function get_thc_model(self, region_id) result(model_holder)
    end type

contains
    ! Managerを初期化するサブルーチン (シミュレーション開始時に一度だけ呼ぶ)
    subroutine initialize(self, Input, ierr)
        class(ThermalMaterialManager_t), intent(inout) :: self
        type(Type_Input), intent(in) :: Input
        integer(int32), intent(inout) :: ierr

        integer(int32) :: i, model_idx
        integer(int32) :: num_unique_regions, num_id
        integer(int32), allocatable :: unique_region_ids(:)

        integer(int32) :: current_region_id

        ierr = 0
        call Input%VTK%get_active_region_info(unique_region_ids, ierr)
        num_unique_regions = Input%Basic%numRegion

        ! ステップ2: 配列を確保
        allocate (self%THC(num_unique_regions))
        allocate (self%region_id_map(num_id))
        self%region_id_map = 0 ! 0は無効なインデックスとする

        ! ステップ3: 事前にあなたのFactoryを呼び出してモデルを生成し、マッピングする
        do model_idx = 1, num_unique_regions
            current_region_id = unique_region_ids(model_idx)

            self%THC(model_idx)%l = Type_THC_3Phase(region_id=current_region_id, &
                                                    lambda1=Input%Regions(model_idx)%Thermal%lambda(1), &
                                                    lambda2=Input%Regions(model_idx)%Thermal%lambda(2), &
                                                    lambda3=Input%Regions(model_idx)%Thermal%lambda(3) &
                                                    )

            self%region_id_map(current_region_id) = model_idx
        end do
    end subroutine initialize

    function get_thc_model(self, region_id) result(model_holder)
        class(ThermalMaterialManager_t), intent(in) :: self
        integer, intent(in) :: region_id
        ! 返り値から POINTER 属性を削除
        type(THCHolder) :: model_holder
        integer :: model_index

        ! マッピング配列を使って、正しいインデックスをO(1)で取得
        model_index = self%region_id_map(region_id)

        ! エラーチェック
        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_thc_model:", region_id
            call exit(-1)
        end if

        ! ポインタ結合( => )ではなく、代入( = )でコピーを返す
        model_holder = self%THC(model_index)

    end function get_thc_model
end module Properties_Thermal_Material_Manager
