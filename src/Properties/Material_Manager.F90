module Properties_Material_Manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Calculate_ThermalConductivity, only:THCHolder
    use :: Calculate_Density, only:DENHolder
    use :: Calculate_SpecificHeat, only:SPHHolder
    use :: Calculate_VolumetricHeatCapacity, only:VHCHolder
    use :: Calculate_GCC, only:GCCHolder
    use :: Calculate_WRF, only:WRFHolder
    use :: Inout_Input, only:Type_Input
    implicit none
    private

    public :: MaterialManager_t

    type :: MaterialManager_t
        private
        type(THCHolder), allocatable :: THC(:)
        type(DENHolder), allocatable :: DEN(:)
        type(SPHHolder), allocatable :: SPH(:)
        type(VHCHolder), allocatable :: VHC(:)
        type(GCCHolder), allocatable :: GCC(:)
        type(WRFHolder), allocatable :: WRF(:)
        ! region_idを配列インデックスに変換するマッピング配列
        integer(int32), allocatable :: region_id_map(:)
    contains
        procedure, pass(self) :: initialize
        procedure, pass(self) :: get_THC
        procedure, pass(self) :: get_DEN
        procedure, pass(self) :: get_SPH
        procedure, pass(self) :: get_VHC
        procedure, pass(self) :: get_GCC
        procedure, pass(self) :: get_WRF
    end type

contains
    ! Managerを初期化するサブルーチン (シミュレーション開始時に一度だけ呼ぶ)
    subroutine initialize(self, Input, ierr)
        class(MaterialManager_t), intent(inout) :: self
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

            call self%THC(model_idx)%initialize(iRegion=current_region_id, Input=Input)
            call self%DEN(model_idx)%initialize(iRegion=current_region_id, Input=Input)
            call self%SPH(model_idx)%initialize(iRegion=current_region_id, Input=Input)
            call self%VHC(model_idx)%initialize(iRegion=current_region_id, Input=Input)
            call self%GCC(model_idx)%initialize(iRegion=current_region_id, Input=Input)
            call self%WRF(model_idx)%initialize(iRegion=current_region_id, Input=Input)

            self%region_id_map(current_region_id) = model_idx
        end do
    end subroutine initialize

    function get_THC(self, region_id) result(model_holder)
        class(MaterialManager_t), intent(in) :: self
        integer, intent(in) :: region_id
        ! 返り値から POINTER 属性を削除
        type(THCHolder) :: model_holder
        integer :: model_index

        ! マッピング配列を使って、正しいインデックスをO(1)で取得
        model_index = self%region_id_map(region_id)

        ! エラーチェック
        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_THC:", region_id
            call exit(-1)
        end if

        ! ポインタ結合( => )ではなく、代入( = )でコピーを返す
        model_holder = self%THC(model_index)

    end function get_THC

    function get_DEN(self, region_id) result(model_holder)
        class(MaterialManager_t), intent(in) :: self
        integer, intent(in) :: region_id
        ! 返り値から POINTER 属性を削除
        type(DENHolder) :: model_holder
        integer :: model_index

        ! マッピング配列を使って、正しいインデックスをO(1)で取得
        model_index = self%region_id_map(region_id)

        ! エラーチェック
        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_DEN:", region_id
            call exit(-1)
        end if

        ! ポインタ結合( => )ではなく、代入( = )でコピーを返す
        model_holder = self%DEN(model_index)

    end function get_DEN

    function get_SPH(self, region_id) result(model_holder)
        class(MaterialManager_t), intent(in) :: self
        integer, intent(in) :: region_id
        ! 返り値から POINTER 属性を削除
        type(SPHHolder) :: model_holder
        integer :: model_index

        ! マッピング配列を使って、正しいインデックスをO(1)で取得
        model_index = self%region_id_map(region_id)

        ! エラーチェック
        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_SPH:", region_id
            call exit(-1)
        end if

        ! ポインタ結合( => )ではなく、代入( = )でコピーを返す
        model_holder = self%SPH(model_index)

    end function get_SPH

    function get_VHC(self, region_id) result(model_holder)
        class(MaterialManager_t), intent(in) :: self
        integer, intent(in) :: region_id
        ! 返り値から POINTER 属性を削除
        type(VHCHolder) :: model_holder
        integer :: model_index

        ! マッピング配列を使って、正しいインデックスをO(1)で取得
        model_index = self%region_id_map(region_id)

        ! エラーチェック
        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_VHC:", region_id
            call exit(-1)
        end if

        ! ポインタ結合( => )ではなく、代入( = )でコピーを返す
        model_holder = self%VHC(model_index)

    end function get_VHC

    ! ここで、GCCHolderとWRFHolderのget関数も同様に実装することができます。
    ! 例えば、get_GCC(self, region_id) と get_WRF(self, region_id) を追加します。
    function get_GCC(self, region_id) result(model_holder)
        class(MaterialManager_t), intent(in) :: self
        integer, intent(in) :: region_id
        ! 返り値から POINTER 属性を削除
        type(GCCHolder) :: model_holder
        integer :: model_index

        ! マッピング配列を使って、正しいインデックスをO(1)で取得
        model_index = self%region_id_map(region_id)

        ! エラーチェック
        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_GCC:", region_id
            call exit(-1)
        end if

        ! ポインタ結合( => )ではなく、代入( = )でコピーを返す
        model_holder = self%GCC(model_index)

    end function get_GCC

    function get_WRF(self, region_id) result(model_holder)
        class(MaterialManager_t), intent(in) :: self
        integer, intent(in) :: region_id
        ! 返り値から POINTER 属性を削除
        type(WRFHolder) :: model_holder
        integer :: model_index

        ! マッピング配列を使って、正しいインデックスをO(1)で取得
        model_index = self%region_id_map(region_id)

        ! エラーチェック
        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_WRF:", region_id
            call exit(-1)
        end if

        ! ポインタ結合( => )ではなく、代入( = )でコピーを返す
        model_holder = self%WRF(model_index)

    end function get_WRF
end module Properties_Material_Manager
