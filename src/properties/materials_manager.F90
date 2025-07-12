module properties_material_manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Inout_Input, only:Type_Input
    use :: module_calculate, only:holder_gccs, holder_wrfs, holder_dens, holder_sphs, holder_vhcs, holder_thcs

    implicit none
    private

    public :: type_material_manager

    type :: type_material_manager
        private
        type(holder_thcs), allocatable :: thc(:)
        type(holder_dens), allocatable :: den(:)
        type(holder_sphs), allocatable :: sph(:)
        type(holder_vhcs), allocatable :: vhc(:)
        type(holder_gccs), allocatable :: gcc(:)
        type(holder_wrfs), allocatable :: wrf(:)

        integer(int32), allocatable :: region_id_map(:)
    contains
        procedure, pass(self) :: initialize
        procedure, pass(self) :: get_thc
        procedure, pass(self) :: get_den
        procedure, pass(self) :: get_sph
        procedure, pass(self) :: get_vhc
        procedure, pass(self) :: get_gcc
        procedure, pass(self) :: get_wrf
    end type

contains
    ! Managerを初期化するサブルーチン (シミュレーション開始時に一度だけ呼ぶ)
    subroutine initialize(self, Input, ierr)
        class(type_material_manager), intent(inout) :: self
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
        allocate (self%thc(num_unique_regions))
        allocate (self%den(num_unique_regions))
        allocate (self%sph(num_unique_regions))
        allocate (self%vhc(num_unique_regions))
        allocate (self%gcc(num_unique_regions))
        allocate (self%wrf(num_unique_regions))

        ! allocate (self%region_id_map(num_unique_regions))
        allocate (self%region_id_map, source=unique_region_ids)
        ! self%region_id_map = 0 ! 0は無効なインデックスとする

        ! ステップ3: 事前にあなたのFactoryを呼び出してモデルを生成し、マッピングする
        do model_idx = 1, num_unique_regions
            current_region_id = unique_region_ids(model_idx)

            call self%thc(model_idx)%initialize(iRegion=current_region_id, Input=Input)
            call self%den(model_idx)%initialize(iRegion=current_region_id, Input=Input)
            call self%sph(model_idx)%initialize(iRegion=current_region_id, Input=Input)
            call self%vhc(model_idx)%initialize(iRegion=current_region_id, Input=Input)
            call self%gcc(model_idx)%initialize(iRegion=current_region_id, Input=Input)
            call self%wrf(model_idx)%initialize(iRegion=current_region_id, Input=Input)

            self%region_id_map(current_region_id) = model_idx
        end do
    end subroutine initialize

    function get_thc(self, region_id) result(model_holder)
        class(type_material_manager), intent(in) :: self
        integer(int32), intent(in) :: region_id
        ! 返り値から POINTER 属性を削除
        type(holder_thcs) :: model_holder
        integer(int32) :: model_index

        ! マッピング配列を使って、正しいインデックスをO(1)で取得
        model_index = self%region_id_map(region_id)

        ! エラーチェック
        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_thc:", region_id
            call exit(-1)
        end if

        ! ポインタ結合( => )ではなく、代入( = )でコピーを返す
        model_holder = self%thc(model_index)

    end function get_thc

    function get_den(self, region_id) result(model_holder)
        class(type_material_manager), intent(in) :: self
        integer(int32), intent(in) :: region_id
        ! 返り値から POINTER 属性を削除
        type(holder_dens) :: model_holder
        integer(int32) :: model_index

        ! マッピング配列を使って、正しいインデックスをO(1)で取得
        model_index = self%region_id_map(region_id)

        ! エラーチェック
        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_den:", region_id
            call exit(-1)
        end if

        ! ポインタ結合( => )ではなく、代入( = )でコピーを返す
        model_holder = self%den(model_index)

    end function get_den

    function get_sph(self, region_id) result(model_holder)
        class(type_material_manager), intent(in) :: self
        integer(int32), intent(in) :: region_id
        ! 返り値から POINTER 属性を削除
        type(holder_sphs) :: model_holder
        integer(int32) :: model_index

        ! マッピング配列を使って、正しいインデックスをO(1)で取得
        model_index = self%region_id_map(region_id)

        ! エラーチェック
        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_sph:", region_id
            call exit(-1)
        end if

        ! ポインタ結合( => )ではなく、代入( = )でコピーを返す
        model_holder = self%sph(model_index)

    end function get_sph

    function get_vhc(self, region_id) result(model_holder)
        class(type_material_manager), intent(in) :: self
        integer(int32), intent(in) :: region_id
        ! 返り値から POINTER 属性を削除
        type(holder_vhcs) :: model_holder
        integer(int32) :: model_index

        ! マッピング配列を使って、正しいインデックスをO(1)で取得
        model_index = self%region_id_map(region_id)

        ! エラーチェック
        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_vhc:", region_id
            call exit(-1)
        end if

        ! ポインタ結合( => )ではなく、代入( = )でコピーを返す
        model_holder = self%vhc(model_index)

    end function get_vhc

    ! ここで、holder_gccとholder_wrfsのget関数も同様に実装することができます。
    ! 例えば、get_gcc(self, region_id) と get_wrf(self, region_id) を追加します。
    function get_gcc(self, region_id) result(model_holder)
        class(type_material_manager), intent(in) :: self
        integer(int32), intent(in) :: region_id
        ! 返り値から POINTER 属性を削除
        type(holder_gccs) :: model_holder
        integer(int32) :: model_index

        ! マッピング配列を使って、正しいインデックスをO(1)で取得

        model_index = self%region_id_map(region_id)

        ! エラーチェック
        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_gcc:", region_id
            call exit(-1)
        end if

        ! ポインタ結合( => )ではなく、代入( = )でコピーを返す
        model_holder = self%gcc(model_index)

    end function get_gcc

    function get_wrf(self, region_id) result(model_holder)
        class(type_material_manager), intent(in) :: self
        integer(int32), intent(in) :: region_id
        ! 返り値から POINTER 属性を削除
        type(holder_wrfs) :: model_holder
        integer(int32) :: model_index

        ! マッピング配列を使って、正しいインデックスをO(1)で取得
        model_index = self%region_id_map(region_id)

        ! エラーチェック
        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_wrf:", region_id
            call exit(-1)
        end if

        ! ポインタ結合( => )ではなく、代入( = )でコピーを返す
        model_holder = self%wrf(model_index)

    end function get_wrf
end module properties_material_manager
