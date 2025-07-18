module properties_material_manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_input, only:Type_input
    use :: module_calculate, only:holder_gccs, holder_wrfs, holder_dens, holder_sphs, holder_vhcs, holder_thcs, & !&
                                  abst_gcc, abst_wrf, abst_den, abst_sph, abst_vhc, abst_thc

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
        procedure, public, pass(self) :: initialize

        procedure, private, pass(self) :: get_thc_holder
        procedure, private, pass(self) :: get_thc_ptr
        generic, public :: get_thc => get_thc_holder, get_thc_ptr

        procedure, private, pass(self) :: get_den_holder
        procedure, private, pass(self) :: get_den_ptr
        generic, public :: get_den => get_den_holder, get_den_ptr

        procedure, private, pass(self) :: get_sph_holder
        procedure, private, pass(self) :: get_sph_ptr
        generic, public :: get_sph => get_sph_holder, get_sph_ptr

        procedure, private, pass(self) :: get_vhc_holder
        procedure, private, pass(self) :: get_vhc_ptr
        generic, public :: get_vhc => get_vhc_holder, get_vhc_ptr

        procedure, private, pass(self) :: get_gcc_holder
        procedure, private, pass(self) :: get_gcc_ptr
        generic, public :: get_gcc => get_gcc_holder, get_gcc_ptr

        procedure, private, pass(self) :: get_wrf_holder
        procedure, private, pass(self) :: get_wrf_ptr
        generic, public :: get_wrf => get_wrf_holder, get_wrf_ptr

    end type

contains
    ! Managerを初期化するサブルーチン (シミュレーション開始時に一度だけ呼ぶ)
    subroutine initialize(self, input, ierr)
        class(type_material_manager), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(inout) :: ierr

        integer(int32) :: i, model_idx
        integer(int32) :: num_unique_regions, num_id
        integer(int32), allocatable :: unique_material_ids(:)

        integer(int32) :: current_material_id

        ierr = 0
        call input%geometry%vtk%get_active_region_info(unique_material_ids, ierr)
        num_unique_regions = input%basic%num_materials

        ! ステップ2: 配列を確保
        allocate (self%thc(num_unique_regions))
        allocate (self%den(num_unique_regions))
        allocate (self%sph(num_unique_regions))
        allocate (self%vhc(num_unique_regions))
        allocate (self%gcc(num_unique_regions))
        allocate (self%wrf(num_unique_regions))

        ! allocate (self%region_id_map(num_unique_regions))
        allocate (self%region_id_map, source=unique_material_ids)
        ! self%region_id_map = 0 ! 0は無効なインデックスとする

        ! ステップ3: 事前にあなたのFactoryを呼び出してモデルを生成し、マッピングする
        do model_idx = 1, num_unique_regions
            current_material_id = unique_material_ids(model_idx)

            call self%thc(model_idx)%initialize(input, current_material_id)
            call self%den(model_idx)%initialize(input, current_material_id)
            call self%sph(model_idx)%initialize(input, current_material_id)
            call self%vhc(model_idx)%initialize(input, current_material_id)
            call self%gcc(model_idx)%initialize(input, current_material_id)
            call self%wrf(model_idx)%initialize(input, current_material_id)

            self%region_id_map(current_material_id) = model_idx
        end do
    end subroutine initialize

    subroutine get_thc_holder(self, region_id, model_holder)
        class(type_material_manager), intent(inout) :: self
        integer(int32), intent(in) :: region_id
        type(holder_thcs), intent(inout) :: model_holder
        integer(int32) :: model_index

        model_index = self%region_id_map(region_id)

        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_thc:", region_id
            call exit(-1)
        end if

        model_holder = self%thc(model_index)

    end subroutine get_thc_holder

    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! THCオブジェクトへのポインタを返す高速なバージョン
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine get_thc_ptr(self, region_id, thc_ptr)
        class(type_material_manager), intent(inout), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_thc), intent(inout), pointer :: thc_ptr
        integer(int32) :: model_index

        model_index = self%region_id_map(region_id)

        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_thc_ptr:", region_id
            nullify (thc_ptr)
            call exit(-1)
        end if

        thc_ptr => self%thc(model_index)%p

    end subroutine get_thc_ptr

    subroutine get_den_holder(self, region_id, model_holder)
        class(type_material_manager), intent(inout) :: self
        integer(int32), intent(in) :: region_id
        type(holder_dens), intent(inout) :: model_holder
        integer(int32) :: model_index

        model_index = self%region_id_map(region_id)

        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_den_holder:", region_id
            call exit(-1)
        end if

        model_holder = self%den(model_index)

    end subroutine get_den_holder

    subroutine get_den_ptr(self, region_id, den_ptr)
        class(type_material_manager), intent(inout), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_den), intent(inout), pointer :: den_ptr
        integer(int32) :: model_index

        model_index = self%region_id_map(region_id)

        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_den_ptr:", region_id
            nullify (den_ptr)
            call exit(-1)
        end if

        den_ptr => self%den(model_index)%p

    end subroutine get_den_ptr

    subroutine get_sph_holder(self, region_id, model_holder)
        class(type_material_manager), intent(inout) :: self
        integer(int32), intent(in) :: region_id
        type(holder_sphs), intent(inout) :: model_holder
        integer(int32) :: model_index

        model_index = self%region_id_map(region_id)

        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_sph_holder:", region_id
            call exit(-1)
        end if

        model_holder = self%sph(model_index)

    end subroutine get_sph_holder

    subroutine get_sph_ptr(self, region_id, sph_ptr)
        class(type_material_manager), intent(inout), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_sph), intent(inout), pointer :: sph_ptr
        integer(int32) :: model_index

        model_index = self%region_id_map(region_id)

        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_sph_ptr:", region_id
            nullify (sph_ptr)
            call exit(-1)
        end if

        sph_ptr => self%sph(model_index)%p

    end subroutine get_sph_ptr

    subroutine get_vhc_holder(self, region_id, model_holder)
        class(type_material_manager), intent(inout) :: self
        integer(int32), intent(in) :: region_id
        type(holder_vhcs), intent(inout) :: model_holder
        integer(int32) :: model_index

        model_index = self%region_id_map(region_id)

        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_vhc_holder:", region_id
            call exit(-1)
        end if

        model_holder = self%vhc(model_index)

    end subroutine get_vhc_holder

    subroutine get_vhc_ptr(self, region_id, vhc_ptr)
        class(type_material_manager), intent(inout), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_vhc), intent(inout), pointer :: vhc_ptr
        integer(int32) :: model_index

        model_index = self%region_id_map(region_id)

        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_vhc_ptr:", region_id
            nullify (vhc_ptr)
            call exit(-1)
        end if

        vhc_ptr => self%vhc(model_index)%p

    end subroutine get_vhc_ptr

    subroutine get_gcc_holder(self, region_id, model_holder)
        class(type_material_manager), intent(inout) :: self
        integer(int32), intent(in) :: region_id
        type(holder_gccs), intent(inout) :: model_holder
        integer(int32) :: model_index

        model_index = self%region_id_map(region_id)

        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_gcc_holder:", region_id
            call exit(-1)
        end if

        model_holder = self%gcc(model_index)

    end subroutine get_gcc_holder

    subroutine get_gcc_ptr(self, region_id, gcc_ptr)
        class(type_material_manager), intent(inout), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_gcc), intent(inout), pointer :: gcc_ptr
        integer(int32) :: model_index

        model_index = self%region_id_map(region_id)

        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_gcc_ptr:", region_id
            nullify (gcc_ptr)
            call exit(-1)
        end if

        gcc_ptr => self%gcc(model_index)%p

    end subroutine get_gcc_ptr

    subroutine get_wrf_holder(self, region_id, model_holder)
        class(type_material_manager), intent(inout) :: self
        integer(int32), intent(in) :: region_id
        type(holder_wrfs), intent(inout) :: model_holder
        integer(int32) :: model_index

        model_index = self%region_id_map(region_id)

        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_wrf_holder:", region_id
            call exit(-1)
        end if

        model_holder = self%wrf(model_index)

    end subroutine get_wrf_holder

    subroutine get_wrf_ptr(self, region_id, wrf_ptr)
        class(type_material_manager), intent(inout), target :: self
        integer(int32), intent(in) :: region_id
        class(abst_wrf), intent(inout), pointer :: wrf_ptr
        integer(int32) :: model_index

        model_index = self%region_id_map(region_id)

        if (model_index == 0) then
            print *, "Error: Invalid region_id in get_wrf_ptr:", region_id
            nullify (wrf_ptr)
            call exit(-1)
        end if

        wrf_ptr => self%wrf(model_index)%p

    end subroutine get_wrf_ptr

end module properties_material_manager
