module domain_fe_factory
    use, intrinsic :: iso_fortran_env
    use :: stdlib_logger, only:global_logger
    use :: stdlib_strings, only:to_string
    use :: module_core, only:vtk_constants
    use :: module_input, only:type_input
    use :: domain_fe, only:abst_fe
    use :: domain_fe_side
    use :: domain_fe_element

    implicit none
    private

    ! --- 公開API ---
    public :: create_fe

    abstract interface
        function abst_fe_constructor(input) result(fe)
            import :: type_input, abst_fe
            implicit none
            type(type_input), intent(in) :: input
            class(abst_fe), allocatable :: fe
        end function abst_fe_constructor
    end interface

    type :: type_fe_constructor
        procedure(abst_fe_constructor), pointer, nopass :: create => null()
    end type type_fe_constructor

    type(type_fe_constructor), allocatable, private, save :: fe_constructor(:)

contains

    !>
    !> @brief 指定されたIDと入力に基づき、FEオブジェクトを生成します（遅延初期化対応）。
    !> @details この関数が初めて呼び出された際に、内部でファクトリの初期化が自動的に行われます。
    !> @param[in] id VTKセルタイプID
    !> @param[in] input FEオブジェクトの生成に必要な入力データ
    !> @return 成功した場合は生成されたFEオブジェクト。失敗した場合は**未確保(deallocated)のオブジェクト**。
    !>
    function create_fe(id, input) result(fe)
        implicit none
        integer(int32), intent(in) :: id
        class(type_input), intent(in) :: input
        class(abst_fe), allocatable :: fe
        character(len=*), parameter :: func_name = "create_fe"

        ! --- 遅延初期化 ---
        ! 初回呼び出し時にファクトリを内部的に初期化する
        if (.not. allocated(fe_constructor)) then
            call initialize_factory_internal()
        end if

        ! --- IDの範囲チェック ---
        if (id < lbound(fe_constructor, 1) .or. id > ubound(fe_constructor, 1)) then
            call global_logger%log_error(func_name//": ID is out of range. ID = "//to_string(id))
            return ! fe は未確保のまま終了
        end if

        ! --- コンストラクタの登録チェック ---
        if (.not. associated(fe_constructor(id)%create)) then
            call global_logger%log_error(func_name//": No constructor registered for ID = "//to_string(id))
            return ! fe は未確保のまま終了
        end if

        ! --- オブジェクトの生成 ---
        fe = fe_constructor(id)%create(input)

    end function create_fe

    !>
    !> @brief ファクトリを内部的に初期化する（非公開手続き）
    !>
    subroutine initialize_factory_internal()
        implicit none
        integer(int32) :: max_fe_types

        max_fe_types = vtk_constants%get_max_cell_id()
        allocate (fe_constructor(0:max_fe_types))

        ! 利用可能なFEコンストラクタをすべて登録
        call register_constructor(vtk_constants%get_cell_type("Line"), construct_side_first)
        call register_constructor(vtk_constants%get_cell_type("QuadraticEdge"), construct_side_second)
        call register_constructor(vtk_constants%get_cell_type("Triangle"), construct_triangle_first)
        call register_constructor(vtk_constants%get_cell_type("Quad"), construct_square_first)
        call register_constructor(vtk_constants%get_cell_type("QuadraticTriangle"), construct_triangle_second)
        call register_constructor(vtk_constants%get_cell_type("QuadraticQuad"), construct_square_second)

    end subroutine initialize_factory_internal

    !>
    !> @brief FEコンストラクタを登録する（内部手続き）
    !>
    subroutine register_constructor(id, constructor)
        implicit none
        integer(int32), intent(in) :: id
        procedure(abst_fe_constructor), pointer :: constructor

        if (associated(fe_constructor(id)%create)) then
            call global_logger%log_warning("register_constructor: Overwriting constructor for ID = "//to_string(id))
        end if

        fe_constructor(id)%create => constructor
    end subroutine register_constructor

end module domain_fe_factory
