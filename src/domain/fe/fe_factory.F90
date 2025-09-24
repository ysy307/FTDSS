!>
!> @brief Factory module for creating finite element objects based on VTK cell type IDs
!>
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

    public :: create_fe

    abstract interface
        !>
        !> @brief Constructor for a finite element object
        !> @param[in] input Input data required to build the FE object
        !>
        !> @return Newly allocated FE object
        !>
        function abst_fe_constructor(input) result(fe)
            import :: type_input, abst_fe
            implicit none
            type(type_input), intent(in) :: input
            class(abst_fe), allocatable :: fe
        end function abst_fe_constructor
    end interface

    !>
    !> @brief Wrapper type that holds a constructor procedure pointer
    !>
    type :: type_fe_constructor
        !>
        !> @brief Procedure pointer to a specific FE constructor
        !>
        procedure(abst_fe_constructor), pointer, nopass :: create => null()
    end type type_fe_constructor

    !>
    !> @brief Table of registered FE constructors indexed by VTK cell type ID
    !>
    type(type_fe_constructor), allocatable, private, save :: fe_constructor(:)

contains

    !>
    !> @brief Create a finite element object based on ID and input (lazy initialization)
    !>
    !> If this function is called for the first time, the internal factory is initialized automatically.
    !> @param[in] id    VTK cell type ID
    !> @param[in] input Input data required to build the FE object
    !>
    !> @return Allocated FE object on success, unallocated object on failure
    !>
    function create_fe(id, input) result(fe)
        implicit none
        integer(int32), intent(in) :: id
        class(type_input), intent(in) :: input
        class(abst_fe), allocatable :: fe
        character(len=*), parameter :: func_name = "create_fe"

        ! --- Lazy initialization ---
        if (.not. allocated(fe_constructor)) then
            call initialize_factory_internal()
        end if

        ! --- Range check for ID ---
        if (id < lbound(fe_constructor, 1) .or. id > ubound(fe_constructor, 1)) then
            call global_logger%log_error(func_name//": ID is out of range. ID = "//to_string(id))
            return
        end if

        ! --- Check constructor registration ---
        if (.not. associated(fe_constructor(id)%create)) then
            call global_logger%log_error(func_name//": No constructor registered for ID = "//to_string(id))
            return
        end if

        ! --- Construct the object ---
        fe = fe_constructor(id)%create(input)

    end function create_fe

    !>
    !> @brief Internal routine to initialize the constructor table
    !>
    subroutine initialize_factory_internal()
        implicit none
        integer(int32) :: max_fe_types

        max_fe_types = vtk_constants%get_max_cell_id()
        allocate (fe_constructor(0:max_fe_types))

        ! Register available FE constructors
        call register_constructor(vtk_constants%get_cell_type("Line"),              construct_side_first) !&
        call register_constructor(vtk_constants%get_cell_type("QuadraticEdge"),     construct_side_second) !&
        call register_constructor(vtk_constants%get_cell_type("Triangle"),          construct_triangle_first) !&
        call register_constructor(vtk_constants%get_cell_type("Quad"),              construct_square_first) !&
        call register_constructor(vtk_constants%get_cell_type("QuadraticTriangle"), construct_triangle_second) !&
        call register_constructor(vtk_constants%get_cell_type("QuadraticQuad"),     construct_square_second) !&

    end subroutine initialize_factory_internal

    !>
    !> @brief Register a constructor procedure for a given cell type ID
    !>
    !> If a constructor is already registered for the ID, it will be overwritten.
    !> @param[in] id          VTK cell type ID
    !> @param[in] constructor Procedure pointer to the constructor
    !>
    subroutine register_constructor(id, constructor)
        implicit none
        integer(int32), intent(in) :: id
        procedure(abst_fe_constructor), intent(in), pointer :: constructor

        if (associated(fe_constructor(id)%create)) then
            call global_logger%log_warning("register_constructor: Overwriting constructor for ID = "//to_string(id))
        end if

        fe_constructor(id)%create => constructor
    end subroutine register_constructor

end module domain_fe_factory
