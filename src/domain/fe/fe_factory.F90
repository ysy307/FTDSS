!>
!> A factory for creating concrete finite element objects based on VTK cell type IDs.
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
        !> Defines the interface for a finite element constructor function.
        !>
        function abst_fe_constructor(input) result(fe)
            import :: type_input, abst_fe
            implicit none
            !> The main input data structure, containing settings required by the constructor.
            type(type_input), intent(in) :: input
            !> The newly created and allocated finite element object.
            class(abst_fe), allocatable :: fe
        end function abst_fe_constructor
    end interface

    !>
    !> A wrapper type that holds a procedure pointer to a specific FE constructor.
    !>
    type :: type_fe_constructor
        procedure(abst_fe_constructor), pointer, nopass :: create => null()
    end type type_fe_constructor

    !>
    !> A table of registered FE constructors, indexed by the VTK cell type ID.
    !> This table is initialized on the first call to `create_fe`.
    !>
    type(type_fe_constructor), allocatable, private, save :: fe_constructor(:)

contains

    !>
    !> Creates a concrete finite element object based on a VTK cell type ID.
    !> This function acts as the public interface to the factory. If called for the first
    !> time, it will automatically initialize the internal constructor table.
    !>
    function create_fe(id, input) result(fe)
        implicit none
        !> The VTK cell type ID for the element to create.
        integer(int32), intent(in) :: id
        !> The main input data structure, required by the element constructor.
        class(type_input), intent(in) :: input
        !> The newly allocated finite element object, or an unallocated object on failure.
        class(abst_fe), allocatable :: fe
        character(len=*), parameter :: func_name = "create_fe"

        ! ==========================================================
        ! Lazy initialization of the factory
        ! ==========================================================
        if (.not. allocated(fe_constructor)) then
            call initialize_factory_internal()
        end if

        ! ==========================================================
        ! Validation and object creation
        ! ==========================================================
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
    !> Initializes the internal constructor table (factory).
    !> This routine allocates the table and registers all available finite element
    !> constructor procedures.
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
    !> Registers a constructor procedure for a given VTK cell type ID.
    !> If a constructor is already registered for the specified ID, it will be
    !> overwritten and a warning will be logged.
    !>
    subroutine register_constructor(id, constructor)
        implicit none
        !> The VTK cell type ID to associate with the constructor.
        integer(int32), intent(in) :: id
        !> A procedure pointer to the constructor function.
        procedure(abst_fe_constructor), intent(in), pointer :: constructor

        if (associated(fe_constructor(id)%create)) then
            call global_logger%log_warning("register_constructor: Overwriting constructor for ID = "//to_string(id))
        end if

        fe_constructor(id)%create => constructor
    end subroutine register_constructor

end module domain_fe_factory
