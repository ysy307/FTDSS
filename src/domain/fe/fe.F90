!>
!> Module for finite element definitions and management
!>
module module_fe
    use, intrinsic :: iso_fortran_env, only: int32
    use :: module_core, only:unique
    use :: module_input, only:type_input
    use :: domain_fe, only:abst_fe, holder_fes
    use :: domain_fe_side, only:type_side_first, type_side_second
    use :: domain_fe_element, only:type_triangle_first, type_triangle_second, &
        type_square_first, type_square_second
    use :: domain_fe_factory, only:create_fe
    implicit none
    private

    !-------------------------------------------------------------------------------------------------------------------------------
    ! derived types
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: abst_fe
    public :: type_side_first
    public :: type_side_second
    public :: type_triangle_first
    public :: type_triangle_second
    public :: type_square_first
    public :: type_square_second

    !-------------------------------------------------------------------------------------------------------------------------------
    ! operation procedures
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: create_fe

    !-------------------------------------------------------------------------------------------------------------------------------
    ! manager for finite elements
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: type_fe_manager

    !> Manager type for handling multiple FE objects
    type :: type_fe_manager
        private
        !> List of wrapper objects holding FE instances
        type(holder_fes), allocatable :: fe_list(:)
        !> Map from FE IDs to indices in fe_list
        integer(int32), allocatable :: fe_map(:)
    contains
        procedure, pass(self), public :: initialize => initialize_fe_manager
        procedure, pass(self), public :: get_fe
    end type type_fe_manager

contains
    !> Initialize the FE manager with specified input, number of FEs, and target IDs
    subroutine initialize_fe_manager(self, input, num_fe, target_ids)
        implicit none
        !> The FE manager object to initialize
        class(type_fe_manager), intent(inout) :: self
        !> Input data for FE creation
        type(type_input), intent(in) :: input
        !> Number of FE objects to create
        integer(int32), intent(in) :: num_fe
        !> Array of FE IDs to initialize
        integer(int32), intent(in) :: target_ids(:)

        integer(int32), allocatable :: unique_ids(:)
        integer(int32) :: i


        if (allocated(self%fe_list)) then
            deallocate (self%fe_list)
        end if

        call unique(target_ids, unique_ids)

        allocate (self%fe_list(size(unique_ids)))
        allocate (self%fe_map(num_fe))
        self%fe_map = 0

        do i = 1, size(unique_ids)
            self%fe_list(i)%fe = create_fe(unique_ids(i), input)
        end do

        ! Create mapping from target_ids to fe_list indices
        do i = 1, num_fe
            self%fe_map(i) = findloc(unique_ids, target_ids(i), 1)
        end do

    end subroutine initialize_fe_manager

    !> Get a pointer to the FE object corresponding to a given ID
    function get_fe(self, fe_id) result(fe)
        implicit none
        !> The FE manager object
        class(type_fe_manager), intent(in), target :: self
        !> The ID of the FE object
        integer(int32), intent(in) :: fe_id
        !> Pointer to the requested FE object
        class(abst_fe), pointer :: fe

#ifdef USE_DEBUG
        ! if (.not. associated(self%fe_list(self%fe_map(fe_id)%fe))) then
        !     error stop "Error: FE ID not found in FE manager."
        ! end if

        ! if (fe_id < 1 .or. fe_id > maxval(self%fe_map)) then
        !     print *, self%fe_map
        !     error stop "Error: FE ID mapping out of bounds."
        ! end if
#endif

        fe => self%fe_list(self%fe_map(fe_id))%fe
    end function get_fe

end module module_fe
