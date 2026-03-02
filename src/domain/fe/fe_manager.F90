module domain_fe_manager
    use, intrinsic :: iso_fortran_env, only: int32
    use :: module_core, only:unique
    use :: domain_base_fe, only:abst_fe, holder_fes
    use :: domain_fe_factory, only:create_fe

    public :: type_fe_manager

    !> Manager type for handling multiple FE objects
    type :: type_fe_manager
        !> List of wrapper objects holding FE instances
        type(holder_fes), private, allocatable :: fe_list(:)
        !> Map from FE IDs to indices in fe_list
        integer(int32), private, allocatable :: fe_map(:)
    contains
        procedure, pass(self), public :: initialize => initialize_fe_manager
        procedure, pass(self), public :: destroy => destroy_fe_manager
        procedure, pass(self), public :: get_fe => get_fe_fe_manager
    end type type_fe_manager

contains
    !> Initialize the FE manager with specified input, number of FEs, and target IDs
    subroutine initialize_fe_manager(self, integration_order, num_fe, target_ids)
        implicit none
        !> The FE manager object to initialize
        class(type_fe_manager), intent(inout) :: self
        !> Input data for FE creation
        integer(int32), intent(in) :: integration_order
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
            self%fe_list(i)%fe = create_fe(unique_ids(i), integration_order)
        end do

        ! Create mapping from target_ids to fe_list indices
        do i = 1, num_fe
            self%fe_map(i) = findloc(unique_ids, target_ids(i), 1)
        end do

    end subroutine initialize_fe_manager

    !> Destroy all FE objects and deallocate memory associated with the FE manager
    subroutine destroy_fe_manager(self)
        implicit none
        class(type_fe_manager), intent(inout) :: self
        integer(int32) :: i

        if (allocated(self%fe_list)) then
            do i = 1, size(self%fe_list)
                if (allocated(self%fe_list(i)%fe)) then
                    call self%fe_list(i)%fe%destroy()
                end if
            end do
            deallocate (self%fe_list)
        end if

        call deallocate_array(self%fe_map)
    end subroutine destroy_fe_manager

    !> Get a pointer to the FE object corresponding to a given ID
    function get_fe_fe_manager(self, fe_id) result(fe)
        implicit none
        !> The FE manager object
        class(type_fe_manager), intent(in), target :: self
        !> The ID of the FE object
        integer(int32), intent(in) :: fe_id
        !> Pointer to the requested FE object
        class(abst_fe), pointer :: fe

        fe => self%fe_list(self%fe_map(fe_id))%fe
    end function get_fe_fe_manager

end module domain_fe_manager
