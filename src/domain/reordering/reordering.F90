module domain_reordering
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_sorting, only:sort, sort_index
    use :: module_core, only:allocate_array, deallocate_array, error_message
    use :: domain_element, only:holder_elements
    use :: domain_adjacency_adjacency_node, only:type_node_adjacency

    implicit none
    private
    public :: type_reordering

    type :: type_reordering
        private
        character(:), allocatable :: algorithm_name
        integer(int32) :: num_nodes = 0
        integer(int32), allocatable :: perm(:) ! CM/RCM indices -> original indices
        integer(int32), allocatable :: iperm(:) ! Original indices -> CM/RCM indices
        logical :: is_reordered_perm = .false.
        logical :: is_reordered_iperm = .false.
    contains
        procedure, public, pass(self) :: initialize => type_reordering_initialize

        procedure, private, pass(self) :: rcm_reorder_method
        procedure, private, pass(self) :: rcm_inverse_method
        procedure, private, pass(self) :: cm_reorder_method
        procedure, private, pass(self) :: cm_inverse_method

        ! CM/RCM ordering -> original ordering
        procedure, private, pass(self) :: to_original_values_int32
        procedure, private, pass(self) :: to_original_values_real64
        generic, public :: to_original_value => to_original_values_int32, & !&
                                                to_original_values_real64 !&
        procedure, private, pass(self) :: to_original_index
        procedure, private, pass(self) :: to_original_indices
        generic, public :: to_original => to_original_index, & !&
                                          to_original_indices !&

        !  original ordering -> CM/RCM ordering
        procedure, private, pass(self) :: to_reordered_values_int32
        procedure, private, pass(self) :: to_reordered_values_real64
        generic, public :: to_reordered_value => to_reordered_values_int32, & !&
                                                 to_reordered_values_real64 !&
        procedure, private, pass(self) :: to_reordered_index
        procedure, private, pass(self) :: to_reordered_indices
        generic, public :: to_reordered => to_reordered_index, & !&
                                           to_reordered_indices !&

        final :: type_reordering_finalize
    end type type_reordering

    interface
        module subroutine rcm_reorder_method(self, elements)
            implicit none
            class(type_reordering), intent(inout) :: self
            type(holder_elements), intent(in) :: elements(:)

        end subroutine rcm_reorder_method

        module subroutine cm_reorder_method(self, elements)
            implicit none
            class(type_reordering), intent(inout) :: self
            type(holder_elements), intent(in) :: elements(:)

        end subroutine cm_reorder_method

        module subroutine rcm_inverse_method(self)
            implicit none
            class(type_reordering), intent(inout) :: self

        end subroutine rcm_inverse_method

        module subroutine cm_inverse_method(self)
            implicit none
            class(type_reordering), intent(inout) :: self
            integer(int32) :: i

        end subroutine cm_inverse_method
    end interface

    interface
        module subroutine to_original_values_int32(self, vector_reordered, vector_original)
            implicit none
            class(type_reordering), intent(in) :: self
            integer(int32), intent(in) :: vector_reordered(:)
            integer(int32), intent(inout) :: vector_original(:)

        end subroutine to_original_values_int32

        module subroutine to_original_values_real64(self, vector_reordered, vector_original)
            implicit none
            class(type_reordering), intent(in) :: self
            real(real64), intent(in) :: vector_reordered(:)
            real(real64), intent(inout) :: vector_original(:)

        end subroutine to_original_values_real64

        module subroutine to_original_index(self, index_reordered, index_original)
            implicit none
            class(type_reordering), intent(in) :: self
            integer(int32), intent(in) :: index_reordered
            integer(int32), intent(inout) :: index_original

        end subroutine to_original_index

        module subroutine to_original_indices(self, indices_reordered, indices_original)
            implicit none
            class(type_reordering), intent(in) :: self
            integer(int32), intent(in) :: indices_reordered(:)
            integer(int32), intent(inout) :: indices_original(:)

        end subroutine to_original_indices

    end interface

    interface
        module subroutine to_reordered_values_int32(self, vector_original, vector_reordered)
            implicit none
            class(type_reordering), intent(in) :: self
            integer(int32), intent(in) :: vector_original(:)
            integer(int32), intent(inout) :: vector_reordered(:)

        end subroutine to_reordered_values_int32

        module subroutine to_reordered_values_real64(self, vector_original, vector_reordered)
            implicit none
            class(type_reordering), intent(in) :: self
            real(real64), intent(in) :: vector_original(:)
            real(real64), intent(inout) :: vector_reordered(:)

        end subroutine to_reordered_values_real64

        module subroutine to_reordered_index(self, index_original, index_reordered)
            implicit none
            class(type_reordering), intent(in) :: self
            integer(int32), intent(in) :: index_original
            integer(int32), intent(inout) :: index_reordered

        end subroutine to_reordered_index

        module subroutine to_reordered_indices(self, indices_original, indices_reordered)
            implicit none
            class(type_reordering), intent(in) :: self
            integer(int32), intent(in) :: indices_original(:)
            integer(int32), intent(inout) :: indices_reordered(:)

        end subroutine to_reordered_indices
    end interface

contains

    subroutine type_reordering_initialize(self, algorithm_name, elements)
        implicit none
        class(type_reordering), intent(inout) :: self
        character(*), intent(in) :: algorithm_name
        type(holder_elements), intent(in) :: elements(:)

        self%algorithm_name = trim(adjustl(algorithm_name))
        select case (self%algorithm_name)
        case ("rcm")
            call self%rcm_reorder_method(elements)
            call self%rcm_inverse_method()
        case ("cm")
            call self%cm_reorder_method(elements)
            call self%cm_inverse_method()
        end select

    end subroutine type_reordering_initialize

    !================================================================!
    ! finalizer
    !================================================================!
    subroutine type_reordering_finalize(self)
        implicit none
        type(type_reordering), intent(inout) :: self

        call deallocate_array(self%perm)
        call deallocate_array(self%iperm)
    end subroutine type_reordering_finalize

end module domain_reordering
