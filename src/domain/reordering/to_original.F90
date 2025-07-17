submodule(domain_reordering) reordering_to_original
    implicit none

contains

    !================================================================!
    ! CM/RCM ordering -> original ordering
    !================================================================!
    module subroutine to_original_values_int32(self, vector_reordered, vector_original)
        implicit none
        class(type_reordering), intent(in) :: self
        integer(int32), intent(in) :: vector_reordered(:)
        integer(int32), intent(inout) :: vector_original(:)

        integer(int32) :: i

        if (size(vector_reordered) /= self%num_nodes .or. size(vector_original) /= self%num_nodes) error stop "Size mismatch"
        if (self%algorithm_name == "none") then
            vector_original(:) = vector_reordered(:)
            return
        end if
        if (.not. self%is_reordered_perm) error stop "'perm' not ready. Call 'reorder' first."

        do i = 1, self%num_nodes
            vector_original(self%perm(i)) = vector_reordered(i)
        end do

    end subroutine to_original_values_int32

    module subroutine to_original_values_real64(self, vector_reordered, vector_original)
        implicit none
        class(type_reordering), intent(in) :: self
        real(real64), intent(in) :: vector_reordered(:)
        real(real64), intent(inout) :: vector_original(:)

        integer(int32) :: i

        if (size(vector_reordered) /= self%num_nodes .or. size(vector_original) /= self%num_nodes) error stop "Size mismatch"
        if (self%algorithm_name == "none") then
            vector_original(:) = vector_reordered(:)
            return
        end if
        if (.not. self%is_reordered_perm) error stop "'perm' not ready. Call 'reorder' first."

        do i = 1, self%num_nodes
            vector_original(self%perm(i)) = vector_reordered(i)
        end do

    end subroutine to_original_values_real64

    module subroutine to_original_index(self, index_reordered, index_original)
        implicit none
        class(type_reordering), intent(in) :: self
        integer(int32), intent(in) :: index_reordered
        integer(int32), intent(inout) :: index_original

        if (self%algorithm_name == "none") then
            index_original = index_reordered
            return
        end if
        if (.not. self%is_reordered_perm) error stop "'perm' not ready. Call 'reorder' first."

        index_original = self%perm(index_reordered)

    end subroutine to_original_index

    module subroutine to_original_indices(self, indices_reordered, indices_original)
        implicit none
        class(type_reordering), intent(in) :: self
        integer(int32), intent(in) :: indices_reordered(:)
        integer(int32), intent(inout) :: indices_original(:)

        integer(int32) :: i

        if (size(indices_reordered) /= size(indices_original)) error stop "Size mismatch"
        if (self%algorithm_name == "none") then
            indices_original(:) = indices_reordered(:)
            return
        end if

        if (.not. self%is_reordered_perm) error stop "'perm' not ready. Call 'reorder' first."

        do i = 1, size(indices_reordered)
            indices_original(i) = self%perm(indices_reordered(i))
        end do

    end subroutine to_original_indices

end submodule reordering_to_original
