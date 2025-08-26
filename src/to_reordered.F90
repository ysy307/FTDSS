submodule(domain_reordering) reordering_to_reordered
    implicit none

contains

    !================================================================!
    !  original ordering -> CM/RCM ordering
    !================================================================!
    module subroutine to_reordered_values_int32(self, vector_original, vector_reordered)
        implicit none
        class(type_reordering), intent(in) :: self
        integer(int32), intent(in) :: vector_original(:)
        integer(int32), intent(inout) :: vector_reordered(:)

        integer(int32) :: i

        if (size(vector_original) /= size(vector_reordered)) error stop "Size mismatch"
        if (self%algorithm_name == "none") then
            vector_reordered(:) = vector_original(:)
            return
        end if
        if (.not. self%is_reordered_iperm) error stop "'iperm' not ready. Call 'invert' first."

        do i = 1, self%num_nodes
            vector_reordered(self%iperm(i)) = vector_original(i)
        end do

    end subroutine to_reordered_values_int32

    module subroutine to_reordered_values_real64(self, vector_original, vector_reordered)
        implicit none
        class(type_reordering), intent(in) :: self
        real(real64), intent(in) :: vector_original(:)
        real(real64), intent(inout) :: vector_reordered(:)

        integer(int32) :: i

        if (size(vector_original) /= size(vector_reordered)) error stop "Size mismatch"
        if (self%algorithm_name == "none") then
            vector_reordered(:) = vector_original(:)
            return
        end if
        if (.not. self%is_reordered_iperm) error stop "'iperm' not ready. Call 'invert' first."

        do i = 1, self%num_nodes
            vector_reordered(self%iperm(i)) = vector_original(i)
        end do

    end subroutine to_reordered_values_real64

    module subroutine to_reordered_index(self, index_original, index_reordered)
        implicit none
        class(type_reordering), intent(in) :: self
        integer(int32), intent(in) :: index_original
        integer(int32), intent(inout) :: index_reordered

        if (self%algorithm_name == "none") then
            index_reordered = index_original
            return
        end if

        if (.not. self%is_reordered_iperm) error stop "'iperm' not ready. Call 'invert' first."

        index_reordered = self%iperm(index_original)

    end subroutine to_reordered_index

    module subroutine to_reordered_indices(self, indices_original, indices_reordered)
        implicit none
        class(type_reordering), intent(in) :: self
        integer(int32), intent(in) :: indices_original(:)
        integer(int32), intent(inout) :: indices_reordered(:)

        integer(int32) :: i

        if (size(indices_original) /= size(indices_reordered)) error stop "Size mismatch"

        if (self%algorithm_name == "none") then
            indices_reordered(:) = indices_original(:)
            return
        end if

        if (.not. self%is_reordered_iperm) error stop "'iperm' not ready. Call 'invert' first."

        do i = 1, size(indices_original)
            indices_reordered(i) = self%iperm(indices_original(i))
        end do

    end subroutine to_reordered_indices

end submodule reordering_to_reordered
