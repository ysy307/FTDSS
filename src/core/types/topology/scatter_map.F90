!> Module for managing a scatter map data structure.
!>
!> Provides a container that maps multi-dimensional local indices
!> within segments (e.g. elements) to storage indices of a matrix.
module core_types_topology_scatter_map
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    public :: type_scatter_map

    !> Scatter map container.
    !>
    !> Stores a mapping:
    !>   (segment, local multi-index) → matrix storage index
    !>
    !> Typically used for assembling element contributions into
    !> sparse matrix formats (e.g. CSR/BSR).
    type :: type_scatter_map
        private

        !> Storage index (e.g. BSR block index)
        integer(int32), allocatable :: data(:)

        !> Segment pointer (CSR-like)
        integer(int32), allocatable :: ptr(:)

        !> Local shape per segment (rank, n_segment)
        integer(int32), allocatable :: shape(:, :)

        !> Row-major strides (rank, n_segment)
        integer(int32), allocatable :: stride(:, :)

        integer(int32) :: n_segment = 0
        integer(int32) :: rank = 0
        logical :: ready = .false.

    contains
        procedure, public :: initialize => initialize_scatter_map
        procedure, public :: destroy => destroy_scatter_map
        procedure, public :: get_index => get_index_scatter_map
        procedure, public :: get_matrix_indices => get_matrix_indices_scatter_map
        procedure, public :: set => set_value_scatter_map
        procedure, private :: get_flat => get_flat_index
    end type type_scatter_map

contains

    !> Initialize scatter map structure.
    subroutine initialize_scatter_map(self, n_segment, rank, shape_in)
        implicit none
        class(type_scatter_map), intent(inout) :: self
        integer(int32), intent(in) :: n_segment
        integer(int32), intent(in) :: rank
        integer(int32), intent(in) :: shape_in(rank, n_segment)

        integer(int32) :: i, d
        integer(int32) :: total_size

        call self%destroy()

        self%n_segment = n_segment
        self%rank = rank

        allocate (self%ptr(n_segment + 1))
        allocate (self%shape(rank, n_segment))
        allocate (self%stride(rank, n_segment))

        self%shape = shape_in
        total_size = 0

        do i = 1, n_segment
            self%ptr(i) = total_size + 1
            total_size = total_size + product(self%shape(:, i))

            self%stride(rank, i) = 1
            do d = rank - 1, 1, -1
                self%stride(d, i) = self%stride(d + 1, i) * self%shape(d + 1, i)
            end do
        end do

        self%ptr(n_segment + 1) = total_size + 1

        allocate (self%data(total_size))
        self%data = 0

        self%ready = .true.
    end subroutine initialize_scatter_map

    !> Compute flat index.
    subroutine get_flat_index(self, seg, idx_local, flat)
        implicit none
        class(type_scatter_map), intent(in) :: self
        integer(int32), intent(in) :: seg
        integer(int32), intent(in) :: idx_local(:)
        integer(int32), intent(inout) :: flat

        integer(int32) :: d

        if (.not. self%ready) stop

        if (seg < 1 .or. seg > self%n_segment) stop
        if (size(idx_local) /= self%rank) stop

        flat = self%ptr(seg)

        do d = 1, self%rank
            if (idx_local(d) < 1 .or. idx_local(d) > self%shape(d, seg)) stop
            flat = flat + (idx_local(d) - 1) * self%stride(d, seg)
        end do
    end subroutine get_flat_index

    !> Get matrix storage index.
    subroutine get_index_scatter_map(self, seg, idx_local, index)
        implicit none
        class(type_scatter_map), intent(in) :: self
        integer(int32), intent(in) :: seg
        integer(int32), intent(in) :: idx_local(:)
        integer(int32), intent(inout) :: index

        integer(int32) :: flat

        call self%get_flat(seg, idx_local, flat)
        index = self%data(flat)
    end subroutine get_index_scatter_map

    subroutine get_matrix_indices_scatter_map(self, seg, indices)
        implicit none
        class(type_scatter_map), intent(in) :: self
        integer(int32), intent(in) :: seg
        integer(int32), intent(inout) :: indices(:, :)

        integer(int32) :: i, j, flat

        if (.not. self%ready) stop
        if (seg < 1 .or. seg > self%n_segment) stop
        if (self%rank /= 2) stop
        if (size(indices, 1) /= self%shape(1, seg) .or. size(indices, 2) /= self%shape(2, seg)) stop

        do j = 1, size(indices, 2)
            do i = 1, size(indices, 1)
                flat = self%ptr(seg) + (i - 1) * self%stride(1, seg) + (j - 1) * self%stride(2, seg)
                indices(i, j) = self%data(flat)
            end do
        end do
    end subroutine get_matrix_indices_scatter_map

    !> Set matrix storage index.
    subroutine set_value_scatter_map(self, seg, idx_local, value)
        implicit none
        class(type_scatter_map), intent(inout) :: self
        integer(int32), intent(in) :: seg
        integer(int32), intent(in) :: idx_local(:)
        integer(int32), intent(in) :: value

        integer(int32) :: flat

        call self%get_flat(seg, idx_local, flat)
        self%data(flat) = value
    end subroutine set_value_scatter_map

    !> Destroy scatter map.
    subroutine destroy_scatter_map(self)
        implicit none
        class(type_scatter_map), intent(inout) :: self

        if (allocated(self%data)) deallocate (self%data)
        if (allocated(self%ptr)) deallocate (self%ptr)
        if (allocated(self%shape)) deallocate (self%shape)
        if (allocated(self%stride)) deallocate (self%stride)

        self%n_segment = 0
        self%rank = 0
        self%ready = .false.
    end subroutine destroy_scatter_map

end module core_types_topology_scatter_map
