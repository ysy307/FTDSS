module Matrix_CRS
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Allocate_Allocate, only:Allocate_Array
    implicit none

    ! public :: Type_CRS
    ! public :: operator(*)
    ! public :: operator(+)
    ! public :: Transpose_CRS

    type :: Type_CRS
        integer(int32) :: nnz ! number of non-zero elements
        integer(int32) :: ncol ! number of columns
        integer(int32) :: nrow ! size of Ptr (nrow+1 entries)
        integer(int32), allocatable :: Ptr(:) ! pointers to row starts (1-based)
        integer(int32), allocatable :: Ind(:) ! column indices of non-zeros
        real(real64), allocatable :: Val(:) ! non-zero values
    contains
        procedure, public, pass(self) :: Find => Find_CRS_Location
        procedure, public, pass(self) :: Copy => Copy_CRS
    end type Type_CRS

    interface operator(*)
        module procedure Matrix_Vector_Product_CRS
    end interface
    interface operator(+)
        module procedure Matrix_Addition_CRS
    end interface

    interface Type_CRS
        module procedure Initialize_CRS
    end interface

contains

    function Initialize_CRS(Elements, nNode) result(A)
        implicit none
        integer(int32), intent(in) :: Elements(:, :)
        integer(int32), intent(in) :: nNode
        type(Type_CRS) :: A

        integer(int32) :: iN, iE, iT, irT, iNC, iNNZ, row_nnz
        integer(int32) :: nElement, nTop
        integer(int32), allocatable :: vertex(:), rowCount(:), tmpInd(:)

        nTop = size(Elements, 1)
        nElement = size(Elements, 2)
        ! Set dimensions
        A%ncol = nNode
        A%nrow = nNode + 1 ! Ptr has nNode+1 entries

        ! Allocate temp arrays
        call Allocate_Array(A%Ptr, A%nrow)
        call Allocate_Array(rowCount, nNode)
        call Allocate_Array(tmpInd, 10_int32 * nNode)
        call Allocate_Array(vertex, nTop)

        A%Ptr(1) = 1
        A%nnz = 0
        do iN = 1, nNode
            rowCount = 0
            row_nnz = 0
            ! Scan elements to build sparsity row
            do iE = 1, nElement
                vertex = Elements(:, iE)
                do iT = 1, nTop
                    if (vertex(iT) == iN) then
                        do irT = 1, nTop
                            rowCount(vertex(irT)) = 1
                        end do
                        exit
                    end if
                end do
            end do
            ! Collect indices
            do iNC = 1, nNode
                if (rowCount(iNC) == 1) then
                    tmpInd(A%nnz + row_nnz + 1) = iNC
                    row_nnz = row_nnz + 1
                end if
            end do
            A%nnz = A%nnz + row_nnz
            A%Ptr(iN + 1) = A%nnz + 1
        end do

        ! Allocate CRS arrays
        call Allocate_Array(A%Ind, A%nnz)
        call Allocate_Array(A%Val, A%nnz)
        do iNNZ = 1, A%nnz
            A%Ind(iNNZ) = tmpInd(iNNZ)
            A%Val(iNNZ) = 0.0_real64
        end do

        deallocate (vertex, rowCount, tmpInd)
    end function Initialize_CRS

    function Matrix_Vector_Product_CRS(A, x) result(y)
        implicit none
        type(Type_CRS), intent(in) :: A
        real(real64), intent(in) :: x(A%ncol)
        real(real64) :: y(A%nrow - 1)
        integer(int32) :: i, j, is, ie
        real(real64) :: sum

        y = 0.0_real64
        do i = 1, A%nrow - 1
            sum = 0.0_real64
            is = A%Ptr(i)
            ie = A%Ptr(i + 1) - 1
            do j = is, ie
                sum = sum + A%Val(j) * x(A%Ind(j))
            end do
            y(i) = sum
        end do
    end function Matrix_Vector_Product_CRS

    function Matrix_Addition_CRS(A, B) result(C)
        implicit none
        type(Type_CRS), intent(in) :: A, B
        type(Type_CRS) :: C
        integer(int32) :: k

        ! Assume same sparsity structure
        C%nrow = A%nrow
        C%ncol = A%ncol
        C%nnz = A%nnz
        call Allocate_Array(C%Ptr, A%nrow)
        call Allocate_Array(C%Ind, A%nnz)
        call Allocate_Array(C%Val, A%nnz)
        C%Ptr = A%Ptr
        C%Ind = A%Ind
        do k = 1, A%nnz
            C%Val(k) = A%Val(k) + B%Val(k)
        end do
    end function Matrix_Addition_CRS

    subroutine Find_CRS_Location(self, column, index_in, loc)
        implicit none
        class(Type_CRS), intent(in) :: self
        integer(int32), intent(in) :: column, index_in
        integer(int32), intent(out) :: loc
        integer(int32) :: i, start, endp

        loc = 0
        start = self%Ptr(column)
        endp = self%Ptr(column + 1) - 1
        do i = start, endp
            if (self%Ind(i) == index_in) then
                loc = i; return
            end if
        end do
    end subroutine Find_CRS_Location

    function Copy_CRS(self) result(B)
        implicit none
        class(Type_CRS) :: self
        type(Type_CRS) :: B
        integer(int32) :: k

        B%nrow = self%nrow
        B%ncol = self%ncol
        B%nnz = self%nnz
        call Allocate_Array(B%Ptr, self%nrow)
        call Allocate_Array(B%Ind, self%nnz)
        call Allocate_Array(B%Val, self%nnz)
        B%Ptr = self%Ptr
        B%Ind = self%Ind
        B%Val = self%Val
    end function Copy_CRS

    function Transpose_CRS(self) result(AT)
        implicit none
        class(Type_CRS) :: self
        type(Type_CRS) :: AT
        integer(int32) :: i, j, row, col, dst
        integer(int32), allocatable :: col_count(:), next_pos(:)

        ! Setup AT dimensions
        AT%nrow = self%ncol + 1
        AT%ncol = self%nrow - 1
        AT%nnz = self%nnz

        ! Count entries per column in A
        call Allocate_Array(col_count, self%ncol)
        col_count(:) = 0
        do row = 1, self%nrow - 1
            do i = self%Ptr(row), self%Ptr(row + 1) - 1
                col_count(self%Ind(i)) = col_count(self%Ind(i)) + 1
            end do
        end do

        ! Build AT%Ptr
        call Allocate_Array(AT%Ptr, AT%nrow)
        AT%Ptr(1) = 1
        do i = 1, AT%nrow - 1
            AT%Ptr(i + 1) = AT%Ptr(i) + col_count(i)
        end do

        ! Allocate Ind, Val and next_pos
        call Allocate_Array(AT%Ind, AT%nnz)
        call Allocate_Array(AT%Val, AT%nnz)
        allocate (next_pos(self%ncol))
        next_pos = AT%Ptr(1:AT%nrow - 1)

        ! Fill AT
        do row = 1, self%nrow - 1
            do i = self%Ptr(row), self%Ptr(row + 1) - 1
                col = self%Ind(i)
                dst = next_pos(col)
                AT%Ind(dst) = row
                AT%Val(dst) = self%Val(i)
                next_pos(col) = dst + 1
            end do
        end do

        deallocate (col_count, next_pos)
    end function Transpose_CRS

end module Matrix_CRS
