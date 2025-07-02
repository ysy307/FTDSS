module Matrix_CRS
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_Allocate, only:Allocate_Array
    use :: Domain_Module, only:Domain_t
    implicit none
    private

    public :: Type_CRS
    public :: operator(*)
    public :: operator(+)
    ! public :: Transpose_CRS

    type :: Type_CRS
        integer(int32) :: nnz ! number of non-zero elements
        integer(int32) :: nrow ! number of rows
        integer(int32) :: nptr ! size of Ptr (nrow+1 entries)
        integer(int32), allocatable :: Ptr(:) ! pointers to row starts (1-based)
        integer(int32), allocatable :: Ind(:) ! column indices of non-zeros
        real(real64), allocatable :: Val(:) ! non-zero values
    contains
        procedure, public, pass(self) :: Find => Find_CRS_Location
        procedure, public, pass(self) :: Copy => Copy_CRS
    end type Type_CRS

    interface operator(*)
        module procedure Matrix_Vector_Product_CRS
        module procedure Multiplication_Scalar_Matrix_CRS
        module procedure Multiplication_Matrix_Scalar_CRS
    end interface
    interface operator(+)
        module procedure Matrix_Addition_CRS
    end interface

    interface Type_CRS
        module procedure Initialize_CRS
    end interface

contains

    function Initialize_CRS(Domain) result(A)
        implicit none
        type(Domain_t), intent(in) :: Domain
        type(Type_CRS) :: A
        integer(int32) :: iTop

        integer(int32) :: iN, iE, iT, irT, iNC, iNNZ, row_nnz, nsize
        integer(int32) :: nNode, nElement
        integer(int32), allocatable :: rowCount(:), tmpInd(:)

        ! Set dimensions
        nNode = Domain%get_numNode()
        nElement = Domain%get_numElement()
        A%nrow = nNode
        A%nptr = nNode + 1

        ! Allocate temp arrays
        call Allocate_Array(A%Ptr, A%nptr)
        call Allocate_Array(rowCount, nNode)
        call Allocate_Array(tmpInd, 30_int32 * nNode)

        A%Ptr(1) = 1
        A%nnz = 0
        do iN = 1, nNode
            rowCount = 0
            row_nnz = 0
            ! Scan elements to build sparsity row
            do iE = 1, nElement
                nsize = Domain%Elements(iE)%e%get_size()
                do iT = 1, nsize
                    if (Domain%Elements(iE)%e%conn(iT) == iN) then
                        do irT = 1, nsize
                            rowCount(Domain%Elements(iE)%e%conn(irT)) = 1
                        end do
                        exit
                    end if
                end do
            end do
            ! Collect indices
            do iNC = 1, nNode
                if (rowCount(iNC) >= 1) then
                    row_nnz = row_nnz + 1
                    tmpInd(A%nnz + row_nnz) = iNC
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
            A%Val(iNNZ) = 0.0d0
        end do

        deallocate (rowCount, tmpInd)
    end function Initialize_CRS

    function Matrix_Vector_Product_CRS(A, x) result(y)
        implicit none
        type(Type_CRS), intent(in) :: A
        real(real64), intent(in) :: x(A%nrow)
        real(real64) :: y(A%nrow)
        integer(int32) :: i, j, is, ie
        real(real64) :: sum

        y(:) = 0.0d0
        do i = 1, A%nrow
            sum = 0.0d0
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
        C = A%Copy()
        do k = 1, A%nnz
            C%Val(k) = A%Val(k) + B%Val(k)
        end do
    end function Matrix_Addition_CRS

    function Multiplication_Scalar_Matrix_CRS(A, b) result(C)
        implicit none
        type(Type_CRS), intent(in) :: A
        real(real64), intent(in) :: b
        type(Type_CRS) :: C
        integer(int32) :: k

        ! Assume same sparsity structure
        C = A%Copy()
        do k = 1, A%nnz
            C%Val(k) = A%Val(k) * b
        end do
    end function Multiplication_Scalar_Matrix_CRS

    function Multiplication_Matrix_Scalar_CRS(a, B) result(C)
        implicit none
        real(real64), intent(in) :: a
        type(Type_CRS), intent(in) :: B
        type(Type_CRS) :: C
        integer(int32) :: k

        ! Assume same sparsity structure
        C = B%Copy()
        do k = 1, B%nnz
            C%Val(k) = B%Val(k) * a
        end do
    end function Multiplication_Matrix_Scalar_CRS

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
        B%nptr = self%nptr
        B%nnz = self%nnz
        call Allocate_Array(B%Ptr, self%nptr)
        call Allocate_Array(B%Ind, self%nnz)
        call Allocate_Array(B%Val, self%nnz)
        B%Ptr(:) = self%Ptr(:)
        B%Ind(:) = self%Ind(:)
        B%Val(:) = self%Val(:)
    end function Copy_CRS

end module Matrix_CRS
