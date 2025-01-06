module Matrix_FindInd
    use, intrinsic :: iso_fortran_env, only : int32, real64
    use :: Types, udmp => undumped, dmp => dumped

    implicit none
    private

    public :: Find_CRS_Index
    public :: Find_CRS_Indexes

    contains

    subroutine Find_CRS_Index(A, serch_column, serch_index, index)
		implicit none
		type(CRS),      intent(in)    :: A
		integer(int32), intent(in)    :: serch_column, serch_index
		integer(int32), intent(inout) :: index
		integer(int32)                :: i, start_index, end_index

		index = 0
		! serch_columnのindex範囲を取得
		start_index = A%Ptr(serch_column - 1)
		end_index   = A%Ptr(serch_column) - 1

		! 範囲内でserch_indexになる値のインデックスを見つける
		do i = start_index, end_index
			if (A%Ind(i) == serch_index - 1) then
				index = i
			end if
		end do

	end subroutine Find_CRS_Index

	subroutine Find_CRS_Indexes(lump, A, serch_index1, serch_index2, serch_index3, indexes)
		implicit none
		type(CRS),      intent(in)    :: A
		integer(int32), intent(in)    :: lump, serch_index1, serch_index2, serch_index3
		integer(int32), intent(inout) :: indexes(:)

		if (lump == udmp) then
			call Find_CRS_Index(A, serch_index1, serch_index1, indexes(1))
			call Find_CRS_Index(A, serch_index1, serch_index2, indexes(2))
			call Find_CRS_Index(A, serch_index1, serch_index3, indexes(3))
			call Find_CRS_Index(A, serch_index2, serch_index1, indexes(4))
			call Find_CRS_Index(A, serch_index2, serch_index2, indexes(5))
			call Find_CRS_Index(A, serch_index2, serch_index3, indexes(6))
			call Find_CRS_Index(A, serch_index3, serch_index1, indexes(7))
			call Find_CRS_Index(A, serch_index3, serch_index2, indexes(8))
			call Find_CRS_Index(A, serch_index3, serch_index3, indexes(9))
		else if (lump == dmp) then
			call Find_CRS_Index(A, serch_index1, serch_index1, indexes(1))
			call Find_CRS_Index(A, serch_index2, serch_index2, indexes(2))
			call Find_CRS_Index(A, serch_index3, serch_index3, indexes(3))
		end if
	end subroutine Find_CRS_Indexes

end module Matrix_FindInd