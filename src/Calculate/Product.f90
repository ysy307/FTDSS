module Calculate_Product
	use omp_lib
    use, intrinsic :: iso_fortran_env, only : int32, real64
    use :: Types
    implicit none
    private

    public :: Matrix_Vector_Product_CRS

    contains

    !* 行列ベクトル積を計算するサブルーチン
	subroutine Matrix_Vector_Product_CRS(A, x, y)
		implicit none
		type(CRS),    intent(in)    :: A
		real(real64), intent(in)    :: x(:)
		real(real64), intent(inout) :: y(:)
		real(real64)                :: vtemp
		integer(int32)              :: i, j, is, ie
		integer(int32)              :: matrix_size

		matrix_size = size(A%ptr) - 1
		y(:) = 0.0d0

		!$omp parallel do private(vtemp, i, j, is, ie)
		do i = 1, matrix_size
			vtemp = 0.0d0

			is = A%ptr(i-1)
			ie = A%ptr(i) - 1
			do j = is, ie
				vtemp = vtemp + A%val(j) * x(A%ind(j)+1)
			end do
			y(i) = vtemp
		end do
		!$omp end parallel do
	end subroutine Matrix_Vector_Product_CRS

end module Calculate_Product