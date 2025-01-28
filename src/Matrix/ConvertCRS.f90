module Matrix_ConvertCRS
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    use :: Allocate_Allocate

    implicit none
    private

    public Convert_CRS

contains

    subroutine Convert_CRS(Geometry, A)
        implicit none
        type(Type_Geometry), intent(in) :: Geometry
        type(CRS), intent(inout) :: A
        ! integer(int32)                  :: i, j, k, l, row_nnz
        integer(int32) :: iN, iE, iT, irT, iNC, iNNZ, row_nnz
        integer(int32) :: nElment, nTop, nNode
        integer(int32), allocatable :: vertex(:), row(:), tmpInd(:)

        nElment = Geometry%Basic%Element
        nTop = 3
        nNode = Geometry%Basic%Node
        ! 0オリジンでベクトルを作成
        call Allocate_Array(A%Ptr, 0_int32, nNode)
        call Allocate_Array(row, 0_int32, nNode - 1_int32)
        call Allocate_Array(tmpInd, 0_int32, 8_int32 * nNode)
        call Allocate_Array(vertex, nTop)

        A%Ptr(0) = 0
        A%nnz = 0
        do iN = 1, nNode
            row(:) = 0
            row_nnz = 0
            do iE = 1, nElment
                ! iE番目の要素の頂点を取得
                get_element_node: &
                & do iT = 1, nTop
                    vertex(iT) = Geometry%Element(iT, iE)
                end do get_element_node
                ! j番目の要素の頂点のうちいずれかの頂点がiNと一致する場合，ベクトルにインクリメント
                increament_vector: &
            & do iT = 1, nTop
                    if (vertex(iT) == iN) then
                        do irT = 1, nTop
                            row(vertex(irT) - 1) = row(vertex(irT) - 1) + 1
                        end do
                        exit increament_vector
                    end if
                end do increament_vector
            end do
            ! ベクトルの非ゼロ要素の数をカウントし，CRSの仮ポインタに格納
            do iNC = 0, nNode - 1
                if (row(iNC) > 0) then
                    tmpInd(row_nnz + A%nnz) = iNC
                    row_nnz = row_nnz + 1
                end if
            end do
            A%nnz = A%nnz + row_nnz
            A%Ptr(iN) = A%nnz
        end do

        ! CRS Matrixのメモリ確保 (0オリジン)
        call Allocate_Array(A%Ind, 0_int32, A%nnz - 1_int32)
        call Allocate_Array(A%val, 0_int32, A%nnz - 1_int32)

        ! CRSの初期化及び値の格納
        do iNNZ = 0, A%nnz - 1
            A%val(iNNZ) = 0.0d0
            A%Ind(iNNZ) = tmpInd(iNNZ)
        end do

        deallocate (vertex)
        deallocate (row)
        deallocate (tmpInd)
    end subroutine Convert_CRS
end module Matrix_ConvertCRS
