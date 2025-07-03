! main.f90 (MATLAB連携ファイル出力機能付き)
program main
    use Matrix_RCM, only: rcm_reorder
    implicit none

    ! メッシュ定義
    integer, parameter :: num_nodes = 16
    integer, parameter :: num_elements = 9
    integer, parameter :: nodes_per_element = 4

    ! 変数宣言
    integer :: elements(nodes_per_element, num_elements)
    integer, allocatable :: perm(:)
    integer :: istat, i
    logical, allocatable :: matrix_before(:, :), matrix_after(:, :)
    integer :: j, k, n1, n2, new_n1, new_n2
    integer :: unit_before, unit_after

    ! サンプルメッシュデータ
    elements = reshape([ &
                       1, 2, 6, 5, 2, 3, 7, 6, 3, 4, 8, 7, &
                       5, 6, 10, 9, 6, 7, 11, 10, 7, 8, 12, 11, &
                       9, 10, 14, 13, 10, 11, 15, 14, 11, 12, 16, 15], &
                       [nodes_per_element, num_elements])

    write (*, *) "==============================================="
    write (*, *) "     Fortran RCM Subroutine Demo"
    write (*, *) "==============================================="

    ! RCMサブルーチンを呼び出す
    call rcm_reorder(num_nodes, num_elements, nodes_per_element, elements, perm, istat)

    if (istat == 0) then
        write (*, *) "RCM reordering successful."
        write (*, *)

        ! ★★★★★ ここからがファイル出力ロジック ★★★★★

        ! 1. 隣接行列をメモリ上に構築
        allocate (matrix_before(num_nodes, num_nodes), matrix_after(num_nodes, num_nodes))
        matrix_before = .false.
        matrix_after = .false.
        do i = 1, num_elements
            do j = 1, nodes_per_element
                do k = j + 1, nodes_per_element
                    n1 = elements(j, i); n2 = elements(k, i)
                    matrix_before(n1, n2) = .true.; matrix_before(n2, n1) = .true.

                    new_n1 = perm(n1); new_n2 = perm(n2)
                    matrix_after(new_n1, new_n2) = .true.; matrix_after(new_n2, new_n1) = .true.
                end do
            end do
        end do

        ! 2. ファイルを開く
        open (newunit=unit_before, file='matrix_before.txt', status='replace', action='write')
        open (newunit=unit_after, file='matrix_after.txt', status='replace', action='write')

        ! 3. 行列をループし、非ゼロ要素を三重項形式 (i, j, value) で書き出す
        do j = 1, num_nodes
            do i = 1, num_nodes
                if (matrix_before(i, j)) then
                    write (unit_before, '(i0, 1x, i0, 1x, f3.1)') i, j, 1.0
                end if
                if (matrix_after(i, j)) then
                    write (unit_after, '(i0, 1x, i0, 1x, f3.1)') i, j, 1.0
                end if
            end do
        end do

        ! 4. ファイルを閉じる
        close (unit_before)
        close (unit_after)

        write (*, *) "Output files for MATLAB have been created:"
        write (*, *) "- matrix_before.txt"
        write (*, *) "- matrix_after.txt"
        write (*, *)

        deallocate (matrix_before, matrix_after)

    else
        write (*, *) "ERROR: RCM reordering failed with status: ", istat
    end if

    write (*, *) "==============================================="

    if (allocated(perm)) deallocate (perm)

    ! --- MATLABでの利用方法 ---
    !
    ! 以下のスクリプトをMATLABで実行すると、行列のスパースパターンを可視化できます。
    !
    ! % MATLAB script to visualize sparse matrices
    !
    ! % RCM適用前のデータを読み込み
    ! data_before = load('matrix_before.txt');
    ! i_b = data_before(:,1);
    ! j_b = data_before(:,2);
    ! v_b = data_before(:,3);
    ! n = max([i_b; j_b]);
    ! A_before = sparse(i_b, j_b, v_b, n, n);
    !
    ! % RCM適用後のデータを読み込み
    ! data_after = load('matrix_after.txt');
    ! i_a = data_after(:,1);
    ! j_a = data_after(:,2);
    ! v_a = data_after(:,3);
    ! n = max([i_a; j_a]);
    ! A_after = sparse(i_a, j_a, v_a, n, n);
    !
    ! % プロット
    ! figure;
    ! subplot(1,2,1);
    ! spy(A_before);
    ! title('Sparsity Pattern BEFORE RCM');
    !
    ! subplot(1,2,2);
    ! spy(A_after);
    ! title('Sparsity Pattern AFTER RCM');
    !

end program main
