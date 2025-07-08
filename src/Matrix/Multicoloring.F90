! Matrix_Multicoloring.f90

! ==============================================================================
! Module: Matrix_Multicoloring
!
! Purpose:
!   新しいデータ構造を持つDomainオブジェクトを受け取り、彩色を実行して、
!   詳細な結果を Domain%Colors に格納します。
! ==============================================================================
module Matrix_Multicoloring
    use, intrinsic :: iso_fortran_env, only: int32, logical32
    ! Domain_Moduleはユーザー提供のものを使用
    use :: Domain_Module, only:Domain_t
    use :: matrix_adjacency, only:type_adjacency
    use :: core_core, only:allocate_array
    implicit none

    private
    public :: Multicoloring ! 公開インタフェース

    type :: type_vertex_degree
        integer(int32) :: index
        integer(int32) :: degree
    end type type_vertex_degree

    interface assignment(=)
        module procedure type_vertex_degree_Assignment
    end interface

contains

    ! ==============================================================================
    ! Subroutine: Multicoloring (Public Driver)
    ! ==============================================================================
    subroutine Multicoloring(Domain)
        implicit none
        class(Domain_t), intent(inout) :: Domain

        ! ローカル変数
        type(type_adjacency) :: adjacency
        integer(int32), allocatable :: temp_colors(:)
        integer(int32) :: temp_num_colors
        integer(int32) :: n_elements
        integer(int32) :: i, c, count_per_color

        n_elements = Domain%get_numElement()
        if (n_elements == 0) return

        ! 1. 隣接行列を構築
        call adjacency%initialize(Domain)

        ! 2. Welsh-Powellアルゴリズムを実行し、基本的な彩色結果を得る
        call Multicoloring_welsh_powell(adjacency, temp_colors, temp_num_colors)

        ! 3a. 基本的な情報を格納
        Domain%Colors%nColor = temp_num_colors
        call allocate_array(Domain%Colors%Color, n_elements)
        Domain%Colors%Color(:) = temp_colors(:)

        ! 3b. 各色ごとの詳細情報 (Colored(:)) を格納
        if (Domain%Colors%nColor > 0) then
            allocate (Domain%Colors%Colored(Domain%Colors%nColor))
            do c = 1, Domain%Colors%nColor
                ! 現在の色(c)を持つ要素の数を数える
                count_per_color = count(temp_colors == c)
                Domain%Colors%Colored(c)%numElements = count_per_color

                if (count_per_color > 0) then
                    ! その色の要素リストを作成して格納
                    call allocate_array(Domain%Colors%Colored(c)%Elements, count_per_color)
                    Domain%Colors%Colored(c)%Elements = pack([(i, i=1, n_elements)], mask=(temp_colors == c))
                end if
            end do
        end if

        call adjacency%destroy()
        deallocate (temp_colors)

    end subroutine Multicoloring

    ! ==============================================================================
    ! Subroutine: Multicoloring_welsh_powell (Private Worker)
    ! (このサブルーチンの実装は以前の回答から変更ありません)
    ! ==============================================================================
    subroutine Multicoloring_welsh_powell(Adjacency, colors, num_colors)
        implicit none
        class(type_adjacency), intent(in) :: Adjacency
        integer(int32), allocatable, intent(inout) :: colors(:)
        integer(int32), intent(inout) :: num_colors

        integer(int32) :: num_vertices !! 無向グラフの頂点数
        integer(int32) :: i, j, k
        integer(int32) :: v_idx, u_idx
        logical(logical32) :: can_color

        type(type_vertex_degree), allocatable :: sorted_vertices(:)
        type(type_vertex_degree) :: temp_vertex

        num_vertices = Adjacency%get_num_elements()
        if (num_vertices == 0) return

        ! 初期化
        call allocate_array(colors, num_vertices)
        num_colors = 0
        colors(:) = 0
        print *, "Number of vertices:", num_vertices
        if (allocated(sorted_vertices)) deallocate (sorted_vertices)
        allocate (sorted_vertices(num_vertices))

        do i = 1, num_vertices
            sorted_vertices(i)%index = i
            sorted_vertices(i)%degree = Adjacency%get(i)
        end do
        ! 頂点を次数の降順にソート
        j = 0
        do i = 2, num_vertices
            temp_vertex = sorted_vertices(i)
            ! jをi-1から1まで逆順にループさせる
            inner_loop: do j = i - 1, 1, -1
                ! 挿入する値(temp_vertex)の方が大きければ、入れ替えの必要はない
                ! -> ループを抜けて、現在のjの次(j+1)に値を挿入する
                if (sorted_vertices(j)%degree <= temp_vertex%degree) then
                    exit inner_loop
                end if

                ! 挿入する値の方が小さいので、j番目の要素を後ろにずらす
                sorted_vertices(j + 1) = sorted_vertices(j)
            end do inner_loop

            ! ループを抜けた位置、または最後まで到達した位置に値を挿入する
            ! jが1まで実行された場合、ループ終了後のjは0になるため、j+1は1になる
            sorted_vertices(j + 1) = temp_vertex
        end do
        ! 彩色アルゴリズムの実行
        can_color = .false.

        do i = 1, num_vertices
            v_idx = sorted_vertices(i)%index
            if (colors(v_idx) /= 0) cycle
            num_colors = num_colors + 1
            colors(v_idx) = num_colors
            do j = i + 1, num_vertices
                u_idx = sorted_vertices(j)%index
                if (colors(u_idx) == 0) then
                    can_color = .true.
                    do k = 1, num_vertices
                        if (colors(k) == num_colors .and. Adjacency%check(u_idx, k)) then
                            can_color = .false.
                            exit
                        end if
                    end do
                    if (can_color) then
                        colors(u_idx) = num_colors
                    end if
                end if
            end do
        end do
        deallocate (sorted_vertices)
    end subroutine Multicoloring_welsh_powell

    subroutine type_vertex_degree_Assignment(vertex1, vertex2)
        implicit none
        type(type_vertex_degree), intent(inout) :: vertex1
        type(type_vertex_degree), intent(in) :: vertex2

        vertex1%index = vertex2%index
        vertex1%degree = vertex2%degree
    end subroutine type_vertex_degree_Assignment

end module Matrix_Multicoloring
