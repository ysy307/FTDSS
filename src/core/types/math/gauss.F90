module core_types_math_gauss
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: core_allocate, only:allocate_array
    use :: core_deallocate, only:deallocate_array
    ! use :: core_types_coordinate, only:type_coordinate_dp
    use :: core_types_coordinate, only:type_coordinate_dp
    use :: core_constants
    implicit none
    private

    public :: type_gauss_integration_rule

    type :: type_gauss_integration_rule
        !> Initialization flag
        logical, private :: initialized = .false.
        !> The interpolation order.
        integer(int32) :: order = 0
        !> The number of Gauss points.
        integer(int32) :: num_gauss = 0
        !> Weights for Gauss integration points.
        real(real64), allocatable :: weight(:)
        !> Local coordinates of Gauss integration points.
        type(type_coordinate_dp), allocatable :: gauss(:)
    contains
        procedure, public, pass(self) :: initialize => initialize_type_gauss_integration_rule
        procedure, public, pass(self) :: destroy => destroy_type_gauss_integration_rule

        ! 内部メモリ管理用
        procedure, private, pass(self) :: initial_setup => initial_setup_gauss_integration_rule
        procedure, private, pass(self) :: compute_triangle_rule => compute_triangle_rule_gauss_integration
        procedure, private, pass(self) :: compute_quad_rule => compute_quad_rule_gauss_integration
        procedure, private, pass(self) :: compute_line_rule => compute_line_rule_gauss_integration
    end type type_gauss_integration_rule

contains

    !>
    !> 指定されたセルタイプと積分次数に応じたガウス積分ルールを初期化する
    !>
    subroutine initialize_type_gauss_integration_rule(self, cell_type, integration_order)
        implicit none
        class(type_gauss_integration_rule), intent(inout) :: self
        integer(int32), intent(in) :: cell_type
        integer(int32), intent(in) :: integration_order

        ! 既存のデータがあれば破棄してリセット
        call self%destroy()

        ! 要求次数を保存
        self%order = integration_order

        ! セルタイプに応じた計算ルーチンを呼び出し
        select case (cell_type)
        case (FE_TRIANGLE, FE_QUADRATIC_TRIANGLE)
            call self%compute_triangle_rule(integration_order)
        case (FE_QUAD, FE_QUADRATIC_QUAD)
            call self%compute_quad_rule(integration_order)
        case (FE_LINE, FE_QUADRATIC_EDGE)
            call self%compute_line_rule(integration_order)
        case default
            self%num_gauss = 0
        end select

        if (self%num_gauss > 0) then
            self%initialized = .true.
        end if
    end subroutine initialize_type_gauss_integration_rule

    !>
    !> デストラクタ
    !>
    subroutine destroy_type_gauss_integration_rule(self)
        implicit none
        class(type_gauss_integration_rule), intent(inout) :: self

        self%order = 0
        self%num_gauss = 0

        call deallocate_array(self%weight)

        if (allocated(self%gauss)) then
            deallocate (self%gauss)
        end if

        self%initialized = .false.
    end subroutine destroy_type_gauss_integration_rule

    !>
    !> 内部セットアップ：メモリ確保と初期化
    !>
    subroutine initial_setup_gauss_integration_rule(self, num_gauss)
        implicit none
        class(type_gauss_integration_rule), intent(inout) :: self
        integer(int32), intent(in) :: num_gauss
        integer(int32) :: i

        call deallocate_array(self%weight)
        if (allocated(self%gauss)) deallocate (self%gauss)

        call allocate_array(self%weight, num_gauss)
        allocate (self%gauss(num_gauss))

        self%weight(:) = 0.0d0
        do i = 1, num_gauss
            call self%gauss(i)%reset()
        end do
    end subroutine initial_setup_gauss_integration_rule

    !--------------------------------------------------------------------------
    ! 計算ロジック
    !--------------------------------------------------------------------------

    !>
    !> 三角形要素用ルール
    !>
    subroutine compute_triangle_rule_gauss_integration(self, order)
        implicit none
        class(type_gauss_integration_rule), intent(inout) :: self
        integer(int32), intent(in) :: order

        select case (order)
        case (1)
            self%num_gauss = 1
            call self%initial_setup(self%num_gauss)

            self%weight(1) = 0.5d0
            call self%gauss(1)%set(1.0d0 / 3.0d0, 1.0d0 / 3.0d0, 0.0d0)

        case (2)
            self%num_gauss = 3
            call self%initial_setup(self%num_gauss)

            self%weight(:) = 1.0d0 / 6.0d0
            call self%gauss(1)%set(1.0d0 / 6.0d0, 1.0d0 / 6.0d0, 0.0d0)
            call self%gauss(2)%set(2.0d0 / 3.0d0, 1.0d0 / 6.0d0, 0.0d0)
            call self%gauss(3)%set(1.0d0 / 6.0d0, 2.0d0 / 3.0d0, 0.0d0)

        case (3)
            self%num_gauss = 4
            call self%initial_setup(self%num_gauss)

            self%weight(1) = -27.0d0 / 96.0d0
            self%weight(2:4) = 25.0d0 / 96.0d0

            call self%gauss(1)%set(1.0d0 / 3.0d0, 1.0d0 / 3.0d0, 0.0d0)
            call self%gauss(2)%set(0.6d0, 0.2d0, 0.0d0)
            call self%gauss(3)%set(0.2d0, 0.6d0, 0.0d0)
            call self%gauss(4)%set(0.2d0, 0.2d0, 0.0d0)

        case (5)
            ! Dunavant 7-point rule (degree 5)
            self%num_gauss = 7
            call self%initial_setup(self%num_gauss)

            self%weight(1) = 0.225d0
            self%weight(2:4) = 0.132394152788506d0
            self%weight(5:7) = 0.125939180544827d0

            call self%gauss(1)%set(1.0d0 / 3.0d0, 1.0d0 / 3.0d0, 0.0d0)

            call self%gauss(2)%set(0.059715871789770d0, 0.470142064105115d0, 0.0d0)
            call self%gauss(3)%set(0.470142064105115d0, 0.059715871789770d0, 0.0d0)
            call self%gauss(4)%set(0.470142064105115d0, 0.470142064105115d0, 0.0d0)

            call self%gauss(5)%set(0.797426985353087d0, 0.101286507323456d0, 0.0d0)
            call self%gauss(6)%set(0.101286507323456d0, 0.797426985353087d0, 0.0d0)
            call self%gauss(7)%set(0.101286507323456d0, 0.101286507323456d0, 0.0d0)

        case default
            self%num_gauss = 1
            call self%initial_setup(self%num_gauss)
            self%weight(1) = 0.5d0
            call self%gauss(1)%set(1.0d0 / 3.0d0, 1.0d0 / 3.0d0, 0.0d0)
        end select
    end subroutine compute_triangle_rule_gauss_integration

    !>
    !> 四角形要素用ルール (1Dの直積)
    !> 一次配列をallocateせず，逐次計算して格納する
    !>
    subroutine compute_quad_rule_gauss_integration(self, order)
        implicit none
        class(type_gauss_integration_rule), intent(inout) :: self
        integer(int32), intent(in) :: order

        integer(int32) :: n_1d, i, j, k
        real(real64) :: w_i, p_i, w_j, p_j

        n_1d = max(2, order)
        self%num_gauss = n_1d * n_1d
        call self%initial_setup(self%num_gauss)

        k = 0
        do j = 1, n_1d
            ! j方向の座標と重みを取得
            call get_legendre_point(n_1d, j, w_j, p_j)

            do i = 1, n_1d
                ! i方向の座標と重みを取得
                call get_legendre_point(n_1d, i, w_i, p_i)

                k = k + 1
                self%weight(k) = w_i * w_j
                call self%gauss(k)%set(p_i, p_j, 0.0d0)
            end do
        end do
    end subroutine compute_quad_rule_gauss_integration

    !>
    !> 線分要素用ルール
    !>
    subroutine compute_line_rule_gauss_integration(self, order)
        implicit none
        class(type_gauss_integration_rule), intent(inout) :: self
        integer(int32), intent(in) :: order

        integer(int32) :: n, i
        real(real64) :: w_i, p_i

        n = max(2, order)
        self%num_gauss = n
        call self%initial_setup(self%num_gauss)

        do i = 1, n
            call get_legendre_point(n, i, w_i, p_i)
            self%weight(i) = w_i
            call self%gauss(i)%set(p_i, 0.0d0, 0.0d0)
        end do
    end subroutine compute_line_rule_gauss_integration

    !>
    !> 1次元ガウス・ルジャンドル求積法：指定されたインデックスの値を返す
    !> 配列を返さずスカラを返すことでアロケーションを回避
    !>
    pure subroutine get_legendre_point(n, idx, w, p)
        implicit none
        integer(int32), intent(in) :: n
        integer(int32), intent(in) :: idx
        real(real64), intent(inout) :: w, p

        real(real64), parameter :: s3 = sqrt(1.0d0 / 3.0d0)
        real(real64), parameter :: s35 = sqrt(3.0d0 / 5.0d0)

        select case (n)
        case (1)
            p = 0.0d0
            w = 2.0d0
        case (2)
            if (idx == 1) then
                p = -s3
                w = 1.0d0
            else
                p = s3
                w = 1.0d0
            end if
        case (3)
            select case (idx)
            case (1)
                p = -s35
                w = 5.0d0 / 9.0d0
            case (2)
                p = 0.0d0
                w = 8.0d0 / 9.0d0
            case (3)
                p = s35
                w = 5.0d0 / 9.0d0
            case default
                p = 0.0d0
                w = 0.0d0
            end select
        case default
            ! Fallback
            p = 0.0d0
            w = 2.0d0
        end select
    end subroutine get_legendre_point

end module core_types_math_gauss
