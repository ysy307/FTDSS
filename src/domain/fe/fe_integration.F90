module domain_fe_integration
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    private

    public :: get_integration_rule

contains

    !>
    !> 指定されたセルタイプと積分次数に応じたガウス積分ルールを取得する
    !>
    subroutine get_integration_rule(cell_type, order_req, num_gauss, weight, gauss)
        implicit none
        character(len=*), intent(in) :: cell_type
        integer(int32), intent(in) :: order_req
        integer(int32), intent(inout) :: num_gauss
        real(real64), allocatable, intent(inout) :: weight(:)
        real(real64), allocatable, intent(inout) :: gauss(:, :)

        select case (cell_type)
        case ("Triangle", "QuadraticTriangle")
            call set_triangle_rule(order_req, num_gauss, weight, gauss)
        case ("Quad", "QuadraticQuad")
            call set_quad_rule(order_req, num_gauss, weight, gauss)
        case ("Line", "QuadraticEdge")
            call set_line_rule(order_req, num_gauss, weight, gauss)
        case default
            num_gauss = 0
        end select
    end subroutine get_integration_rule

    !--------------------------------------------------------------------------
    ! 三角形要素用ルール
    !--------------------------------------------------------------------------
    subroutine set_triangle_rule(order, n, w, g)
        integer(int32), intent(in) :: order
        integer(int32), intent(inout) :: n
        real(real64), allocatable, intent(inout) :: w(:), g(:, :)

        select case (order)
        case (1)
            n = 1
            if (allocated(w)) deallocate (w)
            if (allocated(g)) deallocate (g)
            allocate (w(n), g(3, n))
            g(3, :) = 0.0d0

            w(1) = 0.5d0
            g(1:2, 1) = [1.0d0 / 3.0d0, 1.0d0 / 3.0d0]

        case (2)
            n = 3
            if (allocated(w)) deallocate (w)
            if (allocated(g)) deallocate (g)
            allocate (w(n), g(3, n))
            g(3, :) = 0.0d0

            w(:) = 1.0d0 / 6.0d0
            g(1:2, 1) = [1.0d0 / 6.0d0, 1.0d0 / 6.0d0]
            g(1:2, 2) = [2.0d0 / 3.0d0, 1.0d0 / 6.0d0]
            g(1:2, 3) = [1.0d0 / 6.0d0, 2.0d0 / 3.0d0]

        case (3)
            n = 4
            if (allocated(w)) deallocate (w)
            if (allocated(g)) deallocate (g)
            allocate (w(n), g(3, n))
            g(3, :) = 0.0d0

            w(1) = -27.0d0 / 96.0d0
            w(2:4) = 25.0d0 / 96.0d0
            g(1:2, 1) = [1.0d0 / 3.0d0, 1.0d0 / 3.0d0]
            g(1:2, 2) = [0.6d0, 0.2d0]
            g(1:2, 3) = [0.2d0, 0.6d0]
            g(1:2, 4) = [0.2d0, 0.2d0]

        case (5)
            ! Dunavant 7-point rule (degree 5)
            n = 7
            if (allocated(w)) deallocate (w)
            if (allocated(g)) deallocate (g)
            allocate (w(n), g(3, n))
            g(3, :) = 0.0d0

            w(1) = 0.225d0
            w(2:4) = 0.132394152788506d0
            w(5:7) = 0.125939180544827d0

            g(1:2, 1) = [1.0d0 / 3.0d0, 1.0d0 / 3.0d0]

            g(1:2, 2) = [0.059715871789770d0, 0.470142064105115d0]
            g(1:2, 3) = [0.470142064105115d0, 0.059715871789770d0]
            g(1:2, 4) = [0.470142064105115d0, 0.470142064105115d0]

            g(1:2, 5) = [0.797426985353087d0, 0.101286507323456d0]
            g(1:2, 6) = [0.101286507323456d0, 0.797426985353087d0]
            g(1:2, 7) = [0.101286507323456d0, 0.101286507323456d0]

        case default
            n = 1
            if (allocated(w)) deallocate (w)
            if (allocated(g)) deallocate (g)
            allocate (w(n), g(3, n))
            g(3, :) = 0.0d0

            w(1) = 0.5d0
            g(1:2, 1) = [1.0d0 / 3.0d0, 1.0d0 / 3.0d0]
        end select
    end subroutine set_triangle_rule

    !--------------------------------------------------------------------------
    ! 四角形要素用ルール (1D Gauss-Legendre の直積)
    !--------------------------------------------------------------------------
    subroutine set_quad_rule(order, n, w, g)
        integer(int32), intent(in) :: order
        integer(int32), intent(inout) :: n
        real(real64), allocatable, intent(inout) :: w(:), g(:, :)
        integer(int32) :: n_1d, i, j, k
        real(real64), allocatable :: w_1d(:), p_1d(:)

        ! 最低 2x2 を保証
        n_1d = max(2, order)
        n = n_1d * n_1d

        if (allocated(w)) deallocate (w)
        if (allocated(g)) deallocate (g)
        allocate (w(n), g(3, n))
        g(3, :) = 0.0d0

        allocate (w_1d(n_1d), p_1d(n_1d))
        call get_gauss_legendre_1d(n_1d, w_1d, p_1d)

        k = 0
        do j = 1, n_1d
            do i = 1, n_1d
                k = k + 1
                g(1, k) = p_1d(i)
                g(2, k) = p_1d(j)
                w(k) = w_1d(i) * w_1d(j)
            end do
        end do
    end subroutine set_quad_rule

    !--------------------------------------------------------------------------
    ! 線分要素用ルール (1D Gauss-Legendre)
    !--------------------------------------------------------------------------
    subroutine set_line_rule(order, n, w, g)
        integer(int32), intent(in) :: order
        integer(int32), intent(inout) :: n
        real(real64), allocatable, intent(inout) :: w(:), g(:, :)
        integer(int32) :: i
        real(real64), allocatable :: w_1d(:), p_1d(:)

        n = max(2, order)

        if (allocated(w)) deallocate (w)
        if (allocated(g)) deallocate (g)
        allocate (w(n), g(3, n))
        g(2:3, :) = 0.0d0

        allocate (w_1d(n), p_1d(n))
        call get_gauss_legendre_1d(n, w_1d, p_1d)

        do i = 1, n
            g(1, i) = p_1d(i)
            w(i) = w_1d(i)
        end do
    end subroutine set_line_rule

    !--------------------------------------------------------------------------
    ! 1次元ガウス・ルジャンドル求積法
    !--------------------------------------------------------------------------
    subroutine get_gauss_legendre_1d(n, w, p)
        integer(int32), intent(in) :: n
        real(real64), intent(inout) :: w(:), p(:)
        real(real64), parameter :: s3 = sqrt(1.0d0 / 3.0d0)
        real(real64), parameter :: s35 = sqrt(3.0d0 / 5.0d0)

        select case (n)
        case (1)
            p(1) = 0.0d0
            w(1) = 2.0d0
        case (2)
            p(1) = -s3
            w(1) = 1.0d0
            p(2) = s3
            w(2) = 1.0d0
        case (3)
            p(1) = -s35
            w(1) = 5.0d0 / 9.0d0
            p(2) = 0.0d0
            w(2) = 8.0d0 / 9.0d0
            p(3) = s35
            w(3) = 5.0d0 / 9.0d0
        case default
            p(1) = 0.0d0
            w(1) = 2.0d0
        end select
    end subroutine get_gauss_legendre_1d

end module domain_fe_integration
