module domain_fe_integration
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
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

        ! Internal memory management
        procedure, private, pass(self) :: initial_setup => initial_setup_gauss_integration_rule
        procedure, private, pass(self) :: compute_triangle_rule => compute_triangle_rule_gauss_integration
        procedure, private, pass(self) :: compute_quad_rule => compute_quad_rule_gauss_integration
        procedure, private, pass(self) :: compute_line_rule => compute_line_rule_gauss_integration
        procedure, private, pass(self) :: compute_tetra_rule => compute_tetra_rule_gauss_integration
        procedure, private, pass(self) :: compute_hexa_rule => compute_hexa_rule_gauss_integration
    end type type_gauss_integration_rule

contains

    !>
    !> Initializes the Gauss integration rule for the given cell type and integration order.
    !>
    subroutine initialize_type_gauss_integration_rule(self, cell_type, integration_order)
        implicit none
        class(type_gauss_integration_rule), intent(inout) :: self
        integer(int32), intent(in) :: cell_type
        integer(int32), intent(in) :: integration_order

        ! Destroy and reset any existing data
        call self%destroy()

        ! Store the requested order
        self%order = integration_order

        ! Dispatch to the appropriate rule computation based on cell type
        if (FE_TYPE%TRIANGLE == cell_type) then
            call self%compute_triangle_rule(integration_order)
        else if (FE_TYPE%QUADRATIC_TRIANGLE == cell_type) then
            call self%compute_triangle_rule(integration_order)
        else if (FE_TYPE%LAGRANGE_TRIANGLE == cell_type) then
            call self%compute_triangle_rule(integration_order)
        else if (FE_TYPE%QUAD == cell_type) then
            call self%compute_quad_rule(integration_order)
        else if (FE_TYPE%QUADRATIC_QUAD == cell_type) then
            call self%compute_quad_rule(integration_order)
        else if (FE_TYPE%BIQUADRATIC_QUAD == cell_type) then
            call self%compute_quad_rule(integration_order)
        else if (FE_TYPE%LAGRANGE_QUADRILATERAL == cell_type) then
            call self%compute_quad_rule(integration_order)
        else if (FE_TYPE%LINE == cell_type) then
            call self%compute_line_rule(integration_order)
        else if (FE_TYPE%QUADRATIC_EDGE == cell_type) then
            call self%compute_line_rule(integration_order)
        else if (FE_TYPE%LAGRANGE_CURVE == cell_type) then
            call self%compute_line_rule(integration_order)
        else if (FE_TYPE%TETRA == cell_type) then
            call self%compute_tetra_rule(integration_order)
        else if (FE_TYPE%QUADRATIC_TETRA == cell_type) then
            call self%compute_tetra_rule(integration_order)
        else if (FE_TYPE%LAGRANGE_TETRAHEDRON == cell_type) then
            call self%compute_tetra_rule(integration_order)
        else if (FE_TYPE%HEXAHEDRON == cell_type) then
            call self%compute_hexa_rule(integration_order)
        else if (FE_TYPE%QUADRATIC_HEXAHEDRON == cell_type) then
            call self%compute_hexa_rule(integration_order)
        else if (FE_TYPE%TRIQUADRATIC_HEXAHEDRON == cell_type) then
            call self%compute_hexa_rule(integration_order)
        else if (FE_TYPE%LAGRANGE_HEXAHEDRON == cell_type) then
            call self%compute_hexa_rule(integration_order)
        else
            self%num_gauss = 0
        end if

        if (self%num_gauss > 0) then
            self%initialized = .true.
        end if
    end subroutine initialize_type_gauss_integration_rule

    !>
    !> Destructor: releases all allocated memory.
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
    !> Internal setup: allocate memory and initialize arrays.
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
    ! Computation Logic
    !--------------------------------------------------------------------------

    !>
    !> Integration rule for triangular elements.
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

        case (4)
            ! Dunavant 6-point rule (degree 4)
            self%num_gauss = 6
            call self%initial_setup(self%num_gauss)

            self%weight(1:3) = 0.109951743655322d0
            self%weight(4:6) = 0.223381589678011d0

            call self%gauss(1)%set(0.816847572980459d0, 0.091576213509771d0, 0.0d0)
            call self%gauss(2)%set(0.091576213509771d0, 0.816847572980459d0, 0.0d0)
            call self%gauss(3)%set(0.091576213509771d0, 0.091576213509771d0, 0.0d0)
            call self%gauss(4)%set(0.108103018168070d0, 0.445948490915965d0, 0.0d0)
            call self%gauss(5)%set(0.445948490915965d0, 0.108103018168070d0, 0.0d0)
            call self%gauss(6)%set(0.445948490915965d0, 0.445948490915965d0, 0.0d0)

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
    !> Integration rule for quadrilateral elements (tensor product of 1D rules).
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
            call get_legendre_point(n_1d, j, w_j, p_j)

            do i = 1, n_1d
                call get_legendre_point(n_1d, i, w_i, p_i)

                k = k + 1
                self%weight(k) = w_i * w_j
                call self%gauss(k)%set(p_i, p_j, 0.0d0)
            end do
        end do
    end subroutine compute_quad_rule_gauss_integration

    !>
    !> Integration rule for line elements.
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
    !> Integration rule for tetrahedral elements.
    !> Uses Keast rules on the standard tetrahedron with vertices
    !> (0,0,0), (1,0,0), (0,1,0), (0,0,1). Weights include the 1/6 volume factor.
    !>
    subroutine compute_tetra_rule_gauss_integration(self, order)
        implicit none
        class(type_gauss_integration_rule), intent(inout) :: self
        integer(int32), intent(in) :: order

        real(real64), parameter :: sixth = 1.0d0 / 6.0d0

        select case (order)
        case (1)
            ! 1-point rule (degree 1)
            self%num_gauss = 1
            call self%initial_setup(self%num_gauss)
            self%weight(1) = sixth
            call self%gauss(1)%set(0.25d0, 0.25d0, 0.25d0)

        case (2)
            ! 4-point rule (degree 2)
            self%num_gauss = 4
            call self%initial_setup(self%num_gauss)

            self%weight(:) = sixth / 4.0d0

            call self%gauss(1)%set(0.138196601125011d0, 0.138196601125011d0, 0.138196601125011d0)
            call self%gauss(2)%set(0.585410196624968d0, 0.138196601125011d0, 0.138196601125011d0)
            call self%gauss(3)%set(0.138196601125011d0, 0.585410196624968d0, 0.138196601125011d0)
            call self%gauss(4)%set(0.138196601125011d0, 0.138196601125011d0, 0.585410196624968d0)

        case (3)
            ! 5-point rule (degree 3)
            self%num_gauss = 5
            call self%initial_setup(self%num_gauss)

            self%weight(1) = -4.0d0 / 30.0d0 * sixth / (1.0d0 / 6.0d0)
            self%weight(1) = -2.0d0 * sixth / 15.0d0 * 6.0d0
            ! Keast: weight_centroid = -4/5 * V, weight_vertex = 9/20 * V, V = 1/6
            self%weight(1) = -4.0d0 / 5.0d0 * sixth
            self%weight(2:5) = 9.0d0 / 20.0d0 * sixth

            call self%gauss(1)%set(0.25d0, 0.25d0, 0.25d0)
            call self%gauss(2)%set(1.0d0 / 6.0d0, 1.0d0 / 6.0d0, 1.0d0 / 6.0d0)
            call self%gauss(3)%set(0.5d0, 1.0d0 / 6.0d0, 1.0d0 / 6.0d0)
            call self%gauss(4)%set(1.0d0 / 6.0d0, 0.5d0, 1.0d0 / 6.0d0)
            call self%gauss(5)%set(1.0d0 / 6.0d0, 1.0d0 / 6.0d0, 0.5d0)

        case (4)
            ! 11-point rule (degree 4) - Keast
            self%num_gauss = 11
            call self%initial_setup(self%num_gauss)

            self%weight(1) = -0.013155555555556d0
            self%weight(2:5) = 0.007622222222222d0
            self%weight(6:11) = 0.024888888888889d0

            call self%gauss(1)%set(0.25d0, 0.25d0, 0.25d0)

            call self%gauss(2)%set(0.071428571428571d0, 0.071428571428571d0, 0.071428571428571d0)
            call self%gauss(3)%set(0.785714285714286d0, 0.071428571428571d0, 0.071428571428571d0)
            call self%gauss(4)%set(0.071428571428571d0, 0.785714285714286d0, 0.071428571428571d0)
            call self%gauss(5)%set(0.071428571428571d0, 0.071428571428571d0, 0.785714285714286d0)

            call self%gauss(6)%set(0.399403576166799d0, 0.100596423833201d0, 0.100596423833201d0)
            call self%gauss(7)%set(0.100596423833201d0, 0.399403576166799d0, 0.100596423833201d0)
            call self%gauss(8)%set(0.100596423833201d0, 0.100596423833201d0, 0.399403576166799d0)
            call self%gauss(9)%set(0.399403576166799d0, 0.399403576166799d0, 0.100596423833201d0)
            call self%gauss(10)%set(0.399403576166799d0, 0.100596423833201d0, 0.399403576166799d0)
            call self%gauss(11)%set(0.100596423833201d0, 0.399403576166799d0, 0.399403576166799d0)

        case (5)
            ! 14-point rule (degree 5) - Keast
            self%num_gauss = 14
            call self%initial_setup(self%num_gauss)

            self%weight(1:4) = 0.007349304311636d0
            self%weight(5:8) = 0.011268792317636d0
            self%weight(9:14) = 0.018781320953693d0

            call self%gauss(1)%set(0.092735250310891d0, 0.092735250310891d0, 0.092735250310891d0)
            call self%gauss(2)%set(0.721794249067327d0, 0.092735250310891d0, 0.092735250310891d0)
            call self%gauss(3)%set(0.092735250310891d0, 0.721794249067327d0, 0.092735250310891d0)
            call self%gauss(4)%set(0.092735250310891d0, 0.092735250310891d0, 0.721794249067327d0)

            call self%gauss(5)%set(0.310885941270114d0, 0.310885941270114d0, 0.310885941270114d0)
            call self%gauss(6)%set(0.067342176189659d0, 0.310885941270114d0, 0.310885941270114d0)
            call self%gauss(7)%set(0.310885941270114d0, 0.067342176189659d0, 0.310885941270114d0)
            call self%gauss(8)%set(0.310885941270114d0, 0.310885941270114d0, 0.067342176189659d0)

            call self%gauss(9)%set(0.045503704125650d0, 0.045503704125650d0, 0.454496295874350d0)
            call self%gauss(10)%set(0.045503704125650d0, 0.454496295874350d0, 0.045503704125650d0)
            call self%gauss(11)%set(0.454496295874350d0, 0.045503704125650d0, 0.045503704125650d0)
            call self%gauss(12)%set(0.454496295874350d0, 0.454496295874350d0, 0.045503704125650d0)
            call self%gauss(13)%set(0.454496295874350d0, 0.045503704125650d0, 0.454496295874350d0)
            call self%gauss(14)%set(0.045503704125650d0, 0.454496295874350d0, 0.454496295874350d0)

        case default
            ! Fallback to 1-point rule
            self%num_gauss = 1
            call self%initial_setup(self%num_gauss)
            self%weight(1) = sixth
            call self%gauss(1)%set(0.25d0, 0.25d0, 0.25d0)
        end select
    end subroutine compute_tetra_rule_gauss_integration

    !>
    !> Integration rule for hexahedral elements (tensor product of 1D rules).
    !>
    subroutine compute_hexa_rule_gauss_integration(self, order)
        implicit none
        class(type_gauss_integration_rule), intent(inout) :: self
        integer(int32), intent(in) :: order

        integer(int32) :: n_1d, i, j, k, idx
        real(real64) :: w_i, p_i, w_j, p_j, w_k, p_k

        n_1d = max(2, order)
        self%num_gauss = n_1d * n_1d * n_1d
        call self%initial_setup(self%num_gauss)

        idx = 0
        do k = 1, n_1d
            call get_legendre_point(n_1d, k, w_k, p_k)
            do j = 1, n_1d
                call get_legendre_point(n_1d, j, w_j, p_j)
                do i = 1, n_1d
                    call get_legendre_point(n_1d, i, w_i, p_i)
                    idx = idx + 1
                    self%weight(idx) = w_i * w_j * w_k
                    call self%gauss(idx)%set(p_i, p_j, p_k)
                end do
            end do
        end do
    end subroutine compute_hexa_rule_gauss_integration

    !>
    !> Returns the weight and point for a 1D Gauss-Legendre quadrature rule at a given index.
    !>
    pure subroutine get_legendre_point(n, idx, w, p)
        implicit none
        integer(int32), intent(in) :: n
        integer(int32), intent(in) :: idx
        real(real64), intent(inout) :: w, p

        real(real64), parameter :: s3 = sqrt(1.0d0 / 3.0d0)
        real(real64), parameter :: s35 = sqrt(3.0d0 / 5.0d0)
        ! 4-point Gauss-Legendre
        real(real64), parameter :: p4a = sqrt((3.0d0 - 2.0d0 * sqrt(6.0d0 / 5.0d0)) / 7.0d0)
        real(real64), parameter :: p4b = sqrt((3.0d0 + 2.0d0 * sqrt(6.0d0 / 5.0d0)) / 7.0d0)
        real(real64), parameter :: w4a = (18.0d0 + sqrt(30.0d0)) / 36.0d0
        real(real64), parameter :: w4b = (18.0d0 - sqrt(30.0d0)) / 36.0d0
        ! 5-point Gauss-Legendre
        real(real64), parameter :: p5a = sqrt(5.0d0 - 2.0d0 * sqrt(10.0d0 / 7.0d0)) / 3.0d0
        real(real64), parameter :: p5b = sqrt(5.0d0 + 2.0d0 * sqrt(10.0d0 / 7.0d0)) / 3.0d0
        real(real64), parameter :: w5a = (322.0d0 + 13.0d0 * sqrt(70.0d0)) / 900.0d0
        real(real64), parameter :: w5b = (322.0d0 - 13.0d0 * sqrt(70.0d0)) / 900.0d0
        real(real64), parameter :: w5c = 128.0d0 / 225.0d0

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
        case (4)
            select case (idx)
            case (1)
                p = -p4b; w = w4b
            case (2)
                p = -p4a; w = w4a
            case (3)
                p = p4a; w = w4a
            case (4)
                p = p4b; w = w4b
            case default
                p = 0.0d0; w = 0.0d0
            end select
        case (5)
            select case (idx)
            case (1)
                p = -p5b; w = w5b
            case (2)
                p = -p5a; w = w5a
            case (3)
                p = 0.0d0; w = w5c
            case (4)
                p = p5a; w = w5a
            case (5)
                p = p5b; w = w5b
            case default
                p = 0.0d0; w = 0.0d0
            end select
        case default
            ! Fallback
            p = 0.0d0
            w = 2.0d0
        end select
    end subroutine get_legendre_point

end module domain_fe_integration
