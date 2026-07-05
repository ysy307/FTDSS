!> Unit tests for domain_fe_subcell: interface-split subcell quadrature.
!>
!> Tests verify that build_interface_quadrature_points:
!>   A. Partitions an uncut triangle into one side with sum(weights) = 0.5.
!>   B. Splits a cut triangle EXACTLY: per-side weights match the analytic
!>      sub-areas and total is preserved.
!>   C. Varies continuously with the nodal level set (no jump as the interface
!>      sweeps through the element).
!>   D. Partitions an uncut quad with sum(weights) = 4.0.
!>   E. Splits a cut quad with side sums matching the analytic areas.
!>   F. Degenerates continuously as the interface leaves through a vertex
!>      (vanishing minority side).
!>   G. Integrates a side-wise-constant coefficient exactly on a cut triangle.
!>   H/I. Chemical-potential helpers: psi_ice(0degC) = 0, T_high(P=0) = 0degC.
program test_fe_subcell
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit, error_unit
    use :: testdrive, only: error_type, check
    use :: module_core, only: FE_TYPE, type_coordinate_dp
    use :: domain_base_fe, only: abst_fe
    use :: domain_fe_factory, only: create_fe
    use :: domain_fe_subcell, only: type_subcell_qp, SUBCELL_QP_CAP, build_interface_quadrature_points
    use :: models_phase_change_chemical_potential, only: calc_psi_ice, calc_dpsi_ice_dT, &
                                                         calc_T_high_celsius
    implicit none

    integer(int32) :: total_failures

    total_failures = 0
    call run_all_tests(total_failures)

    if (total_failures > 0) then
        write (error_unit, '(A,I0,A)') "FAILED: ", total_failures, " test(s) failed."
        error stop 1
    end if
    write (output_unit, '(A)') "All fe_subcell tests passed."

contains

    subroutine run_all_tests(failures)
        integer(int32), intent(inout) :: failures

        type(error_type), allocatable :: error

        call test_uncut_triangle(error)
        call report("A: uncut_triangle_partition", error, failures)

        call test_cut_triangle_exact_split(error)
        call report("B: cut_triangle_exact_split", error, failures)

        call test_continuity_in_levelset(error)
        call report("C: continuity_in_levelset", error, failures)

        call test_uncut_quad(error)
        call report("D: uncut_quad_partition", error, failures)

        call test_cut_quad_split(error)
        call report("E: cut_quad_split", error, failures)

        call test_vanishing_minority_side(error)
        call report("F: vanishing_minority_side", error, failures)

        call test_sidewise_constant_integration(error)
        call report("G: sidewise_constant_integration", error, failures)

        call test_psi_ice_at_freezing(error)
        call report("H: psi_ice_at_freezing_point", error, failures)

        call test_T_high_zero_pressure(error)
        call report("I: T_high_at_zero_pressure", error, failures)

    end subroutine run_all_tests

    subroutine report(name, error, failures)
        character(len=*), intent(in) :: name
        type(error_type), allocatable, intent(inout) :: error
        integer(int32), intent(inout) :: failures

        if (allocated(error)) then
            write (output_unit, '(A,A)') "  [FAIL] ", name
            failures = failures + 1
            deallocate (error)
        else
            write (output_unit, '(A,A)') "  [PASS] ", name
        end if
    end subroutine report

    ! =========================================================================
    ! Helpers
    ! =========================================================================
    pure function side_weight_sum(qps, n, plus_side) result(s)
        type(type_subcell_qp), intent(in) :: qps(:)
        integer(int32), intent(in) :: n
        logical, intent(in) :: plus_side
        real(real64) :: s
        integer(int32) :: i
        s = 0.0d0
        do i = 1, n
            if (qps(i)%is_plus_side .eqv. plus_side) s = s + qps(i)%weight
        end do
    end function side_weight_sum

    ! =========================================================================
    ! A. Uncut triangle: everything on one side, total = reference area 0.5.
    ! =========================================================================
    subroutine test_uncut_triangle(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_qp) :: qps(SUBCELL_QP_CAP)
        integer(int32) :: n_qps
        real(real64) :: phi(3)

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)

        phi = [1.0d0, 2.0d0, 3.0d0]
        n_qps = 0
        call build_interface_quadrature_points(fe, phi, qps, n_qps)

        call check(error, n_qps > 0, "Expected quadrature points for uncut triangle")
        if (allocated(error)) return
        call check(error, abs(side_weight_sum(qps, n_qps, .true.) - 0.5d0) < 1.0d-13, &
                   "Plus side should carry the whole reference area 0.5")
        if (allocated(error)) return
        call check(error, abs(side_weight_sum(qps, n_qps, .false.)) < 1.0d-13, &
                   "Minus side should carry zero weight for uncut triangle")
    end subroutine test_uncut_triangle

    ! =========================================================================
    ! B. Cut triangle, exact split.
    !
    ! phi = [-0.5, 1.0, 1.0]: phi^h = -0.5 + 1.5*(xi+eta), zero line xi+eta = 1/3.
    ! Minus region: {xi+eta < 1/3}, area = 0.5*(1/3)^2 = 1/18. Plus: 0.5 - 1/18.
    ! =========================================================================
    subroutine test_cut_triangle_exact_split(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_qp) :: qps(SUBCELL_QP_CAP)
        integer(int32) :: n_qps
        real(real64) :: phi(3), a_minus

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)

        phi = [-0.5d0, 1.0d0, 1.0d0]
        a_minus = 0.5d0 * (1.0d0 / 3.0d0)**2
        n_qps = 0
        call build_interface_quadrature_points(fe, phi, qps, n_qps)

        call check(error, n_qps > 0, "Expected quadrature points for cut triangle")
        if (allocated(error)) return
        call check(error, abs(side_weight_sum(qps, n_qps, .false.) - a_minus) < 1.0d-13, &
                   "Minus side must equal the analytic area 1/18 exactly")
        if (allocated(error)) return
        call check(error, abs(side_weight_sum(qps, n_qps, .true.) - (0.5d0 - a_minus)) < 1.0d-13, &
                   "Plus side must equal 0.5 - 1/18 exactly")
    end subroutine test_cut_triangle_exact_split

    ! =========================================================================
    ! C. Continuity: side weights are Lipschitz in the nodal level set.
    ! Perturbing phi by delta changes each side sum by O(delta).
    ! =========================================================================
    subroutine test_continuity_in_levelset(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_qp) :: qps(SUBCELL_QP_CAP)
        integer(int32) :: n_qps
        real(real64) :: phi(3)
        real(real64) :: w_prev, w_curr, max_jump
        real(real64), parameter :: dphi = 1.0d-4
        integer(int32) :: k

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)

        max_jump = 0.0d0
        w_prev = -1.0d0
        do k = 0, 200
            phi = [-0.5d0 + real(k, real64) * dphi, 1.0d0, 1.0d0]
            n_qps = 0
            call build_interface_quadrature_points(fe, phi, qps, n_qps)
            w_curr = side_weight_sum(qps, n_qps, .false.)
            if (k > 0) max_jump = max(max_jump, abs(w_curr - w_prev))
            w_prev = w_curr
        end do

        ! d(area)/d(phi1) is bounded by ~0.25 here; allow a small safety factor.
        call check(error, max_jump < 1.0d-3, &
                   "Side weights must vary continuously with the nodal level set")
    end subroutine test_continuity_in_levelset

    ! =========================================================================
    ! D. Uncut quad: total = reference area 4.0 on the minus side.
    ! =========================================================================
    subroutine test_uncut_quad(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_qp) :: qps(SUBCELL_QP_CAP)
        integer(int32) :: n_qps
        real(real64) :: phi(4)

        fe = create_fe(FE_TYPE%QUAD%ID, 1)

        phi = [-1.0d0, -1.0d0, -1.0d0, -1.0d0]
        n_qps = 0
        call build_interface_quadrature_points(fe, phi, qps, n_qps)

        call check(error, n_qps > 0, "Expected quadrature points for uncut quad")
        if (allocated(error)) return
        call check(error, abs(side_weight_sum(qps, n_qps, .false.) - 4.0d0) < 1.0d-12, &
                   "Minus side should carry the whole reference area 4.0")
    end subroutine test_uncut_quad

    ! =========================================================================
    ! E. Cut quad: nodal phi = [-1, 1, 1, -1] (VTK corner order) splits
    ! [-1,1]^2 along xi = 0 into two halves of area 2 each.
    ! =========================================================================
    subroutine test_cut_quad_split(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_qp) :: qps(SUBCELL_QP_CAP)
        integer(int32) :: n_qps
        real(real64) :: phi(4)

        fe = create_fe(FE_TYPE%QUAD%ID, 1)

        ! Corners (-1,-1), (1,-1), (1,1), (-1,1): phi = xi.
        phi = [-1.0d0, 1.0d0, 1.0d0, -1.0d0]
        n_qps = 0
        call build_interface_quadrature_points(fe, phi, qps, n_qps)

        call check(error, n_qps > 0, "Expected quadrature points for cut quad")
        if (allocated(error)) return
        call check(error, abs(side_weight_sum(qps, n_qps, .true.) - 2.0d0) < 1.0d-12, &
                   "Plus side of the quad must have area 2")
        if (allocated(error)) return
        call check(error, abs(side_weight_sum(qps, n_qps, .false.) - 2.0d0) < 1.0d-12, &
                   "Minus side of the quad must have area 2")
    end subroutine test_cut_quad_split

    ! =========================================================================
    ! F. Vanishing minority side: as the lone vertex value goes to zero, the
    ! minority-side weight vanishes continuously.
    ! =========================================================================
    subroutine test_vanishing_minority_side(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_qp) :: qps(SUBCELL_QP_CAP)
        integer(int32) :: n_qps
        real(real64) :: phi(3), w_minus

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)

        phi = [-1.0d-9, 1.0d0, 1.0d0]
        n_qps = 0
        call build_interface_quadrature_points(fe, phi, qps, n_qps)
        w_minus = side_weight_sum(qps, n_qps, .false.)

        call check(error, w_minus < 1.0d-15, &
                   "Minority side weight must vanish as the interface leaves the element")
    end subroutine test_vanishing_minority_side

    ! =========================================================================
    ! G. Side-wise-constant coefficient is integrated exactly on a cut element:
    ! integral of c(x) = c_minus on the minus side, c_plus on the plus side.
    ! =========================================================================
    subroutine test_sidewise_constant_integration(error)
        type(error_type), allocatable, intent(inout) :: error

        class(abst_fe), allocatable :: fe
        type(type_subcell_qp) :: qps(SUBCELL_QP_CAP)
        integer(int32) :: n_qps, i
        real(real64) :: phi(3), integral, expected, a_minus
        real(real64), parameter :: c_minus = 3.0d0, c_plus = 7.0d0

        fe = create_fe(FE_TYPE%TRIANGLE%ID, 1)

        phi = [-0.5d0, 1.0d0, 1.0d0]
        a_minus = 0.5d0 * (1.0d0 / 3.0d0)**2
        expected = c_minus * a_minus + c_plus * (0.5d0 - a_minus)

        n_qps = 0
        call build_interface_quadrature_points(fe, phi, qps, n_qps)

        integral = 0.0d0
        do i = 1, n_qps
            if (qps(i)%is_plus_side) then
                integral = integral + c_plus * qps(i)%weight
            else
                integral = integral + c_minus * qps(i)%weight
            end if
        end do

        call check(error, abs(integral - expected) < 1.0d-12, &
                   "Side-wise-constant coefficient must be integrated exactly")
    end subroutine test_sidewise_constant_integration

    ! =========================================================================
    ! H. calc_psi_ice: at T = 0degC, psi_ice should be zero.
    ! =========================================================================
    subroutine test_psi_ice_at_freezing(error)
        type(error_type), allocatable, intent(inout) :: error

        real(real64) :: psi

        psi = 0.0d0
        call calc_psi_ice(0.0d0, psi)
        call check(error, abs(psi) < 1.0d-12, "psi_ice should be 0 at T = 0degC")
    end subroutine test_psi_ice_at_freezing

    ! =========================================================================
    ! I. calc_T_high_celsius: at P_w = 0 and rho_w = 1000, T_high should be 0degC.
    ! =========================================================================
    subroutine test_T_high_zero_pressure(error)
        type(error_type), allocatable, intent(inout) :: error

        real(real64) :: T_high

        T_high = -99.0d0
        call calc_T_high_celsius(0.0d0, 1000.0d0, T_high)
        call check(error, abs(T_high) < 1.0d-10, "T_high should be 0degC at P_w = 0")
    end subroutine test_T_high_zero_pressure

end program test_fe_subcell
