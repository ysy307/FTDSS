program test_physics
    use, intrinsic :: iso_fortran_env
#ifdef _MPI
    use :: mpi_f08
#endif
    use :: module_core
    use :: module_physics
    implicit none

    integer(int32) :: unit
    integer(int32) :: ierr
#ifdef _MPI
    call MPI_Init(ierr)
#endif
    open (newunit=unit, file="log/test/physics.log", status="replace", action="write", iostat=ierr)
    write (unit, '(A)') "Physics module test started."
    call test_iapws()
    write (unit, '(A)') "Physics module test completed."
    close (unit)
#ifdef _MPI
    call MPI_Finalize(ierr)
#endif
contains
    subroutine test_iapws()
        implicit none
        call test_iapws_region1()

    end subroutine test_iapws

    subroutine test_iapws_region1()
        implicit none
        integer(int32), parameter :: test_points = 3
        real(real64), parameter :: T(test_points) = [300.0d0, 300.0d0, 500.0d0]
        real(real64), parameter :: p(test_points) = [3.0d6, 80.0d6, 3.0d6]

        real(real64) :: nu(test_points)
        real(real64) :: h(test_points)
        real(real64) :: u(test_points)
        real(real64) :: s(test_points)
        real(real64) :: cp(test_points)
        real(real64) :: w(test_points)

        real(real64), parameter :: nu_exact(test_points) = [0.100215168d-2, 0.971180894d-3, 0.120241800d-2]
        real(real64), parameter :: h_exact(test_points) = [0.115331273d6, 0.184142828d6, 0.975542239d6]
        real(real64), parameter :: u_exact(test_points) = [0.112324818d6, 0.106448356d6, 0.971934985d6]
        real(real64), parameter :: s_exact(test_points) = [0.392294792d3, 0.368563852d3, 0.258041912d4]
        real(real64), parameter :: cp_exact(test_points) = [0.417301218d4, 0.401008987d4, 0.465580682d4]
        real(real64), parameter :: w_exact(test_points) = [0.150773921d4, 0.163469054d4, 0.124071337d4]

        integer(int32) :: i

        do i = 1, test_points
            nu(i) = get_nu_iapws97_region1(T(i), p(i))
            h(i) = get_h_iapws97_region1(T(i), p(i))
            u(i) = get_u_iapws97_region1(T(i), p(i))
            s(i) = get_s_iapws97_region1(T(i), p(i))
            cp(i) = get_cp_iapws97_region1(T(i), p(i))
            w(i) = get_w_iapws97_region1(T(i), p(i))
        end do

        call check_variables(nu, nu_exact, "IAPWS Region 1 specific volume")
        call check_variables(h, h_exact, "IAPWS Region 1 enthalpy")
        call check_variables(u, u_exact, "IAPWS Region 1 internal energy")
        call check_variables(s, s_exact, "IAPWS Region 1 entropy")
        call check_variables(cp, cp_exact, "IAPWS Region 1 isobaric heat capacity")
        call check_variables(w, w_exact, "IAPWS Region 1 speed of sound")

    end subroutine test_iapws_region1

    subroutine check_variables(v, v_exa, v_name)
        implicit none
        real(real64), intent(in) :: v(:)
        real(real64), intent(in) :: v_exa(:)
        character(len=*), intent(in) :: v_name

        real(real64), parameter :: tol = 1.0d-8
        integer(int32) :: i

        if (any(abs(v - v_exa) / v_exa > tol)) then
            write (unit, '(a)') "[FAIL] Variable check failed: "//v_name
            write (unit, '(a9,3a22)') "", "Computed", "Expected", "Difference"
            do i = 1, size(v)
                write (unit, '(a9,3es22.11)') "Details:", v(i), v_exa(i), v(i) - v_exa(i)
            end do

        else
            write (unit, '(a)') "[PASS] Variable check passed: "//v_name
        end if

    end subroutine check_variables

end program test_physics
