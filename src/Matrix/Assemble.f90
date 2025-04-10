module Matrix_Assemble
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Matrix_CRS
#ifdef _OPENMP
    use omp_lib
#endif
    implicit none
    ! private
contains
    subroutine Assemble_Mass_Lumped_231(A, Elements, Area, C, N)
        implicit none
        type(Type_CRS), intent(inout) :: A
        integer(int32), intent(in) :: Elements(:, :)
        real(real64), intent(in) :: Area(:)
        real(real64), intent(in) :: C(:)
        integer(int32), intent(in) :: N

        integer(int32) :: iN, p1, p2, p3
        integer(int32) :: indexes(3)
        real(real64) :: CoeA, C1, C2, C3

        do iN = 1, N
            p1 = Elements(1, iN)
            p2 = Elements(2, iN)
            p3 = Elements(3, iN)
            CoeA = Area(iN) / 3.0d0

            call A%Find(p1, p1, indexes(1))
            call A%Find(p2, p2, indexes(2))
            call A%Find(p3, p3, indexes(3))

            A%val(indexes(1)) = A%val(indexes(1)) + C(p1) * CoeA
            A%val(indexes(2)) = A%val(indexes(2)) + C(p2) * CoeA
            A%val(indexes(3)) = A%val(indexes(3)) + C(p3) * CoeA
        end do

    end subroutine Assemble_Mass_Lumped_231

    subroutine Assemble_Diffusion_231(A, Elements, Basis, Area, lambda, N)
        implicit none
        type(Type_CRS), intent(inout) :: A
        integer(int32), intent(in) :: Elements(:, :)
        real(real64), intent(in) :: Basis(:, :, :)
        real(real64), intent(in) :: Area(:)
        real(real64), intent(in) :: lambda(:)
        integer(int32), intent(in) :: N

        integer(int32) :: iN, p1, p2, p3
        integer(int32) :: indexes(9), iind
        real(real64) :: CoeA, be1, be2, be3, ga1, ga2, ga3

        do iN = 1, N
            p1 = Elements(1, iN)
            p2 = Elements(2, iN)
            p3 = Elements(3, iN)
            CoeA = (lambda(p1) + lambda(p2) + lambda(p3)) / (12.0d0 * Area(iN))

            be1 = Basis(1, 2, iN)
            be2 = Basis(2, 2, iN)
            be3 = Basis(3, 2, iN)
            ga1 = Basis(1, 3, iN)
            ga2 = Basis(2, 3, iN)
            ga3 = Basis(3, 3, iN)

            call A%Find(p1, p1, indexes(1))
            call A%Find(p1, p2, indexes(2))
            call A%Find(p1, p3, indexes(3))
            call A%Find(p2, p1, indexes(4))
            call A%Find(p2, p2, indexes(5))
            call A%Find(p2, p3, indexes(6))
            call A%Find(p3, p1, indexes(7))
            call A%Find(p3, p2, indexes(8))
            call A%Find(p3, p3, indexes(9))

            A%Val(indexes(1)) = A%Val(indexes(1)) + (be1 * be1 + ga1 * ga1) * CoeA
            A%Val(indexes(2)) = A%Val(indexes(2)) + (be1 * be2 + ga1 * ga2) * CoeA
            A%Val(indexes(3)) = A%Val(indexes(3)) + (be1 * be3 + ga1 * ga3) * CoeA
            A%Val(indexes(4)) = A%Val(indexes(4)) + (be2 * be1 + ga2 * ga1) * CoeA
            A%Val(indexes(5)) = A%Val(indexes(5)) + (be2 * be2 + ga2 * ga2) * CoeA
            A%Val(indexes(6)) = A%Val(indexes(6)) + (be2 * be3 + ga2 * ga3) * CoeA
            A%Val(indexes(7)) = A%Val(indexes(7)) + (be3 * be1 + ga3 * ga1) * CoeA
            A%Val(indexes(8)) = A%Val(indexes(8)) + (be3 * be2 + ga3 * ga2) * CoeA
            A%Val(indexes(9)) = A%Val(indexes(9)) + (be3 * be3 + ga3 * ga3) * CoeA
        end do
    end subroutine Assemble_Diffusion_231

    subroutine Assemble_Diffusion_Dispersity_231(A, Elements, Basis, Area, lambda_xx, lambda_xy, lambda_yy, N)
        implicit none
        type(Type_CRS), intent(inout) :: A
        integer(int32), intent(in) :: Elements(:, :)
        real(real64), intent(in) :: Basis(:, :, :)
        real(real64), intent(in) :: Area(:)
        real(real64), intent(in) :: lambda_xx(:)
        real(real64), intent(in) :: lambda_xy(:)
        real(real64), intent(in) :: lambda_yy(:)
        integer(int32), intent(in) :: N

        integer(int32) :: iN, p1, p2, p3
        integer(int32) :: indexes(9), iind
        real(real64) :: CoeA, be1, be2, be3, ga1, ga2, ga3
        real(real64) :: lambda1, lambda2, lambda3

        do iN = 1, N
            p1 = Elements(1, iN)
            p2 = Elements(2, iN)
            p3 = Elements(3, iN)
            !! lambda1 : Local lambda_xx
            !! lambda2 : Local lambda_xy
            !! lambda3 : Local lambda_yy
            lambda1 = (lambda_xx(p1) + lambda_xx(p2) + lambda_xx(p3)) / 3.0d0
            lambda2 = (lambda_xy(p1) + lambda_xy(p2) + lambda_xy(p3)) / 3.0d0
            lambda3 = (lambda_yy(p1) + lambda_yy(p2) + lambda_yy(p3)) / 3.0d0
            CoeA = 1.0d0 / (4.0d0 * Area(iN))

            be1 = Basis(1, 2, iN)
            be2 = Basis(2, 2, iN)
            be3 = Basis(3, 2, iN)
            ga1 = Basis(1, 3, iN)
            ga2 = Basis(2, 3, iN)
            ga3 = Basis(3, 3, iN)

            call A%Find(p1, p1, indexes(1))
            call A%Find(p1, p2, indexes(2))
            call A%Find(p1, p3, indexes(3))
            call A%Find(p2, p1, indexes(4))
            call A%Find(p2, p2, indexes(5))
            call A%Find(p2, p3, indexes(6))
            call A%Find(p3, p1, indexes(7))
            call A%Find(p3, p2, indexes(8))
            call A%Find(p3, p3, indexes(9))

            A%Val(indexes(1)) = A%Val(indexes(1)) + (lambda1 * be1 * be1 + lambda2 * (be1 * ga1 + ga1 * be1) + lambda3 * ga1 * ga1) * CoeA
            A%Val(indexes(2)) = A%Val(indexes(2)) + (lambda1 * be1 * be2 + lambda2 * (be1 * ga2 + ga1 * be2) + lambda3 * ga1 * ga2) * CoeA
            A%Val(indexes(3)) = A%Val(indexes(3)) + (lambda1 * be1 * be3 + lambda2 * (be1 * ga3 + ga1 * be3) + lambda3 * ga1 * ga3) * CoeA
            A%Val(indexes(4)) = A%Val(indexes(4)) + (lambda1 * be2 * be1 + lambda2 * (be2 * ga1 + ga2 * be1) + lambda3 * ga2 * ga1) * CoeA
            A%Val(indexes(5)) = A%Val(indexes(5)) + (lambda1 * be2 * be2 + lambda2 * (be2 * ga2 + ga2 * be2) + lambda3 * ga2 * ga2) * CoeA
            A%Val(indexes(6)) = A%Val(indexes(6)) + (lambda1 * be2 * be3 + lambda2 * (be2 * ga3 + ga2 * be3) + lambda3 * ga2 * ga3) * CoeA
            A%Val(indexes(7)) = A%Val(indexes(7)) + (lambda1 * be3 * be1 + lambda2 * (be3 * ga1 + ga3 * be1) + lambda3 * ga3 * ga1) * CoeA
            A%Val(indexes(8)) = A%Val(indexes(8)) + (lambda1 * be3 * be2 + lambda2 * (be3 * ga2 + ga3 * be2) + lambda3 * ga3 * ga2) * CoeA
            A%Val(indexes(9)) = A%Val(indexes(9)) + (lambda1 * be3 * be3 + lambda2 * (be3 * ga3 + ga3 * be3) + lambda3 * ga3 * ga3) * CoeA

        end do
    end subroutine Assemble_Diffusion_Dispersity_231
end module Matrix_Assemble
