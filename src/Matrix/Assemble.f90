module Matrix_Assemble
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Matrix_CRS
    use :: Solver_Element
#ifdef _OPENMP
    use omp_lib
#endif
    implicit none
contains
    subroutine Assemble_Mass_1(A, Elements, C)
        implicit none
        type(Type_CRS), intent(inout) :: A
        type(ElementHolder), allocatable :: Elements(:)
        real(real64), intent(in) :: C(:)

        integer(int32) :: index, nNodes
        integer(int32) :: il, jl, iG
        real(real64) :: val
        real(real64) :: xi, eta, weight, detJ

        integer(int32) :: iE

        do iE = 1, size(Elements)
            nNodes = Elements(iE)%e%getNumNodes()
            do il = 1, nNodes
                do jl = 1, nNodes
                    val = 0.0d0
                    call A%Find(Elements(iE)%e%conn(il), Elements(iE)%e%conn(jl), index)
                    do iG = 1, Elements(iE)%e%nGauss
                        xi = Elements(iE)%e%gauss(1, iG)
                        eta = Elements(iE)%e%gauss(2, iG)
                        weight = Elements(iE)%e%weight(iG)
                        val = val + (Elements(iE)%e%psi(il, xi, eta) * &
                                     Elements(iE)%e%psi(jl, xi, eta) * &
                                     Elements(iE)%e%Jac_Det(xi, eta) * &
                                     weight * &
                                     C(Elements(iE)%e%conn(il)))
                    end do
                    A%Val(index) = A%Val(index) + val
                end do
            end do
        end do

    end subroutine Assemble_Mass_1

    subroutine Assemble_Diffusion_1_Isotropic(A, Elements, lambda)
        implicit none
        type(Type_CRS), intent(inout) :: A
        type(ElementHolder), allocatable, intent(in) :: Elements(:)
        real(real64), intent(in) :: lambda(:)

        integer(int32) :: iE, il, jl, iG
        integer(int32) :: index, nNodes
        real(real64) :: val, mean_lambda
        real(real64) :: xi, eta, weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j

        do iE = 1, size(Elements)

            ! 節点数取得
            nNodes = Elements(iE)%e%getNumNodes()
            ! 要素内での平均拡散係数
            mean_lambda = sum(lambda(Elements(iE)%e%conn(:))) / dble(nNodes)
            do il = 1, nNodes
                do jl = 1, nNodes
                    val = 0.0d0
                    do iG = 1, Elements(iE)%e%nGauss
                        xi = Elements(iE)%e%gauss(1, iG)
                        eta = Elements(iE)%e%gauss(2, iG)
                        weight = Elements(iE)%e%weight(iG)

                        ! ヤコビアン行列式
                        detJ = Elements(iE)%e%Jac_Det(xi, eta)

                        ! 形状関数勾配（x,y方向）
                        dNdx_i = (Elements(iE)%e%Jac(2, 2, xi, eta) * &
                                  Elements(iE)%e%dpsi_dxi(il, eta) - &
                                  Elements(iE)%e%Jac(2, 1, xi, eta) * &
                                  Elements(iE)%e%dpsi_deta(il, xi) &
                                  ) / detJ
                        dNdy_i = (-Elements(iE)%e%Jac(1, 2, xi, eta) * &
                                  Elements(iE)%e%dpsi_dxi(il, eta) + &
                                  Elements(iE)%e%Jac(1, 1, xi, eta) * &
                                  Elements(iE)%e%dpsi_deta(il, xi) &
                                  ) / detJ
                        dNdx_j = (Elements(iE)%e%Jac(2, 2, xi, eta) * &
                                  Elements(iE)%e%dpsi_dxi(jl, eta) - &
                                  Elements(iE)%e%Jac(2, 1, xi, eta) * &
                                  Elements(iE)%e%dpsi_deta(jl, xi) &
                                  ) / detJ
                        dNdy_j = (-Elements(iE)%e%Jac(1, 2, xi, eta) * &
                                  Elements(iE)%e%dpsi_dxi(jl, eta) + &
                                  Elements(iE)%e%Jac(1, 1, xi, eta) * &
                                  Elements(iE)%e%dpsi_deta(jl, xi) &
                                  ) / detJ

                        val = val + (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * weight * detJ
                    end do
                    call A%Find(Elements(iE)%e%conn(il), Elements(iE)%e%conn(jl), index)
                    A%Val(index) = A%Val(index) + val * mean_lambda
                end do
            end do
        end do

    end subroutine Assemble_Diffusion_1_Isotropic
end module Matrix_Assemble
