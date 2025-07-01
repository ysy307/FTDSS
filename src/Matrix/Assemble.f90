module Matrix_Assemble
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Matrix_CRS
    use :: Domain_Module, only:Domain_t
#ifdef _OPENMP
    use omp_lib
#endif
    implicit none
contains
    subroutine Assemble_Mass_Heat_1_NonSeg(A, Domain, Temperature, Porosity)
        implicit none
        type(Type_CRS), intent(inout) :: A
        type(Domain_t) :: Domain
        real(real64), intent(in) :: Temperature(:)
        real(real64), intent(in) :: Porosity(:)

        integer(int32) :: index, nNodes, nGauss
        integer(int32) :: il, jl, iG
        real(real64) :: val
        real(real64) :: xi, eta, weight, detJ
        real(real64) :: T_g, phi_g

        integer(int32) :: iE

        do iE = 1, Domain%nElement
            nNodes = Domain%Elements(iE)%e%getNumNodes()
            do il = 1, nNodes
                do jl = 1, nNodes
                    val = 0.0d0
                    call A%Find(Domain%Elements(iE)%e%conn(il), Domain%Elements(iE)%e%conn(jl), index)
                    nGauss = Domain%Elements(iE)%e%nGauss
                    do iG = 1, nGauss
                        xi = Domain%Elements(iE)%e%gauss(1, iG)
                        eta = Domain%Elements(iE)%e%gauss(2, iG)
                        weight = Domain%Elements(iE)%e%weight(iG)
                        detJ = Domain%Elements(iE)%e%Jac_Det(xi, eta)
                        T_g = 0.0d0
                        phi_g = 0.0d0

                        T_g = Domain%Elements(iE)%e%Interpolate(xi, eta, Temperature)
                        phi_g = Domain%Elements(iE)%e%Interpolate(xi, eta, Porosity)
                        ! val = val + (Domain%Elements(iE)%e%psi(il, xi, eta) * &
                        !              Domain%Elements(iE)%e%psi(jl, xi, eta) * &
                        !              Domain%Elements(iE)%e%Jac_Det(xi, eta) * &
                        !              weight * &
                        !              C(Domain%Elements(iE)%e%conn(il)))
                        ! end do
                    end do
                    A%Val(index) = A%Val(index) + val
                end do
            end do
        end do

    end subroutine Assemble_Mass_Heat_1_NonSeg

    subroutine Assemble_Diffusion_1_Isotropic(A, Domain, lambda)
        implicit none
        type(Type_CRS), intent(inout) :: A
        type(Domain_t), intent(in) :: Domain
        real(real64), intent(in) :: lambda(:)

        integer(int32) :: iE, il, jl, iG
        integer(int32) :: index, nNodes
        real(real64) :: val, mean_lambda
        real(real64) :: xi, eta, weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j

        do iE = 1, Domain%nElement

            ! 節点数取得
            nNodes = Domain%Elements(iE)%e%getNumNodes()
            ! 要素内での平均拡散係数
            mean_lambda = sum(lambda(Domain%Elements(iE)%e%conn(:))) / dble(nNodes)
            do il = 1, nNodes
                do jl = 1, nNodes
                    val = 0.0d0
                    do iG = 1, Domain%Elements(iE)%e%nGauss
                        xi = Domain%Elements(iE)%e%gauss(1, iG)
                        eta = Domain%Elements(iE)%e%gauss(2, iG)
                        weight = Domain%Elements(iE)%e%weight(iG)

                        ! ヤコビアン行列式
                        detJ = Domain%Elements(iE)%e%Jac_Det(xi, eta)

                        ! 形状関数勾配（x,y方向）
                        dNdx_i = (Domain%Elements(iE)%e%Jac(2, 2, xi, eta) * &
                                  Domain%Elements(iE)%e%dpsi_dxi(il, xi, eta) - &
                                  Domain%Elements(iE)%e%Jac(2, 1, xi, eta) * &
                                  Domain%Elements(iE)%e%dpsi_deta(il, xi, eta) &
                                  ) / detJ
                        dNdy_i = (-Domain%Elements(iE)%e%Jac(1, 2, xi, eta) * &
                                  Domain%Elements(iE)%e%dpsi_dxi(il, xi, eta) + &
                                  Domain%Elements(iE)%e%Jac(1, 1, xi, eta) * &
                                  Domain%Elements(iE)%e%dpsi_deta(il, xi, eta) &
                                  ) / detJ
                        dNdx_j = (Domain%Elements(iE)%e%Jac(2, 2, xi, eta) * &
                                  Domain%Elements(iE)%e%dpsi_dxi(jl, xi, eta) - &
                                  Domain%Elements(iE)%e%Jac(2, 1, xi, eta) * &
                                  Domain%Elements(iE)%e%dpsi_deta(jl, xi, eta) &
                                  ) / detJ
                        dNdy_j = (-Domain%Elements(iE)%e%Jac(1, 2, xi, eta) * &
                                  Domain%Elements(iE)%e%dpsi_dxi(jl, xi, eta) + &
                                  Domain%Elements(iE)%e%Jac(1, 1, xi, eta) * &
                                  Domain%Elements(iE)%e%dpsi_deta(jl, xi, eta) &
                                  ) / detJ

                        val = val + (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * weight * detJ
                    end do
                    call A%Find(Domain%Elements(iE)%e%conn(il), Domain%Elements(iE)%e%conn(jl), index)
                    A%Val(index) = A%Val(index) + val * mean_lambda
                end do
            end do
        end do

    end subroutine Assemble_Diffusion_1_Isotropic
end module Matrix_Assemble
