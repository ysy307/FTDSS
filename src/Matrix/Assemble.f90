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

        integer(int32) :: index
        integer(int32) :: ilocal, jlocal, iGauss
        real(real64) :: val

        integer(int32) :: iElem

        do iElem = 1, size(Elements)
            do ilocal = 1, Elements(iElem)%p%size
                do jlocal = 1, Elements(iElem)%p%size
                    call A%Find(Elements(iElem)%p%conn(ilocal), Elements(iElem)%p%conn(jlocal), index)
                    do iGauss = 1, Elements(iElem)%p%nGauss
                        val = Elements(iElem)%p%shape(ilocal, Elements(iElem)%p%gauss(1, iGauss), Elements(iElem)%p%gauss(2, iGauss)) * &
                              Elements(iElem)%p%shape(jlocal, Elements(iElem)%p%gauss(1, iGauss), Elements(iElem)%p%gauss(2, iGauss)) * &
                              Elements(iElem)%p%weight(iGauss) * &
                              C(Elements(iElem)%p%conn(ilocal))
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

        integer(int32) :: iElem, ilocal, jlocal, iGauss
        integer(int32) :: index, nNodes
        real(real64) :: val, mean_lambda
        real(real64) :: xi, eta, weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j

        do iElem = 1, size(Elements)

            ! 節点数取得
            nNodes = Elements(iElem)%p%getNumNodes()
            ! 要素内での平均拡散係数
            mean_lambda = sum(lambda(Elements(iElem)%p%conn(:))) / real(nNodes, real64)
            do ilocal = 1, nNodes
                do jlocal = 1, nNodes
                    val = 0.0d0
                    do iGauss = 1, Elements(iElem)%p%nGauss
                        xi = Elements(iElem)%p%gauss(1, iGauss)
                        eta = Elements(iElem)%p%gauss(2, iGauss)
                        weight = Elements(iElem)%p%weight(iGauss)

                        ! ヤコビアン行列式
                        detJ = Elements(iElem)%p%Jacobian_Det(xi, eta)

                        ! 形状関数勾配（x,y方向）
                        dNdx_i = (Elements(iElem)%p%Jacobian_components(2, 2, xi, eta) * &
                                  Elements(iElem)%p%shape_dxi(ilocal, eta) - &
                                  Elements(iElem)%p%Jacobian_components(1, 2, xi, eta) * &
                                  Elements(iElem)%p%shape_deta(ilocal, xi) &
                                  ) / detJ
                        dNdy_i = (-Elements(iElem)%p%Jacobian_components(2, 1, xi, eta) * &
                                  Elements(iElem)%p%shape_dxi(ilocal, eta) + &
                                  Elements(iElem)%p%Jacobian_components(1, 1, xi, eta) * &
                                  Elements(iElem)%p%shape_deta(ilocal, xi) &
                                  ) / detJ
                        dNdx_j = (Elements(iElem)%p%Jacobian_components(2, 2, xi, eta) * &
                                  Elements(iElem)%p%shape_dxi(jlocal, eta) - &
                                  Elements(iElem)%p%Jacobian_components(1, 2, xi, eta) * &
                                  Elements(iElem)%p%shape_deta(jlocal, xi) &
                                  ) / detJ
                        dNdy_j = (-Elements(iElem)%p%Jacobian_components(2, 1, xi, eta) * &
                                  Elements(iElem)%p%shape_dxi(jlocal, eta) + &
                                  Elements(iElem)%p%Jacobian_components(1, 1, xi, eta) * &
                                  Elements(iElem)%p%shape_deta(jlocal, xi) &
                                  ) / detJ

                        val = val + (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * weight * detJ
                    end do
                    call A%Find(Elements(iElem)%p%conn(ilocal), Elements(iElem)%p%conn(jlocal), index)
                    A%Val(index) = A%Val(index) + val * mean_lambda
                end do
            end do
        end do

    end subroutine Assemble_Diffusion_1_Isotropic
end module Matrix_Assemble
