submodule(Core_Side) Core_Side_First
    implicit none
contains

    module function getNumNodes_SideFirst(self) result(n)
        implicit none
        class(SideFirst), intent(in) :: self
        integer(int32) :: n

        n = self%size
    end function getNumNodes_SideFirst

    module function psi_SideFirst(self, i, xi) result(psi)
        implicit none
        class(SideFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: xi
        real(real64) :: psi

        select case (i)
        case (1)
            psi = 0.5d0 * (1.0d0 - xi)
        case (2)
            psi = 0.5d0 * (1.0d0 + xi)
        case default
            psi = 0.0d0
        end select
    end function psi_SideFirst

    module function dpsi_dxi_SideFirst(self, i) result(dpsi)
        implicit none
        class(SideFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64) :: dpsi

        select case (i)
        case (1)
            dpsi = -0.5d0
        case (2)
            dpsi = 0.5d0
        case default
            dpsi = 0.0d0
        end select
    end function dpsi_dxi_SideFirst

end submodule Core_Side_First
