submodule(Domain_Side) Domain_Side_First
    implicit none
contains

    module function SideFirst_Construct(iSide, Global_Coordinate, Connectivity, GroupID) result(Structure)
        implicit none
        integer(int32), intent(in) :: iSide
        type(DP3d), pointer, intent(in) :: Global_Coordinate
        integer(int32), intent(in) :: Connectivity(2)
        integer(int32), intent(in) :: GroupID
        class(Abst_SideType), allocatable :: Structure

        integer(int32), parameter :: nsize = 2
        integer(int32) :: i

        allocate (SideFirst :: Structure)
        Structure%id = iSide
        Structure%type = 3
        Structure%group = GroupID

        Structure%size = nsize
        allocate (Structure%conn(nsize))
        Structure%conn(:) = Connectivity(1:nsize)

        allocate (Structure%X(nsize))
        allocate (Structure%Y(nsize))
        allocate (Structure%Z(nsize))
        do i = 1, nsize
            nullify (Structure%X(i)%val)
            nullify (Structure%Y(i)%val)
            nullify (Structure%Z(i)%val)
            Structure%X(i)%val => Global_Coordinate%x(Structure%conn(i))
            Structure%Y(i)%val => Global_Coordinate%y(Structure%conn(i))
            Structure%Z(i)%val => Global_Coordinate%z(Structure%conn(i))
        end do

        Structure%nGauss = 1
        call Allocate_Array(Structure%weight, Structure%nGauss)
        call Allocate_Array(Structure%gauss, Structure%nGauss)
        Structure%weight(:) = [0.0d0]
        Structure%gauss(:) = [2.0d0]
    end function SideFirst_Construct

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

end submodule Domain_Side_First
