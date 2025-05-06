submodule(Condition_Boundary) Condition_Boundary_Thermal_CRS
    implicit none
contains
    module function Type_BC_Thermal_CRS_Construct(Input) result(Structure)
        implicit none
        type(Type_Input), intent(in) :: Input
        class(Abstract_Condition_BC), allocatable :: Structure

        integer(int32) :: minimum, maximum
        integer(int32) :: iBC, iGroup

        if (allocated(Structure)) deallocate (Structure)
        allocate (Type_BC_Thermal_CRS :: Structure)

        allocate (Structure%BCGroup, source=Input%Conditions%Groups)

        allocate (Structure%Time, source=Input%Conditions%Time)
        select case (Input%Basic%Input_TimeUnit)
        case ("Second")
            Structure%Time(:) = Structure%Time(:) * 1.0d0
        case ("Minute")
            Structure%Time(:) = Structure%Time(:) * 60.0d0
        case ("Hour")
            Structure%Time(:) = Structure%Time(:) * 3600.0d0
        case ("Day")
            Structure%Time(:) = Structure%Time(:) * 86400.0d0
        case ("Year")
            Structure%Time(:) = Structure%Time(:) * 31557600.0d0
        end select

        Structure%numBCGroup = size(Structure%BCGroup)
        minimum = minval(Structure%BCGroup)
        maximum = maxval(Structure%BCGroup)
        allocate (Structure%BCInfo(minimum:maximum))

        do iBC = 1, Structure%numBCGroup
            iGroup = Structure%BCGroup(iBC)
            Structure%BCInfo(iGroup)%type = Input%Conditions%Heat(iGroup)%type
            Structure%BCInfo(iGroup)%isUniform = Input%Conditions%Heat(iGroup)%isUniform
            allocate (Structure%BCInfo(iGroup)%value, source=Input%Conditions%Heat(iGroup)%value)
        end do

    end function Type_BC_Thermal_CRS_Construct

    module subroutine Type_BC_Thermal_CRS_Fix_BC_All(self, A, b, Sides, time)
        implicit none
        class(Type_BC_Thermal_CRS), intent(in) :: self
        type(Type_CRS), intent(inout) :: A
        real(real64), intent(inout) :: b(:)
        type(SideHolder), intent(in) :: Sides(:)
        real(real64), intent(in), optional :: time

        integer(int32) :: iEdge, iGroup
        integer(int32) :: numEdges
        real(real64) :: timeCoe, Dval
        integer(int32) :: i, idx

        do i = 1, size(self%Time) - 1
            if (self%Time(i) < time .and. time <= self%Time(i + 1)) then
                timeCoe = (time - self%Time(i)) / (self%Time(i + 1) - self%Time(i))
                idx = i
                exit
            end if
        end do

        numEdges = size(Sides)

        do iEdge = 1, numEdges
            iGroup = Sides(iEdge)%s%SideGroup
            select case (self%BCInfo(iGroup)%type)
            case (Dirichlet)
                Dval = (self%BCInfo(iGroup)%value(idx) * (1.0d0 - timeCoe) + &
                        self%BCInfo(iGroup)%value(idx + 1) * timeCoe)
                call self%Fix_BC_Dirichlet( &
                    A=A, &
                    b=b, &
                    Info=self%BCInfo(iGroup), &
                    Edge=Sides(iEdge)%s%Conn(1:2), &
                    Dval=Dval)
            end select
        end do

    end subroutine Type_BC_Thermal_CRS_Fix_BC_All

    module subroutine Type_BC_Thermal_CRS_Fix_BC_RHS(self, b, Sides, time)
        implicit none
        class(Type_BC_Thermal_CRS), intent(in) :: self
        real(real64), intent(inout) :: b(:)
        type(SideHolder), intent(in) :: Sides(:)
        real(real64), intent(in), optional :: time

        integer(int32) :: iEdge, iGroup
        integer(int32) :: numEdges
        real(real64) :: timeCoe, Dval
        integer(int32) :: i, idx

        numEdges = size(Sides)

        if (present(time)) then
            do i = 1, size(self%Time) - 1
                if (time >= self%Time(i) .and. time < self%Time(i + 1)) then
                    timeCoe = (time - self%Time(i)) / (self%Time(i + 1) - self%Time(i))
                    idx = i
                    exit
                end if
            end do

            do iEdge = 1, numEdges
                iGroup = Sides(iEdge)%s%SideGroup
                select case (self%BCInfo(iGroup)%type)
                case (Dirichlet)
                    Dval = (self%BCInfo(iGroup)%value(idx) * (1.0d0 - timeCoe) + &
                            self%BCInfo(iGroup)%value(idx + 1) * timeCoe)
                    call self%Fix_BC_Dirichlet( &
                        b=b, &
                        Info=self%BCInfo(iGroup), &
                        Edge=Sides(iEdge)%s%Conn(1:2), &
                        Dval=Dval)
                end select
            end do
        else
            do iEdge = 1, numEdges
                iGroup = Sides(iEdge)%s%SideGroup
                select case (self%BCInfo(iGroup)%type)
                case (Dirichlet)
                    Dval = self%BCInfo(iGroup)%value(1)
                    call self%Fix_BC_Dirichlet( &
                        b=b, &
                        Info=self%BCInfo(iGroup), &
                        Edge=Sides(iEdge)%s%Conn(1:2), &
                        Dval=Dval)
                end select
            end do
        end if

    end subroutine Type_BC_Thermal_CRS_Fix_BC_RHS

    module subroutine Type_BC_Thermal_CRS_Dirichlet(self, A, b, Info, Edge, Dval)
        implicit none
        class(Type_BC_Thermal_CRS), intent(in) :: self
        type(Type_CRS), intent(inout), optional :: A
        real(real64), intent(inout) :: b(:)
        type(Condition_BC_Local), intent(in) :: Info
        integer(int32), intent(in) :: Edge(2)
        real(real64), intent(in) :: Dval

        call Fix_BC_CRS_Dirichlet( &
            A=A, &
            b=b, &
            Info=Info, &
            Edge=Edge, &
            Dval=Dval)

    end subroutine Type_BC_Thermal_CRS_Dirichlet

    module subroutine Type_BC_Thermal_CRS_Dirichlet_NR(self, A, b, Info, Edge)
        implicit none
        class(Type_BC_Thermal_CRS), intent(in) :: self
        type(Type_CRS), intent(inout), optional :: A
        real(real64), intent(inout) :: b(:)
        type(Condition_BC_Local), intent(in) :: Info
        integer(int32), intent(in) :: Edge(2)

        call Fix_BC_CRS_Dirichlet( &
            A=A, &
            b=b, &
            Info=Info, &
            Edge=Edge, &
            Dval=0.0d0)

    end subroutine Type_BC_Thermal_CRS_Dirichlet_NR

end submodule Condition_Boundary_Thermal_CRS
