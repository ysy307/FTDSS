submodule(Condition_Fix_Boundary) Condition_Fix_Boundary_Base
    implicit none
contains
    module subroutine Fix_BC_CRS_Dirichlet(A, b, Info, Edge, Dval)
        implicit none
        type(Type_CRS), intent(inout), optional :: A
        real(real64), intent(inout) :: b(:)
        type(Condition_BC_Local), intent(in) :: Info
        integer(int32), intent(in) :: Edge(2)
        real(real64), intent(in) :: Dval

        integer(int32) :: i, ind, ps, pe
        integer(int32) :: p1, p2

        if (Info%isUniform) then
            p1 = Edge(1)
            p2 = Edge(2)

            if (present(A)) then
                call A%Find(p1, p1, ind)
                ps = A%Ptr(p1)
                pe = A%Ptr(p1 + 1) - 1
                A%val(ps:pe) = 0.0d0
                A%val(ind) = 1.0d0

                call A%Find(p2, p2, ind)
                ps = A%Ptr(p2)
                pe = A%Ptr(p2 + 1) - 1
                A%val(ps:pe) = 0.0d0
                A%val(ind) = 1.0d0
            end if

            b(p1) = Dval
            b(p2) = Dval
        end if

    end subroutine Fix_BC_CRS_Dirichlet

end submodule Condition_Fix_Boundary_Base
