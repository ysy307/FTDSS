submodule(domain_base_fe) domain_base_fe_base
    implicit none
contains
    module subroutine initialize_abst_fe(self, type, dimension, order, num_nodes, integration_order)
        implicit none
        class(abst_fe), intent(inout) :: self
        integer(int32), intent(in) :: type
        integer(int32), intent(in) :: dimension
        integer(int32), intent(in) :: order
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in) :: integration_order

        self%type = type
        self%dimension = dimension
        self%num_nodes = num_nodes

        call self%integration_rule%initialize(self%type, integration_order)
    end subroutine initialize_abst_fe

    module subroutine destroy_abst_fe(self)
        implicit none
        class(abst_fe), intent(inout) :: self

        self%type = 0
        self%num_nodes = 0
        self%dimension = 0
        call self%integration_rule%destroy()
    end subroutine destroy_abst_fe

    module pure elemental subroutine get_type(self, type)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32), intent(inout) :: type

        type = self%type
    end subroutine get_type

    module pure elemental subroutine get_num_nodes(self, num_nodes)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32), intent(inout) :: num_nodes

        num_nodes = self%num_nodes
    end subroutine get_num_nodes

    module pure elemental subroutine get_dimension(self, dimension)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32), intent(inout) :: dimension

        dimension = self%dimension
    end subroutine get_dimension

    module pure elemental subroutine get_order(self, order)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32), intent(inout) :: order

        order = self%integration_rule%order
    end subroutine get_order

    module pure elemental subroutine get_num_gauss(self, num_gauss)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32), intent(inout) :: num_gauss

        num_gauss = self%integration_rule%num_gauss
    end subroutine get_num_gauss

    module subroutine get_weight(self, weight)
        implicit none
        class(abst_fe), intent(in), target :: self
        real(real64), intent(inout), pointer, contiguous, dimension(:) :: weight

        weight => self%integration_rule%weight
    end subroutine get_weight

    module subroutine get_gauss(self, gauss)
        implicit none
        class(abst_fe), intent(in), target :: self
        type(type_coordinate_dp), intent(inout), pointer, contiguous, dimension(:) :: gauss

        gauss => self%integration_rule%gauss
    end subroutine get_gauss

    module subroutine get_integration_rule(self, integration_rule)
        implicit none
        class(abst_fe), intent(in), target :: self
        type(type_gauss_integration_rule), intent(inout), pointer :: integration_rule

        integration_rule => self%integration_rule
    end subroutine get_integration_rule

    module subroutine display_abst_fe(self, unit_in)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit
        integer(int32) :: i

        unit = optval(unit_in, output_unit)

        write (unit, '(a)') "========================================================"
        write (unit, '(a)') " Finite Element Information "
        write (unit, '(a)') "========================================================"
        write (unit, '(a, i0)') "Element Type       : ", self%type
        write (unit, '(a, i0)') "Dimension          : ", self%dimension
        write (unit, '(a, i0)') "Order              : ", self%integration_rule%order
        write (unit, '(a, i0)') "Number of Nodes    : ", self%num_nodes
        write (unit, '(a, i0)') "Number of Gauss Pts: ", self%integration_rule%num_gauss

        if (self%integration_rule%num_gauss > 0) then
            write (unit, '(a)') "--------------------------------------------------------"
            write (unit, '(a)') "Gauss Quadrature Points and Weights:"
            write (unit, '(a)') "--------------------------------------------------------"
            do i = 1, self%integration_rule%num_gauss
                write (unit, '(a, i2, a, 3(f12.8, a), f12.8)') "  GP ", i, ": (", &
                    self%integration_rule%gauss(i)%x, ", ", self%integration_rule%gauss(i)%y, ", ", &
                    self%integration_rule%gauss(i)%z, ")  Weight = ", self%integration_rule%weight(i)
            end do
        end if
        write (unit, '(a)') "========================================================"
    end subroutine display_abst_fe

end submodule domain_base_fe_base
