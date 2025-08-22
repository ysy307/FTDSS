submodule(calculate_wrf) calculate_wrf_bc
    implicit none
contains
    module function construct_type_wrf_bc(input) result(property)
        implicit none
        type(type_materials_wrf), intent(in) :: input
        class(abst_wrf), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_wrf_bc :: property)

        select type (this => property)
        type is (type_wrf_bc)
            this%theta_r = input%theta_r
            this%theta_s = input%theta_S
            this%alpha1 = input%alpha1
            this%n1 = input%n1
        end select

    end function construct_type_wrf_bc

    module pure function calculate_wrf_bc(self, h) result(theta_w)
        implicit none
        class(type_wrf_bc), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: theta_w

        if (h < self%alpha1) then
            theta_w = self%theta_r + (self%theta_s - self%theta_r) * (self%alpha1 / h)**self%n1
        else
            theta_w = self%theta_s
        end if

    end function calculate_wrf_bc

    module pure function calculate_wrf_bc_derivative(self, h) result(dqw_dh)
        implicit none
        class(type_wrf_bc), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: dqw_dh

        !@note alpha1 must be negative
        if (h < self%alpha1) then
            dqw_dh = -(self%theta_s - self%theta_r) * self%n1 * (self%alpha1 / h)**(self%n1 + 1.0d0) / self%alpha1
        else
            dqw_dh = 0.0d0
        end if
    end function calculate_wrf_bc_derivative

end submodule calculate_wrf_bc
