submodule(domain_mesh_side) domain_mesh_side_second
    implicit none
contains

    module function construct_side_second(id, global_coordinate, input) result(side)
        implicit none
        integer(int32), intent(in) :: id
        type(type_dp_3d), pointer, intent(in) :: global_coordinate
        type(type_input), intent(in) :: input
        class(abst_side), allocatable :: side

        integer(int32) :: num_nodes, num_gauss
        real(real64), allocatable :: weight(:)
        real(real64), allocatable :: gauss(:, :)

        allocate (type_side_second :: side)

        num_nodes = input%geometry%vtk%cells(id)%num_nodes_in_cell

        select case (input%basic%geometry_settings%integration_type)
        case ("full")
            num_gauss = 2_int32
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, 3_int32, num_gauss)

            weight(:) = [1.0d0, 1.0d0]
            gauss(:, 1) = [-sqrt(1.0d0 / 3.0d0), 0.0d0, 0.0d0]
            gauss(:, 2) = [sqrt(1.0d0 / 3.0d0), 0.0d0, 0.0d0]
        case ("reduced")
            num_gauss = 1_int32
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, 3_int32, num_gauss)

            weight(:) = [0.0d0]
            gauss(:, 1) = [2.0d0, 0.0d0, 0.0d0]
        case ("free")
            num_gauss = 2_int32
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, 3_int32, num_gauss)

            weight(:) = [1.0d0, 1.0d0]
            gauss(:, 1) = [-sqrt(1.0d0 / 3.0d0), 0.0d0, 0.0d0]
            gauss(:, 2) = [sqrt(1.0d0 / 3.0d0), 0.0d0, 0.0d0]
        end select

        call side%initialize(id=id, &
                             type=input%geometry%vtk%cells(id)%cell_type, &
                             group=input%geometry%vtk%cells(id)%cell_entity_id, &
                             dimension=input%geometry%vtk%cells(id)%get_dimension(), &
                             order=input%geometry%vtk%cells(id)%get_order(), &
                             num_nodes=num_nodes, &
                             connectivity=input%geometry%vtk%cells(id)%connectivity(1:num_nodes), &
                             num_gauss=num_gauss, &
                             weight=weight, &
                             gauss=gauss, &
                             global_coordinate=global_coordinate)

    end function construct_side_second

    module pure function get_length_side_second(self) result(length)
        implicit none
        class(type_side_second), intent(in) :: self
        real(real64) :: length

        real(real64), parameter :: xi1 = -1.0d0 / sqrt(3.0d0)
        real(real64), parameter :: xi2 = 1.0d0 / sqrt(3.0d0)
        ! -----------------------------------------------

        type(type_dp_vector_3d) :: r1, r2
        real(real64) :: det1, det2

        r1 = type_dp_vector_3d(-1.0d0 / sqrt(3.0d0), 0.0d0, 0.0d0)
        r2 = type_dp_vector_3d(1.0d0 / sqrt(3.0d0), 0.0d0, 0.0d0)

        ! 各ガウス積分点でのヤコビ行列式 det(J) = ds/dξ を計算
        det1 = self%jacobian_det(r1)
        det2 = self%jacobian_det(r2)

        ! ガウス求積法で長さを計算: Length ≈ w1*det(J(ξ1)) + w2*det(J(ξ2))
        length = det1 + det2

    end function get_length_side_second

    module pure elemental function psi_side_second(self, i, r) result(psi)
        implicit none
        class(type_side_second), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: psi

        select case (i)
        case (1)
            psi = 0.5d0 * r%x * (1.0d0 - r%x)
        case (2)
            psi = 1.0d0 * r%x**2.0d0
        case (3)
            psi = 0.5d0 * r%x * (1.0d0 + r%x)
        case default
            psi = 0.0d0
        end select
    end function psi_side_second

    module pure elemental function dpsi_side_second(self, i, j, r) result(dpsi)
        implicit none
        class(type_side_second), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: dpsi

        select case (j)
        case (1)
            select case (i)
            case (1)
                dpsi = 0.5d0 - r%x
            case (2)
                dpsi = 2.0d0 * r%x
            case (3)
                dpsi = 0.5d0 + r%x
            case default
                dpsi = 0.0d0
            end select
        case default
            dpsi = 0.0d0
        end select
    end function dpsi_side_second

    pure elemental module function jacobian_side_second(self, i, j, r) result(jacobian)
        implicit none
        class(type_side_second), intent(in) :: self
        integer(int32), intent(in) :: i ! Global coordinate direction (1=x, 2=y, 3=z)
        integer(int32), intent(in) :: j ! Local coordinate direction (1=ξ)
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: jacobian

        integer(int32) :: k
        real(real64) :: dpsi_val
        type(type_dp_vector_3d) :: coord

        jacobian = 0.0d0

        ! The Jacobian is defined as J = sum(dNi/dξ * xi) over all nodes
        if (j == 1) then
            do k = 1, self%get_num_nodes()
                ! Get the shape function derivative at point r
                dpsi_val = self%dpsi(k, 1, r)
                ! Get the coordinates of node k
                coord = self%get_coordinate(k)

                select case (i)
                case (1) ! x-component
                    jacobian = jacobian + dpsi_val * coord%x
                case (2) ! y-component
                    jacobian = jacobian + dpsi_val * coord%y
                case (3) ! z-component
                    jacobian = jacobian + dpsi_val * coord%z
                end select
            end do
        end if

    end function jacobian_side_second

    pure elemental module function jacobian_det_side_second(self, r) result(jacobian_det)
        implicit none
        class(type_side_second), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: jacobian_det

        real(real64) :: jac_x, jac_y, jac_z

        ! Get the three components of the Jacobian vector at point r
        jac_x = self%jacobian(1, 1, r)
        jac_y = self%jacobian(2, 1, r)
        jac_z = self%jacobian(3, 1, r)

        ! The determinant is the vector's magnitude (norm), which is ds/dξ
        jacobian_det = sqrt(jac_x**2 + jac_y**2 + jac_z**2)

    end function jacobian_det_side_second

end submodule domain_mesh_side_Second
