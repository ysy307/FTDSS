module conditions_boundary_manager
    use :: iso_fortran_env
    use :: module_core, only:allocate_array, deallocate_array
    use :: module_domain, only:type_domain
    use :: module_matrix, only:type_crs
    use :: module_input, only:type_input
    use :: conditions_boundary, only:abst_bc_thermal, type_bc_thermal_adiabatic, type_bc_thermal_dirichlet
    implicit none
    private

    public :: holder_bcs
    public :: type_bc

    type :: holder_bcs
        class(abst_bc_thermal), allocatable :: t
    end type holder_bcs

    type :: type_bc
        integer(int32) :: num_boundaries
        type(holder_bcs), allocatable :: bc(:)
    contains
        procedure :: initialize => initialize_type_bc
        procedure :: apply_crs => apply_type_bc_crs
        ! generic :: apply => apply_crs
    end type type_bc

contains

    subroutine initialize_type_bc(self, input, domain)
        class(type_bc), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_domain), intent(in) :: domain

        integer(int32), allocatable :: group_ids(:)
        integer(int32) :: i, i_material

        real(real64) :: time_conv

        self%num_boundaries = input%conditions%num_boundaries

        ! 2. 各グループのコンテナを確保
        if (allocated(self%bc)) deallocate (self%bc)
        allocate (self%bc(self%num_boundaries))

        select case (input%conditions%time_control%simulation_period%unit)
        case ("Second")
            time_conv = 1.0d0
        case ("Minute")
            time_conv = 60.0d0
        case ("Hour")
            time_conv = 3600.0d0
        case ("Day")
            time_conv = 86400.0d0
        case ("Year")
            time_conv = 31557600.0d0
        end select

        ! 3. 各グループをループして、対応するBCオブジェクトを生成・セットアップ
        do i = 1, self%num_boundaries
            i_material = input%conditions%boundary_conditions(i)%id
            if (input%basic%analysis_controls%calculate_thermal) then
                select case (input%conditions%boundary_conditions(i)%thermal%type)
                case ("dirichlet")
                    allocate (type_bc_thermal_dirichlet :: self%bc(i)%t)
                    call self%bc(i)%t%initialize(input, domain, i, i_material, time_conv)
                case ("adiabatic")
                    allocate (type_bc_thermal_adiabatic :: self%bc(i)%t)
                    call self%bc(i)%t%initialize(input, domain, i, i_material, time_conv)
                end select
            end if
            if (input%basic%analysis_controls%calculate_hydraulic) then
                ! select case (input%conditions%boundary_conditions(i)%thermal%type)
                ! case ("dirichlet")
                !     allocate (type_bc_thermal_dirichlet :: self%bc(i)%t)
                !     call self%bc(i)%t%initialize(input, domain, i_material, time_conv)
                ! case ("adiabatic")
                !     allocate (type_bc_thermal_adiabatic :: self%bc(i)%t)
                !     call self%bc(i)%t%initialize(input, domain, i_material, time_conv)
                ! end select
            end if
        end do
    end subroutine initialize_type_bc

    subroutine apply_type_bc_crs(self, boundary_target, current_time, A, b, domain, mode)
        class(type_bc), intent(inout) :: self
        character(*), intent(in) :: boundary_target
        real(real64), intent(in) :: current_time
        type(type_crs), intent(inout), optional :: A
        real(real64), intent(inout) :: b(:)
        type(type_domain), intent(in) :: domain
        integer(int32), intent(in), optional :: mode

        integer(int32) :: i

        ! --------------------------------------------------------------------------
        ! 1st Pass: Apply all non-Dirichlet boundary conditions (e.g., Neumann, Adiabatic)
        ! --------------------------------------------------------------------------
        select case (trim(adjustl(boundary_target)))
        case ('thermal')
            do i = 1, self%num_boundaries
                if (allocated(self%bc(i)%t)) then
                    select type (bc => self%bc(i)%t)
                    type is (type_bc_thermal_dirichlet)
                        ! This is a Dirichlet BC, so we skip it in the first pass.
                        cycle
                    class default
                        ! This is any other type of BC, apply it now.
                        if (present(A)) then
                            ! Apply the BC using CRS matrix format.
                            if (present(mode)) then
                                call bc%apply_crs(current_time=current_time, &
                                                  A=A, &
                                                  b=b, &
                                                  domain=domain, &
                                                  mode=mode)
                            else
                                call bc%apply_crs(current_time=current_time, &
                                                  b=b, &
                                                  domain=domain)
                            end if
                        else
                            ! Apply the BC without matrix A.
                            if (present(mode)) then
                                call bc%apply_crs(current_time=current_time, &
                                                  b=b, &
                                                  domain=domain, &
                                                  mode=mode)
                            else
                                call bc%apply_crs(current_time=current_time, &
                                                  b=b, &
                                                  domain=domain)
                            end if
                        end if
                    end select
                end if
            end do

            ! --------------------------------------------------------------------------
            ! 2nd Pass: Apply ONLY the Dirichlet boundary conditions
            ! --------------------------------------------------------------------------
            do i = 1, self%num_boundaries
                if (allocated(self%bc(i)%t)) then
                    select type (bc => self%bc(i)%t)
                    type is (type_bc_thermal_dirichlet)
                        ! This is a Dirichlet BC, apply it in the final pass.
                        if (present(A)) then
                            ! Apply the BC using CRS matrix format.
                            if (present(mode)) then
                                call bc%apply_crs(current_time=current_time, &
                                                  A=A, &
                                                  b=b, &
                                                  domain=domain, &
                                                  mode=mode)
                            else
                                call bc%apply_crs(current_time=current_time, &
                                                  b=b, &
                                                  domain=domain)
                            end if
                        else
                            ! Apply the BC without matrix A.
                            if (present(mode)) then
                                call bc%apply_crs(current_time=current_time, &
                                                  b=b, &
                                                  domain=domain, &
                                                  mode=mode)
                            else
                                call bc%apply_crs(current_time=current_time, &
                                                  b=b, &
                                                  domain=domain)
                            end if
                        end if
                    class default
                        ! All other types were handled in the first pass, so do nothing.
                        cycle
                    end select
                end if
            end do
        end select

    end subroutine apply_type_bc_crs

end module conditions_boundary_manager
