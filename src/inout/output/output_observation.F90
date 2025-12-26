submodule(inout_output) inout_output_observation
    use :: iso_fortran_env, only:int32, real64
    use :: stdlib_strings, only:to_string
    implicit none

contains

    module subroutine initialize_type_output_observation(self, input, domain, dir_output, variable_name)
        implicit none
        class(type_output_observation), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_domain), intent(inout) :: domain
        character(*), intent(in) :: dir_output
        character(*), intent(in) :: variable_name

        integer(int32) :: iObs, iElem, num_elements
        integer(int32) :: elem_id, comp_dim, calc_type
        type(type_coordinate_dp) :: cartesian, normalized
        logical :: inside

        class(abst_fe), pointer :: fe
        integer(int32), pointer :: conn(:)
        real(real64), allocatable :: ele_coords(:, :)

        ! --- 設定の取得 ---
        self%type = input%output_settings%history_output%observation_type

        if (trim(self%type) == "none") then
            self%do_output = .false.
            return
        else
            self%do_output = .true.
        end if

        self%num_observations = input%output_settings%history_output%num_observations

        select case (trim(self%type))
        case ("node_ids")
            if (allocated(self%node_ids)) deallocate (self%node_ids)
            allocate (self%node_ids, source=input%output_settings%history_output%node_ids)

        case ("coordinates")
            call self%coordinate%initialize(self%num_observations)
            do iObs = 1, self%num_observations
                self%coordinate%x(iObs) = input%output_settings%history_output%coordinates(iObs)%x
                self%coordinate%y(iObs) = input%output_settings%history_output%coordinates(iObs)%y
                self%coordinate%z(iObs) = input%output_settings%history_output%coordinates(iObs)%z
            end do

            ! 要素ID保存用配列の確保
            if (allocated(self%element_ids)) deallocate (self%element_ids)
            allocate (self%element_ids(self%num_observations))
            self%element_ids = -1

            if (allocated(self%coordinate_normalized)) deallocate (self%coordinate_normalized)
            allocate (self%coordinate_normalized(self%num_observations))

            ! --- 座標探索ロジック ---
            num_elements = domain%get_num_elements()
            comp_dim = domain%get_computation_dimension()
            calc_type = input%basic%simulation_settings%calculate_type

            do iObs = 1, self%num_observations
                ! 観測点座標セット (探索点)
                if (comp_dim == 2) then
                    if (calc_type == 1) then ! XY (2D)
                        call cartesian%set(self%coordinate%x(iObs), self%coordinate%y(iObs), 0.0d0)
                    else ! XZ (2D)
                        ! XZ平面の場合、Y成分にZ座標を入れて2D探索を行う (domain側の格納形式に合わせる)
                        call cartesian%set(self%coordinate%x(iObs), self%coordinate%z(iObs), 0.0d0)
                    end if
                else ! 3D
                    call cartesian%set(self%coordinate%x(iObs), self%coordinate%y(iObs), self%coordinate%z(iObs))
                end if

                do iElem = 1, num_elements
                    call domain%get_element(iElem, fe)
                    if (.not. associated(fe)) cycle
                    call domain%get_connectivity(iElem, conn)
                    if (.not. associated(conn)) cycle

                    if (allocated(ele_coords)) deallocate (ele_coords)
                    allocate (ele_coords(comp_dim, size(conn)))

                    ele_coords = domain%nodes%coordinates(1:comp_dim, conn)

                    ! 包含判定
                    call fe%is_inside(cartesian, normalized, ele_coords, conn, inside)

                    if (inside) then
                        self%element_ids(iObs) = iElem
                        self%coordinate_normalized(iObs) = normalized
                        exit
                    end if
                end do

                ! デバッグ用: 見つからない場合の警告
                if (self%element_ids(iObs) == -1) then
                    print *, "Warning: Observation point ", iObs, " is outside the domain."
                end if
            end do
        end select

        if (associated(self%get_values)) nullify (self%get_values)

        select case (trim(adjustl(variable_name)))
        case ("temperature")
            self%name = trim(adjustl(variable_name))
            self%unit = "degC"
            self%file_name = trim(adjustl(dir_output))//"obsf_T."//trim(adjustl(input%output_settings%history_output%file_format))
            self%num_unit = 99999999
            select case (trim(self%type))
            case ("node_ids"); self%get_values => get_observations_temperature
            case ("coordinates"); self%get_values => interpolate_observations_temperature
            end select
        case ("ice_saturation")
            self%name = trim(adjustl(variable_name))
            self%unit = "-"
            self%file_name = trim(adjustl(dir_output))//"obsf_Si."//trim(adjustl(input%output_settings%history_output%file_format))
            self%num_unit = 99999999
            select case (trim(self%type))
            case ("node_ids"); self%get_values => get_observations_si
            case ("coordinates"); self%get_values => interpolate_observations_si
            end select
        case ("thermal_conductivity")
            self%name = trim(adjustl(variable_name))
            self%unit = "W/m/K"
            self%file_name = trim(adjustl(dir_output))//"obsf_TC."//trim(adjustl(input%output_settings%history_output%file_format))
            self%num_unit = 99999999
            select case (trim(self%type))
            case ("node_ids"); self%get_values => get_observations_thc
            case ("coordinates"); self%get_values => interpolate_observations_thc
            end select
        case ("volumetric_heat_capacity")
            self%name = trim(adjustl(variable_name))
            self%unit = "J/m^3/K"
            self%file_name = trim(adjustl(dir_output))//"obsf_C."//trim(adjustl(input%output_settings%history_output%file_format))
            self%num_unit = 99999999
            select case (trim(self%type))
            case ("node_ids"); self%get_values => get_observations_vhc
            case ("coordinates"); self%get_values => interpolate_observations_vhc
            end select
        case ("pressure")
            self%name = trim(adjustl(variable_name))
            self%unit = "m"
            self%file_name = trim(adjustl(dir_output))//"obsf_P."//trim(adjustl(input%output_settings%history_output%file_format))
            self%num_unit = 99999999
            select case (trim(self%type))
            case ("node_ids"); self%get_values => get_observations_pw
            case ("coordinates"); self%get_values => interpolate_observations_pw
            end select
        case ("water_flux")
            self%name = trim(adjustl(variable_name))
            self%unit = "m/s"
            self%file_name = trim(adjustl(dir_output))//"obsf_Flux."//trim(adjustl(input%output_settings%history_output%file_format))
            self%num_unit = 99999999
            self%num_observations = self%num_observations * 3
        case ("hydraulic_conductivity")
            self%name = trim(adjustl(variable_name))
            self%unit = "m/s"
            self%file_name = trim(adjustl(dir_output))//"obsf_K."//trim(adjustl(input%output_settings%history_output%file_format))
            self%num_unit = 99999999
        end select

        if (associated(self%write_line)) nullify (self%write_line)
        if (associated(self%write_header)) nullify (self%write_header)

        select case (trim(adjustl(input%output_settings%history_output%file_format)))
        case ("dat")
            self%write_header => write_observation_header_dat
            self%write_line => output_observation_line_dat
        case ("csv")
            self%write_header => write_observation_header_csv
            self%write_line => output_observation_line_csv
        end select

    end subroutine initialize_type_output_observation

    ! ==============================================================================
    ! Writer Subroutines
    ! ==============================================================================

    subroutine write_observation_header_dat(self, time_unit)
        implicit none
        class(type_output_observation), intent(inout) :: self
        character(*), intent(in) :: time_unit

        integer(int32) :: iObs, num_observations, elem_id

        if (.not. self%do_output) return

        num_observations = self%num_observations

        open (newunit=self%num_unit, file=trim(adjustl(self%file_name)), status='replace', action='write')

        write (self%num_unit, '(a)') "# "//trim(self%name)//" time variation"
        write (self%num_unit, '(a)') "#"

        select case (trim(self%type))
        case ("node_ids")
            write (self%num_unit, '(a)') "# Observation Node ID"
            do iObs = 1, num_observations
                write (self%num_unit, '(a,i0,a,x,i0)') "# Node ID ", iObs, ":", self%node_ids(iObs)
            end do
        case ("coordinates")
            write (self%num_unit, '(a)') "# Observation Coordinate (x,y,z)"
            do iObs = 1, num_observations
                elem_id = -1
                if (allocated(self%element_ids)) elem_id = self%element_ids(iObs)
                write (self%num_unit, '(a,x,i0,a,3(x,es18.11,a),a,i0)') &
                    "#    Point", iObs, ": (", &
                    self%coordinate%x(iObs), ",", &
                    self%coordinate%y(iObs), ",", &
                    self%coordinate%z(iObs), ")", &
                    " => Element ID: ", elem_id
            end do
        end select

        write (self%num_unit, '(a)') "#"
        write (self%num_unit, '(a)') "# Output Unit: Time ["//trim(adjustl(time_unit))//"], " &
            //trim(self%name)//" ["//trim(self%unit)//"]"
        write (self%num_unit, '(a)') "#"

        select case (trim(self%name))
        case ("water_flux")
            write (self%num_unit, '(a,'//to_string(num_observations)//'(2x,a))') &
                "Time", (("Obs"//to_string(iObs)//"_x", "Obs"//to_string(iObs)//"_y", "Obs"//to_string(iObs)//"_z"), &
                         iObs=1, num_observations / 3)
        case default
            write (self%num_unit, '(a,'//to_string(num_observations)//'(2x,a))') &
                "Time", ("Obs"//to_string(iObs), iObs=1, num_observations)
        end select

    end subroutine write_observation_header_dat

    subroutine write_observation_header_csv(self, time_unit)
        implicit none
        class(type_output_observation), intent(inout) :: self
        character(*), intent(in) :: time_unit

        integer(int32) :: iObs, num_observations, elem_id

        if (.not. self%do_output) return

        num_observations = self%num_observations

        open (newunit=self%num_unit, file=trim(adjustl(self%file_name)), status='replace', action='write')

        write (self%num_unit, '(a)') "# "//trim(self%name)//" time variation"
        write (self%num_unit, '(a)') "#"

        select case (trim(self%type))
        case ("node_ids")
            write (self%num_unit, '(a)') "# Observation Node ID"
            do iObs = 1, num_observations
                write (self%num_unit, '(a,i0,a,x,i0)') "# Node ID ", iObs, ":", self%node_ids(iObs)
            end do
        case ("coordinates")
            write (self%num_unit, '(a)') "# Observation Coordinate (x,y,z)"
            do iObs = 1, num_observations
                elem_id = -1
                if (allocated(self%element_ids)) elem_id = self%element_ids(iObs)
                write (self%num_unit, '(a,x,i0,a,3(x,es18.11,a),a,i0)') &
                    "#    Point", iObs, ": (", &
                    self%coordinate%x(iObs), ",", &
                    self%coordinate%y(iObs), ",", &
                    self%coordinate%z(iObs), ")", &
                    " => Element ID: ", elem_id
            end do
        end select

        write (self%num_unit, '(a)') "#"
        write (self%num_unit, '(a)') "# Output Unit: Time ["//trim(adjustl(time_unit))//"], " &
            //trim(self%name)//" ["//trim(self%unit)//"]"
        write (self%num_unit, '(a)') "#"

        select case (trim(self%name))
        case ("water_flux")
            write (self%num_unit, '(a,'//to_string(num_observations)//'(",",a))') &
                "Time", (("Obs"//to_string(iObs)//"_x", "Obs"//to_string(iObs)//"_y", "Obs"//to_string(iObs)//"_z"), &
                         iObs=1, num_observations / 3)
        case default
            write (self%num_unit, '(a,'//to_string(num_observations)//'(",",a))') &
                "Time", ("Obs"//to_string(iObs), iObs=1, num_observations)
        end select

    end subroutine write_observation_header_csv

    subroutine output_observation_line_dat(self, unit, time, values)
        implicit none
        class(type_output_observation), intent(in) :: self
        integer(int32), intent(in) :: unit
        real(real64), intent(in) :: time
        real(real64), intent(in) :: values(:)
        write (unit, '(*(es22.15,:,2x))') time, values(1:self%num_observations)
    end subroutine output_observation_line_dat

    subroutine output_observation_line_csv(self, unit, time, values)
        implicit none
        class(type_output_observation), intent(in) :: self
        integer(int32), intent(in) :: unit
        real(real64), intent(in) :: time
        real(real64), intent(in) :: values(:)
        write (unit, '(*(es22.15,:,","))') time, values(1:self%num_observations)
    end subroutine output_observation_line_csv

    ! ==============================================================================
    ! Interpolation / Getter Subroutines
    ! ==============================================================================

    subroutine interpolate_observations_temperature(self, obs_values, domain, &
                                                    nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(inout) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)

        integer(int32) :: iObs, elem_id
        integer(int32), pointer :: p_conn(:)
        class(abst_fe), pointer :: fe
        real(real64) :: val

        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return
        if (.not. present(domain)) return

        do iObs = 1, self%num_observations
            if (allocated(self%element_ids)) then
                elem_id = self%element_ids(iObs)
                if (elem_id > 0) then
                    call domain%get_element(elem_id, fe)
                    call domain%get_connectivity(elem_id, p_conn)

                    if (associated(fe) .and. associated(p_conn)) then
                        call fe%lerp(self%coordinate_normalized(iObs), nodal_temperature, p_conn, val)
                        obs_values(iObs) = val
                    end if
                end if
            end if
        end do
    end subroutine interpolate_observations_temperature

    subroutine get_observations_temperature(self, obs_values, domain, &
                                            nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(inout) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)

        integer(int32) :: iObs

        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return

        do iObs = 1, self%num_observations
            if (iObs <= size(self%node_ids)) then
                if (self%node_ids(iObs) > 0 .and. self%node_ids(iObs) <= size(nodal_temperature)) then
                    obs_values(iObs) = nodal_temperature(self%node_ids(iObs))
                end if
            end if
        end do
    end subroutine get_observations_temperature

    subroutine interpolate_observations_si(self, obs_values, domain, &
                                           nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(inout) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)

        integer(int32) :: iObs, elem_id
        integer(int32), pointer :: p_conn(:)
        class(abst_fe), pointer :: fe
        real(real64) :: val_T, val_phi

        obs_values(:) = 0.0d0
        if (.not. present(domain)) return
        if (.not. present(nodal_temperature) .or. .not. present(nodal_porosity)) return

        do iObs = 1, self%num_observations
            if (allocated(self%element_ids)) then
                elem_id = self%element_ids(iObs)
                if (elem_id > 0) then
                    call domain%get_element(elem_id, fe)
                    call domain%get_connectivity(elem_id, p_conn)
                    if (associated(fe) .and. associated(p_conn)) then
                        call fe%lerp(self%coordinate_normalized(iObs), nodal_temperature, p_conn, val_T)
                        call fe%lerp(self%coordinate_normalized(iObs), nodal_porosity, p_conn, val_phi)
                        obs_values(iObs) = 0.0d0
                    end if
                end if
            end if
        end do
    end subroutine interpolate_observations_si

    subroutine get_observations_si(self, obs_values, domain, &
                                   nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(inout) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)
        obs_values(:) = 0.0d0
    end subroutine get_observations_si

    subroutine interpolate_observations_thc(self, obs_values, domain, &
                                            nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(inout) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)
        obs_values(:) = 0.0d0
    end subroutine interpolate_observations_thc

    subroutine get_observations_thc(self, obs_values, domain, &
                                    nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(inout) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)
        obs_values(:) = 0.0d0
    end subroutine get_observations_thc

    subroutine interpolate_observations_vhc(self, obs_values, domain, &
                                            nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(inout) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)
        obs_values(:) = 0.0d0
    end subroutine interpolate_observations_vhc

    subroutine get_observations_vhc(self, obs_values, domain, &
                                    nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(inout) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)
        obs_values(:) = 0.0d0
    end subroutine get_observations_vhc

    subroutine interpolate_observations_pw(self, obs_values, domain, &
                                           nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(inout) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)

        integer(int32) :: iObs, elem_id
        integer(int32), pointer :: p_conn(:)
        class(abst_fe), pointer :: fe
        real(real64) :: val

        obs_values(:) = 0.0d0
        if (.not. present(domain)) return
        if (.not. present(nodal_pw)) return

        do iObs = 1, self%num_observations
            if (allocated(self%element_ids)) then
                elem_id = self%element_ids(iObs)
                if (elem_id > 0) then
                    call domain%get_element(elem_id, fe)
                    call domain%get_connectivity(elem_id, p_conn)
                    if (associated(fe) .and. associated(p_conn)) then
                        call fe%lerp(self%coordinate_normalized(iObs), nodal_pw, p_conn, val)
                        obs_values(iObs) = val
                    end if
                end if
            end if
        end do
    end subroutine interpolate_observations_pw

    subroutine get_observations_pw(self, obs_values, domain, &
                                   nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(inout) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)
        integer(int32) :: iObs
        obs_values(:) = 0.0d0
        if (.not. present(nodal_pw)) return
        do iObs = 1, self%num_observations
            if (iObs <= size(self%node_ids)) then
                if (self%node_ids(iObs) > 0 .and. self%node_ids(iObs) <= size(nodal_pw)) then
                    obs_values(iObs) = nodal_pw(self%node_ids(iObs))
                end if
            end if
        end do
    end subroutine get_observations_pw

end submodule inout_output_observation
