submodule(io_output_observation) output_observation_base
    implicit none

contains

    module subroutine initialize_type_output_observation(self, dir_output, config)
        implicit none
        class(type_output_observation), intent(inout) :: self
        character(*), intent(in) :: dir_output
        type(type_config_observation), intent(in) :: config

        integer(int32) :: i
        integer(int32) :: iostat

        type(type_constant_id) :: file_format

        self%settings(:)%do_output = .false.
        self%observation_type = config%point_type
        file_format = config%file_format

        if (self%observation_type == OUTPUT_OBSERVATION_TYPES%NONE) then
            return
        end if

        self%num_observations = config%num_observations

        if (allocated(self%observation_points)) deallocate (self%observation_points)
        allocate (self%observation_points(self%num_observations))

        select case (self%observation_type%ID)
        case (OUTPUT_OBSERVATION_TYPES%NODE_IDS%ID)
            do i = 1, self%num_observations
                allocate (type_observation_point_node :: self%observation_points(i)%point)
                select type (p => self%observation_points(i)%point)
                type is (type_observation_point_node)
                    call p%initialize(config%observation_geometries(i))
                end select
            end do
        case (OUTPUT_OBSERVATION_TYPES%COORDINATES%ID)
            do i = 1, self%num_observations
                allocate (type_observation_point_coordinate :: self%observation_points(i)%point)
                select type (p => self%observation_points(i)%point)
                type is (type_observation_point_coordinate)
                    call p%initialize(config%observation_geometries(i))
                end select
            end do
        end select

        do i = 1, size(config%output_variables)
            associate (variable => config%output_variables(i))
                select case (variable%ID)
                case (OUTPUT_VARIABLE_TYPES%TEMPERATURE%ID)
                    self%settings(variable%ID)%do_output = .true.
                    self%settings(variable%ID)%variable_type = variable
                    self%settings(variable%ID)%variable_unit = "deg C"
                    self%settings(variable%ID)%file_name = strip(dir_output)//"obsf_T."//strip(file_format%NAME)
                    self%settings(variable%ID)%io_unit = open (strip(self%settings(variable%ID)%file_name), "wt", iostat=iostat)
                    if (iostat /= 0) call raise_error(ERROR_CODES%OPEN_FILE_FAILED, self%settings(variable%ID)%file_name)
                case (OUTPUT_VARIABLE_TYPES%WATER_CONTENT%ID)
                    self%settings(variable%ID)%do_output = .true.
                    self%settings(variable%ID)%variable_type = variable
                    self%settings(variable%ID)%variable_unit = "m3/m3"
                    self%settings(variable%ID)%file_name = strip(dir_output)//"obsf_Qw."//strip(file_format%NAME)
                    self%settings(variable%ID)%io_unit = open (strip(self%settings(variable%ID)%file_name), "wt", iostat=iostat)
                    if (iostat /= 0) call raise_error(ERROR_CODES%OPEN_FILE_FAILED, self%settings(variable%ID)%file_name)
                case (OUTPUT_VARIABLE_TYPES%ICE_CONTENT%ID)
                    self%settings(variable%ID)%do_output = .true.
                    self%settings(variable%ID)%variable_type = variable
                    self%settings(variable%ID)%variable_unit = "m3/m3"
                    self%settings(variable%ID)%file_name = strip(dir_output)//"obsf_Qi."//strip(file_format%NAME)
                    self%settings(variable%ID)%io_unit = open (strip(self%settings(variable%ID)%file_name), "wt", iostat=iostat)
                    if (iostat /= 0) call raise_error(ERROR_CODES%OPEN_FILE_FAILED, self%settings(variable%ID)%file_name)
                case (OUTPUT_VARIABLE_TYPES%VAPOR_CONTENT%ID)
                    self%settings(variable%ID)%do_output = .true.
                    self%settings(variable%ID)%variable_type = variable
                    self%settings(variable%ID)%variable_unit = "m3/m3"
                    self%settings(variable%ID)%file_name = strip(dir_output)//"obsf_Qv."//strip(file_format%NAME)
                    self%settings(variable%ID)%io_unit = open (strip(self%settings(variable%ID)%file_name), "wt", iostat=iostat)
                    if (iostat /= 0) call raise_error(ERROR_CODES%OPEN_FILE_FAILED, self%settings(variable%ID)%file_name)
                case (OUTPUT_VARIABLE_TYPES%THERMAL_CONDUCTIVITY%ID)
                    self%settings(variable%ID)%do_output = .true.
                    self%settings(variable%ID)%variable_type = variable
                    self%settings(variable%ID)%variable_unit = "W/m/K"
                    self%settings(variable%ID)%file_name = strip(dir_output)//"obsf_TC."//strip(file_format%NAME)
                    self%settings(variable%ID)%io_unit = open (strip(self%settings(variable%ID)%file_name), "wt", iostat=iostat)
                    if (iostat /= 0) call raise_error(ERROR_CODES%OPEN_FILE_FAILED, self%settings(variable%ID)%file_name)
                case (OUTPUT_VARIABLE_TYPES%VOLUMETRIC_HEAT_CAPACITY%ID)
                    self%settings(variable%ID)%do_output = .true.
                    self%settings(variable%ID)%variable_type = variable
                    self%settings(variable%ID)%variable_unit = "J/m3/K"
                    self%settings(variable%ID)%file_name = strip(dir_output)//"obsf_C."//strip(file_format%NAME)
                    self%settings(variable%ID)%io_unit = open (strip(self%settings(variable%ID)%file_name), "wt", iostat=iostat)
                    if (iostat /= 0) call raise_error(ERROR_CODES%OPEN_FILE_FAILED, self%settings(variable%ID)%file_name)
                case (OUTPUT_VARIABLE_TYPES%PRESSURE%ID)
                    self%settings(variable%ID)%do_output = .true.
                    self%settings(variable%ID)%variable_type = variable
                    self%settings(variable%ID)%variable_unit = "m"
                    self%settings(variable%ID)%file_name = strip(dir_output)//"obsf_P."//strip(file_format%NAME)
                    self%settings(variable%ID)%io_unit = open (strip(self%settings(variable%ID)%file_name), "wt", iostat=iostat)
                    if (iostat /= 0) call raise_error(ERROR_CODES%OPEN_FILE_FAILED, self%settings(variable%ID)%file_name)
                case (OUTPUT_VARIABLE_TYPES%WATER_FLUX%ID)
                    self%settings(variable%ID)%do_output = .true.
                    self%settings(variable%ID)%variable_type = variable
                    self%settings(variable%ID)%variable_unit = "m/s"
                    self%settings(variable%ID)%file_name = strip(dir_output)//"obsf_Flux."//strip(file_format%NAME)
                    self%settings(variable%ID)%io_unit = open (strip(self%settings(variable%ID)%file_name), "wt", iostat=iostat)
                    if (iostat /= 0) call raise_error(ERROR_CODES%OPEN_FILE_FAILED, self%settings(variable%ID)%file_name)
                case (OUTPUT_VARIABLE_TYPES%HYDRAULIC_CONDUCTIVITY%ID)
                    self%settings(variable%ID)%do_output = .true.
                    self%settings(variable%ID)%variable_type = variable
                    self%settings(variable%ID)%variable_unit = "m/s"
                    self%settings(variable%ID)%file_name = strip(dir_output)//"obsf_K."//strip(file_format%NAME)
                    self%settings(variable%ID)%io_unit = open (strip(self%settings(variable%ID)%file_name), "wt", iostat=iostat)
                    if (iostat /= 0) call raise_error(ERROR_CODES%OPEN_FILE_FAILED, self%settings(variable%ID)%file_name)
                end select
            end associate
        end do

        ! --- デリミタとフォーマット文字列の設定 ---
        select case (file_format%ID)
        case (FILE_FORMATS%CSV%ID)
            do i = 1, OUTPUT_VARIABLE_TYPES%NUM_ID
                if (self%settings(i)%do_output) then
                    self%settings(i)%delimiter = ","
                end if
            end do
        case (FILE_FORMATS%DAT%ID)
            do i = 1, OUTPUT_VARIABLE_TYPES%NUM_ID
                if (self%settings(i)%do_output) then
                    self%settings(i)%delimiter = "  "
                end if
            end do
        case default
            do i = 1, OUTPUT_VARIABLE_TYPES%NUM_ID
                if (self%settings(i)%do_output) then
                    self%settings(i)%delimiter = "  "
                end if
            end do
        end select

        do i = 1, OUTPUT_VARIABLE_TYPES%NUM_ID
            if (self%settings(i)%do_output) then
                ! [修正] フォーマット文字列を変数ごとに設定
                self%settings(i)%fmt_line = '(*(es22.15,:,"'//self%settings(i)%delimiter//'"))'
            end if
        end do

        do i = 1, OUTPUT_VARIABLE_TYPES%NUM_ID
            if (self%settings(i)%do_output) then
                call self%write_header(self%settings(i)%variable_type, config%output_time_unit_name)
            end if
        end do

    end subroutine initialize_type_output_observation

    module subroutine destroy_type_output_observation(self)
        implicit none
        class(type_output_observation), intent(inout) :: self

        integer(int32) :: i

        do i = 1, OUTPUT_VARIABLE_TYPES%NUM_ID
            if (self%settings(i)%do_output) then
                close (self%settings(i)%io_unit)
            end if
        end do

    end subroutine destroy_type_output_observation

    ! ==============================================================================
    ! Writer Subroutines
    ! ==============================================================================

    module subroutine write_observation_header(self, output_variable_type, output_time_unit_name)
        implicit none
        class(type_output_observation), intent(inout) :: self
        type(type_constant_id), intent(in) :: output_variable_type
        character(*), intent(in) :: output_time_unit_name

        integer(int32) :: i
        character(4096) :: header_line

        associate (output_settings => self%settings(output_variable_type%ID))
            if (.not. output_settings%do_output) return
            if (.not. check_unit_writable(output_settings%io_unit)) then
                call raise_error(ERROR_CODES%WRITE_FILE_FAILED, output_settings%file_name)
            end if

            write (output_settings%io_unit, '(a)') "# "//strip(output_settings%variable_type%NAME)//" time variation"
            write (output_settings%io_unit, '(a)') "#"

            ! --- 観測点の情報出力（ポリモーフィズムを利用） ---
            select case (self%observation_type%ID)
            case (OUTPUT_OBSERVATION_TYPES%NODE_IDS%ID)
                write (output_settings%io_unit, '(a)') "# Observation Node ID"
                do i = 1, self%num_observations
                    select type (p => self%observation_points(i)%point)
                    type is (type_observation_point_node)
                        write (output_settings%io_unit, '(a,i0,a,1x,i0)') "# Node ID ", i, ":", p%node_id
                    end select
                end do

            case (OUTPUT_OBSERVATION_TYPES%COORDINATES%ID)
                write (output_settings%io_unit, '(a)') "# Observation Coordinate (x,y,z)"
                do i = 1, self%num_observations
                    select type (p => self%observation_points(i)%point)
                    type is (type_observation_point_coordinate)
                        write (output_settings%io_unit, '(a,1x,i0,a,3(1x,es18.11,a),a,i0)') &
                            "#    Point", i, ": (", &
                            p%coordinate%x, ",", &
                            p%coordinate%y, ",", &
                            p%coordinate%z, ")", &
                            " => Element ID: ", p%fe_id
                    end select
                end do
            end select

            write (output_settings%io_unit, '(a)') "#"
            write (output_settings%io_unit, '(a)') "# Output Unit: Time ["//strip(output_time_unit_name)//"], " &
                //strip(output_settings%variable_type%NAME)//" ["//strip(output_settings%variable_unit)//"]"
            write (output_settings%io_unit, '(a)') "#"

            ! Build the header line explicitly so the format always matches the item count.
            header_line = "Time"
            select case (output_settings%variable_type%ID)
            case (OUTPUT_VARIABLE_TYPES%WATER_FLUX%ID)
                do i = 1, self%num_observations / 3
                    header_line = trim(header_line)//output_settings%delimiter//"Obs"//to_string(i)//"_x" &
                                  //output_settings%delimiter//"Obs"//to_string(i)//"_y" &
                                  //output_settings%delimiter//"Obs"//to_string(i)//"_z"
                end do
            case default
                do i = 1, self%num_observations
                    header_line = trim(header_line)//output_settings%delimiter//"Obs"//to_string(i)
                end do
            end select
            write (output_settings%io_unit, '(a)') trim(header_line)

        end associate

    end subroutine write_observation_header

    module subroutine write_observation_line(self, output_variable_type, time, output_values)
        implicit none
        class(type_output_observation), intent(in) :: self
        type(type_constant_id), intent(in) :: output_variable_type
        real(real64), intent(in) :: time
        real(real64), intent(in) :: output_values(:)

        associate (output_settings => self%settings(output_variable_type%ID))
            if (.not. output_settings%do_output) return
            if (.not. check_unit_writable(output_settings%io_unit)) then
                call raise_error(ERROR_CODES%WRITE_FILE_FAILED, output_settings%file_name)
            end if

            write (output_settings%io_unit, output_settings%fmt_line) time, output_values(1:self%num_observations)
        end associate
    end subroutine write_observation_line

    module subroutine output_history_output_observation(self, time, temperature, water_content, ice_content, &
                                                        vapor_content, pressure)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(in) :: time
        real(real64), intent(in), optional :: temperature(:)
        real(real64), intent(in), optional :: water_content(:)
        real(real64), intent(in), optional :: ice_content(:)
        real(real64), intent(in), optional :: vapor_content(:)
        real(real64), intent(in), optional :: pressure(:)

        real(real64), allocatable :: obs_values(:)
        integer(int32) :: i

        if (self%num_observations <= 0) return

        allocate (obs_values(self%num_observations))

        ! Process temperature
        if (present(temperature)) then
            if (self%settings(OUTPUT_VARIABLE_TYPES%TEMPERATURE%ID)%do_output) then
                do i = 1, self%num_observations
                    call self%observation_points(i)%extract_value(temperature, obs_values(i))
                end do
                call self%write_line(OUTPUT_VARIABLE_TYPES%TEMPERATURE, time, obs_values)
            end if
        end if

        ! Process pressure
        if (present(pressure)) then
            if (self%settings(OUTPUT_VARIABLE_TYPES%PRESSURE%ID)%do_output) then
                do i = 1, self%num_observations
                    call self%observation_points(i)%extract_value(pressure, obs_values(i))
                end do
                call self%write_line(OUTPUT_VARIABLE_TYPES%PRESSURE, time, obs_values)
            end if
        end if

        ! Process water content
        if (present(water_content)) then
            if (self%settings(OUTPUT_VARIABLE_TYPES%WATER_CONTENT%ID)%do_output) then
                do i = 1, self%num_observations
                    call self%observation_points(i)%extract_value(water_content, obs_values(i))
                end do
                call self%write_line(OUTPUT_VARIABLE_TYPES%WATER_CONTENT, time, obs_values)
            end if
        end if

        ! Process ice content
        if (present(ice_content)) then
            if (self%settings(OUTPUT_VARIABLE_TYPES%ICE_CONTENT%ID)%do_output) then
                do i = 1, self%num_observations
                    call self%observation_points(i)%extract_value(ice_content, obs_values(i))
                end do
                call self%write_line(OUTPUT_VARIABLE_TYPES%ICE_CONTENT, time, obs_values)
            end if
        end if

        ! Process vapor content
        if (present(vapor_content)) then
            if (self%settings(OUTPUT_VARIABLE_TYPES%VAPOR_CONTENT%ID)%do_output) then
                do i = 1, self%num_observations
                    call self%observation_points(i)%extract_value(vapor_content, obs_values(i))
                end do
                call self%write_line(OUTPUT_VARIABLE_TYPES%VAPOR_CONTENT, time, obs_values)
            end if
        end if



        deallocate (obs_values)
    end subroutine output_history_output_observation

end submodule output_observation_base
