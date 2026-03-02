submodule(io_output_observation) output_observation_base
    implicit none

contains

    module subroutine initialize_type_output_observation(self, input, domain, dir_output, variable_type)
        implicit none
        class(type_output_observation), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_domain), intent(inout) :: domain
        character(*), intent(in) :: dir_output
        type(type_constant_id), intent(in) :: variable_type

        !     integer(int32) :: iObs, iElem, num_elements
        !     integer(int32) :: elem_id, comp_dim, calc_type
        !     type(type_coordinate_dp) :: cartesian, normalized
        !     logical :: inside

        !     class(abst_fe), pointer :: fe
        !     integer(int32), pointer, contiguous :: conn(:)
        !     real(real64), allocatable :: ele_coords(:, :)

        type(type_constant_value) :: file_format

        !     ! --- 設定の取得 ---
        !     self%variable_type = input%output_settings%history_output%observation_type

        !     if (self%variable_type == OUTPUT_OBSERVATION_TYPES%NONE) then
        !         self%do_output = .false.
        !         return
        !     else
        !         self%do_output = .true.
        !     end if
        !     ! --- デリミタとフォーマット文字列の設定 ---
        !     select case (file_format%ID)
        !     case (FILE_FORMATS%CSV%ID)
        !         self%delimiter = ","
        !     case (FILE_FORMATS%DAT%ID)
        !         self%delimiter = "  "
        !     case default
        !         self%delimiter = "  "
        !     end select

        !     self%fmt_line = '(*(es22.15,:,"'//self%delimiter//'"))'

        !     self%self%num_observations = input%output_settings%history_output%self%num_observations

        !     select case (trim(self%type))
        !     case ("node_ids")
        !         if (allocated(self%node_ids)) deallocate (self%node_ids)
        !         allocate (self%node_ids, source=input%output_settings%history_output%node_ids)

        !     case ("coordinates")
        !         call self%coordinate%initialize(self%self%num_observations)
        !         do iObs = 1, self%self%num_observations
        !             self%coordinate%x(iObs) = input%output_settings%history_output%coordinates(iObs)%x
        !             self%coordinate%y(iObs) = input%output_settings%history_output%coordinates(iObs)%y
        !             self%coordinate%z(iObs) = input%output_settings%history_output%coordinates(iObs)%z
        !         end do

        !         ! 要素ID保存用配列の確保
        !         if (allocated(self%element_ids)) deallocate (self%element_ids)
        !         allocate (self%element_ids(self%self%num_observations))
        !         self%element_ids = -1

        !         if (allocated(self%coordinate_normalized)) deallocate (self%coordinate_normalized)
        !         allocate (self%coordinate_normalized(self%self%num_observations))

        !         ! --- 座標探索ロジック ---
        !         call domain%get_num_fe(num_elements)
        !         call domain%get_computation_dimension(comp_dim)
        !         calc_type = input%basic%simulation_settings%calculate_type

        !         do iObs = 1, self%self%num_observations
        !             ! 観測点座標セット (探索点)
        !             if (comp_dim == 2) then
        !                 if (calc_type == 1) then ! XY (2D)
        !                     call cartesian%set(self%coordinate%x(iObs), self%coordinate%y(iObs), 0.0d0)
        !                 else ! XZ (2D)
        !                     ! XZ平面の場合、Y成分にZ座標を入れて2D探索を行う (domain側の格納形式に合わせる)
        !                     call cartesian%set(self%coordinate%x(iObs), self%coordinate%z(iObs), 0.0d0)
        !                 end if
        !             else ! 3D
        !                 call cartesian%set(self%coordinate%x(iObs), self%coordinate%y(iObs), self%coordinate%z(iObs))
        !             end if

        !             do iElem = 1, num_elements
        !                 call domain%get_fe(iElem, fe)
        !                 if (.not. associated(fe)) cycle
        !                 call domain%get_fe_connectivity(iElem, conn)
        !                 if (.not. associated(conn)) cycle
        !                 call domain%get_fe_coordinate(iElem, ele_coords)

        !                 ! 包含判定
        !                 call fe%is_inside(cartesian, normalized, ele_coords, inside)

        !                 if (inside) then
        !                     self%element_ids(iObs) = iElem
        !                     self%coordinate_normalized(iObs) = normalized
        !                     exit
        !                 end if
        !             end do

        !             ! デバッグ用: 見つからない場合の警告
        !             if (self%element_ids(iObs) == -1) then
        !                 print *, "Warning: Observation point ", iObs, " is outside the domain."
        !             end if
        !         end do
        !     end select

        !     if (associated(self%get_values)) nullify (self%get_values)

        select case (self%variable_type%ID)
        case (OUTPUT_VARIABLE_TYPES%TEMPERATURE%ID)
            self%variable_unit = "deg C"
            self%file_name = strip(dir_output)//"obsf_T."//strip(file_format%NAME)
            self%io_unit = 99999999
        case (OUTPUT_VARIABLE_TYPES%WATER_CONTENT%ID)
            self%variable_unit = "-"
            self%file_name = strip(dir_output)//"obsf_Si."//strip(file_format%NAME)
            self%io_unit = 99999999
        case (OUTPUT_VARIABLE_TYPES%THERMAL_CONDUCTIVITY%ID)
            self%variable_unit = "W/m/K"
            self%file_name = strip(dir_output)//"obsf_TC."//strip(file_format%NAME)
            self%io_unit = 99999999
        case (OUTPUT_VARIABLE_TYPES%VOLUMETRIC_HEAT_CAPACITY%ID)
            self%variable_unit = "J/m3/K"
            self%file_name = strip(dir_output)//"obsf_C."//strip(file_format%NAME)
            self%io_unit = 99999999
        case (OUTPUT_VARIABLE_TYPES%PRESSURE%ID)
            self%variable_unit = "m"
            self%file_name = strip(dir_output)//"obsf_P."//strip(file_format%NAME)
            self%io_unit = 99999999
        case (OUTPUT_VARIABLE_TYPES%WATER_FLUX%ID)
            self%variable_unit = "m/s"
            self%file_name = strip(dir_output)//"obsf_Flux."//strip(file_format%NAME)
            self%io_unit = 99999999
            self%num_observations = self%num_observations * 3
        case (OUTPUT_VARIABLE_TYPES%HYDRAULIC_CONDUCTIVITY%ID)
            self%variable_unit = "m/s"
            self%file_name = strip(dir_output)//"obsf_K."//strip(file_format%NAME)
            self%io_unit = 99999999
        end select

    end subroutine initialize_type_output_observation

    ! ==============================================================================
    ! Writer Subroutines
    ! ==============================================================================

    module subroutine write_observation_header(self, output_time_unit)
        implicit none
        class(type_output_observation), intent(inout) :: self
        type(type_constant_id), intent(in) :: output_time_unit

        integer(int32) :: i

        if (.not. self%do_output) return

        open (newunit=self%io_unit, file=strip(self%file_name), status='replace', action='write')

        write (self%io_unit, '(a)') "# "//trim(self%variable_type%NAME)//" time variation"
        write (self%io_unit, '(a)') "#"

        ! --- 観測点の情報出力（ポリモーフィズムを利用） ---
        select case (self%observation_type%ID)
        case (OUTPUT_OBSERVATION_TYPES%NODE_IDS%ID)
            write (self%io_unit, '(a)') "# Observation Node ID"
            do i = 1, self%num_observations
                select type (p => self%observation_points(i)%point)
                type is (type_observation_point_node)
                    write (self%io_unit, '(a,i0,a,1x,i0)') "# Node ID ", i, ":", p%node_id
                end select
            end do

        case (OUTPUT_OBSERVATION_TYPES%COORDINATES%ID)
            write (self%io_unit, '(a)') "# Observation Coordinate (x,y,z)"
            do i = 1, self%num_observations
                select type (p => self%observation_points(i)%point)
                type is (type_observation_point_coordinate)
                    write (self%io_unit, '(a,1x,i0,a,3(1x,es18.11,a),a,i0)') &
                        "#    Point", i, ": (", &
                        p%coordinate%x, ",", &
                        p%coordinate%y, ",", &
                        p%coordinate%z, ")", &
                        " => Element ID: ", p%element_id
                end select
            end do
        end select

        write (self%io_unit, '(a)') "#"
        write (self%io_unit, '(a)') "# Output Unit: Time ["//trim(output_time_unit%NAME)//"], " &
            //trim(self%variable_type%NAME)//" ["//trim(self%variable_unit)//"]"
        write (self%io_unit, '(a)') "#"

        ! --- ヘッダー行の出力 ---
        select case (self%variable_type%ID)
        case (OUTPUT_VARIABLE_TYPES%WATER_FLUX%ID)
            write (self%io_unit, '(a,'//to_string(self%num_observations)//'("'//self%delimiter//'",a))') &
                "Time", (("Obs"//to_string(i)//"_x", "Obs"//to_string(i)//"_y", "Obs"//to_string(i)//"_z"), &
                         i=1, self%num_observations / 3)
        case default
            write (self%io_unit, '(a,'//to_string(self%num_observations)//'("'//self%delimiter//'",a))') &
                "Time", ("Obs"//to_string(i), i=1, self%num_observations)
        end select

    end subroutine write_observation_header

    module subroutine write_observation_line(self, time, values)
        implicit none
        class(type_output_observation), intent(in) :: self
        real(real64), intent(in) :: time
        real(real64), intent(in) :: values(:)

        write (self%io_unit, self%fmt_line) time, values(1:self%num_observations)
    end subroutine write_observation_line

    module pure function should_output_overall(self) result(should_output)
        implicit none
        class(type_output_observation), intent(in) :: self
        logical :: should_output

        should_output = self%do_output

    end function should_output_overall

    module subroutine extract_value_coordinate(self, nodal_values, value)
        implicit none
        class(type_observation_point_coordinate), intent(in) :: self
        real(real64), intent(in) :: nodal_values(:)
        real(real64), intent(inout) :: value

        value = 0.0d0

        if (associated(self%fe) .and. allocated(self%connectivity)) then
            call self%fe%lerp(self%coordinate_normalized, nodal_values(self%connectivity), value)
        end if

    end subroutine extract_value_coordinate

    module subroutine extract_value_node(self, nodal_values, value)
        implicit none
        class(type_observation_point_node), intent(in) :: self
        real(real64), intent(in) :: nodal_values(:)
        real(real64), intent(inout) :: value

        if (value_in_range(self%node_id, 1, size(nodal_values))) then
            value = nodal_values(self%node_id)
        else
            value = 0.0d0
        end if
    end subroutine extract_value_node

end submodule output_observation_base
