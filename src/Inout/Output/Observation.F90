submodule(Inout_Output) Inout_Output_Obaservation
    ! use :: stdlib_strings, only:to_string
    implicit none
contains
    module subroutine Write_Observation_Header(self, data_label, var_unit, num_unit, filename)
        use stdlib_strings, only: to_string
        implicit none
        class(Type_Output) :: self
        character(*), intent(in) :: data_label, var_unit, filename
        integer(int32), intent(inout) :: num_unit

        integer(int32) :: iObs, nObs

        nObs = self%Observation%NumObservation

        open (newunit=num_unit, file=trim(adjustl(filename)), status='replace', action='write')

        write (num_unit, '(a)') "# "//trim(data_label)//" time variation"
        write (num_unit, '(a)') "#"

        select case (self%Observation%ObservationType)
        case (1)
            write (num_unit, '(a)') "# Observation Node ID"
            do iObs = 1, nObs
                write (num_unit, '(a,i0,a,x,i0)') "# Node ID ", iObs, ":", self%Observation%ObsNodeID(iObs)
            end do
        case (2)
            write (num_unit, '(a)') "# Observation Coordinate (x,y,z)"
            do iObs = 1, nObs
                write (num_unit, '(a,x,i0,a,3(x,es18.11,a),2a)') &
                    "#    Point", iObs, ": (", &
                    self%Observation%Cood_Obs%x(iObs), ",", &
                    self%Observation%Cood_Obs%y(iObs), ",", &
                    self%Observation%Cood_Obs%z(iObs), ")", &
                    " => Element ID: ", &
                    self%Observation%Element(iObs)%e%ElementID
            end do
        end select

        write (num_unit, '(a)') "#"
        write (num_unit, '(a)') "# Output Unit: Time ["//trim(adjustl(self%Output_TimeUnit))//"], "//trim(data_label)//" ["//trim(var_unit)//"]"
        write (num_unit, '(a)') "#"
        write (num_unit, '(a,'//to_string(nObs)//'(2x,a))') "Time", ("Obs"//to_string(iObs), iObs=1, nObs)
    end subroutine Write_Observation_Header

end submodule Inout_Output_Obaservation
