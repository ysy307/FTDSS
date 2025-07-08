module Conditions_Boundary_Manager
    use :: iso_fortran_env
    use :: Condition_Boundary
    use :: Inout_Input, only:Type_Input
    ! use json_fortran_module ! json-fortranなどのライブラリを想定
    implicit none
    private
    public :: BCManager

    !
    ! Holder: 各境界グループのBCを保持する
    !
    type :: BoundaryGroup
        integer :: id = 0
        ! 熱BCを保持するポリモーフィックなポインタ
        class(Abst_BC_Thermal), allocatable :: T
        ! 将来の拡張:
        ! class(BC_Hydraulic_Abstract), allocatable :: H
    end type BoundaryGroup

    !
    ! Manager: 全てのBCを管理する
    !
    type :: BCManager
        type(BoundaryGroup), allocatable :: groups(:)
    contains
        procedure :: setup => setup_BC_Manager
        procedure :: apply_CRS => apply_BC_Manager_CRS
        ! generic :: apply => apply_CRS
    end type BCManager

contains

    subroutine setup_BC_Manager(self, Input, Domain)
        class(BCManager), intent(inout) :: self
        type(Type_Input), intent(in) :: Input
        type(type_domain), intent(in) :: Domain

        ! class(json_value), intent(in) :: json_bc_obj ! "BoundaryConditions"に対応するJSONオブジェクト
        integer(int32), allocatable :: group_ids(:)
        integer(int32) :: i, n_groups, iGroup
        character(len=128) :: bc_type_str

        real(real64) :: time_conv

        allocate (group_ids, source=Input%Conditions%Groups)
        n_groups = size(group_ids)

        ! 2. 各グループのコンテナを確保
        if (allocated(self%groups)) deallocate (self%groups)
        allocate (self%groups(n_groups))

        select case (Input%Basic%Input_TimeUnit)
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
        do i = 1, n_groups
            iGroup = Input%Conditions%Groups(i)

            self%groups(i)%id = iGroup
            ! -- 熱BCの初期化 --
            select case (Input%Conditions%Heat(iGroup)%type)
            case ("Dirichlet")
                allocate (BC_Thermal_Dirichlet :: self%groups(i)%T)
                call self%groups(i)%T%setup(Input%Conditions, time_conv, iGroup, Domain)

            case ("Adiabatic")
                allocate (BC_Thermal_Adiabatic :: self%groups(i)%T)
                call self%groups(i)%T%setup(Input%Conditions, time_conv, iGroup, Domain)
            end select
        end do
    end subroutine setup_BC_Manager

    subroutine apply_BC_Manager_CRS(self, BC_Type, current_time, A, b, Domain, mode)
        class(BCManager), intent(inout) :: self
        character(1), intent(in) :: BC_Type
        real(real64), intent(in) :: current_time
        type(Type_CRS), intent(inout), optional :: A
        real(real64), intent(inout) :: b(:)
        type(type_domain), intent(in) :: Domain
        integer(int32), intent(in), optional :: mode

        integer(int32) :: iGroup

        ! --------------------------------------------------------------------------
        ! 1st Pass: Apply all non-Dirichlet boundary conditions (e.g., Neumann, Adiabatic)
        ! --------------------------------------------------------------------------
        select case (trim(adjustl(BC_Type)))
        case ('T')
            do iGroup = 1, size(self%groups)
                if (allocated(self%groups(iGroup)%T)) then
                    select type (bc => self%groups(iGroup)%T)
                    type is (BC_Thermal_Dirichlet)
                        ! This is a Dirichlet BC, so we skip it in the first pass.
                        cycle
                    class default
                        ! This is any other type of BC, apply it now.
                        if (present(A)) then
                            ! Apply the BC using CRS matrix format.
                            call bc%apply_CRS(current_time=current_time, &
                                              A=A, &
                                              b=b, &
                                              Domain=Domain, &
                                              mode=mode)
                        else
                            call bc%apply_CRS(current_time=current_time, &
                                              b=b, &
                                              Domain=Domain, &
                                              mode=mode)
                        end if
                    end select
                end if
            end do

            ! --------------------------------------------------------------------------
            ! 2nd Pass: Apply ONLY the Dirichlet boundary conditions
            ! --------------------------------------------------------------------------
            do iGroup = 1, size(self%groups)
                if (allocated(self%groups(iGroup)%T)) then
                    select type (bc => self%groups(iGroup)%T)
                    type is (BC_Thermal_Dirichlet)
                        ! This is a Dirichlet BC, apply it in the final pass.
                        if (present(A)) then
                            ! Apply the BC using CRS matrix format.
                            call bc%apply_CRS(current_time=current_time, &
                                              A=A, &
                                              b=b, &
                                              Domain=Domain, &
                                              mode=mode)
                        else
                            call bc%apply_CRS(current_time=current_time, &
                                              b=b, &
                                              Domain=Domain, &
                                              mode=mode)
                        end if
                    class default
                        ! All other types were handled in the first pass, so do nothing.
                        cycle
                    end select
                end if
            end do
        end select

    end subroutine apply_BC_Manager_CRS

end module Conditions_Boundary_Manager
