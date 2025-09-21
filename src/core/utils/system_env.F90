! =================================================================
! 低レイヤー: 環境変数をOSから読み込むだけの汎用モジュール
! =================================================================
module core_system_env
    use :: mpi_f08
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private
    public :: get_env_string

contains

    !> 指定された環境変数の値を文字列として取得する (MPI対応)
    !> ランク0が代表して読み込み、全ランクにブロードキャストする。
    subroutine get_env_string(env_var_name, value)
        implicit none
        character(len=*), intent(in) :: env_var_name
        character(len=:), allocatable, intent(inout) :: value

        character(len=2048) :: buffer
        integer(int32) :: nulpos
        integer(int32) :: status
        integer(int32) :: my_rank, ierr

        call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
        if (my_rank == 0) then
            call get_environment_variable(env_var_name, buffer, status=status)
            if (status /= 0) buffer = ''
        end if
        call MPI_Bcast(buffer, len(buffer), MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)

        if (len_trim(buffer) > 0) then
            nulpos = scan(buffer, achar(0))
            if (nulpos > 0) then
                allocate (character(len=nulpos - 1) :: value)
                value = buffer(1:nulpos - 1)
            else
                allocate (character(len=len_trim(buffer)) :: value)
                value = buffer(1:len_trim(buffer))
            end if
        else
            value = ''
        end if

    end subroutine get_env_string

end module core_system_env
