module module_input
    use :: io_input, only: &
        type_input
    use :: io_input_translator, only: &
        input_translator
    implicit none
    private

    public :: type_input
    public :: input_translator

end module module_input
