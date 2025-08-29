module module_linalg
    use :: linalg_vector_ops, only:norm_1, norm_2, norm_inf, dot
    implicit none
    private

    public :: norm_1
    public :: norm_2
    public :: norm_inf
    public :: dot

end module module_linalg
