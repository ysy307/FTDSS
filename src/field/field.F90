module module_field
    use :: field_jacobian_matrix
    use :: field_residual_vector
    use :: field_operations
    implicit none
    private

    public :: type_jacobian_matrix
    public :: type_residual_vector

    public :: ftdss_gemv
    public :: ftdss_sub
    public :: ftdss_dot
end module module_field
