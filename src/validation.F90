module core_validation
    use :: core_error, only: &
        raise_error
    use :: core_check_nan, only: &
        has_nan
    use :: core_check_range, only: &
        value_in_range
    use :: core_check_length, only: &
        check_match_length
    implicit none
    public

end module core_validation
