!> IDR(s) solver — Induced Dimension Reduction.
!> Reference: Sonneveld & van Gijzen (2008).
!> Default s=4.
submodule(numerical_solver_interface) impl_solve_type_solver_idr_s
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
contains

    module subroutine initialize_type_solver_idr_s(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_idr_s), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: ierr, j

        self%ID = solver_settings%ID
        self%name = "IDR(s)"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations
        self%s_param = 4

        call self%r%initialize(self%num_nodes)
        call self%v%initialize(self%num_nodes)
        call self%t%initialize(self%num_nodes)
        call self%z%initialize(self%num_nodes)

        if (allocated(self%P)) deallocate (self%P)
        allocate (self%P(self%s_param))
        do j = 1, self%s_param
            call self%P(j)%initialize(self%num_nodes)
        end do

        if (allocated(self%G)) deallocate (self%G)
        allocate (self%G(self%s_param))
        do j = 1, self%s_param
            call self%G(j)%initialize(self%num_nodes)
        end do

        if (allocated(self%U)) deallocate (self%U)
        allocate (self%U(self%s_param))
        do j = 1, self%s_param
            call self%U(j)%initialize(self%num_nodes)
        end do

        call allocate_array(self%M_idr, self%s_param, self%s_param)
        call allocate_array(self%f, self%s_param)
        call allocate_array(self%c, self%s_param)

        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        self%status = SOLVER_STATUS%SUCCESS%ID
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_idr_s

    module subroutine solve_type_solver_idr_s(self, A, b, x)
        implicit none
        class(type_solver_idr_s), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: omega, resid, resid0, denom, nr, nt
        integer(int32) :: iter, j, k, s, ierr
        real(real64), pointer :: r_data(:)

        s = self%s_param

        call self%residual_history%zero()
        call self%pc%setup(A)

        ! r = b - Ax
        call matvec(A, x, self%r, ierr)
        call vector_axpyz(-1.0d0, self%r, b, self%r)

        resid0 = vector_norm2(self%r)
        call self%residual_history%set(MATRIX_OPS%INS, 1, resid0)
        if (resid0 < self%tolerance) then
            self%current_iteration = 0
            self%status = SOLVER_STATUS%SUCCESS%ID
            return
        end if

        ! Initialize P with random shadow space (use r and shifted copies)
        call self%P(1)%copy(self%r)
        r_data => self%r%get_data()
        do j = 2, s
            call self%P(j)%copy(self%r)
            ! Shift to create linearly independent vectors
            call vector_scale(1.0d0 / real(j, real64), self%P(j))
            ! Small perturbation for independence
            r_data => self%P(j)%get_data()
            if (associated(r_data) .and. size(r_data) >= j) then
                r_data(j) = r_data(j) + 1.0d0
            end if
        end do

        ! Initialize G and U
        do j = 1, s
            call self%G(j)%copy(self%r)
            call self%U(j)%zero()
        end do

        omega = 1.0d0

        ! Build initial M = P^T G
        do k = 1, s
            do j = 1, s
                self%M_idr(j, k) = vector_dot(self%P(j), self%G(k))
            end do
        end do

        iter = 0

        outer: do while (iter < self%max_iterations)

            ! f = P^T r
            do j = 1, s
                self%f(j) = vector_dot(self%P(j), self%r)
            end do

            ! Inner loop
            do k = 1, s
                iter = iter + 1
                if (iter > self%max_iterations) exit outer
                self%current_iteration = iter

                ! Solve M*c = f (simple Gaussian elimination for small s)
                call solve_small_system(s, self%M_idr, self%f, self%c)

                ! v = r - sum c_j * G_j
                call self%v%copy(self%r)
                do j = 1, s
                    call vector_axpy(-self%c(j), self%G(j), self%v)
                end do

                ! Precondition
                call self%pc%apply(self%v, self%z)

                ! t = sum c_j * U_j + omega * z
                call self%t%zero()
                do j = 1, s
                    call vector_axpy(self%c(j), self%U(j), self%t)
                end do
                call vector_axpy(omega, self%z, self%t)

                ! U_k = t
                call self%U(k)%copy(self%t)

                ! G_k = A * U_k
                call matvec(A, self%U(k), self%G(k), ierr)

                ! Update M column k
                do j = 1, s
                    self%M_idr(j, k) = vector_dot(self%P(j), self%G(k))
                end do

                ! alpha = f(k) / M(k,k)
                if (abs(self%M_idr(k, k)) <= tiny(1.0d0)) then
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    return
                end if

                denom = self%f(k) / self%M_idr(k, k)

                ! x = x + denom * U_k
                call vector_axpy(denom, self%U(k), x)

                ! r = r - denom * G_k
                call vector_axpy(-denom, self%G(k), self%r)

                resid = vector_norm2(self%r)
                call self%residual_history%set(MATRIX_OPS%INS, iter, resid)

                if (resid < self%tolerance .or. &
                    (resid0 > self%tolerance .and. resid < self%relative_tolerance * resid0)) then
                    self%status = SOLVER_STATUS%SUCCESS%ID
                    return
                end if

                ! Update f
                do j = 1, s
                    self%f(j) = self%f(j) - denom * self%M_idr(j, k)
                end do
            end do

            ! Intermediate step: preconditioned residual
            call self%pc%apply(self%r, self%z)
            call matvec(A, self%z, self%v, ierr)

            ! omega = (v, r) / (v, v)
            nt = vector_dot(self%v, self%v)
            nr = vector_dot(self%v, self%r)
            if (abs(nt) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            omega = nr / nt

            ! x = x + omega * z
            call vector_axpy(omega, self%z, x)

            ! r = r - omega * v
            call vector_axpy(-omega, self%v, self%r)

            iter = iter + 1
            if (iter > self%max_iterations) exit outer
            self%current_iteration = iter

            resid = vector_norm2(self%r)
            call self%residual_history%set(MATRIX_OPS%INS, iter, resid)

            if (resid < self%tolerance .or. &
                (resid0 > self%tolerance .and. resid < self%relative_tolerance * resid0)) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                return
            end if

            if (was_interrupted()) stop
        end do outer

        self%status = SOLVER_STATUS%MAXITER%ID

    end subroutine solve_type_solver_idr_s

    !> Solve small dense system Mc = f using partial pivoting.
    pure subroutine solve_small_system(n, M, f, c)
        implicit none
        integer(int32), intent(in) :: n
        real(real64), intent(in) :: M(n, n)
        real(real64), intent(in) :: f(n)
        real(real64), intent(inout) :: c(n)

        real(real64) :: A_loc(n, n), b_loc(n), pivot, factor
        integer(int32) :: i, j, k, max_idx

        A_loc = M
        b_loc = f

        ! Forward elimination with partial pivoting
        do k = 1, n - 1
            max_idx = k
            do i = k + 1, n
                if (abs(A_loc(i, k)) > abs(A_loc(max_idx, k))) max_idx = i
            end do
            if (max_idx /= k) then
                A_loc(k, :) = A_loc(k, :) + A_loc(max_idx, :)
                A_loc(max_idx, :) = A_loc(k, :) - A_loc(max_idx, :)
                A_loc(k, :) = A_loc(k, :) - A_loc(max_idx, :)
                pivot = b_loc(k); b_loc(k) = b_loc(max_idx); b_loc(max_idx) = pivot
            end if
            if (abs(A_loc(k, k)) <= tiny(1.0d0)) cycle
            do i = k + 1, n
                factor = A_loc(i, k) / A_loc(k, k)
                A_loc(i, k + 1:n) = A_loc(i, k + 1:n) - factor * A_loc(k, k + 1:n)
                b_loc(i) = b_loc(i) - factor * b_loc(k)
            end do
        end do

        ! Back substitution
        c = 0.0d0
        do i = n, 1, -1
            if (abs(A_loc(i, i)) > tiny(1.0d0)) then
                c(i) = b_loc(i)
                do j = i + 1, n
                    c(i) = c(i) - A_loc(i, j) * c(j)
                end do
                c(i) = c(i) / A_loc(i, i)
            end if
        end do

    end subroutine solve_small_system

    module subroutine destroy_type_solver_idr_s(self)
        implicit none
        class(type_solver_idr_s), intent(inout) :: self
        integer(int32) :: j

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        call self%r%destroy()
        call self%v%destroy()
        call self%t%destroy()
        call self%z%destroy()
        if (allocated(self%P)) then
            do j = 1, size(self%P)
                call self%P(j)%destroy()
            end do
            deallocate (self%P)
        end if
        if (allocated(self%G)) then
            do j = 1, size(self%G)
                call self%G(j)%destroy()
            end do
            deallocate (self%G)
        end if
        if (allocated(self%U)) then
            do j = 1, size(self%U)
                call self%U(j)%destroy()
            end do
            deallocate (self%U)
        end if
        call deallocate_array(self%M_idr)
        call deallocate_array(self%f)
        call deallocate_array(self%c)
        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_idr_s

end submodule impl_solve_type_solver_idr_s
