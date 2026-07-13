!> ETKF (Ensemble Transform Kalman Filter) solver.
!>
!> Algorithm summary:
!> \[ \mathbf{C} = \mathbf{A}_Y^\top \mathbf{R}^{-1} \mathbf{A}_Y \]
!> \[ \mathbf{T} = \left(\mathbf{I} + \frac{\mathbf{C}}{N-1}\right)^{-1/2} \quad \text{(via DSYEV)} \]
!> \[ \bar{\mathbf{x}}^a = \bar{\mathbf{x}}^f + \mathbf{A}_f \mathbf{w}^a, \quad
!>    \mathbf{w}^a = \mathbf{T}^2 \mathbf{A}_Y^\top \mathbf{R}^{-1} \mathbf{d} \,/\,(N-1) \]
!> \[ \mathbf{X}^a = \bar{\mathbf{x}}^a \mathbf{1}^\top + \mathbf{A}_f \mathbf{T} \]
module physics_governing_atmosphere_etkf
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: physics_governing_atmosphere_ensemble, only: type_ensemble_manager, type_atmos_state
    use :: physics_governing_atmosphere_observation, only: type_observation_manager
    implicit none
    private

    public :: type_etkf_solver

    type :: type_etkf_solver
        integer(int32) :: n_members = 0
        integer(int32) :: n_obs     = 3
    contains
        procedure, public, pass(self) :: initialize        => initialize_etkf_solver
        procedure, public, pass(self) :: calculate_analysis => calculate_analysis_etkf
    end type type_etkf_solver

contains

    subroutine initialize_etkf_solver(self, n_members)
        implicit none
        class(type_etkf_solver), intent(inout) :: self
        integer(int32), intent(in) :: n_members

        self%n_members = n_members
    end subroutine initialize_etkf_solver

    !> Performs ETKF analysis and updates ensemble members in-place.
    !> @param[in] y        observation vector (n_obs)
    !> @param[in] R_diag_in diagonal of observation error covariance (n_obs)
    subroutine calculate_analysis_etkf(self, ensemble, obs_manager, y_in, R_diag_in)
        implicit none
        class(type_etkf_solver), intent(inout) :: self
        type(type_ensemble_manager), intent(inout) :: ensemble
        type(type_observation_manager), intent(inout) :: obs_manager
        real(real64), intent(in) :: y_in(:)
        real(real64), intent(in) :: R_diag_in(:)

        external :: dsyev, dgemm, dgemv

        integer(int32) :: n_ens, n_state, n_obs_loc, k, i
        ! Ensemble matrices
        real(real64), allocatable :: Af(:, :)          ! n_state x n_ens anomaly
        real(real64), allocatable :: xf_bar(:)         ! n_state
        real(real64), allocatable :: xa_bar(:)         ! n_state
        real(real64), allocatable :: Af_Y(:, :)        ! n_obs x n_ens
        real(real64), allocatable :: Af_Y_Rinv(:, :)   ! n_obs x n_ens  (R^{-1} * Af_Y)
        real(real64), allocatable :: C_mat(:, :)       ! n_ens x n_ens
        real(real64), allocatable :: A_eig(:, :)       ! n_ens x n_ens  (eigvec workspace)
        real(real64), allocatable :: T_mat(:, :)       ! n_ens x n_ens  transform
        real(real64), allocatable :: T2_mat(:, :)      ! n_ens x n_ens  = T * T
        real(real64), allocatable :: T_col_scaled(:, :)! n_ens x n_ens  intermediate
        real(real64), allocatable :: Xa_anom(:, :)     ! n_state x n_ens
        real(real64), allocatable :: H_mat(:, :)       ! n_obs x n_state
        real(real64), allocatable :: eigval(:)
        real(real64), allocatable :: work(:)
        real(real64), allocatable :: y(:), R_diag(:)
        real(real64), allocatable :: d(:)
        real(real64), allocatable :: wa(:)
        real(real64), allocatable :: tmp_obs(:), tmp_nens(:), tmp_state(:)
        type(type_atmos_state) :: mean_state
        integer(int32) :: info, lwork

        n_ens   = ensemble%n_members
        n_state = 3 * ensemble%n_nodes
        n_obs_loc = size(y_in)

        allocate (y(n_obs_loc), R_diag(n_obs_loc))
        y      = y_in
        R_diag = R_diag_in

        ! Ensemble mean
        call ensemble%compute_mean(mean_state)
        allocate (xf_bar(n_state))
        xf_bar(1:ensemble%n_nodes)                         = mean_state%T
        xf_bar(ensemble%n_nodes + 1:2*ensemble%n_nodes)    = mean_state%q
        xf_bar(2*ensemble%n_nodes + 1:n_state)             = mean_state%U

        ! Anomaly matrix Af (n_state x n_ens)
        call ensemble%compute_anomalies(mean_state, Af)

        ! Observation operator H (n_obs x n_state)
        call obs_manager%build_H(mean_state, H_mat)

        ! Af_Y = H * Af  (n_obs x n_ens)
        allocate (Af_Y(n_obs_loc, n_ens))
        call dgemm('N', 'N', n_obs_loc, n_ens, n_state, 1.0d0, H_mat, n_obs_loc, Af, n_state, 0.0d0, Af_Y, n_obs_loc)

        ! Af_Y_Rinv = diag(1/R) * Af_Y  (n_obs x n_ens)
        allocate (Af_Y_Rinv(n_obs_loc, n_ens))
        do i = 1, n_obs_loc
            if (R_diag(i) > 0.0d0) then
                Af_Y_Rinv(i, :) = Af_Y(i, :) / R_diag(i)
            else
                Af_Y_Rinv(i, :) = 0.0d0
            end if
        end do

        ! C = Af_Y^T * R^{-1} * Af_Y  (n_ens x n_ens)
        allocate (C_mat(n_ens, n_ens))
        call dgemm('T', 'N', n_ens, n_ens, n_obs_loc, 1.0d0, Af_Y, n_obs_loc, Af_Y_Rinv, n_obs_loc, 0.0d0, C_mat, n_ens)

        ! Form (I + C/(n_ens-1)) for eigendecomposition
        allocate (A_eig(n_ens, n_ens))
        A_eig = C_mat / real(n_ens - 1, real64)
        do k = 1, n_ens
            A_eig(k, k) = A_eig(k, k) + 1.0d0
        end do

        ! DSYEV: A_eig = Q * diag(eigval) * Q^T
        allocate (eigval(n_ens))
        lwork = max(1, 3*n_ens + 64)
        allocate (work(lwork))
        call dsyev('V', 'U', n_ens, A_eig, n_ens, eigval, work, lwork, info)
        deallocate (work)
        if (info /= 0) then
            write (*, '(A,I0)') '[DA] DSYEV failed, info=', info
            call clean_up()
            return
        end if
        ! A_eig now holds eigenvectors Q as columns

        ! T_col_scaled(:,k) = Q(:,k) / sqrt(eigval(k))
        allocate (T_col_scaled(n_ens, n_ens))
        do k = 1, n_ens
            T_col_scaled(:, k) = A_eig(:, k) / sqrt(max(eigval(k), 1.0d-14))
        end do

        ! T = T_col_scaled * Q^T  (= (I + C/(n_ens-1))^{-1/2})
        allocate (T_mat(n_ens, n_ens))
        call dgemm('N', 'T', n_ens, n_ens, n_ens, 1.0d0, T_col_scaled, n_ens, A_eig, n_ens, 0.0d0, T_mat, n_ens)

        ! T2 = T * T  (= (I + C/(n_ens-1))^{-1})
        allocate (T2_mat(n_ens, n_ens))
        call dgemm('N', 'N', n_ens, n_ens, n_ens, 1.0d0, T_mat, n_ens, T_mat, n_ens, 0.0d0, T2_mat, n_ens)

        ! Ensemble perturbation update: Xa_anom = Af * T  (n_state x n_ens)
        allocate (Xa_anom(n_state, n_ens))
        call dgemm('N', 'N', n_state, n_ens, n_ens, 1.0d0, Af, n_state, T_mat, n_ens, 0.0d0, Xa_anom, n_state)

        ! Mean update
        allocate (d(n_obs_loc), tmp_obs(n_obs_loc), tmp_nens(n_ens), tmp_state(n_state), wa(n_ens))

        ! d = y - H * xf_bar
        call dgemv('N', n_obs_loc, n_state, 1.0d0, H_mat, n_obs_loc, xf_bar, 1, 0.0d0, tmp_obs, 1)
        d = y - tmp_obs

        ! R^{-1} * d
        do i = 1, n_obs_loc
            if (R_diag(i) > 0.0d0) then
                tmp_obs(i) = d(i) / R_diag(i)
            else
                tmp_obs(i) = 0.0d0
            end if
        end do

        ! Af_Y^T * (R^{-1} * d)  (n_ens vector)
        call dgemv('T', n_obs_loc, n_ens, 1.0d0, Af_Y, n_obs_loc, tmp_obs, 1, 0.0d0, tmp_nens, 1)

        ! wa = T2 * tmp_nens / (n_ens-1)
        call dgemv('N', n_ens, n_ens, 1.0d0 / real(n_ens - 1, real64), T2_mat, n_ens, tmp_nens, 1, 0.0d0, wa, 1)

        ! xa_bar = xf_bar + Af * wa
        allocate (xa_bar(n_state))
        call dgemv('N', n_state, n_ens, 1.0d0, Af, n_state, wa, 1, 0.0d0, tmp_state, 1)
        xa_bar = xf_bar + tmp_state

        ! Reconstruct analysis ensemble
        do k = 1, n_ens
            ensemble%members(k)%T = xa_bar(1:ensemble%n_nodes) &
                                   + Xa_anom(1:ensemble%n_nodes, k)
            ensemble%members(k)%q = max(0.0d0, &
                xa_bar(ensemble%n_nodes + 1:2*ensemble%n_nodes) + &
                Xa_anom(ensemble%n_nodes + 1:2*ensemble%n_nodes, k))
            ensemble%members(k)%U = max(0.0d0, &
                xa_bar(2*ensemble%n_nodes + 1:n_state) + &
                Xa_anom(2*ensemble%n_nodes + 1:n_state, k))
        end do

        write (*, '(A)') '[DA] ETKF analysis completed.'
        call clean_up()

    contains
        subroutine clean_up()
            implicit none
            if (allocated(Af))           deallocate (Af)
            if (allocated(xf_bar))       deallocate (xf_bar)
            if (allocated(xa_bar))       deallocate (xa_bar)
            if (allocated(Af_Y))         deallocate (Af_Y)
            if (allocated(Af_Y_Rinv))    deallocate (Af_Y_Rinv)
            if (allocated(C_mat))        deallocate (C_mat)
            if (allocated(A_eig))        deallocate (A_eig)
            if (allocated(T_mat))        deallocate (T_mat)
            if (allocated(T2_mat))       deallocate (T2_mat)
            if (allocated(T_col_scaled)) deallocate (T_col_scaled)
            if (allocated(Xa_anom))      deallocate (Xa_anom)
            if (allocated(H_mat))        deallocate (H_mat)
            if (allocated(eigval))       deallocate (eigval)
            if (allocated(y))            deallocate (y)
            if (allocated(R_diag))       deallocate (R_diag)
            if (allocated(d))            deallocate (d)
            if (allocated(wa))           deallocate (wa)
            if (allocated(tmp_obs))      deallocate (tmp_obs)
            if (allocated(tmp_nens))     deallocate (tmp_nens)
            if (allocated(tmp_state))    deallocate (tmp_state)
            call mean_state%destroy()
        end subroutine clean_up

    end subroutine calculate_analysis_etkf

end module physics_governing_atmosphere_etkf
