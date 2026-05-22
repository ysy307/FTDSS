!> Ensemble manager for 1D atmospheric state (U, T, q) over a vertical column.
module physics_governing_atmosphere_ensemble
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    private

    public :: type_atmos_state
    public :: type_ensemble_manager

    real(real64), parameter :: pi_atm = 3.14159265358979323846d0

    !> Single atmospheric state on a 1D vertical column.
    type :: type_atmos_state
        integer(int32) :: n_nodes = 0
        real(real64), allocatable :: z(:)  ! height [m]
        real(real64), allocatable :: T(:)  ! temperature [C]
        real(real64), allocatable :: q(:)  ! specific humidity [kg/kg]
        real(real64), allocatable :: U(:)  ! wind speed [m/s]
    contains
        procedure, public, pass(self) :: initialize => initialize_atmos_state
        procedure, public, pass(self) :: destroy    => destroy_atmos_state
    end type type_atmos_state

    !> Manages an ensemble of atmospheric states.
    type :: type_ensemble_manager
        integer(int32) :: n_members  = 0
        integer(int32) :: n_nodes    = 0
        real(real64)   :: max_height = 0.0d0
        type(type_atmos_state), allocatable :: members(:)
    contains
        procedure, public, pass(self) :: initialize        => initialize_ensemble_manager
        procedure, public, pass(self) :: destroy           => destroy_ensemble_manager
        procedure, public, pass(self) :: add_perturbation  => add_perturbation_ensemble
        procedure, public, pass(self) :: compute_mean      => compute_mean_ensemble
        procedure, public, pass(self) :: compute_anomalies => compute_anomalies_ensemble
    end type type_ensemble_manager

contains

    subroutine initialize_atmos_state(self, n_nodes, z_nodes)
        implicit none
        class(type_atmos_state), intent(inout) :: self
        integer(int32), intent(in) :: n_nodes
        real(real64), intent(in) :: z_nodes(:)

        self%n_nodes = n_nodes
        allocate (self%z(n_nodes), self%T(n_nodes), self%q(n_nodes), self%U(n_nodes))
        self%z = z_nodes
        self%T = 3.0d0    ! background [C]: Jan mean near-surface temp (updated by ETKF)
        self%q = 3.0d-3   ! background [kg/kg]: typical winter specific humidity
        self%U = 2.0d0    ! background [m/s]: light background wind
    end subroutine initialize_atmos_state

    subroutine destroy_atmos_state(self)
        implicit none
        class(type_atmos_state), intent(inout) :: self

        if (allocated(self%z)) deallocate (self%z)
        if (allocated(self%T)) deallocate (self%T)
        if (allocated(self%q)) deallocate (self%q)
        if (allocated(self%U)) deallocate (self%U)
        self%n_nodes = 0
    end subroutine destroy_atmos_state

    !> Initializes ensemble with uniform vertical mesh from z=0 to max_height.
    !> @param[in] size      number of ensemble members
    !> @param[in] max_height top of atmosphere column [m]
    !> @param[in] num_nodes  number of vertical grid points
    subroutine initialize_ensemble_manager(self, size, max_height, num_nodes)
        implicit none
        class(type_ensemble_manager), intent(inout) :: self
        integer(int32), intent(in) :: size
        real(real64), intent(in) :: max_height
        integer(int32), intent(in) :: num_nodes

        integer(int32) :: k
        real(real64), allocatable :: z_nodes(:)

        self%n_members  = size
        self%n_nodes    = num_nodes
        self%max_height = max_height

        allocate (z_nodes(num_nodes))
        do k = 1, num_nodes
            z_nodes(k) = max_height * real(k - 1, real64) / real(num_nodes - 1, real64)
        end do

        allocate (self%members(size))
        do k = 1, size
            call self%members(k)%initialize(num_nodes, z_nodes)
        end do

        deallocate (z_nodes)
    end subroutine initialize_ensemble_manager

    subroutine destroy_ensemble_manager(self)
        implicit none
        class(type_ensemble_manager), intent(inout) :: self

        integer(int32) :: k

        if (allocated(self%members)) then
            do k = 1, self%n_members
                call self%members(k)%destroy()
            end do
            deallocate (self%members)
        end if
        self%n_members = 0
        self%n_nodes   = 0
    end subroutine destroy_ensemble_manager

    ! Box-Muller transform: U(0,1) -> N(0,1)
    subroutine random_normal_atm(x)
        implicit none
        real(real64), intent(inout) :: x

        real(real64) :: u1, u2

        u1 = 0.0d0
        do while (u1 <= 0.0d0)
            call random_number(u1)
        end do
        call random_number(u2)
        x = sqrt(-2.0d0 * log(u1)) * cos(2.0d0 * pi_atm * u2)
    end subroutine random_normal_atm

    !> Adds Gaussian random perturbations to all ensemble members.
    subroutine add_perturbation_ensemble(self, sigma_T, sigma_q, sigma_U)
        implicit none
        class(type_ensemble_manager), intent(inout) :: self
        real(real64), intent(in) :: sigma_T
        real(real64), intent(in) :: sigma_q
        real(real64), intent(in) :: sigma_U

        integer(int32) :: k
        real(real64) :: rn

        ! One random scalar per member per variable, applied uniformly to all nodes.
        ! This creates perfect vertical correlation so ETKF can propagate obs information.
        call random_seed()
        do k = 1, self%n_members
            call random_normal_atm(rn)
            self%members(k)%T(:) = self%members(k)%T(:) + sigma_T * rn
            call random_normal_atm(rn)
            self%members(k)%q(:) = max(0.0d0, self%members(k)%q(:) + sigma_q * rn)
            call random_normal_atm(rn)
            self%members(k)%U(:) = max(0.0d0, self%members(k)%U(:) + sigma_U * rn)
        end do
    end subroutine add_perturbation_ensemble

    !> Computes ensemble mean state into mean_state.
    subroutine compute_mean_ensemble(self, mean_state)
        implicit none
        class(type_ensemble_manager), intent(inout) :: self
        type(type_atmos_state), intent(inout) :: mean_state

        integer(int32) :: k
        real(real64) :: inv_N

        if (self%n_members == 0 .or. self%n_nodes == 0) return

        if (.not. allocated(mean_state%T)) then
            call mean_state%initialize(self%n_nodes, self%members(1)%z)
        end if

        mean_state%T = 0.0d0
        mean_state%q = 0.0d0
        mean_state%U = 0.0d0

        do k = 1, self%n_members
            mean_state%T = mean_state%T + self%members(k)%T
            mean_state%q = mean_state%q + self%members(k)%q
            mean_state%U = mean_state%U + self%members(k)%U
        end do

        inv_N = 1.0d0 / real(self%n_members, real64)
        mean_state%T = mean_state%T * inv_N
        mean_state%q = mean_state%q * inv_N
        mean_state%U = mean_state%U * inv_N
    end subroutine compute_mean_ensemble

    !> Returns anomaly matrix Xf(n_state, n_members), n_state = 3*n_nodes.
    !> State vector ordering: [T(1:n), q(1:n), U(1:n)].
    subroutine compute_anomalies_ensemble(self, mean_state, Xf)
        implicit none
        class(type_ensemble_manager), intent(inout) :: self
        type(type_atmos_state), intent(in) :: mean_state
        real(real64), allocatable, intent(inout) :: Xf(:, :)

        integer(int32) :: k, n

        n = self%n_nodes
        if (allocated(Xf)) deallocate (Xf)
        allocate (Xf(3*n, self%n_members))

        do k = 1, self%n_members
            Xf(1:n, k)           = self%members(k)%T - mean_state%T
            Xf(n + 1:2*n, k)     = self%members(k)%q - mean_state%q
            Xf(2*n + 1:3*n, k)   = self%members(k)%U - mean_state%U
        end do
    end subroutine compute_anomalies_ensemble

end module physics_governing_atmosphere_ensemble
