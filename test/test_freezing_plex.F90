#include <petsc/finclude/petsc.h>
#include <petsc/finclude/petscdm.h>
#include <petsc/finclude/petscdmplex.h>
#include <petsc/finclude/petscdmlabel.h>
#include <petsc/finclude/petscsnes.h>
#include <petsc/finclude/petscksp.h>

module freezing_model
    use petsc
    use petscdm
    use petscdmplex
    use petscsnes
    use petscksp
    implicit none

    PetscInt, parameter :: BC_DIRICHLET = 1_PETSC_INT_KIND
    PetscInt, parameter :: BC_NEUMANN   = 2_PETSC_INT_KIND
    PetscInt, parameter :: BC_ROBIN     = 3_PETSC_INT_KIND

    integer, parameter :: MAX_BOUNDARIES = 32
    integer, parameter :: MAX_PROBES = 64

    type :: BoundaryCondition
        character(len=64) :: label_name = ""
        PetscInt :: kind = 0_PETSC_INT_KIND
        PetscReal :: value = 0.0_PETSC_REAL_KIND
        PetscReal :: coefficient = 0.0_PETSC_REAL_KIND
        PetscReal :: ambient = 0.0_PETSC_REAL_KIND
    end type BoundaryCondition

    type :: SolverConfig
        character(len=32) :: snes_type = "newtonls"
        character(len=32) :: linesearch_type = "bt"
        character(len=32) :: ksp_type = "gmres"
        character(len=32) :: pc_type = "ilu"
        PetscReal :: snes_rtol = 1.0e-8_PETSC_REAL_KIND
        PetscReal :: snes_atol = 1.0e-10_PETSC_REAL_KIND
        PetscReal :: snes_stol = 1.0e-12_PETSC_REAL_KIND
        PetscInt :: snes_max_it = 50_PETSC_INT_KIND
        PetscReal :: ksp_rtol = 1.0e-10_PETSC_REAL_KIND
        PetscReal :: ksp_atol = 1.0e-50_PETSC_REAL_KIND
        PetscInt :: ksp_max_it = 1000_PETSC_INT_KIND
    end type SolverConfig

    type :: AppCtx
        DM :: dm
        PetscSection :: section
        PetscSection :: global_section
        PetscSection :: coordinate_section
        Vec :: coordinates_local
        Vec :: t_old

        PetscInt :: c_start = 0_PETSC_INT_KIND
        PetscInt :: c_end = 0_PETSC_INT_KIND
        PetscInt :: f_start = 0_PETSC_INT_KIND
        PetscInt :: f_end = 0_PETSC_INT_KIND
        PetscInt :: v_start = 0_PETSC_INT_KIND
        PetscInt :: v_end = 0_PETSC_INT_KIND

        character(len=256) :: mesh_file = "mesh/domain.msh"
        character(len=256) :: output_directory = "out"

        PetscReal :: phi = 0.30_PETSC_REAL_KIND
        PetscReal :: tf = 0.0_PETSC_REAL_KIND
        PetscReal :: power_a = -6.02_PETSC_REAL_KIND
        !> Width [K] below tf of a C1 blend onto the power law. 0 keeps the raw law.
        PetscReal :: smoothing_width = 0.0_PETSC_REAL_KIND

        PetscReal :: rho_s = 2800.0_PETSC_REAL_KIND
        PetscReal :: rho_w = 1000.0_PETSC_REAL_KIND
        PetscReal :: rho_i = 917.0_PETSC_REAL_KIND

        PetscReal :: c_s = 912.0_PETSC_REAL_KIND
        PetscReal :: c_w = 4180.0_PETSC_REAL_KIND
        PetscReal :: c_i = 2100.0_PETSC_REAL_KIND

        PetscReal :: lambda_s = 3.78_PETSC_REAL_KIND
        PetscReal :: lambda_w = 0.60_PETSC_REAL_KIND
        PetscReal :: lambda_i = 2.20_PETSC_REAL_KIND

        PetscReal :: latent_heat = 334560.0_PETSC_REAL_KIND

        PetscReal :: t_initial = 18.0_PETSC_REAL_KIND
        PetscReal :: dt = 300.0_PETSC_REAL_KIND
        PetscReal :: t_end = 864000.0_PETSC_REAL_KIND

        PetscInt :: output_interval = 144_PETSC_INT_KIND

        logical :: profile_enabled = .true.
        PetscReal :: profile_y = 0.0_PETSC_REAL_KIND
        PetscReal :: profile_tolerance = 1.0e-8_PETSC_REAL_KIND

        PetscInt :: n_boundary = 0_PETSC_INT_KIND
        type(BoundaryCondition), allocatable :: boundaries(:)

        PetscInt :: nbc = 0_PETSC_INT_KIND
        PetscInt, allocatable :: bc_indices(:)
        PetscReal, allocatable :: bc_values(:)

        PetscInt :: nprobe = 0_PETSC_INT_KIND
        PetscReal, allocatable :: probe_x(:)
        PetscReal, allocatable :: probe_y(:)

        PetscInt, allocatable :: probe_indices(:,:)
        PetscReal, allocatable :: probe_weights(:,:)
        PetscInt, allocatable :: probe_cells(:)
        PetscReal, allocatable :: probe_xi(:)
        PetscReal, allocatable :: probe_eta(:)

        type(SolverConfig) :: solver
    end type AppCtx

contains

    pure function lower_string(value) result(result)
        implicit none
        character(len=*), intent(in) :: value
        character(len=len(value)) :: result
        integer :: i
        integer :: code

        result = value

        do i = 1, len(value)
            code = iachar(result(i:i))

            if (code >= iachar('A') .and. code <= iachar('Z')) then
                result(i:i) = achar(code + iachar('a') - iachar('A'))
            end if
        end do
    end function lower_string


    subroutine read_input_file(filename, ctx)
        implicit none

        character(len=*), intent(in) :: filename
        type(AppCtx), intent(inout) :: ctx

        character(len=256) :: mesh_file
        character(len=256) :: output_directory

        PetscReal :: phi
        PetscReal :: tf
        PetscReal :: power_a
        PetscReal :: smoothing_width
        PetscReal :: rho_s
        PetscReal :: rho_w
        PetscReal :: rho_i
        PetscReal :: c_s
        PetscReal :: c_w
        PetscReal :: c_i
        PetscReal :: lambda_s
        PetscReal :: lambda_w
        PetscReal :: lambda_i
        PetscReal :: latent_heat
        PetscReal :: t_initial
        PetscReal :: dt
        PetscReal :: t_end
        PetscInt :: output_interval

        logical :: profile_enabled
        PetscReal :: profile_y
        PetscReal :: profile_tolerance

        integer :: n_boundary
        character(len=64) :: bc_label(MAX_BOUNDARIES)
        character(len=32) :: bc_type(MAX_BOUNDARIES)
        PetscReal :: bc_value(MAX_BOUNDARIES)
        PetscReal :: bc_coefficient(MAX_BOUNDARIES)
        PetscReal :: bc_ambient(MAX_BOUNDARIES)

        integer :: nprobe
        PetscReal :: probe_x(MAX_PROBES)
        PetscReal :: probe_y(MAX_PROBES)

        character(len=32) :: snes_type
        character(len=32) :: linesearch_type
        character(len=32) :: ksp_type
        character(len=32) :: pc_type
        PetscReal :: snes_rtol
        PetscReal :: snes_atol
        PetscReal :: snes_stol
        integer :: snes_max_it
        PetscReal :: ksp_rtol
        PetscReal :: ksp_atol
        integer :: ksp_max_it

        integer :: unit
        integer :: ios
        integer :: i
        character(len=512) :: iomsg
        character(len=32) :: bc_type_lower

        namelist /case/ &
            mesh_file, &
            output_directory, &
            phi, &
            tf, &
            power_a, &
            smoothing_width, &
            rho_s, &
            rho_w, &
            rho_i, &
            c_s, &
            c_w, &
            c_i, &
            lambda_s, &
            lambda_w, &
            lambda_i, &
            latent_heat, &
            t_initial, &
            dt, &
            t_end, &
            output_interval, &
            profile_enabled, &
            profile_y, &
            profile_tolerance, &
            n_boundary, &
            bc_label, &
            bc_type, &
            bc_value, &
            bc_coefficient, &
            bc_ambient, &
            nprobe, &
            probe_x, &
            probe_y, &
            snes_type, &
            linesearch_type, &
            ksp_type, &
            pc_type, &
            snes_rtol, &
            snes_atol, &
            snes_stol, &
            snes_max_it, &
            ksp_rtol, &
            ksp_atol, &
            ksp_max_it

        mesh_file = ctx%mesh_file
        output_directory = ctx%output_directory

        phi = ctx%phi
        tf = ctx%tf
        power_a = ctx%power_a
        smoothing_width = ctx%smoothing_width
        rho_s = ctx%rho_s
        rho_w = ctx%rho_w
        rho_i = ctx%rho_i
        c_s = ctx%c_s
        c_w = ctx%c_w
        c_i = ctx%c_i
        lambda_s = ctx%lambda_s
        lambda_w = ctx%lambda_w
        lambda_i = ctx%lambda_i
        latent_heat = ctx%latent_heat
        t_initial = ctx%t_initial
        dt = ctx%dt
        t_end = ctx%t_end
        output_interval = ctx%output_interval

        profile_enabled = ctx%profile_enabled
        profile_y = ctx%profile_y
        profile_tolerance = ctx%profile_tolerance

        n_boundary = 0
        bc_label = ""
        bc_type = ""
        bc_value = 0.0_PETSC_REAL_KIND
        bc_coefficient = 0.0_PETSC_REAL_KIND
        bc_ambient = 0.0_PETSC_REAL_KIND

        nprobe = 0
        probe_x = 0.0_PETSC_REAL_KIND
        probe_y = 0.0_PETSC_REAL_KIND

        snes_type = ctx%solver%snes_type
        linesearch_type = ctx%solver%linesearch_type
        ksp_type = ctx%solver%ksp_type
        pc_type = ctx%solver%pc_type
        snes_rtol = ctx%solver%snes_rtol
        snes_atol = ctx%solver%snes_atol
        snes_stol = ctx%solver%snes_stol
        snes_max_it = int(ctx%solver%snes_max_it)
        ksp_rtol = ctx%solver%ksp_rtol
        ksp_atol = ctx%solver%ksp_atol
        ksp_max_it = int(ctx%solver%ksp_max_it)

        open( &
            newunit=unit, &
            file=trim(filename), &
            status="old", &
            action="read", &
            iostat=ios, &
            iomsg=iomsg &
        )

        if (ios /= 0) then
            write(*, '(A)') "Failed to open input file."
            write(*, '(A,I0)') "IOSTAT = ", ios
            write(*, '(A,A)') "IOMSG  = ", trim(iomsg)
            error stop
        end if

        read(unit, nml=case, iostat=ios, iomsg=iomsg)

        if (ios /= 0) then
            write(*, '(A)') "Failed to read NAMELIST."
            write(*, '(A,I0)') "IOSTAT = ", ios
            write(*, '(A,A)') "IOMSG  = ", trim(iomsg)
            close(unit)
            error stop
        end if

        close(unit)

        if (n_boundary < 0 .or. n_boundary > MAX_BOUNDARIES) then
            error stop "n_boundary is outside the supported range."
        end if

        if (nprobe < 0 .or. nprobe > MAX_PROBES) then
            error stop "nprobe is outside the supported range."
        end if

        if (phi <= 0.0_PETSC_REAL_KIND .or. phi >= 1.0_PETSC_REAL_KIND) then
            error stop "phi must satisfy 0 < phi < 1."
        end if

        if (dt <= 0.0_PETSC_REAL_KIND) then
            error stop "dt must be positive."
        end if

        if (t_end < 0.0_PETSC_REAL_KIND) then
            error stop "t_end must be non-negative."
        end if

        if (output_interval <= 0_PETSC_INT_KIND) then
            error stop "output_interval must be positive."
        end if

        ctx%mesh_file = trim(mesh_file)
        ctx%output_directory = trim(output_directory)

        ctx%phi = phi
        ctx%tf = tf
        ctx%power_a = power_a
        ctx%smoothing_width = smoothing_width
        ctx%rho_s = rho_s
        ctx%rho_w = rho_w
        ctx%rho_i = rho_i
        ctx%c_s = c_s
        ctx%c_w = c_w
        ctx%c_i = c_i
        ctx%lambda_s = lambda_s
        ctx%lambda_w = lambda_w
        ctx%lambda_i = lambda_i
        ctx%latent_heat = latent_heat
        ctx%t_initial = t_initial
        ctx%dt = dt
        ctx%t_end = t_end
        ctx%output_interval = output_interval

        ctx%profile_enabled = profile_enabled
        ctx%profile_y = profile_y
        ctx%profile_tolerance = profile_tolerance

        ctx%n_boundary = int(n_boundary, PETSC_INT_KIND)

        if (allocated(ctx%boundaries)) deallocate(ctx%boundaries)
        allocate(ctx%boundaries(n_boundary))

        do i = 1, n_boundary
            if (len_trim(bc_label(i)) == 0) then
                error stop "Every boundary condition requires bc_label."
            end if

            ctx%boundaries(i)%label_name = trim(bc_label(i))
            ctx%boundaries(i)%value = bc_value(i)
            ctx%boundaries(i)%coefficient = bc_coefficient(i)
            ctx%boundaries(i)%ambient = bc_ambient(i)

            bc_type_lower = trim(lower_string(bc_type(i)))

            select case (trim(bc_type_lower))
            case ("dirichlet")
                ctx%boundaries(i)%kind = BC_DIRICHLET
            case ("neumann")
                ctx%boundaries(i)%kind = BC_NEUMANN
            case ("robin")
                ctx%boundaries(i)%kind = BC_ROBIN
            case default
                write(*, '(A,I0,A,A)') "Unknown bc_type at index ", i, ": ", trim(bc_type(i))
                error stop
            end select
        end do

        ctx%nprobe = int(nprobe, PETSC_INT_KIND)

        if (allocated(ctx%probe_x)) deallocate(ctx%probe_x)
        if (allocated(ctx%probe_y)) deallocate(ctx%probe_y)

        allocate(ctx%probe_x(nprobe))
        allocate(ctx%probe_y(nprobe))

        if (nprobe > 0) then
            ctx%probe_x = probe_x(1:nprobe)
            ctx%probe_y = probe_y(1:nprobe)
        end if

        ctx%solver%snes_type = trim(snes_type)
        ctx%solver%linesearch_type = trim(linesearch_type)
        ctx%solver%ksp_type = trim(ksp_type)
        ctx%solver%pc_type = trim(pc_type)
        ctx%solver%snes_rtol = snes_rtol
        ctx%solver%snes_atol = snes_atol
        ctx%solver%snes_stol = snes_stol
        ctx%solver%snes_max_it = int(snes_max_it, PETSC_INT_KIND)
        ctx%solver%ksp_rtol = ksp_rtol
        ctx%solver%ksp_atol = ksp_atol
        ctx%solver%ksp_max_it = int(ksp_max_it, PETSC_INT_KIND)
    end subroutine read_input_file


    subroutine ensure_output_directory(ctx)
        implicit none

        type(AppCtx), intent(in) :: ctx

        character(len=1024) :: command
        integer :: exitstat

        command = 'mkdir -p -- "' // trim(ctx%output_directory) // '"'

        call execute_command_line( &
            trim(command), &
            wait=.true., &
            exitstat=exitstat &
        )

        if (exitstat /= 0) then
            error stop "Failed to create output directory."
        end if
    end subroutine ensure_output_directory


    !> Cubic blend coefficients on s = tf - t in [0, w].
    !>
    !> The raw law theta_i = phi (1 - (1+s)^a) leaves d theta_i / dT jumping from
    !> 0 to -phi*a at s = 0, so d h / dT jumps by ~180x across the freezing point
    !> and Newton has no valid linearisation there. The blend matches the raw law
    !> in value and slope at s = w and reaches s = 0 with zero slope, making h
    !> C1 across tf.
    pure subroutine blend_coefficients(ctx, w, c2, c3)
        implicit none

        type(AppCtx), intent(in) :: ctx
        PetscReal, intent(in) :: w

        PetscReal, intent(inout) :: c2
        PetscReal, intent(inout) :: c3

        PetscReal :: theta_w
        PetscReal :: slope_w

        theta_w = ctx%phi*(1.0_PETSC_REAL_KIND - (1.0_PETSC_REAL_KIND + w)**ctx%power_a)
        slope_w = -ctx%phi*ctx%power_a*(1.0_PETSC_REAL_KIND + w)**(ctx%power_a - 1.0_PETSC_REAL_KIND)

        c2 = (3.0_PETSC_REAL_KIND*theta_w - slope_w*w)/w**2
        c3 = (slope_w*w - 2.0_PETSC_REAL_KIND*theta_w)/w**3
    end subroutine blend_coefficients


    !> Integral of the raw power law from 0 to s.
    pure function raw_ice_integral(s, ctx) result(j)
        implicit none

        PetscReal, intent(in) :: s
        type(AppCtx), intent(in) :: ctx

        PetscReal :: j
        PetscReal :: y
        PetscReal :: ap1

        y = 1.0_PETSC_REAL_KIND + s
        ap1 = ctx%power_a + 1.0_PETSC_REAL_KIND

        if (abs(ap1) <= epsilon(1.0_PETSC_REAL_KIND)) then
            j = ctx%phi*(s - log(y))
        else
            j = ctx%phi*(s - (y**ap1 - 1.0_PETSC_REAL_KIND)/ap1)
        end if
    end function raw_ice_integral


    pure function ice_content(t, ctx) result(qice)
        implicit none

        PetscReal, intent(in) :: t
        type(AppCtx), intent(in) :: ctx

        PetscReal :: qice
        PetscReal :: s
        PetscReal :: c2
        PetscReal :: c3

        s = ctx%tf - t

        if (s <= 0.0_PETSC_REAL_KIND) then
            qice = 0.0_PETSC_REAL_KIND
        else if (ctx%smoothing_width > 0.0_PETSC_REAL_KIND .and. s < ctx%smoothing_width) then
            call blend_coefficients(ctx, ctx%smoothing_width, c2, c3)
            qice = c2*s**2 + c3*s**3
        else
            qice = ctx%phi*(1.0_PETSC_REAL_KIND - (1.0_PETSC_REAL_KIND + s)**ctx%power_a)
        end if
    end function ice_content


    pure function dice_content_dt(t, ctx) result(dqice_dt)
        implicit none

        PetscReal, intent(in) :: t
        type(AppCtx), intent(in) :: ctx

        PetscReal :: dqice_dt
        PetscReal :: s
        PetscReal :: c2
        PetscReal :: c3

        s = ctx%tf - t

        if (s <= 0.0_PETSC_REAL_KIND) then
            dqice_dt = 0.0_PETSC_REAL_KIND
        else if (ctx%smoothing_width > 0.0_PETSC_REAL_KIND .and. s < ctx%smoothing_width) then
            call blend_coefficients(ctx, ctx%smoothing_width, c2, c3)
            dqice_dt = -(2.0_PETSC_REAL_KIND*c2*s + 3.0_PETSC_REAL_KIND*c3*s**2)
        else
            dqice_dt = ctx%phi*ctx%power_a &
                *(1.0_PETSC_REAL_KIND + s)**(ctx%power_a - 1.0_PETSC_REAL_KIND)
        end if
    end function dice_content_dt


    !> Integral of theta_i from tf to t, so that d(iqice)/dt = theta_i(t).
    pure function integrated_ice_content(t, ctx) result(iqice)
        implicit none

        PetscReal, intent(in) :: t
        type(AppCtx), intent(in) :: ctx

        PetscReal :: iqice
        PetscReal :: s
        PetscReal :: w
        PetscReal :: c2
        PetscReal :: c3
        PetscReal :: j

        s = ctx%tf - t
        w = ctx%smoothing_width

        if (s <= 0.0_PETSC_REAL_KIND) then
            iqice = 0.0_PETSC_REAL_KIND
            return
        end if

        if (w > 0.0_PETSC_REAL_KIND) then
            call blend_coefficients(ctx, w, c2, c3)

            if (s < w) then
                j = c2*s**3/3.0_PETSC_REAL_KIND + c3*s**4/4.0_PETSC_REAL_KIND
            else
                j = c2*w**3/3.0_PETSC_REAL_KIND + c3*w**4/4.0_PETSC_REAL_KIND &
                    + raw_ice_integral(s, ctx) - raw_ice_integral(w, ctx)
            end if
        else
            j = raw_ice_integral(s, ctx)
        end if

        iqice = -j
    end function integrated_ice_content


    pure function volumetric_heat_capacity(t, ctx) result(cp)
        implicit none

        PetscReal, intent(in) :: t
        type(AppCtx), intent(in) :: ctx

        PetscReal :: cp
        PetscReal :: theta_i
        PetscReal :: theta_w
        PetscReal :: theta_s

        theta_i = ice_content(t, ctx)
        theta_w = ctx%phi - theta_i
        theta_s = 1.0_PETSC_REAL_KIND - ctx%phi

        cp = &
            theta_s*ctx%rho_s*ctx%c_s &
            + theta_w*ctx%rho_w*ctx%c_w &
            + theta_i*ctx%rho_i*ctx%c_i
    end function volumetric_heat_capacity


    pure function apparent_heat_capacity(t, ctx) result(ca)
        implicit none

        PetscReal, intent(in) :: t
        type(AppCtx), intent(in) :: ctx

        PetscReal :: ca

        ca = volumetric_heat_capacity(t, ctx) &
            - ctx%rho_i*ctx%latent_heat*dice_content_dt(t, ctx)
    end function apparent_heat_capacity


    pure function enthalpy(t, ctx) result(h)
        implicit none

        PetscReal, intent(in) :: t
        type(AppCtx), intent(in) :: ctx

        PetscReal :: h
        PetscReal :: c_unfrozen
        PetscReal :: delta_c
        PetscReal :: iqice
        PetscReal :: qice

        c_unfrozen = &
            (1.0_PETSC_REAL_KIND - ctx%phi)*ctx%rho_s*ctx%c_s &
            + ctx%phi*ctx%rho_w*ctx%c_w

        delta_c = ctx%rho_i*ctx%c_i - ctx%rho_w*ctx%c_w

        if (t <= ctx%tf) then
            iqice = integrated_ice_content(t, ctx)
            qice = ice_content(t, ctx)

            h = &
                c_unfrozen*(t - ctx%tf) &
                + delta_c*iqice &
                - ctx%rho_i*ctx%latent_heat*qice
        else
            h = c_unfrozen*(t - ctx%tf)
        end if
    end function enthalpy


    pure function denthalpy_dt(t, ctx) result(dh_dt)
        implicit none

        PetscReal, intent(in) :: t
        type(AppCtx), intent(in) :: ctx

        PetscReal :: dh_dt

        dh_dt = apparent_heat_capacity(t, ctx)
    end function denthalpy_dt


    pure function thermal_conductivity(t, ctx) result(lambda)
        implicit none

        PetscReal, intent(in) :: t
        type(AppCtx), intent(in) :: ctx

        PetscReal :: lambda
        PetscReal :: theta_i
        PetscReal :: theta_w
        PetscReal :: theta_s

        theta_i = ice_content(t, ctx)
        theta_w = ctx%phi - theta_i
        theta_s = 1.0_PETSC_REAL_KIND - ctx%phi

        lambda = &
            ctx%lambda_s**theta_s &
            * ctx%lambda_w**theta_w &
            * ctx%lambda_i**theta_i
    end function thermal_conductivity


    pure function dthermal_conductivity_dt(t, ctx) result(dlambda_dt)
        implicit none

        PetscReal, intent(in) :: t
        type(AppCtx), intent(in) :: ctx

        PetscReal :: dlambda_dt
        PetscReal :: lambda
        PetscReal :: dqice_dt

        lambda = thermal_conductivity(t, ctx)
        dqice_dt = dice_content_dt(t, ctx)

        dlambda_dt = lambda*dqice_dt*log(ctx%lambda_i/ctx%lambda_w)
    end function dthermal_conductivity_dt


    subroutine shape_quad4(xi, eta, n, dndxi, dndeta)
        implicit none

        PetscReal, intent(in) :: xi
        PetscReal, intent(in) :: eta

        PetscReal, intent(inout) :: n(4)
        PetscReal, intent(inout) :: dndxi(4)
        PetscReal, intent(inout) :: dndeta(4)

        n(1) = 0.25_PETSC_REAL_KIND*(1.0_PETSC_REAL_KIND - xi)*(1.0_PETSC_REAL_KIND - eta)
        n(2) = 0.25_PETSC_REAL_KIND*(1.0_PETSC_REAL_KIND + xi)*(1.0_PETSC_REAL_KIND - eta)
        n(3) = 0.25_PETSC_REAL_KIND*(1.0_PETSC_REAL_KIND + xi)*(1.0_PETSC_REAL_KIND + eta)
        n(4) = 0.25_PETSC_REAL_KIND*(1.0_PETSC_REAL_KIND - xi)*(1.0_PETSC_REAL_KIND + eta)

        dndxi(1) = -0.25_PETSC_REAL_KIND*(1.0_PETSC_REAL_KIND - eta)
        dndxi(2) =  0.25_PETSC_REAL_KIND*(1.0_PETSC_REAL_KIND - eta)
        dndxi(3) =  0.25_PETSC_REAL_KIND*(1.0_PETSC_REAL_KIND + eta)
        dndxi(4) = -0.25_PETSC_REAL_KIND*(1.0_PETSC_REAL_KIND + eta)

        dndeta(1) = -0.25_PETSC_REAL_KIND*(1.0_PETSC_REAL_KIND - xi)
        dndeta(2) = -0.25_PETSC_REAL_KIND*(1.0_PETSC_REAL_KIND + xi)
        dndeta(3) =  0.25_PETSC_REAL_KIND*(1.0_PETSC_REAL_KIND + xi)
        dndeta(4) =  0.25_PETSC_REAL_KIND*(1.0_PETSC_REAL_KIND - xi)
    end subroutine shape_quad4


    subroutine physical_gradients(xy, dndxi, dndeta, dndx, dndy, detj)
        implicit none

        PetscReal, intent(in) :: xy(2,4)
        PetscReal, intent(in) :: dndxi(4)
        PetscReal, intent(in) :: dndeta(4)

        PetscReal, intent(inout) :: dndx(4)
        PetscReal, intent(inout) :: dndy(4)
        PetscReal, intent(inout) :: detj

        PetscReal :: dx_dxi
        PetscReal :: dx_deta
        PetscReal :: dy_dxi
        PetscReal :: dy_deta
        integer :: a

        dx_dxi = 0.0_PETSC_REAL_KIND
        dx_deta = 0.0_PETSC_REAL_KIND
        dy_dxi = 0.0_PETSC_REAL_KIND
        dy_deta = 0.0_PETSC_REAL_KIND

        do a = 1, 4
            dx_dxi = dx_dxi + dndxi(a)*xy(1,a)
            dx_deta = dx_deta + dndeta(a)*xy(1,a)
            dy_dxi = dy_dxi + dndxi(a)*xy(2,a)
            dy_deta = dy_deta + dndeta(a)*xy(2,a)
        end do

        detj = dx_dxi*dy_deta - dx_deta*dy_dxi

        do a = 1, 4
            dndx(a) = (dy_deta*dndxi(a) - dy_dxi*dndeta(a))/detj
            dndy(a) = (-dx_deta*dndxi(a) + dx_dxi*dndeta(a))/detj
        end do
    end subroutine physical_gradients


    subroutine element_residual(ctx, xy, te, te_old, re)
        implicit none

        type(AppCtx), intent(in) :: ctx
        PetscReal, intent(in) :: xy(2,4)
        PetscScalar, intent(in) :: te(4)
        PetscScalar, intent(in) :: te_old(4)
        PetscScalar, intent(inout) :: re(4)

        PetscReal :: gp(2)
        PetscReal :: n(4)
        PetscReal :: dndxi(4)
        PetscReal :: dndeta(4)
        PetscReal :: dndx(4)
        PetscReal :: dndy(4)
        PetscReal :: detj
        PetscScalar :: tgp
        PetscScalar :: tgp_old
        PetscScalar :: grad_tx
        PetscScalar :: grad_ty
        PetscReal :: h_new
        PetscReal :: h_old
        PetscReal :: lambda
        integer :: ig
        integer :: jg
        integer :: a

        gp(1) = -1.0_PETSC_REAL_KIND/sqrt(3.0_PETSC_REAL_KIND)
        gp(2) =  1.0_PETSC_REAL_KIND/sqrt(3.0_PETSC_REAL_KIND)

        re = 0.0_PETSC_REAL_KIND

        do jg = 1, 2
            do ig = 1, 2
                call shape_quad4(gp(ig), gp(jg), n, dndxi, dndeta)
                call physical_gradients(xy, dndxi, dndeta, dndx, dndy, detj)

                tgp = sum(n*te)
                tgp_old = sum(n*te_old)
                grad_tx = sum(dndx*te)
                grad_ty = sum(dndy*te)

                h_new = enthalpy(real(tgp, PETSC_REAL_KIND), ctx)
                h_old = enthalpy(real(tgp_old, PETSC_REAL_KIND), ctx)
                lambda = thermal_conductivity(real(tgp, PETSC_REAL_KIND), ctx)

                do a = 1, 4
                    re(a) = re(a) &
                        + n(a)*(h_new - h_old)/ctx%dt*detj &
                        + lambda*(dndx(a)*grad_tx + dndy(a)*grad_ty)*detj
                end do
            end do
        end do
    end subroutine element_residual


    subroutine element_jacobian(ctx, xy, te, je)
        implicit none

        type(AppCtx), intent(in) :: ctx
        PetscReal, intent(in) :: xy(2,4)
        PetscScalar, intent(in) :: te(4)
        PetscScalar, intent(inout) :: je(4,4)

        PetscReal :: gp(2)
        PetscReal :: n(4)
        PetscReal :: dndxi(4)
        PetscReal :: dndeta(4)
        PetscReal :: dndx(4)
        PetscReal :: dndy(4)
        PetscReal :: detj
        PetscScalar :: tgp
        PetscScalar :: grad_tx
        PetscScalar :: grad_ty
        PetscReal :: dh_dt
        PetscReal :: lambda
        PetscReal :: dlambda_dt
        PetscScalar :: grad_test_t
        PetscReal :: grad_test_trial
        integer :: ig
        integer :: jg
        integer :: a
        integer :: b

        gp(1) = -1.0_PETSC_REAL_KIND/sqrt(3.0_PETSC_REAL_KIND)
        gp(2) =  1.0_PETSC_REAL_KIND/sqrt(3.0_PETSC_REAL_KIND)

        je = 0.0_PETSC_REAL_KIND

        do jg = 1, 2
            do ig = 1, 2
                call shape_quad4(gp(ig), gp(jg), n, dndxi, dndeta)
                call physical_gradients(xy, dndxi, dndeta, dndx, dndy, detj)

                tgp = sum(n*te)
                grad_tx = sum(dndx*te)
                grad_ty = sum(dndy*te)

                dh_dt = denthalpy_dt(real(tgp, PETSC_REAL_KIND), ctx)
                lambda = thermal_conductivity(real(tgp, PETSC_REAL_KIND), ctx)
                dlambda_dt = dthermal_conductivity_dt(real(tgp, PETSC_REAL_KIND), ctx)

                do a = 1, 4
                    grad_test_t = dndx(a)*grad_tx + dndy(a)*grad_ty

                    do b = 1, 4
                        grad_test_trial = dndx(a)*dndx(b) + dndy(a)*dndy(b)

                        je(a,b) = je(a,b) &
                            + n(a)*dh_dt*n(b)/ctx%dt*detj &
                            + lambda*grad_test_trial*detj &
                            + dlambda_dt*n(b)*grad_test_t*detj
                    end do
                end do
            end do
        end do
    end subroutine element_jacobian


    subroutine load_mesh(ctx, ierr)
        implicit none

        type(AppCtx), intent(inout) :: ctx
        PetscErrorCode, intent(inout) :: ierr

        ierr = 0

        call PetscOptionsSetValue(PETSC_NULL_OPTIONS, "-dm_plex_gmsh_use_regions", "true", ierr)
        if (ierr /= 0) return

        call DMPlexCreateGmshFromFile(PETSC_COMM_SELF, trim(ctx%mesh_file), PETSC_TRUE, ctx%dm, ierr)
        if (ierr /= 0) return

        call PetscObjectSetName(ctx%dm, "mesh", ierr)
        if (ierr /= 0) return

        call DMSetFromOptions(ctx%dm, ierr)
        if (ierr /= 0) return

        call DMPlexGetHeightStratum(ctx%dm, 0_PETSC_INT_KIND, ctx%c_start, ctx%c_end, ierr)
        if (ierr /= 0) return

        call DMPlexGetHeightStratum(ctx%dm, 1_PETSC_INT_KIND, ctx%f_start, ctx%f_end, ierr)
        if (ierr /= 0) return

        call DMPlexGetDepthStratum(ctx%dm, 0_PETSC_INT_KIND, ctx%v_start, ctx%v_end, ierr)
        if (ierr /= 0) return

        call DMGetCoordinateSection(ctx%dm, ctx%coordinate_section, ierr)
        if (ierr /= 0) return

        call DMGetCoordinatesLocal(ctx%dm, ctx%coordinates_local, ierr)
    end subroutine load_mesh


    subroutine setup_vertex_section(ctx, ierr)
        implicit none

        type(AppCtx), intent(inout) :: ctx
        PetscErrorCode, intent(inout) :: ierr

        PetscSection :: section
        PetscInt :: p_start
        PetscInt :: p_end
        PetscInt :: vertex

        call DMPlexGetChart(ctx%dm, p_start, p_end, ierr)
        if (ierr /= 0) return

        call PetscSectionCreate(PETSC_COMM_SELF, section, ierr)
        if (ierr /= 0) return

        call PetscSectionSetNumFields(section, 1_PETSC_INT_KIND, ierr)
        if (ierr /= 0) return

        call PetscSectionSetFieldComponents(section, 0_PETSC_INT_KIND, 1_PETSC_INT_KIND, ierr)
        if (ierr /= 0) return

        call PetscSectionSetFieldName(section, 0_PETSC_INT_KIND, "temperature", ierr)
        if (ierr /= 0) return

        call PetscSectionSetComponentName(section, 0_PETSC_INT_KIND, 0_PETSC_INT_KIND, "T", ierr)
        if (ierr /= 0) return

        call PetscSectionSetChart(section, p_start, p_end, ierr)
        if (ierr /= 0) return

        do vertex = ctx%v_start, ctx%v_end - 1
            call PetscSectionSetDof(section, vertex, 1_PETSC_INT_KIND, ierr)
            if (ierr /= 0) return

            call PetscSectionSetFieldDof(section, vertex, 0_PETSC_INT_KIND, 1_PETSC_INT_KIND, ierr)
            if (ierr /= 0) return
        end do

        call PetscSectionSetUp(section, ierr)
        if (ierr /= 0) return

        call DMSetLocalSection(ctx%dm, section, ierr)
        if (ierr /= 0) return

        call PetscSectionDestroy(section, ierr)
        if (ierr /= 0) return

        call DMGetLocalSection(ctx%dm, ctx%section, ierr)
        if (ierr /= 0) return

        call DMGetGlobalSection(ctx%dm, ctx%global_section, ierr)
    end subroutine setup_vertex_section


    subroutine sort_quad_nodes(indices, xy, ierr)
        implicit none

        PetscInt, intent(inout) :: indices(4)
        PetscReal, intent(inout) :: xy(2,4)
        PetscErrorCode, intent(inout) :: ierr

        PetscInt :: idx_tmp
        PetscReal :: xy_tmp(2)
        PetscReal :: angle(4)
        PetscReal :: cx
        PetscReal :: cy
        PetscReal :: area2
        integer :: i
        integer :: j
        integer :: min_index

        cx = sum(xy(1,:))/4.0_PETSC_REAL_KIND
        cy = sum(xy(2,:))/4.0_PETSC_REAL_KIND

        do i = 1, 4
            angle(i) = atan2(xy(2,i) - cy, xy(1,i) - cx)
        end do

        do i = 1, 3
            min_index = i

            do j = i + 1, 4
                if (angle(j) < angle(min_index)) min_index = j
            end do

            if (min_index /= i) then
                idx_tmp = indices(i)
                indices(i) = indices(min_index)
                indices(min_index) = idx_tmp

                xy_tmp = xy(:,i)
                xy(:,i) = xy(:,min_index)
                xy(:,min_index) = xy_tmp

                call swap_real(angle(i), angle(min_index))
            end if
        end do

        area2 = &
            xy(1,1)*xy(2,2) - xy(1,2)*xy(2,1) &
            + xy(1,2)*xy(2,3) - xy(1,3)*xy(2,2) &
            + xy(1,3)*xy(2,4) - xy(1,4)*xy(2,3) &
            + xy(1,4)*xy(2,1) - xy(1,1)*xy(2,4)

        if (area2 <= 0.0_PETSC_REAL_KIND) then
            ierr = PETSC_ERR_ARG_WRONG
        end if
    end subroutine sort_quad_nodes


    subroutine swap_real(a, b)
        implicit none

        PetscReal, intent(inout) :: a
        PetscReal, intent(inout) :: b

        PetscReal :: tmp

        tmp = a
        a = b
        b = tmp
    end subroutine swap_real


    subroutine get_cell_nodes(ctx, cell, coordinate_array, indices, xy, ierr)
        implicit none

        type(AppCtx), intent(in) :: ctx
        PetscInt, intent(in) :: cell
        PetscScalar, intent(in) :: coordinate_array(:)
        PetscInt, intent(inout) :: indices(4)
        PetscReal, intent(inout) :: xy(2,4)
        PetscErrorCode, intent(inout) :: ierr

        PetscInt, pointer :: closure(:)
        PetscInt :: num_points
        PetscInt :: point
        PetscInt :: global_offset
        PetscInt :: coordinate_offset
        integer :: k
        integer :: nvertex

        indices = -1_PETSC_INT_KIND
        xy = 0.0_PETSC_REAL_KIND
        nullify(closure)

        call DMPlexGetTransitiveClosure(ctx%dm, cell, PETSC_TRUE, num_points, closure, ierr)
        if (ierr /= 0) return

        nvertex = 0

        do k = 1, num_points
            point = closure(2*k - 1)

            if (point < ctx%v_start .or. point >= ctx%v_end) cycle

            nvertex = nvertex + 1

            if (nvertex > 4) then
                ierr = PETSC_ERR_ARG_SIZ
                exit
            end if

            call PetscSectionGetOffset(ctx%global_section, point, global_offset, ierr)
            if (ierr /= 0) exit

            call PetscSectionGetOffset(ctx%coordinate_section, point, coordinate_offset, ierr)
            if (ierr /= 0) exit

            indices(nvertex) = global_offset
            xy(1,nvertex) = real(coordinate_array(coordinate_offset + 1), PETSC_REAL_KIND)
            xy(2,nvertex) = real(coordinate_array(coordinate_offset + 2), PETSC_REAL_KIND)
        end do

        call DMPlexRestoreTransitiveClosure(ctx%dm, cell, PETSC_TRUE, num_points, closure, ierr)
        if (ierr /= 0) return

        if (nvertex /= 4) then
            write(*, '(A,I0,A,I0)') "Cell ", cell, " has vertex count ", nvertex
            ierr = PETSC_ERR_ARG_SIZ
            return
        end if

        call sort_quad_nodes(indices, xy, ierr)
    end subroutine get_cell_nodes


    subroutine get_edge_nodes(ctx, edge, coordinate_array, indices, xy, ierr)
        implicit none

        type(AppCtx), intent(in) :: ctx
        PetscInt, intent(in) :: edge
        PetscScalar, intent(in) :: coordinate_array(:)
        PetscInt, intent(inout) :: indices(2)
        PetscReal, intent(inout) :: xy(2,2)
        PetscErrorCode, intent(inout) :: ierr

        PetscInt, pointer :: closure(:)
        PetscInt :: num_points
        PetscInt :: point
        PetscInt :: global_offset
        PetscInt :: coordinate_offset
        integer :: k
        integer :: nvertex

        indices = -1_PETSC_INT_KIND
        xy = 0.0_PETSC_REAL_KIND
        nullify(closure)

        call DMPlexGetTransitiveClosure(ctx%dm, edge, PETSC_TRUE, num_points, closure, ierr)
        if (ierr /= 0) return

        nvertex = 0

        do k = 1, num_points
            point = closure(2*k - 1)

            if (point < ctx%v_start .or. point >= ctx%v_end) cycle

            nvertex = nvertex + 1

            if (nvertex > 2) then
                ierr = PETSC_ERR_ARG_SIZ
                exit
            end if

            call PetscSectionGetOffset(ctx%global_section, point, global_offset, ierr)
            if (ierr /= 0) exit

            call PetscSectionGetOffset(ctx%coordinate_section, point, coordinate_offset, ierr)
            if (ierr /= 0) exit

            indices(nvertex) = global_offset
            xy(1,nvertex) = real(coordinate_array(coordinate_offset + 1), PETSC_REAL_KIND)
            xy(2,nvertex) = real(coordinate_array(coordinate_offset + 2), PETSC_REAL_KIND)
        end do

        call DMPlexRestoreTransitiveClosure(ctx%dm, edge, PETSC_TRUE, num_points, closure, ierr)
        if (ierr /= 0) return

        if (nvertex /= 2) then
            ierr = PETSC_ERR_ARG_SIZ
        end if
    end subroutine get_edge_nodes


    subroutine validate_q4_mesh(ctx, ierr)
        implicit none

        type(AppCtx), intent(in) :: ctx
        PetscErrorCode, intent(inout) :: ierr

        PetscScalar, pointer :: coordinate_array(:)
        PetscInt :: cell
        PetscInt :: indices(4)
        PetscReal :: xy(2,4)
        PetscReal :: gp(2)
        PetscReal :: n(4)
        PetscReal :: dndxi(4)
        PetscReal :: dndeta(4)
        PetscReal :: dndx(4)
        PetscReal :: dndy(4)
        PetscReal :: detj
        integer :: ig
        integer :: jg

        gp(1) = -1.0_PETSC_REAL_KIND/sqrt(3.0_PETSC_REAL_KIND)
        gp(2) =  1.0_PETSC_REAL_KIND/sqrt(3.0_PETSC_REAL_KIND)

        call VecGetArrayRead(ctx%coordinates_local, coordinate_array, ierr)
        if (ierr /= 0) return

        do cell = ctx%c_start, ctx%c_end - 1
            call get_cell_nodes(ctx, cell, coordinate_array, indices, xy, ierr)
            if (ierr /= 0) exit

            do jg = 1, 2
                do ig = 1, 2
                    call shape_quad4(gp(ig), gp(jg), n, dndxi, dndeta)
                    call physical_gradients(xy, dndxi, dndeta, dndx, dndy, detj)

                    if (detj <= 0.0_PETSC_REAL_KIND) then
                        write(*, '(A,I0,A,ES24.16)') "Non-positive detJ in cell ", cell, ": ", detj
                        ierr = PETSC_ERR_ARG_WRONG
                        exit
                    end if
                end do

                if (ierr /= 0) exit
            end do

            if (ierr /= 0) exit
        end do

        call VecRestoreArrayRead(ctx%coordinates_local, coordinate_array, ierr)
    end subroutine validate_q4_mesh


    subroutine validate_boundary_labels(ctx, ierr)
        implicit none

        type(AppCtx), intent(in) :: ctx
        PetscErrorCode, intent(inout) :: ierr

        PetscBool :: has_label
        integer :: ib

        do ib = 1, int(ctx%n_boundary)
            call DMHasLabel(ctx%dm, trim(ctx%boundaries(ib)%label_name), has_label, ierr)
            if (ierr /= 0) return

            if (.not. has_label) then
                write(*, '(A,A)') "Missing Gmsh Physical Group / DMLabel: ", trim(ctx%boundaries(ib)%label_name)
                ierr = PETSC_ERR_ARG_WRONG
                return
            end if
        end do
    end subroutine validate_boundary_labels


    subroutine append_unique_dirichlet(indices, values, count, index, value, ierr)
        implicit none

        PetscInt, intent(inout) :: indices(:)
        PetscReal, intent(inout) :: values(:)
        integer, intent(inout) :: count
        PetscInt, intent(in) :: index
        PetscReal, intent(in) :: value
        PetscErrorCode, intent(inout) :: ierr

        PetscReal :: tolerance
        integer :: i

        tolerance = 100.0_PETSC_REAL_KIND*epsilon(1.0_PETSC_REAL_KIND)*max(1.0_PETSC_REAL_KIND, abs(value))

        do i = 1, count
            if (indices(i) /= index) cycle

            if (abs(values(i) - value) > tolerance) then
                write(*, '(A,I0)') "Conflicting Dirichlet values at DOF ", index
                ierr = PETSC_ERR_ARG_WRONG
            end if

            return
        end do

        count = count + 1

        if (count > size(indices)) then
            ierr = PETSC_ERR_ARG_SIZ
            return
        end if

        indices(count) = index
        values(count) = value
    end subroutine append_unique_dirichlet


    subroutine setup_dirichlet_dofs(ctx, ierr)
        implicit none

        type(AppCtx), intent(inout) :: ctx
        PetscErrorCode, intent(inout) :: ierr

        PetscScalar, pointer :: coordinate_array(:)
        PetscInt, allocatable :: temp_indices(:)
        PetscReal, allocatable :: temp_values(:)
        PetscInt :: edge
        PetscInt :: marker
        PetscInt :: edge_indices(2)
        PetscReal :: edge_xy(2,2)
        integer :: max_count
        integer :: count
        integer :: ib
        integer :: a

        max_count = max(1, int(ctx%v_end - ctx%v_start))

        allocate(temp_indices(max_count))
        allocate(temp_values(max_count))

        temp_indices = -1_PETSC_INT_KIND
        temp_values = 0.0_PETSC_REAL_KIND
        count = 0

        call VecGetArrayRead(ctx%coordinates_local, coordinate_array, ierr)
        if (ierr /= 0) then
            deallocate(temp_indices)
            deallocate(temp_values)
            return
        end if

        do ib = 1, int(ctx%n_boundary)
            if (ctx%boundaries(ib)%kind /= BC_DIRICHLET) cycle

            do edge = ctx%f_start, ctx%f_end - 1
                call DMGetLabelValue(ctx%dm, trim(ctx%boundaries(ib)%label_name), edge, marker, ierr)
                if (ierr /= 0) exit

                if (marker < 0_PETSC_INT_KIND) cycle

                call get_edge_nodes(ctx, edge, coordinate_array, edge_indices, edge_xy, ierr)
                if (ierr /= 0) exit

                do a = 1, 2
                    call append_unique_dirichlet( &
                        temp_indices, &
                        temp_values, &
                        count, &
                        edge_indices(a), &
                        ctx%boundaries(ib)%value, &
                        ierr &
                    )

                    if (ierr /= 0) exit
                end do

                if (ierr /= 0) exit
            end do

            if (ierr /= 0) exit
        end do

        call VecRestoreArrayRead(ctx%coordinates_local, coordinate_array, ierr)

        if (ierr /= 0) then
            deallocate(temp_indices)
            deallocate(temp_values)
            return
        end if

        ctx%nbc = int(count, PETSC_INT_KIND)

        if (allocated(ctx%bc_indices)) deallocate(ctx%bc_indices)
        if (allocated(ctx%bc_values)) deallocate(ctx%bc_values)

        allocate(ctx%bc_indices(count))
        allocate(ctx%bc_values(count))

        if (count > 0) then
            ctx%bc_indices = temp_indices(1:count)
            ctx%bc_values = temp_values(1:count)
        end if

        deallocate(temp_indices)
        deallocate(temp_values)

        write(*, '(A,I0)') "Dirichlet DOF count = ", count
    end subroutine setup_dirichlet_dofs


    subroutine initialize_temperature(ctx, x, ierr)
        implicit none

        type(AppCtx), intent(in) :: ctx
        Vec :: x
        PetscErrorCode, intent(inout) :: ierr

        PetscScalar, pointer :: x_array(:)
        PetscScalar, pointer :: old_array(:)
        integer :: ibc

        call VecSet(x, ctx%t_initial, ierr)
        if (ierr /= 0) return

        call VecSet(ctx%t_old, ctx%t_initial, ierr)
        if (ierr /= 0) return

        if (ctx%nbc <= 0_PETSC_INT_KIND) return

        call VecGetArray(x, x_array, ierr)
        if (ierr /= 0) return

        call VecGetArray(ctx%t_old, old_array, ierr)
        if (ierr /= 0) then
            call VecRestoreArray(x, x_array, ierr)
            return
        end if

        do ibc = 1, int(ctx%nbc)
            x_array(ctx%bc_indices(ibc) + 1) = ctx%bc_values(ibc)
            old_array(ctx%bc_indices(ibc) + 1) = ctx%bc_values(ibc)
        end do

        call VecRestoreArray(ctx%t_old, old_array, ierr)
        if (ierr /= 0) return

        call VecRestoreArray(x, x_array, ierr)
    end subroutine initialize_temperature


    subroutine assemble_boundary_residual(ctx, x_array, f_array, ierr)
        implicit none

        type(AppCtx), intent(in) :: ctx
        PetscScalar, intent(in) :: x_array(:)
        PetscScalar, intent(inout) :: f_array(:)
        PetscErrorCode, intent(inout) :: ierr

        PetscScalar, pointer :: coordinate_array(:)
        PetscInt :: edge
        PetscInt :: marker
        PetscInt :: indices(2)
        PetscReal :: xy(2,2)
        PetscScalar :: te(2)
        PetscReal :: gp(2)
        PetscReal :: n(2)
        PetscReal :: dxds
        PetscReal :: dyds
        PetscReal :: jac_line
        PetscReal :: tgp
        PetscReal :: flux
        integer :: ib
        integer :: ig
        integer :: a

        gp(1) = -1.0_PETSC_REAL_KIND/sqrt(3.0_PETSC_REAL_KIND)
        gp(2) =  1.0_PETSC_REAL_KIND/sqrt(3.0_PETSC_REAL_KIND)

        call VecGetArrayRead(ctx%coordinates_local, coordinate_array, ierr)
        if (ierr /= 0) return

        do ib = 1, int(ctx%n_boundary)
            if (ctx%boundaries(ib)%kind == BC_DIRICHLET) cycle

            do edge = ctx%f_start, ctx%f_end - 1
                call DMGetLabelValue(ctx%dm, trim(ctx%boundaries(ib)%label_name), edge, marker, ierr)
                if (ierr /= 0) exit

                if (marker < 0_PETSC_INT_KIND) cycle

                call get_edge_nodes(ctx, edge, coordinate_array, indices, xy, ierr)
                if (ierr /= 0) exit

                te(1) = x_array(indices(1) + 1)
                te(2) = x_array(indices(2) + 1)

                dxds = 0.5_PETSC_REAL_KIND*(xy(1,2) - xy(1,1))
                dyds = 0.5_PETSC_REAL_KIND*(xy(2,2) - xy(2,1))
                jac_line = sqrt(dxds*dxds + dyds*dyds)

                do ig = 1, 2
                    n(1) = 0.5_PETSC_REAL_KIND*(1.0_PETSC_REAL_KIND - gp(ig))
                    n(2) = 0.5_PETSC_REAL_KIND*(1.0_PETSC_REAL_KIND + gp(ig))

                    select case (ctx%boundaries(ib)%kind)
                    case (BC_NEUMANN)
                        flux = ctx%boundaries(ib)%value
                    case (BC_ROBIN)
                        tgp = real(sum(n*te), PETSC_REAL_KIND)
                        flux = ctx%boundaries(ib)%coefficient*(tgp - ctx%boundaries(ib)%ambient)
                    case default
                        flux = 0.0_PETSC_REAL_KIND
                    end select

                    do a = 1, 2
                        f_array(indices(a) + 1) = f_array(indices(a) + 1) + n(a)*flux*jac_line
                    end do
                end do
            end do

            if (ierr /= 0) exit
        end do

        call VecRestoreArrayRead(ctx%coordinates_local, coordinate_array, ierr)
    end subroutine assemble_boundary_residual


    subroutine assemble_boundary_jacobian(ctx, jac, ierr)
        implicit none

        type(AppCtx), intent(in) :: ctx
        Mat :: jac
        PetscErrorCode, intent(inout) :: ierr

        PetscScalar, pointer :: coordinate_array(:)
        PetscInt :: edge
        PetscInt :: marker
        PetscInt :: indices(2)
        PetscReal :: xy(2,2)
        PetscReal :: gp(2)
        PetscReal :: n(2)
        PetscReal :: dxds
        PetscReal :: dyds
        PetscReal :: jac_line
        PetscScalar :: je(2,2)
        integer :: ib
        integer :: ig
        integer :: a
        integer :: b

        gp(1) = -1.0_PETSC_REAL_KIND/sqrt(3.0_PETSC_REAL_KIND)
        gp(2) =  1.0_PETSC_REAL_KIND/sqrt(3.0_PETSC_REAL_KIND)

        call VecGetArrayRead(ctx%coordinates_local, coordinate_array, ierr)
        if (ierr /= 0) return

        do ib = 1, int(ctx%n_boundary)
            if (ctx%boundaries(ib)%kind /= BC_ROBIN) cycle

            do edge = ctx%f_start, ctx%f_end - 1
                call DMGetLabelValue(ctx%dm, trim(ctx%boundaries(ib)%label_name), edge, marker, ierr)
                if (ierr /= 0) exit

                if (marker < 0_PETSC_INT_KIND) cycle

                call get_edge_nodes(ctx, edge, coordinate_array, indices, xy, ierr)
                if (ierr /= 0) exit

                dxds = 0.5_PETSC_REAL_KIND*(xy(1,2) - xy(1,1))
                dyds = 0.5_PETSC_REAL_KIND*(xy(2,2) - xy(2,1))
                jac_line = sqrt(dxds*dxds + dyds*dyds)

                je = 0.0_PETSC_REAL_KIND

                do ig = 1, 2
                    n(1) = 0.5_PETSC_REAL_KIND*(1.0_PETSC_REAL_KIND - gp(ig))
                    n(2) = 0.5_PETSC_REAL_KIND*(1.0_PETSC_REAL_KIND + gp(ig))

                    do a = 1, 2
                        do b = 1, 2
                            je(a,b) = je(a,b) + ctx%boundaries(ib)%coefficient*n(a)*n(b)*jac_line
                        end do
                    end do
                end do

                call MatSetValues(jac, 2_PETSC_INT_KIND, indices, 2_PETSC_INT_KIND, indices, je, ADD_VALUES, ierr)
                if (ierr /= 0) exit
            end do

            if (ierr /= 0) exit
        end do

        call VecRestoreArrayRead(ctx%coordinates_local, coordinate_array, ierr)
    end subroutine assemble_boundary_jacobian


    subroutine inverse_map_quad4(xy, xp, yp, xi, eta, inside, ierr)
        implicit none

        PetscReal, intent(in) :: xy(2,4)
        PetscReal, intent(in) :: xp
        PetscReal, intent(in) :: yp
        PetscReal, intent(inout) :: xi
        PetscReal, intent(inout) :: eta
        logical, intent(inout) :: inside
        PetscErrorCode, intent(inout) :: ierr

        PetscReal :: n(4)
        PetscReal :: dndxi(4)
        PetscReal :: dndeta(4)
        PetscReal :: x
        PetscReal :: y
        PetscReal :: rx
        PetscReal :: ry
        PetscReal :: dx_dxi
        PetscReal :: dx_deta
        PetscReal :: dy_dxi
        PetscReal :: dy_deta
        PetscReal :: detj
        PetscReal :: delta_xi
        PetscReal :: delta_eta
        PetscReal :: element_scale
        PetscReal :: physical_tolerance
        PetscReal :: reference_tolerance
        integer :: iter
        integer :: a

        xi = 0.0_PETSC_REAL_KIND
        eta = 0.0_PETSC_REAL_KIND
        inside = .false.

        element_scale = max( &
            sqrt((xy(1,2) - xy(1,1))**2 + (xy(2,2) - xy(2,1))**2), &
            sqrt((xy(1,3) - xy(1,2))**2 + (xy(2,3) - xy(2,2))**2), &
            sqrt((xy(1,4) - xy(1,3))**2 + (xy(2,4) - xy(2,3))**2), &
            sqrt((xy(1,1) - xy(1,4))**2 + (xy(2,1) - xy(2,4))**2) &
        )

        physical_tolerance = max( &
            1.0e-12_PETSC_REAL_KIND*max(1.0_PETSC_REAL_KIND, element_scale), &
            100.0_PETSC_REAL_KIND*epsilon(1.0_PETSC_REAL_KIND)*max(1.0_PETSC_REAL_KIND, maxval(abs(xy))) &
        )

        reference_tolerance = 1.0e-10_PETSC_REAL_KIND

        do iter = 1, 30
            call shape_quad4(xi, eta, n, dndxi, dndeta)

            x = sum(n*xy(1,:))
            y = sum(n*xy(2,:))

            rx = x - xp
            ry = y - yp

            if (sqrt(rx*rx + ry*ry) <= physical_tolerance) exit

            dx_dxi = 0.0_PETSC_REAL_KIND
            dx_deta = 0.0_PETSC_REAL_KIND
            dy_dxi = 0.0_PETSC_REAL_KIND
            dy_deta = 0.0_PETSC_REAL_KIND

            do a = 1, 4
                dx_dxi = dx_dxi + dndxi(a)*xy(1,a)
                dx_deta = dx_deta + dndeta(a)*xy(1,a)
                dy_dxi = dy_dxi + dndxi(a)*xy(2,a)
                dy_deta = dy_deta + dndeta(a)*xy(2,a)
            end do

            detj = dx_dxi*dy_deta - dx_deta*dy_dxi

            if (abs(detj) <= epsilon(1.0_PETSC_REAL_KIND)*max(1.0_PETSC_REAL_KIND, element_scale*element_scale)) then
                ierr = PETSC_ERR_ARG_WRONG
                return
            end if

            delta_xi = (dy_deta*rx - dx_deta*ry)/detj
            delta_eta = (-dy_dxi*rx + dx_dxi*ry)/detj

            xi = xi - delta_xi
            eta = eta - delta_eta

            if (abs(xi) > 10.0_PETSC_REAL_KIND .or. abs(eta) > 10.0_PETSC_REAL_KIND) then
                inside = .false.
                return
            end if
        end do

        call shape_quad4(xi, eta, n, dndxi, dndeta)

        x = sum(n*xy(1,:))
        y = sum(n*xy(2,:))

        rx = x - xp
        ry = y - yp

        if (sqrt(rx*rx + ry*ry) > physical_tolerance) then
            inside = .false.
            return
        end if

        inside = &
            xi >= -1.0_PETSC_REAL_KIND - reference_tolerance &
            .and. xi <= 1.0_PETSC_REAL_KIND + reference_tolerance &
            .and. eta >= -1.0_PETSC_REAL_KIND - reference_tolerance &
            .and. eta <= 1.0_PETSC_REAL_KIND + reference_tolerance
    end subroutine inverse_map_quad4


    subroutine setup_probe_interpolation(ctx, ierr)
        implicit none

        type(AppCtx), intent(inout) :: ctx
        PetscErrorCode, intent(inout) :: ierr

        PetscScalar, pointer :: coordinate_array(:)
        PetscInt :: cell
        PetscInt :: indices(4)
        PetscReal :: xy(2,4)
        PetscReal :: n(4)
        PetscReal :: dndxi(4)
        PetscReal :: dndeta(4)
        PetscReal :: xi
        PetscReal :: eta
        PetscReal :: xmin
        PetscReal :: xmax
        PetscReal :: ymin
        PetscReal :: ymax
        PetscReal :: bbox_tolerance
        logical :: inside
        logical :: found
        integer :: p

        if (ctx%nprobe <= 0_PETSC_INT_KIND) return

        if (allocated(ctx%probe_indices)) deallocate(ctx%probe_indices)
        if (allocated(ctx%probe_weights)) deallocate(ctx%probe_weights)
        if (allocated(ctx%probe_cells)) deallocate(ctx%probe_cells)
        if (allocated(ctx%probe_xi)) deallocate(ctx%probe_xi)
        if (allocated(ctx%probe_eta)) deallocate(ctx%probe_eta)

        allocate(ctx%probe_indices(4,int(ctx%nprobe)))
        allocate(ctx%probe_weights(4,int(ctx%nprobe)))
        allocate(ctx%probe_cells(int(ctx%nprobe)))
        allocate(ctx%probe_xi(int(ctx%nprobe)))
        allocate(ctx%probe_eta(int(ctx%nprobe)))

        ctx%probe_indices = -1_PETSC_INT_KIND
        ctx%probe_weights = 0.0_PETSC_REAL_KIND
        ctx%probe_cells = -1_PETSC_INT_KIND
        ctx%probe_xi = 0.0_PETSC_REAL_KIND
        ctx%probe_eta = 0.0_PETSC_REAL_KIND

        call VecGetArrayRead(ctx%coordinates_local, coordinate_array, ierr)
        if (ierr /= 0) return

        do p = 1, int(ctx%nprobe)
            found = .false.

            do cell = ctx%c_start, ctx%c_end - 1
                call get_cell_nodes(ctx, cell, coordinate_array, indices, xy, ierr)
                if (ierr /= 0) exit

                xmin = minval(xy(1,:))
                xmax = maxval(xy(1,:))
                ymin = minval(xy(2,:))
                ymax = maxval(xy(2,:))

                bbox_tolerance = 1.0e-10_PETSC_REAL_KIND*max( &
                    1.0_PETSC_REAL_KIND, &
                    xmax - xmin, &
                    ymax - ymin, &
                    abs(ctx%probe_x(p)), &
                    abs(ctx%probe_y(p)) &
                )

                if (ctx%probe_x(p) < xmin - bbox_tolerance) cycle
                if (ctx%probe_x(p) > xmax + bbox_tolerance) cycle
                if (ctx%probe_y(p) < ymin - bbox_tolerance) cycle
                if (ctx%probe_y(p) > ymax + bbox_tolerance) cycle

                xi = 0.0_PETSC_REAL_KIND
                eta = 0.0_PETSC_REAL_KIND
                inside = .false.

                call inverse_map_quad4( &
                    xy, &
                    ctx%probe_x(p), &
                    ctx%probe_y(p), &
                    xi, &
                    eta, &
                    inside, &
                    ierr &
                )

                if (ierr /= 0) exit
                if (.not. inside) cycle

                call shape_quad4(xi, eta, n, dndxi, dndeta)

                ctx%probe_indices(:,p) = indices
                ctx%probe_weights(:,p) = n
                ctx%probe_cells(p) = cell
                ctx%probe_xi(p) = xi
                ctx%probe_eta(p) = eta

                found = .true.
                exit
            end do

            if (ierr /= 0) exit

            if (.not. found) then
                write(*, '(A,I0,A,2ES24.16)') &
                    "Probe point is outside the Q4 mesh. Probe ", &
                    p, &
                    ": ", &
                    ctx%probe_x(p), &
                    ctx%probe_y(p)

                ierr = PETSC_ERR_ARG_WRONG
                exit
            end if

            write(*, '(A,I0,A,2ES14.6,A,I0,A,2ES14.6)') &
                "Probe ", &
                p, &
                ": x,y = ", &
                ctx%probe_x(p), &
                ctx%probe_y(p), &
                ", cell = ", &
                ctx%probe_cells(p), &
                ", xi,eta = ", &
                ctx%probe_xi(p), &
                ctx%probe_eta(p)
        end do

        call VecRestoreArrayRead(ctx%coordinates_local, coordinate_array, ierr)
    end subroutine setup_probe_interpolation


    subroutine FormFunction(snes, x, f, ctx, ierr)
        implicit none

        SNES :: snes
        Vec :: x
        Vec :: f
        type(AppCtx) :: ctx
        PetscErrorCode :: ierr

        PetscScalar, pointer :: x_array(:)
        PetscScalar, pointer :: old_array(:)
        PetscScalar, pointer :: f_array(:)
        PetscScalar, pointer :: coordinate_array(:)
        PetscInt :: cell
        PetscInt :: indices(4)
        PetscReal :: xy(2,4)
        PetscScalar :: te(4)
        PetscScalar :: te_old(4)
        PetscScalar :: re(4)
        integer :: a
        integer :: ibc

        ierr = 0

        call VecGetArrayRead(x, x_array, ierr)
        if (ierr /= 0) return

        call VecGetArrayRead(ctx%t_old, old_array, ierr)
        if (ierr /= 0) return

        call VecGetArray(f, f_array, ierr)
        if (ierr /= 0) return

        call VecGetArrayRead(ctx%coordinates_local, coordinate_array, ierr)
        if (ierr /= 0) return

        f_array = 0.0_PETSC_REAL_KIND

        do cell = ctx%c_start, ctx%c_end - 1
            call get_cell_nodes(ctx, cell, coordinate_array, indices, xy, ierr)
            if (ierr /= 0) return

            do a = 1, 4
                te(a) = x_array(indices(a) + 1)
                te_old(a) = old_array(indices(a) + 1)
            end do

            call element_residual(ctx, xy, te, te_old, re)

            do a = 1, 4
                f_array(indices(a) + 1) = f_array(indices(a) + 1) + re(a)
            end do
        end do

        call VecRestoreArrayRead(ctx%coordinates_local, coordinate_array, ierr)
        if (ierr /= 0) return

        call assemble_boundary_residual(ctx, x_array, f_array, ierr)
        if (ierr /= 0) return

        do ibc = 1, int(ctx%nbc)
            f_array(ctx%bc_indices(ibc) + 1) = &
                x_array(ctx%bc_indices(ibc) + 1) - ctx%bc_values(ibc)
        end do

        call VecRestoreArray(f, f_array, ierr)
        if (ierr /= 0) return

        call VecRestoreArrayRead(ctx%t_old, old_array, ierr)
        if (ierr /= 0) return

        call VecRestoreArrayRead(x, x_array, ierr)
    end subroutine FormFunction


    subroutine FormJacobian(snes, x, jac, jac_prec, ctx, ierr)
        implicit none

        SNES :: snes
        Vec :: x
        Mat :: jac
        Mat :: jac_prec
        type(AppCtx) :: ctx
        PetscErrorCode :: ierr

        PetscScalar, pointer :: x_array(:)
        PetscScalar, pointer :: coordinate_array(:)
        PetscInt :: cell
        PetscInt :: indices(4)
        PetscReal :: xy(2,4)
        PetscScalar :: te(4)
        PetscScalar :: je(4,4)
        integer :: a

        ierr = 0

        call MatZeroEntries(jac, ierr)
        if (ierr /= 0) return

        call VecGetArrayRead(x, x_array, ierr)
        if (ierr /= 0) return

        call VecGetArrayRead(ctx%coordinates_local, coordinate_array, ierr)
        if (ierr /= 0) return

        do cell = ctx%c_start, ctx%c_end - 1
            call get_cell_nodes(ctx, cell, coordinate_array, indices, xy, ierr)
            if (ierr /= 0) return

            do a = 1, 4
                te(a) = x_array(indices(a) + 1)
            end do

            call element_jacobian(ctx, xy, te, je)

            call MatSetValues(jac, 4_PETSC_INT_KIND, indices, 4_PETSC_INT_KIND, indices, je, ADD_VALUES, ierr)
            if (ierr /= 0) return
        end do

        call VecRestoreArrayRead(ctx%coordinates_local, coordinate_array, ierr)
        if (ierr /= 0) return

        call assemble_boundary_jacobian(ctx, jac, ierr)
        if (ierr /= 0) return

        call VecRestoreArrayRead(x, x_array, ierr)
        if (ierr /= 0) return

        call MatAssemblyBegin(jac, MAT_FINAL_ASSEMBLY, ierr)
        if (ierr /= 0) return

        call MatAssemblyEnd(jac, MAT_FINAL_ASSEMBLY, ierr)
        if (ierr /= 0) return

        if (ctx%nbc > 0_PETSC_INT_KIND) then
            call MatZeroRows( &
                jac, &
                ctx%nbc, &
                ctx%bc_indices, &
                1.0_PETSC_REAL_KIND, &
                PETSC_NULL_VEC, &
                PETSC_NULL_VEC, &
                ierr &
            )
        end if
    end subroutine FormJacobian


    subroutine configure_solver(ctx, snes, ierr)
        implicit none

        type(AppCtx), intent(in) :: ctx
        SNES :: snes
        PetscErrorCode, intent(inout) :: ierr

        SNESLineSearch :: linesearch
        KSP :: ksp
        PC :: pc
        character(len=32) :: snes_type_lower

        call SNESSetType(snes, trim(ctx%solver%snes_type), ierr)
        if (ierr /= 0) return

        snes_type_lower = trim(lower_string(ctx%solver%snes_type))

        if (trim(snes_type_lower) == "newtonls") then
            call SNESGetLineSearch(snes, linesearch, ierr)
            if (ierr /= 0) return

            call SNESLineSearchSetType(linesearch, trim(ctx%solver%linesearch_type), ierr)
            if (ierr /= 0) return
        end if

        call SNESSetTolerances( &
            snes, &
            ctx%solver%snes_atol, &
            ctx%solver%snes_rtol, &
            ctx%solver%snes_stol, &
            ctx%solver%snes_max_it, &
            PETSC_CURRENT_INTEGER, &
            ierr &
        )
        if (ierr /= 0) return

        call SNESGetKSP(snes, ksp, ierr)
        if (ierr /= 0) return

        call KSPSetType(ksp, trim(ctx%solver%ksp_type), ierr)
        if (ierr /= 0) return

        call KSPSetTolerances( &
            ksp, &
            ctx%solver%ksp_rtol, &
            ctx%solver%ksp_atol, &
            PETSC_CURRENT_REAL, &
            ctx%solver%ksp_max_it, &
            ierr &
        )
        if (ierr /= 0) return

        call KSPGetPC(ksp, pc, ierr)
        if (ierr /= 0) return

        call PCSetType(pc, trim(ctx%solver%pc_type), ierr)
    end subroutine configure_solver


    subroutine write_solution(ctx, x, step, time, ierr)
        implicit none

        type(AppCtx), intent(in) :: ctx
        Vec :: x
        PetscInt, intent(in) :: step
        PetscReal, intent(in) :: time
        PetscErrorCode, intent(inout) :: ierr

        PetscViewer :: viewer
        character(len=512) :: filename

        write(filename, '(A,"/temperature_",I6.6,".vtu")') trim(ctx%output_directory), step

        call DMSetOutputSequenceNumber(ctx%dm, step, time, ierr)
        if (ierr /= 0) return

        call PetscViewerVTKOpen(PETSC_COMM_SELF, trim(filename), FILE_MODE_WRITE, viewer, ierr)
        if (ierr /= 0) return

        call VecView(x, viewer, ierr)
        if (ierr /= 0) return

        call PetscViewerDestroy(viewer, ierr)
    end subroutine write_solution


    subroutine write_profile_csv(ctx, x, step, time, ierr)
        implicit none

        type(AppCtx), intent(in) :: ctx
        Vec :: x
        PetscInt, intent(in) :: step
        PetscReal, intent(in) :: time
        PetscErrorCode, intent(inout) :: ierr

        PetscScalar, pointer :: x_array(:)
        PetscScalar, pointer :: coordinate_array(:)
        PetscInt :: vertex
        PetscInt :: global_offset
        PetscInt :: coordinate_offset
        PetscReal :: xcoord
        PetscReal :: ycoord
        PetscReal :: t
        PetscReal :: qice
        PetscReal :: fr
        PetscReal :: lambda
        PetscReal :: cp
        PetscReal :: ca
        character(len=512) :: filename
        integer :: unit

        if (.not. ctx%profile_enabled) return

        call VecGetArrayRead(x, x_array, ierr)
        if (ierr /= 0) return

        call VecGetArrayRead(ctx%coordinates_local, coordinate_array, ierr)
        if (ierr /= 0) return

        write(filename, '(A,"/profile_",I6.6,".csv")') trim(ctx%output_directory), step

        open(newunit=unit, file=trim(filename), status="replace", action="write")
        write(unit, '(A)') "x,y,T,Qice,Fr,lambda,Cp,Ca,time_s,time_day"

        do vertex = ctx%v_start, ctx%v_end - 1
            call PetscSectionGetOffset(ctx%coordinate_section, vertex, coordinate_offset, ierr)
            if (ierr /= 0) exit

            xcoord = real(coordinate_array(coordinate_offset + 1), PETSC_REAL_KIND)
            ycoord = real(coordinate_array(coordinate_offset + 2), PETSC_REAL_KIND)

            if (abs(ycoord - ctx%profile_y) > ctx%profile_tolerance) cycle

            call PetscSectionGetOffset(ctx%global_section, vertex, global_offset, ierr)
            if (ierr /= 0) exit

            t = real(x_array(global_offset + 1), PETSC_REAL_KIND)
            qice = ice_content(t, ctx)
            fr = qice/ctx%phi
            lambda = thermal_conductivity(t, ctx)
            cp = volumetric_heat_capacity(t, ctx)
            ca = apparent_heat_capacity(t, ctx)

            write(unit, '(10(ES24.16,:,","))') &
                xcoord, &
                ycoord, &
                t, &
                qice, &
                fr, &
                lambda, &
                cp, &
                ca, &
                time, &
                time/86400.0_PETSC_REAL_KIND
        end do

        close(unit)

        call VecRestoreArrayRead(ctx%coordinates_local, coordinate_array, ierr)
        if (ierr /= 0) return

        call VecRestoreArrayRead(x, x_array, ierr)
    end subroutine write_profile_csv


    subroutine append_probe_history(ctx, x, step, time, ierr)
        implicit none

        type(AppCtx), intent(in) :: ctx
        Vec :: x
        PetscInt, intent(in) :: step
        PetscReal, intent(in) :: time
        PetscErrorCode, intent(inout) :: ierr

        PetscScalar, pointer :: x_array(:)
        PetscReal, allocatable :: probe_t(:)
        PetscScalar :: value
        character(len=512) :: filename
        integer :: p
        integer :: a
        integer :: unit

        if (ctx%nprobe <= 0_PETSC_INT_KIND) return

        if (.not. allocated(ctx%probe_indices)) then
            ierr = PETSC_ERR_ARG_WRONG
            return
        end if

        allocate(probe_t(int(ctx%nprobe)))
        probe_t = 0.0_PETSC_REAL_KIND

        call VecGetArrayRead(x, x_array, ierr)
        if (ierr /= 0) then
            deallocate(probe_t)
            return
        end if

        do p = 1, int(ctx%nprobe)
            value = 0.0_PETSC_REAL_KIND

            do a = 1, 4
                value = value &
                    + ctx%probe_weights(a,p) &
                    * x_array(ctx%probe_indices(a,p) + 1)
            end do

            probe_t(p) = real(value, PETSC_REAL_KIND)
        end do

        call VecRestoreArrayRead(x, x_array, ierr)
        if (ierr /= 0) then
            deallocate(probe_t)
            return
        end if

        filename = trim(ctx%output_directory) // "/probe_temperature.csv"

        if (step == 0_PETSC_INT_KIND) then
            open(newunit=unit, file=trim(filename), status="replace", action="write")

            write(unit, '(A)', advance="no") "step,time_s,time_day"

            do p = 1, int(ctx%nprobe)
                write(unit, '(",T_probe_",I0)', advance="no") p
            end do

            write(unit, *)
        else
            open(newunit=unit, file=trim(filename), status="old", action="write", position="append")
        end if

        write(unit, '(I0)', advance="no") step
        write(unit, '(",",ES24.16)', advance="no") time
        write(unit, '(",",ES24.16)', advance="no") time/86400.0_PETSC_REAL_KIND

        do p = 1, int(ctx%nprobe)
            write(unit, '(",",ES24.16)', advance="no") probe_t(p)
        end do

        write(unit, *)
        close(unit)

        deallocate(probe_t)
    end subroutine append_probe_history


    subroutine report_temperature_range(x, step, time, ierr)
        implicit none

        Vec :: x
        PetscInt, intent(in) :: step
        PetscReal, intent(in) :: time
        PetscErrorCode, intent(inout) :: ierr

        PetscInt :: index_min
        PetscInt :: index_max
        PetscReal :: tmin
        PetscReal :: tmax

        call VecMin(x, index_min, tmin, ierr)
        if (ierr /= 0) return

        call VecMax(x, index_max, tmax, ierr)
        if (ierr /= 0) return

        write(*, '(A,I0,A,ES12.4,A,ES16.8,A,ES16.8)') &
            "Step = ", &
            step, &
            ", time_s = ", &
            time, &
            ", Tmin = ", &
            tmin, &
            ", Tmax = ", &
            tmax
    end subroutine report_temperature_range


    subroutine destroy_context_arrays(ctx)
        implicit none

        type(AppCtx), intent(inout) :: ctx

        if (allocated(ctx%boundaries)) deallocate(ctx%boundaries)
        if (allocated(ctx%bc_indices)) deallocate(ctx%bc_indices)
        if (allocated(ctx%bc_values)) deallocate(ctx%bc_values)
        if (allocated(ctx%probe_x)) deallocate(ctx%probe_x)
        if (allocated(ctx%probe_y)) deallocate(ctx%probe_y)
        if (allocated(ctx%probe_indices)) deallocate(ctx%probe_indices)
        if (allocated(ctx%probe_weights)) deallocate(ctx%probe_weights)
        if (allocated(ctx%probe_cells)) deallocate(ctx%probe_cells)
        if (allocated(ctx%probe_xi)) deallocate(ctx%probe_xi)
        if (allocated(ctx%probe_eta)) deallocate(ctx%probe_eta)
    end subroutine destroy_context_arrays

end module freezing_model


program freezing_plex
    use petsc
    use petscdm
    use petscdmplex
    use petscsnes
    use petscksp
    use freezing_model
    implicit none

    PetscErrorCode :: ierr
    type(AppCtx) :: ctx
    Vec :: temperature
    Vec :: residual
    Mat :: jacobian
    SNES :: snes
    PetscInt :: step
    PetscInt :: nsteps
    PetscInt :: iterations
    PetscReal :: time

    PetscCallA(PetscInitialize(ierr))

    call read_input_file("input.nml", ctx)
    call ensure_output_directory(ctx)

    call load_mesh(ctx, ierr)
    if (ierr /= 0) error stop "Failed to load the Gmsh mesh."

    call setup_vertex_section(ctx, ierr)
    if (ierr /= 0) error stop "Failed to setup the vertex section."

    call validate_q4_mesh(ctx, ierr)
    if (ierr /= 0) error stop "The mesh must contain valid 2D Q4 elements only."

    call validate_boundary_labels(ctx, ierr)
    if (ierr /= 0) error stop "Boundary label validation failed."

    call setup_dirichlet_dofs(ctx, ierr)
    if (ierr /= 0) error stop "Failed to setup Dirichlet DOFs."

    call setup_probe_interpolation(ctx, ierr)
    if (ierr /= 0) error stop "Failed to setup Q4 probe interpolation."

    PetscCallA(DMCreateGlobalVector(ctx%dm, temperature, ierr))
    PetscCallA(PetscObjectSetName(temperature, "temperature", ierr))
    PetscCallA(VecDuplicate(temperature, ctx%t_old, ierr))
    PetscCallA(VecDuplicate(temperature, residual, ierr))

    call initialize_temperature(ctx, temperature, ierr)
    if (ierr /= 0) error stop "Failed to initialize temperature."

    PetscCallA(DMCreateMatrix(ctx%dm, jacobian, ierr))
    PetscCallA(MatSetOption(jacobian, MAT_KEEP_NONZERO_PATTERN, PETSC_TRUE, ierr))
    PetscCallA(MatSetOption(jacobian, MAT_ROW_ORIENTED, PETSC_FALSE, ierr))

    PetscCallA(SNESCreate(PETSC_COMM_SELF, snes, ierr))
    PetscCallA(SNESSetFunction(snes, residual, FormFunction, ctx, ierr))
    PetscCallA(SNESSetJacobian(snes, jacobian, jacobian, FormJacobian, ctx, ierr))

    call configure_solver(ctx, snes, ierr)
    if (ierr /= 0) error stop "Failed to configure SNES/KSP/PC."

    ! Namelist values are defaults; the PETSc option database overrides them.
    PetscCallA(SNESSetFromOptions(snes, ierr))

    time = 0.0_PETSC_REAL_KIND

    call report_temperature_range(temperature, 0_PETSC_INT_KIND, time, ierr)
    if (ierr /= 0) error stop "Failed to report initial temperature range."

    call write_solution(ctx, temperature, 0_PETSC_INT_KIND, time, ierr)
    if (ierr /= 0) error stop "Failed to write the initial solution."

    call write_profile_csv(ctx, temperature, 0_PETSC_INT_KIND, time, ierr)
    if (ierr /= 0) error stop "Failed to write the initial profile."

    call append_probe_history(ctx, temperature, 0_PETSC_INT_KIND, time, ierr)
    if (ierr /= 0) error stop "Failed to write the initial probes."

    nsteps = int(ctx%t_end/ctx%dt, PETSC_INT_KIND)

    do step = 1_PETSC_INT_KIND, nsteps
        time = min(real(step, PETSC_REAL_KIND)*ctx%dt, ctx%t_end)

        PetscCallA(SNESSolve(snes, PETSC_NULL_VEC, temperature, ierr))
        PetscCallA(SNESGetIterationNumber(snes, iterations, ierr))

        write(*, '(A,I0,A,F12.6,A,I0)') &
            "Step = ", &
            step, &
            ", time = ", &
            time/3600.0_PETSC_REAL_KIND, &
            " h, SNES iterations = ", &
            iterations

        call report_temperature_range(temperature, step, time, ierr)
        if (ierr /= 0) error stop "Failed to report temperature range."

        call append_probe_history(ctx, temperature, step, time, ierr)
        if (ierr /= 0) error stop "Failed to write probe history."

        PetscCallA(VecCopy(temperature, ctx%t_old, ierr))

        if (mod(step, ctx%output_interval) == 0_PETSC_INT_KIND .or. step == nsteps) then
            call write_solution(ctx, temperature, step, time, ierr)
            if (ierr /= 0) error stop "Failed to write the solution."

            call write_profile_csv(ctx, temperature, step, time, ierr)
            if (ierr /= 0) error stop "Failed to write the profile."
        end if
    end do

    PetscCallA(SNESDestroy(snes, ierr))
    PetscCallA(MatDestroy(jacobian, ierr))
    PetscCallA(VecDestroy(residual, ierr))
    PetscCallA(VecDestroy(ctx%t_old, ierr))
    PetscCallA(VecDestroy(temperature, ierr))

    call destroy_context_arrays(ctx)

    PetscCallA(DMDestroy(ctx%dm, ierr))
    PetscCallA(PetscFinalize(ierr))
end program freezing_plex
