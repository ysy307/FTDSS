module Calculate_Update
    use, intrinsic :: iso_fortran_env, only : int32, real64
	use omp_lib
    use :: Types, g => GravityAcceleration
	use :: RootFinding_SecantMethod,  only : Secant_method
	use :: RootFinding_BinaryFinding, only : Binary_finding
    implicit none
    private
    real(real64),   parameter :: threshold = 1.0d-20
	real(real64),   parameter :: TtoK = 273.15d0
	integer(int32), parameter :: Linear = 1, pTransition = 2, NonLinear = 3, nTransition = 4


    public :: Update_Parameters_Heat
	public :: Update_Parameters_Water
	public :: Update_theta
	public :: Update_Si
	public :: Update_Phase_Revise
	public :: Update_Gradient


    contains

    subroutine Update_Parameters_Heat(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver

        call Update_theta(Solver)
		call Update_Variables_Geometric (Solver%Heat%Variables%lambda%pre(:), Solver%Heat%Constants%ThermalConductivity, Solver%Si%pre, Solver%Heat%Constants%Porosity, Solver%Flags)
		call Update_Variables_Arithmetic(Solver%Heat%Variables%rho%pre(:),    Solver%Heat%Constants%Density,             Solver%Si%pre, Solver%Heat%Constants%Porosity, Solver%Flags)
		call Update_Variables_Arithmetic(Solver%Heat%Variables%Cp%pre(:),     Solver%Heat%Constants%HeatCapacity,        Solver%Si%pre, Solver%Heat%Constants%Porosity, Solver%Flags)
		call Update_Variables_Arithmetic(Solver%Heat%Variables%Cs%pre(:),     Solver%Heat%Constants%SpecificHeat,        Solver%Si%pre, Solver%Heat%Constants%Porosity, Solver%Flags)

		if (Solver%Flags%isGCC) then
			call Update_Cpa_GCC(Solver)
		else if (Solver%Flags%isPower) then
			call Update_Cpa_Power(Solver)
		end if
		if (Solver%Flags%isGCC .or. Solver%Flags%isPower) then
			call Update_Phase(Solver)
			call Update_Cadiv(Solver)
		end if
		if (Solver%isWater) then
			call Update_Gradient(Solver, Solver%P%pre(:), Solver%Water%Variables%hGrad)
			call Update_Flux(Solver%Water%Variables%wFlux, Solver%Water%Variables%Klh%pre(:), Solver%Water%Variables%hGrad)
			call Update_Gradient(Solver, Solver%T%pre(:), Solver%Heat%Variables%TGrad)
			! print*,Solver%Heat%Constants%HeatCapacity%water
			! print*,Solver%Water%Variables%wFlux%x(1:30)*Solver%T%pre(1:30)*Solver%Heat%Constants%HeatCapacity%water
			! stop
			call Update_Heat_Flux(Solver%Heat%Variables%TFlux, Solver%Heat%Variables%lambda%pre(:), Solver%Heat%Variables%TGrad, Solver%Heat%Constants%HeatCapacity%water, Solver%Water%Variables%wFlux, Solver%T%pre)
		end if

    end subroutine Update_Parameters_Heat

	subroutine Update_Parameters_Water(Solver)
		implicit none
		type(SolverInfo), intent(inout) :: Solver

		call Update_Variables_Log(Solver%Water%Variables%Klh%pre(:), Solver%Water%Constants%HydraulicConductivity, Solver%Si%pre, Solver%Heat%Constants%Porosity, Solver%Flags)

	end subroutine Update_Parameters_Water

    subroutine Update_theta(Solver)
		implicit none
        type(SolverInfo), intent(inout) :: Solver

		integer(int32) :: iN
        real(real64)   :: Porosity

        Porosity = Solver%Heat%Constants%Porosity

        do iN = 1, Solver%N%node
            Solver%mwater%pre(iN) = Porosity * (1 - Solver%Si%pre(iN))
            ! Water content is limited to the range of threshold to Porosity
            if (Solver%mwater%pre(iN) < Porosity * threshold) then
                Solver%mwater%pre(iN) = Porosity * threshold
            else if (Solver%mwater%pre(iN) > Porosity) then
                Solver%mwater%pre(iN) = Porosity
            end if

            ! Ice content is limited to the range of threshold to Porosity
            Solver%mIce%pre(iN) = Porosity * Solver%Si%pre(iN)
            Solver%mIce%dif(iN) = Solver%mIce%pre(iN) - Solver%mIce%old(iN)
            if (Solver%mIce%dif(iN) > Porosity) then
                Solver%mIce%dif(iN) = Porosity
            else if (Solver%mIce%dif(iN) < - Porosity) then
                Solver%mIce%dif(iN) = - Porosity
            end if
        end do
	end subroutine Update_theta

    subroutine Update_Variables_Arithmetic(Targets, Const, Si, Porosity, Flags)
        implicit none
        real(real64), intent(inout) :: Targets(:)
        type(Phases), intent(in)    :: Const
        real(real64), intent(in)    :: Si(:)
        real(real64), intent(in)    :: Porosity
        type(Flag),   intent(in)    :: Flags

        real(real64)                :: unfrozen, frozen
        integer(int32)              :: iN, nTarget

        nTarget    = size(Targets)
        unfrozen   = Const%soil * (1.0d0 - Porosity) + Const%water * Porosity
        frozen     = Const%soil * (1.0d0 - Porosity) + Const%ice   * Porosity
        if (Flags%isTRM .and. Flags%isSwitchTRM) then
            do iN = 1, nTarget
				if (Si(iN) == 1.0d0) Targets(iN) = frozen
            end do
		else
			do iN = 1, nTarget
				Targets(iN) = Si(iN) * frozen + (1.0d0 - Si(iN)) * unfrozen
			end do
		end if
    end subroutine Update_Variables_Arithmetic

    subroutine Update_Variables_Geometric(Targets, Const, Si, Porosity, Flags)
        implicit none
        real(real64), intent(inout) :: Targets(:)
        type(Phases), intent(in)    :: Const
        real(real64), intent(in)    :: Si(:)
        real(real64), intent(in)    :: Porosity
        type(Flag),   intent(in)    :: Flags

        real(real64)                :: unfrozen, frozen, rtWI
        integer(int32)              :: iN, nTarget

        nTarget  = size(Targets)
		unfrozen = Const%soil ** (1.0d0 - Porosity) * Const%water ** Porosity
		frozen   = Const%soil ** (1.0d0 - Porosity) * Const%ice   ** Porosity
		rtWI     = Const%ice / Const%water

        if (Flags%isTRM .and. Flags%isSwitchTRM) then
            do iN = 1, nTarget
				if (Si(iN) == 1.0d0) Targets(iN) = frozen
            end do
		else
			do iN = 1, nTarget
				Targets(iN) = unfrozen * rtWI **(Porosity * Si(iN))
			end do
		end if
    end subroutine Update_Variables_Geometric

    subroutine Update_Variables_Log(Targets, Const, Si, Porosity, Flags)
        implicit none
        real(real64), intent(inout) :: Targets(:)
        type(Phases), intent(in)    :: Const
        real(real64), intent(in)    :: Si(:)
        real(real64), intent(in)    :: Porosity
        type(Flag),   intent(in)    :: Flags

		real(real64)                :: k
        integer(int32)              :: iN, nTarget

        nTarget  = size(Targets)
		k = log10(Const%ice / Const%soil)

        ! if (Flags%isTRM .and. Flags%isSwitchTRM) then
        !     do iN = 1, nTarget
		! 		if (Si(iN) == 1.0d0) Targets(iN) = Const%soil
        !     end do
		! else
			do iN = 1, nTarget
				Targets(iN) = Const%soil * 10 ** (k * Si(iN))
			end do
		! end if
    end subroutine Update_Variables_Log


	subroutine Update_Cpa_GCC(Solver)
		implicit none
		type(SolverInfo), intent(inout) :: Solver

		real(real64)				    :: Qs, Qr, alpha, n, m, Tf, Lf, Dice, Cp
		real(real64) 				    :: A, B, C, T
		integer(int32)				    :: iN

		Qs    = Solver%Heat%Latent%GCC%thetaS
		Qr    = Solver%Heat%Latent%GCC%thetaR
		alpha = Solver%Heat%Latent%GCC%alpha
		n     = Solver%Heat%Latent%GCC%n
		m     = Solver%Heat%Latent%GCC%m
		Tf    = Solver%Heat%Latent%GCC%Tf
		Lf    = Solver%Heat%Latent%Lf
		Dice  = Solver%Heat%Latent%rhoI

		A = Qs - Qr
		B = alpha * Lf / g

		do iN = 1, Solver%N%node
			T  = Solver%T%pre(iN)
			Cp = Solver%Heat%Variables%Cp%pre(iN)
			if (T < Tf) then
				C = (T + TtoK) / (Tf + TtoK)
				Solver%Heat%Variables%Ca%pre(iN) = Cp - Lf * Dice * (A * B ** 2 * n * m * log(C) * abs(B * log(C)) ** (n - 2.d0)) &
											     / ((T + TtoK) * (1.d0 + abs(B * log(C)) ** n) ** (m + 1.d0))
			else
				Solver%Heat%Variables%Ca%pre(iN) = Cp
			end if
		end do

	end subroutine Update_Cpa_GCC

	subroutine Update_Cpa_Power(Solver)
		implicit none
		type(SolverInfo), intent(inout) :: Solver

		real(real64)				    :: Lf, Dice, phi, Tf, a, Cp, T
		integer(int32)                  :: iN

		Lf   = Solver%Heat%Latent%Lf
		Dice = Solver%Heat%Latent%rhoI
		phi  = Solver%Heat%Latent%Power%phi
		Tf   = Solver%Heat%Latent%Power%Tf
		a    = Solver%Heat%Latent%Power%a

		do iN = 1, Solver%N%node
			T  = Solver%T%pre(iN)
			Cp = Solver%Heat%Variables%Cp%pre(iN)
			if (T < Tf) then
				Solver%Heat%Variables%Ca%pre(iN) = Cp - Lf * Dice * phi * a * (1.0d0 - T + Tf) ** (a-1.0d0)
			else
				Solver%Heat%Variables%Ca%pre(iN) = Cp
			end if
		end do

	end subroutine Update_Cpa_Power

	subroutine Update_Phase(Solver)
		implicit none
		type(SolverInfo), intent(inout) :: Solver

		integer(int32)                  :: iN
		real(real64)                    :: Tp, To

		do iN = 1, Solver%N%node
			Tp = Solver%T%pre(iN)
			To = Solver%T%old(iN)
			if (Tp >= 0.0d0 .and. To >= 0.0d0) then
				Solver%Heat%Variables%Phase(iN) = Linear
			else if (Tp < 0.0d0 .and. To < 0.0d0) then
				Solver%Heat%Variables%Phase(iN) = NonLinear
			else if (Tp < 0.0d0 .and. To >= 0.0d0) then
				Solver%Heat%Variables%Phase(iN) = pTransition
			else
				Solver%Heat%Variables%Phase(iN) = nTransition
			end if
		end do
	end subroutine Update_Phase

	subroutine Update_Phase_Revise(Solver)
		implicit none
		type(SolverInfo), intent(inout) :: Solver

		integer(int32)                  :: iN
		real(real64)                    :: Tp, To

		do iN = 1, Solver%N%node
			Tp = Solver%T%pre(iN)
			To = Solver%T%old(iN)
			if (Tp >= 0.0d0 .and. To >= 0.0d0) then
				Solver%Heat%Variables%Phase(iN) = Linear
			else if (Tp < 0.0d0 .and. To < 0.0d0) then
				Solver%Heat%Variables%Phase(iN) = NonLinear
			else if (Tp < 0.0d0 .and. To >= 0.0d0) then
				Solver%Heat%Variables%Phase(iN) = pTransition
				! call Secant_method(Solver, iN)
				call Binary_finding(Solver, iN)
			else
				Solver%Heat%Variables%Phase(iN) = nTransition
			end if
		end do
	end subroutine Update_Phase_Revise

	subroutine Update_Cadiv(Solver)
		implicit none
		type(SolverInfo), intent(inout) :: Solver
		integer(int32)				    :: iN

		do iN =1, Solver%N%node
			if (Solver%Heat%Variables%Phase(iN) == Linear) then
				Solver%Heat%Variables%Ca%div(iN) =Solver%Heat%Variables%Ca%pre(iN)
			else if (Solver%Heat%Variables%Phase(iN) == pTransition) then
				if (Solver%Flags%isGCC) then
					Solver%Heat%Variables%Ca%div(iN) = (Solver%Heat%Variables%Ca%pre(iN) * Solver%Heat%Latent%GCC%Ca_max)   ** 0.5d0
				else if (Solver%Flags%isPower) then
					Solver%Heat%Variables%Ca%div(iN) = (Solver%Heat%Variables%Ca%pre(iN) * Solver%Heat%Latent%Power%Ca_max) ** 0.5d0
				end if
			else if (Solver%Heat%Variables%Phase(iN) == NonLinear) then
				Solver%Heat%Variables%Ca%div(iN) = (Solver%Heat%Variables%Ca%pre(iN) * Solver%Heat%Variables%Ca%old(iN)) ** 0.5d0
			end if
		end do
	end subroutine Update_Cadiv

	subroutine Update_Si(Solver)
		implicit none
		type(SolverInfo), intent(inout) :: Solver
		real(real64)                    :: A, B, C
		real(real64)				    :: Qs, Qr, alpha, n, m, Lf, Dice, Tf
		integer(int32)                  :: iN

		if (Solver%Flags%isGCC) then
			Qs    = Solver%Heat%Latent%GCC%thetaS
			Qr    = Solver%Heat%Latent%GCC%thetaR
			alpha = Solver%Heat%Latent%GCC%alpha
			n     = Solver%Heat%Latent%GCC%n
			m     = Solver%Heat%Latent%GCC%m
			Lf    = Solver%Heat%Latent%Lf
			Dice  = Solver%Heat%Latent%rhoI
			Tf    = Solver%Heat%Latent%GCC%Tf

			A = Qs - Qr
			B = alpha * Lf / g
			do iN = 1, Solver%N%node
				C = (Solver%T%pre(iN) + TtoK) / (Tf + TtoK)

				if (Solver%T%pre(iN) < Tf) then
					Solver%Si%pre(iN) = (A * (1.0d0 - (1.0d0 + abs(B * log(C)) ** n) ** (- m))) / Qs
				else
					Solver%Si%pre(iN) = 0.0d0
				end if
			end do
		else if (Solver%Flags%isPower) then
			Tf	  = Solver%Heat%Latent%Power%Tf
			alpha = Solver%Heat%Latent%Power%a
			do iN = 1, Solver%N%node
				if (Solver%T%pre(iN) < Tf) then
					Solver%Si%pre(iN) = 1.0d0 - (1.0d0 - Solver%T%pre(iN) + Tf) ** alpha
				else
					Solver%Si%pre(iN) = 0.0d0
				end if
			end do
		end if

	end subroutine Update_Si

	subroutine Update_Gradient(Solver, ar, Grad)
		implicit none
		type(SolverInfo), intent(inout) :: Solver
		real(real64),     intent(in)    :: ar(:)
		type(DP2d),       intent(inout) :: Grad
		integer(int32)                  :: counter(SOlver%N%node)
		integer(int32)                  :: iN, p1, p2, p3
		real(real64)                    :: gx, gy

		counter(:) = 0
		Grad%x(:) = 0.0d0
		Grad%y(:) = 0.0d0

		do iN = 1, Solver%N%element
			p1 = Solver%N%pElement(1, iN)
			p2 = Solver%N%pElement(2, iN)
			p3 = Solver%N%pElement(3, iN)

			gx = (Solver%N%Basis%b(1, iN) * ar(p1) + Solver%N%Basis%b(2, iN) * ar(p2) + Solver%N%Basis%b(3, iN) * ar(p3)) / (2.0d0 * Solver%N%eArea(iN))
			gy = (Solver%N%Basis%c(1, iN) * ar(p1) + Solver%N%Basis%c(2, iN) * ar(p2) + Solver%N%Basis%c(3, iN) * ar(p3)) / (2.0d0 * Solver%N%eArea(iN))
			Grad%x(p1) = Grad%x(p1) + gx
			Grad%y(p1) = Grad%y(p1) + gy
			Grad%x(p2) = Grad%x(p2) + gx
			Grad%y(p2) = Grad%y(p2) + gy
			Grad%x(p3) = Grad%x(p3) + gx
			Grad%y(p3) = Grad%y(p3) + gy

			counter(p1) = counter(p1) + 1
			counter(p2) = counter(p2) + 1
			counter(p3) = counter(p3) + 1
		end do
		Grad%x(:) = Grad%x(:) / counter(:)
		Grad%y(:) = Grad%y(:) / counter(:)

	end subroutine Update_Gradient

	subroutine Update_Flux(Flux, C, Grad)
		implicit none
		type(DP2d),   intent(inout) :: Flux
		type(DP2d),   intent(in)    :: Grad
		real(real64), intent(in)    :: C(:)

		Flux%x(:) = - C(:) * Grad%x(:)
		Flux%y(:) = - C(:) * Grad%y(:)

	end subroutine Update_Flux

	subroutine Update_Heat_Flux(TFlux, C, TGrad, Cw, Qw, T)
		implicit none
		type(DP2d),   intent(inout) :: TFlux
		type(DP2d),   intent(in)    :: TGrad
		real(real64), intent(in)    :: C(:)
		real(real64), intent(in)    :: Cw
		type(DP2d),   intent(in)    :: Qw
		real(real64), intent(in)    :: T(:)

		TFlux%x(:) = - C(:) * TGrad%x(:)
		TFlux%y(:) = - C(:) * TGrad%y(:)
		! print*, TFlux%x(:)
		! stop

	end subroutine Update_Heat_Flux
end module Calculate_Update