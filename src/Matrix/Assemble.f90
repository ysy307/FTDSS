module Matrix_Assemble
    use, intrinsic :: iso_fortran_env, only : int32, real64
    use omp_lib
    use :: Types, udmp => undumped, dmp => dumped
	use :: Allocate_Allocate
	use :: Allocate_Structure
	use :: error
    use :: Matrix_FindInd
    use :: Calculate_Product, only : SpMV => Matrix_Vector_Product_CRS
    implicit none
    private

	real(real64), parameter :: D3  = 0.33333333333333333333333333333d0 ! 1/3
	real(real64), parameter :: D4  = 0.25000000000000000000000000000d0 ! 1/4
	real(real64), parameter :: D12 = 0.08333333333333333333333333333d0 ! 1/12
	real(real64), parameter :: D24 = 0.04166666666666666666666666666d0 ! 1/24

    type(CRS) :: tmpCRS1, tmpCRS2
	real(real64), allocatable :: tmpA(:,:)

    public :: Init_Assemble
    public :: Assemble_GM_Heat
	public :: Assemble_GM_Water
    public :: Assemble_GM_Heat_IC
	public :: Assemble_GM_Water_IC


	contains

    subroutine Init_Assemble(A)
        implicit none
        type(CRS), intent(in) :: A

        call Duplicate_CRS(A, tmpCRS1)
        tmpCRS1%val(:) = 0.0d0
        call Duplicate_CRS(A, tmpCRS2)
        tmpCRS2%val(:) = 0.0d0
    end subroutine Init_Assemble

	subroutine Set_C(C, case, p, C1, C2)
		implicit none
		type(DF), intent(in) :: C
		integer(int32), intent(in) :: case, p
		real(real64), intent(inout) :: C1, C2

		if (case == 1) then
			C1 = C%pre(p)
			C2 = C%old(p)
		else if (case == 2) then
			C1 = C%div(p)
			C2 = C%old(p)
		else if (case == 3) then
			C1 = C%div(p)
			C2 = C%div(p)
		else if (case == 4) then
			C1 = C%div(p)
			C2 = C%old(p)
		end if
	end subroutine Set_C

	!* 時間項の要素行列を全体CRS行列に組み込むサブルーチン
	subroutine Calc_GM_Time_TRM(Solver)
		implicit none
        type(SolverInfo), intent(inout) :: Solver
		integer(int32)                  :: iN, p1, p2, p3
		integer(int32)                  :: indexes(Solver%N%Shcoe ** 2)
		real(real64)                    :: CoeA, C1, C2, C3, C

		tmpCRS1%val(:) = 0.0d0
		do iN = 1, Solver%N%element
			p1   = Solver%N%pElement(1, iN)
			p2   = Solver%N%pElement(2, iN)
			p3   = Solver%N%pElement(3, iN)
			CoeA = Solver%N%eArea(iN) * D12 / Solver%Time%dt
			C1   = Solver%Heat%Variables%Cp%pre(p1)
			C2   = Solver%Heat%Variables%Cp%pre(p2)
			C3   = Solver%Heat%Variables%Cp%pre(p3)

			call Find_CRS_Indexes(udmp, tmpCRS1, p1, p2, p3, indexes(:))

			tmpCRS1%Val(indexes(1)) = tmpCRS1%Val(indexes(1)) + CoeA * C1 * 2.d0
			tmpCRS1%Val(indexes(2)) = tmpCRS1%Val(indexes(2)) + CoeA * C1
			tmpCRS1%Val(indexes(3)) = tmpCRS1%Val(indexes(3)) + CoeA * C1
			tmpCRS1%Val(indexes(4)) = tmpCRS1%Val(indexes(4)) + CoeA * C2
			tmpCRS1%Val(indexes(5)) = tmpCRS1%Val(indexes(5)) + CoeA * C2 * 2.d0
			tmpCRS1%Val(indexes(6)) = tmpCRS1%Val(indexes(6)) + CoeA * C2
			tmpCRS1%Val(indexes(7)) = tmpCRS1%Val(indexes(7)) + CoeA * C3
			tmpCRS1%Val(indexes(8)) = tmpCRS1%Val(indexes(8)) + CoeA * C3
			tmpCRS1%Val(indexes(9)) = tmpCRS1%Val(indexes(9)) + CoeA * C3 * 2.d0
		end do

	end subroutine Calc_GM_Time_TRM

	subroutine Calc_GM_Time_TRM_F(Solver)
		implicit none
        type(SolverInfo), intent(inout) :: Solver
		integer(int32)                  :: iN, p1, p2, p3
		integer(int32)                  :: indexes(Solver%N%Shcoe ** 2)
		real(real64)                    :: CoeA, C1, C2, C3, C

		if (.not. allocated(tmpA)) call Allocate_Array(tmpA, Solver%N%node, Solver%N%node)
		tmpA(:,:) = 0.0d0
		do iN = 1, Solver%N%element
			p1   = Solver%N%pElement(1, iN)
			p2   = Solver%N%pElement(2, iN)
			p3   = Solver%N%pElement(3, iN)
			CoeA = Solver%N%eArea(iN) * D12 / Solver%Time%dt
			C1   = Solver%Heat%Variables%Cp%pre(p1)
			C2   = Solver%Heat%Variables%Cp%pre(p2)
			C3   = Solver%Heat%Variables%Cp%pre(p3)

			tmpA(p1,p1) = tmpA(p1,p1) + CoeA * C1 * 2.d0
			tmpA(p1,p2) = tmpA(p1,p2) + CoeA * C1
			tmpA(p1,p3) = tmpA(p1,p3) + CoeA * C1
			tmpA(p2,p1) = tmpA(p2,p1) + CoeA * C2
			tmpA(p2,p2) = tmpA(p2,p2) + CoeA * C2 * 2.d0
			tmpA(p2,p3) = tmpA(p2,p3) + CoeA * C2
			tmpA(p3,p1) = tmpA(p3,p1) + CoeA * C3
			tmpA(p3,p2) = tmpA(p3,p2) + CoeA * C3
			tmpA(p3,p3) = tmpA(p3,p3) + CoeA * C3 * 2.d0
		end do

	end subroutine Calc_GM_Time_TRM_F

	subroutine Calc_GM_Time_Divide(Solver)
		implicit none
        type(SolverInfo), intent(inout) :: Solver
		integer(int32)                  :: iN, p1, p2, p3
		integer(int32)                  :: indexes(Solver%N%Shcoe)
		real(real64) 					:: CoeA, C11, C12, C13, C21, C22, C23

		tmpCRS1%val(:) = 0.0d0
		tmpCRS2%val(:) = 0.0d0
		do iN =1, Solver%N%element
			p1   = Solver%N%pElement(1, iN)
			p2   = Solver%N%pElement(2, iN)
			p3   = Solver%N%pElement(3, iN)
			CoeA = Solver%N%eArea(iN) * D3 / Solver%Time%dt
			call Set_C(Solver%Heat%Variables%Ca, Solver%Heat%Variables%Phase(p1), p1, C11, C21)
			call Set_C(Solver%Heat%Variables%Ca, Solver%Heat%Variables%Phase(p2), p2, C12, C22)
			call Set_C(Solver%Heat%Variables%Ca, Solver%Heat%Variables%Phase(p3), p3, C13, C23)

			call Find_CRS_Indexes(dmp, tmpCRS1, p1, p2, p3, indexes(:))

			tmpCRS1%Val(indexes(1)) = tmpCRS1%Val(indexes(1)) + CoeA * C11
			tmpCRS1%Val(indexes(2)) = tmpCRS1%Val(indexes(2)) + CoeA * C12
			tmpCRS1%Val(indexes(3)) = tmpCRS1%Val(indexes(3)) + CoeA * C13

			tmpCRS2%Val(indexes(1)) = tmpCRS2%Val(indexes(1)) + CoeA * C21
			tmpCRS2%Val(indexes(2)) = tmpCRS2%Val(indexes(2)) + CoeA * C22
			tmpCRS2%Val(indexes(3)) = tmpCRS2%Val(indexes(3)) + CoeA * C23
		end do

	end subroutine Calc_GM_Time_Divide

	subroutine Calc_GM_Time_Richards(Solver)
		implicit none
        type(SolverInfo), intent(inout) :: Solver
		integer(int32)                  :: iN, p1, p2, p3
		integer(int32)                  :: indexes(Solver%N%Shcoe)
		real(real64)                    :: CoeA, C1, C2, C3

		tmpCRS1%val(:) = 0.0d0
		do iN =1, Solver%N%element
			p1   = Solver%N%pElement(1, iN)
			p2   = Solver%N%pElement(2, iN)
			p3   = Solver%N%pElement(3, iN)
			CoeA = SOlver%Water%Constants%zeta * Solver%N%eArea(iN) * D3 / Solver%Time%dt


			call Find_CRS_Indexes(udmp, tmpCRS1, p1, p2, p3, indexes(:))

			tmpCRS1%Val(indexes(1)) = tmpCRS1%Val(indexes(1)) + CoeA
			tmpCRS1%Val(indexes(2)) = tmpCRS1%Val(indexes(2)) + CoeA
			tmpCRS1%Val(indexes(3)) = tmpCRS1%Val(indexes(3)) + CoeA
		end do
	end subroutine Calc_GM_Time_Richards

	!* 拡散項の要素行列を全体CRS行列に組み込むサブルーチン
	subroutine Calc_GM_Diffusion_TRM(Solver)
		implicit none
        type(SolverInfo), intent(inout) :: Solver
		integer(int32)                  :: iN, p1, p2, p3
		integer(int32)                  :: indexes(Solver%N%Shcoe ** 2)
		real(real64)                    :: CoeA, be1, be2, be3, ga1, ga2, ga3, Lm1, Lm2, Lm3

		Solver%Heat%LHS_A%val(:) = 0.0d0
		do iN =1, Solver%N%element
			p1   = Solver%N%pElement(1, iN)
			p2   = Solver%N%pElement(2, iN)
			p3   = Solver%N%pElement(3, iN)
			CoeA = D4 / Solver%N%eArea(iN)
			Lm1  = Solver%Heat%Variables%lambda%pre(p1)
			Lm2  = Solver%Heat%Variables%lambda%pre(p2)
			Lm3  = Solver%Heat%Variables%lambda%pre(p3)
			be1  = Solver%N%Basis%b(1, iN)
			be2  = Solver%N%Basis%b(2, iN)
			be3  = Solver%N%Basis%b(3, iN)
			ga1  = Solver%N%Basis%c(1, iN)
			ga2  = Solver%N%Basis%c(2, iN)
			ga3  = Solver%N%Basis%c(3, iN)

			call Find_CRS_Indexes(udmp, tmpCRS1, p1, p2, p3, indexes(:))

			Solver%Heat%LHS_A%Val(indexes(1)) = Solver%Heat%LHS_A%Val(indexes(1)) + Lm1 * (be1 * be1 + ga1 * ga1) * CoeA
			Solver%Heat%LHS_A%Val(indexes(2)) = Solver%Heat%LHS_A%Val(indexes(2)) + Lm1 * (be1 * be2 + ga1 * ga2) * CoeA
			Solver%Heat%LHS_A%Val(indexes(3)) = Solver%Heat%LHS_A%Val(indexes(3)) + Lm1 * (be1 * be3 + ga1 * ga3) * CoeA
			Solver%Heat%LHS_A%Val(indexes(4)) = Solver%Heat%LHS_A%Val(indexes(4)) + Lm2 * (be2 * be1 + ga2 * ga1) * CoeA
			Solver%Heat%LHS_A%Val(indexes(5)) = Solver%Heat%LHS_A%Val(indexes(5)) + Lm2 * (be2 * be2 + ga2 * ga2) * CoeA
			Solver%Heat%LHS_A%Val(indexes(6)) = Solver%Heat%LHS_A%Val(indexes(6)) + Lm2 * (be2 * be3 + ga2 * ga3) * CoeA
			Solver%Heat%LHS_A%Val(indexes(7)) = Solver%Heat%LHS_A%Val(indexes(7)) + Lm3 * (be3 * be1 + ga3 * ga1) * CoeA
			Solver%Heat%LHS_A%Val(indexes(8)) = Solver%Heat%LHS_A%Val(indexes(8)) + Lm3 * (be3 * be2 + ga3 * ga2) * CoeA
			Solver%Heat%LHS_A%Val(indexes(9)) = Solver%Heat%LHS_A%Val(indexes(9)) + Lm3 * (be3 * be3 + ga3 * ga3) * CoeA
		end do
	end subroutine Calc_GM_Diffusion_TRM

	subroutine Calc_GM_Diffusion(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver
		integer(int32)                  :: iN, p1, p2, p3
		integer(int32)                  :: indexes(Solver%N%Shcoe ** 2)
		real(real64)                    :: CoeA, be1, be2, be3, ga1, ga2, ga3, Lm1, Lm2, Lm3, Lm

		Solver%Heat%LHS_A%val(:) = 0.0d0
		do iN =1, Solver%N%element
			p1   = Solver%N%pElement(1, iN)
			p2   = Solver%N%pElement(2, iN)
			p3   = Solver%N%pElement(3, iN)
			CoeA = D4 / Solver%N%eArea(iN)
			Lm1  = Solver%Heat%Variables%lambda%pre(p1)
			Lm2  = Solver%Heat%Variables%lambda%pre(p2)
			Lm3  = Solver%Heat%Variables%lambda%pre(p3)
			Lm   = (Lm1 + Lm2 + Lm3) * D3
			be1  = Solver%N%Basis%b(1, iN)
			be2  = Solver%N%Basis%b(2, iN)
			be3  = Solver%N%Basis%b(3, iN)
			ga1  = Solver%N%Basis%c(1, iN)
			ga2  = Solver%N%Basis%c(2, iN)
			ga3  = Solver%N%Basis%c(3, iN)

			call Find_CRS_Indexes(udmp, tmpCRS1, p1, p2, p3, indexes(:))

			Solver%Heat%LHS_A%Val(indexes(1)) = Solver%Heat%LHS_A%Val(indexes(1)) + Lm * (be1 * be1 + ga1 * ga1) * CoeA
			Solver%Heat%LHS_A%Val(indexes(2)) = Solver%Heat%LHS_A%Val(indexes(2)) + Lm * (be1 * be2 + ga1 * ga2) * CoeA
			Solver%Heat%LHS_A%Val(indexes(3)) = Solver%Heat%LHS_A%Val(indexes(3)) + Lm * (be1 * be3 + ga1 * ga3) * CoeA
			Solver%Heat%LHS_A%Val(indexes(4)) = Solver%Heat%LHS_A%Val(indexes(4)) + Lm * (be2 * be1 + ga2 * ga1) * CoeA
			Solver%Heat%LHS_A%Val(indexes(5)) = Solver%Heat%LHS_A%Val(indexes(5)) + Lm * (be2 * be2 + ga2 * ga2) * CoeA
			Solver%Heat%LHS_A%Val(indexes(6)) = Solver%Heat%LHS_A%Val(indexes(6)) + Lm * (be2 * be3 + ga2 * ga3) * CoeA
			Solver%Heat%LHS_A%Val(indexes(7)) = Solver%Heat%LHS_A%Val(indexes(7)) + Lm * (be3 * be1 + ga3 * ga1) * CoeA
			Solver%Heat%LHS_A%Val(indexes(8)) = Solver%Heat%LHS_A%Val(indexes(8)) + Lm * (be3 * be2 + ga3 * ga2) * CoeA
			Solver%Heat%LHS_A%Val(indexes(9)) = Solver%Heat%LHS_A%Val(indexes(9)) + Lm * (be3 * be3 + ga3 * ga3) * CoeA
		end do
	end subroutine Calc_GM_Diffusion

	subroutine Calc_GM_Diffusion_Richards(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver
		integer(int32)                  :: iN, p1, p2, p3
		integer(int32)                  :: indexes(Solver%N%Shcoe ** 2)
		real(real64)                    :: CoeA, be1, be2, be3, ga1, ga2, ga3, K1, K2, K3, K

		Solver%Water%LHS_A%val(:) = 0.0d0
		do iN =1, Solver%N%element
			p1   = Solver%N%pElement(1, iN)
			p2   = Solver%N%pElement(2, iN)
			p3   = Solver%N%pElement(3, iN)
			CoeA = D4 / Solver%N%eArea(iN)
			K1  = Solver%Water%Variables%Klh%pre(p1)
			K2  = Solver%Water%Variables%Klh%pre(p2)
			K3  = Solver%Water%Variables%Klh%pre(p3)
			K   = (K1 + K2 + K3) * D3
			be1  = Solver%N%Basis%b(1, iN)
			be2  = Solver%N%Basis%b(2, iN)
			be3  = Solver%N%Basis%b(3, iN)
			ga1  = Solver%N%Basis%c(1, iN)
			ga2  = Solver%N%Basis%c(2, iN)
			ga3  = Solver%N%Basis%c(3, iN)

			call Find_CRS_Indexes(udmp, tmpCRS1, p1, p2, p3, indexes(:))

			Solver%Water%LHS_A%Val(indexes(1)) = Solver%Water%LHS_A%Val(indexes(1)) + K * (be1 * be1 + ga1 * ga1) * CoeA
			Solver%Water%LHS_A%Val(indexes(2)) = Solver%Water%LHS_A%Val(indexes(2)) + K * (be1 * be2 + ga1 * ga2) * CoeA
			Solver%Water%LHS_A%Val(indexes(3)) = Solver%Water%LHS_A%Val(indexes(3)) + K * (be1 * be3 + ga1 * ga3) * CoeA
			Solver%Water%LHS_A%Val(indexes(4)) = Solver%Water%LHS_A%Val(indexes(4)) + K * (be2 * be1 + ga2 * ga1) * CoeA
			Solver%Water%LHS_A%Val(indexes(5)) = Solver%Water%LHS_A%Val(indexes(5)) + K * (be2 * be2 + ga2 * ga2) * CoeA
			Solver%Water%LHS_A%Val(indexes(6)) = Solver%Water%LHS_A%Val(indexes(6)) + K * (be2 * be3 + ga2 * ga3) * CoeA
			Solver%Water%LHS_A%Val(indexes(7)) = Solver%Water%LHS_A%Val(indexes(7)) + K * (be3 * be1 + ga3 * ga1) * CoeA
			Solver%Water%LHS_A%Val(indexes(8)) = Solver%Water%LHS_A%Val(indexes(8)) + K * (be3 * be2 + ga3 * ga2) * CoeA
			Solver%Water%LHS_A%Val(indexes(9)) = Solver%Water%LHS_A%Val(indexes(9)) + K * (be3 * be3 + ga3 * ga3) * CoeA
		end do

	end subroutine Calc_GM_Diffusion_Richards

	subroutine Calc_GM_Diffusion_Richards_F(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver
		integer(int32)                  :: iN, p1, p2, p3
		real(real64)                    :: CoeA, be1, be2, be3, ga1, ga2, ga3, K1, K2, K3, K

		Solver%Water%RA(:,:) = 0.0d0
		do iN =1, Solver%N%element
			p1   = Solver%N%pElement(1, iN)
			p2   = Solver%N%pElement(2, iN)
			p3   = Solver%N%pElement(3, iN)
			CoeA = D4 / Solver%N%eArea(iN)
			K1  = Solver%Water%Variables%Klh%pre(p1)
			K2  = Solver%Water%Variables%Klh%pre(p2)
			K3  = Solver%Water%Variables%Klh%pre(p3)
			K   = (K1 * K2 * K3) ** D3

			be1  = Solver%N%Basis%b(1, iN)
			be2  = Solver%N%Basis%b(2, iN)
			be3  = Solver%N%Basis%b(3, iN)
			ga1  = Solver%N%Basis%c(1, iN)
			ga2  = Solver%N%Basis%c(2, iN)
			ga3  = Solver%N%Basis%c(3, iN)

			Solver%Water%RA(p1, p1) = Solver%Water%RA(p1, p1) + K * (be1 * be1 + ga1 * ga1) * CoeA
			Solver%Water%RA(p1, p2) = Solver%Water%RA(p1, p2) + K * (be1 * be2 + ga1 * ga2) * CoeA
			Solver%Water%RA(p1, p3) = Solver%Water%RA(p1, p3) + K * (be1 * be3 + ga1 * ga3) * CoeA
			Solver%Water%RA(p2, p1) = Solver%Water%RA(p2, p1) + K * (be2 * be1 + ga2 * ga1) * CoeA
			Solver%Water%RA(p2, p2) = Solver%Water%RA(p2, p2) + K * (be2 * be2 + ga2 * ga2) * CoeA
			Solver%Water%RA(p2, p3) = Solver%Water%RA(p2, p3) + K * (be2 * be3 + ga2 * ga3) * CoeA
			Solver%Water%RA(p3, p1) = Solver%Water%RA(p3, p1) + K * (be3 * be1 + ga3 * ga1) * CoeA
			Solver%Water%RA(p3, p2) = Solver%Water%RA(p3, p2) + K * (be3 * be2 + ga3 * ga2) * CoeA
			Solver%Water%RA(p3, p3) = Solver%Water%RA(p3, p3) + K * (be3 * be3 + ga3 * ga3) * CoeA
		end do

	end subroutine Calc_GM_Diffusion_Richards_F

	!* 拡散・移流項の要素行列を全体CRS行列に組み込むサブルーチン
	subroutine Calc_GM_Diffusion_Advection_TRM(Solver)
		implicit none
        type(SolverInfo), intent(inout) :: Solver
		integer(int32)                  :: iN, p1, p2, p3
		integer(int32)                  :: indexes(Solver%N%Shcoe ** 2)
		real(real64)					:: be1, be2, be3, ga1, ga2, ga3
		real(real64)                    :: CoeA, Lm1, Lm2, Lm3
		real(real64)					:: CoeB, u1, u2, u3, v1, v2, v3, AD1, AD2, AD3

		Solver%Heat%LHS_A%val(:) = 0.0d0
		do iN =1, Solver%N%element
			p1   = Solver%N%pElement(1, iN)
			p2   = Solver%N%pElement(2, iN)
			p3   = Solver%N%pElement(3, iN)
			be1  = Solver%N%Basis%b(1, iN)
			be2  = Solver%N%Basis%b(2, iN)
			be3  = Solver%N%Basis%b(3, iN)
			ga1  = Solver%N%Basis%c(1, iN)
			ga2  = Solver%N%Basis%c(2, iN)
			ga3  = Solver%N%Basis%c(3, iN)

			CoeA = D4 / Solver%N%eArea(iN)
			Lm1  = Solver%Heat%Variables%lambda%pre(p1)
			Lm2  = Solver%Heat%Variables%lambda%pre(p2)
			Lm3  = Solver%Heat%Variables%lambda%pre(p3)

			CoeB = Solver%Heat%Constants%HeatCapacity%water / 6.0d0
			u1   = Solver%Water%Variables%wFlux%x(p1)
			u2   = Solver%Water%Variables%wFlux%x(p2)
			u3   = Solver%Water%Variables%wFlux%x(p3)
			v1   = Solver%Water%Variables%wFlux%y(p1)
			v2   = Solver%Water%Variables%wFlux%y(p2)
			v3   = Solver%Water%Variables%wFlux%y(p3)
			AD1  = u1 * be1 + v1 * ga1
			AD2  = u2 * be2 + v2 * ga2
			AD3  = u3 * be3 + v3 * ga3

			call Find_CRS_Indexes(udmp, tmpCRS1, p1, p2, p3, indexes(:))

			Solver%Heat%LHS_A%Val(indexes(1)) = Solver%Heat%LHS_A%Val(indexes(1)) + Lm1 * (be1 * be1 + ga1 * ga1) * CoeA &
											  + AD1 * CoeB
			Solver%Heat%LHS_A%Val(indexes(2)) = Solver%Heat%LHS_A%Val(indexes(2)) + Lm1 * (be1 * be2 + ga1 * ga2) * CoeA &
											  + AD2 * CoeB
			Solver%Heat%LHS_A%Val(indexes(3)) = Solver%Heat%LHS_A%Val(indexes(3)) + Lm1 * (be1 * be3 + ga1 * ga3) * CoeA &
											  + AD3 * CoeB
			Solver%Heat%LHS_A%Val(indexes(4)) = Solver%Heat%LHS_A%Val(indexes(4)) + Lm2 * (be2 * be1 + ga2 * ga1) * CoeA &
											  + AD1 * CoeB
			Solver%Heat%LHS_A%Val(indexes(5)) = Solver%Heat%LHS_A%Val(indexes(5)) + Lm2 * (be2 * be2 + ga2 * ga2) * CoeA &
											  + AD2 * CoeB
			Solver%Heat%LHS_A%Val(indexes(6)) = Solver%Heat%LHS_A%Val(indexes(6)) + Lm2 * (be2 * be3 + ga2 * ga3) * CoeA &
											  + AD3 * CoeB
			Solver%Heat%LHS_A%Val(indexes(7)) = Solver%Heat%LHS_A%Val(indexes(7)) + Lm3 * (be3 * be1 + ga3 * ga1) * CoeA &
											  + AD1 * CoeB
			Solver%Heat%LHS_A%Val(indexes(8)) = Solver%Heat%LHS_A%Val(indexes(8)) + Lm3 * (be3 * be2 + ga3 * ga2) * CoeA &
											  + AD2 * CoeB
			Solver%Heat%LHS_A%Val(indexes(9)) = Solver%Heat%LHS_A%Val(indexes(9)) + Lm3 * (be3 * be3 + ga3 * ga3) * CoeA &
											  + AD3 * CoeB
		end do
	end subroutine Calc_GM_Diffusion_Advection_TRM

	subroutine Calc_GM_Diffusion_Advection_TRM_F(Solver)
		implicit none
        type(SolverInfo), intent(inout) :: Solver
		integer(int32)                  :: iN, p1, p2, p3
		integer(int32)                  :: indexes(Solver%N%Shcoe ** 2)
		real(real64)					:: be1, be2, be3, ga1, ga2, ga3
		real(real64)                    :: CoeA, Lm1, Lm2, Lm3
		real(real64)					:: CoeB, u1, u2, u3, v1, v2, v3, AD1, AD2, AD3

		Solver%Heat%RA(:,:) = 0.0d0
		! print*,Solver%Heat%RA(1000,:)
		! stop
		do iN =1, Solver%N%element
			p1   = Solver%N%pElement(1, iN)
			p2   = Solver%N%pElement(2, iN)
			p3   = Solver%N%pElement(3, iN)
			be1  = Solver%N%Basis%b(1, iN)
			be2  = Solver%N%Basis%b(2, iN)
			be3  = Solver%N%Basis%b(3, iN)
			ga1  = Solver%N%Basis%c(1, iN)
			ga2  = Solver%N%Basis%c(2, iN)
			ga3  = Solver%N%Basis%c(3, iN)

			CoeA = D4 / Solver%N%eArea(iN)
			Lm1  = Solver%Heat%Variables%lambda%pre(p1)
			Lm2  = Solver%Heat%Variables%lambda%pre(p2)
			Lm3  = Solver%Heat%Variables%lambda%pre(p3)

			CoeB = Solver%Heat%Constants%HeatCapacity%water / 6.0d0
			u1   = Solver%Water%Variables%wFlux%x(p1)
			u2   = Solver%Water%Variables%wFlux%x(p2)
			u3   = Solver%Water%Variables%wFlux%x(p3)
			v1   = Solver%Water%Variables%wFlux%y(p1)
			v2   = Solver%Water%Variables%wFlux%y(p2)
			v3   = Solver%Water%Variables%wFlux%y(p3)
			AD1  = 0.0d0 ! u1 * be1 + v1 * ga1
			AD2  = 0.0d0 ! u2 * be2 + v2 * ga2
			AD3  = 0.0d0 ! u3 * be3 + v3 * ga3
			! AD1  = u1 * be1 + v1 * ga1
			! AD2  = u2 * be2 + v2 * ga2
			! AD3  = u3 * be3 + v3 * ga3
			! print*,p1,p2,p3
			! if (iN==100) stop

			! call Find_CRS_Indexes(udmp, tmpCRS1, p1, p2, p3, indexes(:))

			Solver%Heat%RA(p1, p1) = Solver%Heat%RA(p1, p1) + Lm1 * (be1 * be1 + ga1 * ga1) * CoeA &
											  + AD1 * CoeB
			Solver%Heat%RA(p1, p2) = Solver%Heat%RA(p1, p2) + Lm2 * (be1 * be2 + ga1 * ga2) * CoeA &
											  + AD2 * CoeB
			Solver%Heat%RA(p1, p3) = Solver%Heat%RA(p1, p3) + Lm3 * (be1 * be3 + ga1 * ga3) * CoeA &
											  + AD3 * CoeB
			Solver%Heat%RA(p2, p1) = Solver%Heat%RA(p2, p1) + Lm1 * (be2 * be1 + ga2 * ga1) * CoeA &
											  + AD1 * CoeB
			Solver%Heat%RA(p2, p2) = Solver%Heat%RA(p2, p2) + Lm2 * (be2 * be2 + ga2 * ga2) * CoeA &
											  + AD2 * CoeB
			Solver%Heat%RA(p2, p3) = Solver%Heat%RA(p2, p3) + Lm3 * (be2 * be3 + ga2 * ga3) * CoeA &
											  + AD3 * CoeB
			Solver%Heat%RA(p3, p1) = Solver%Heat%RA(p3, p1) + Lm1 * (be3 * be1 + ga3 * ga1) * CoeA &
											  + AD1 * CoeB
			Solver%Heat%RA(p3, p2) = Solver%Heat%RA(p3, p2) + Lm2 * (be3 * be2 + ga3 * ga2) * CoeA &
											  + AD2 * CoeB
			Solver%Heat%RA(p3, p3) = Solver%Heat%RA(p3, p1) + Lm3 * (be3 * be3 + ga3 * ga3) * CoeA &
											  + AD3 * CoeB
		end do
	end subroutine Calc_GM_Diffusion_Advection_TRM_F

	subroutine Calc_GM_Diffusion_Advection(Solver)
		implicit none
        type(SolverInfo), intent(inout) :: Solver
		integer(int32)                  :: iN, p1, p2, p3
		integer(int32)                  :: indexes(Solver%N%Shcoe ** 2)
		real(real64)					:: be1, be2, be3, ga1, ga2, ga3
		real(real64)                    :: CoeA, Lm1, Lm2, Lm3, Lm
		real(real64)					:: CoeB, u1, u2, u3, v1, v2, v3

		Solver%Heat%LHS_A%val(:) = 0.0d0
		do iN =1, Solver%N%element
			p1   = Solver%N%pElement(1, iN)
			p2   = Solver%N%pElement(2, iN)
			p3   = Solver%N%pElement(3, iN)
			be1  = Solver%N%Basis%b(1, iN)
			be2  = Solver%N%Basis%b(2, iN)
			be3  = Solver%N%Basis%b(3, iN)
			ga1  = Solver%N%Basis%c(1, iN)
			ga2  = Solver%N%Basis%c(2, iN)
			ga3  = Solver%N%Basis%c(3, iN)

			CoeA = D4 / Solver%N%eArea(iN)
			Lm1  = Solver%Heat%Variables%lambda%pre(p1)
			Lm2  = Solver%Heat%Variables%lambda%pre(p2)
			Lm3  = Solver%Heat%Variables%lambda%pre(p3)
			Lm   = (Lm1 * Lm2 * Lm3) ** D3
			! Lm   = (Lm1 + Lm2 + Lm3) * D3

			CoeB = Solver%Heat%Constants%HeatCapacity%water * D24
			u1   = Solver%Water%Variables%wFlux%x(p1)
			u2   = Solver%Water%Variables%wFlux%x(p2)
			u3   = Solver%Water%Variables%wFlux%x(p3)
			v1   = Solver%Water%Variables%wFlux%y(p1)
			v2   = Solver%Water%Variables%wFlux%y(p2)
			v3   = Solver%Water%Variables%wFlux%y(p3)
			! write(*,'(6es14.5)') u1, u2, u3, v1, v2, v3

			call Find_CRS_Indexes(udmp, tmpCRS1, p1, p2, p3, indexes(:))

			Solver%Heat%LHS_A%Val(indexes(1)) = Solver%Heat%LHS_A%Val(indexes(1)) + Lm * (be1 * be1 + ga1 * ga1) * CoeA &
											  + ((2.d0 * u1 + u2 + u3) * be1 + (2.d0 * v1 + v2 + v3) * ga1) * CoeB
			Solver%Heat%LHS_A%Val(indexes(2)) = Solver%Heat%LHS_A%Val(indexes(2)) + Lm * (be1 * be2 + ga1 * ga2) * CoeA &
											  + ((2.d0 * u1 + u2 + u3) * be2 + (2.d0 * v1 + v2 + v3) * ga2) * CoeB
			Solver%Heat%LHS_A%Val(indexes(3)) = Solver%Heat%LHS_A%Val(indexes(3)) + Lm * (be1 * be3 + ga1 * ga3) * CoeA &
											  + ((2.d0 * u1 + u2 + u3) * be3 + (2.d0 * v1 + v2 + v3) * ga3) * CoeB
			Solver%Heat%LHS_A%Val(indexes(4)) = Solver%Heat%LHS_A%Val(indexes(4)) + Lm * (be2 * be1 + ga2 * ga1) * CoeA	&
											  + ((u1 + 2.d0 * u2 + u3) * be1 + (v1 + 2.d0 * v2 + v3) * ga1) * CoeB
			Solver%Heat%LHS_A%Val(indexes(5)) = Solver%Heat%LHS_A%Val(indexes(5)) + Lm * (be2 * be2 + ga2 * ga2) * CoeA	&
											  + ((u1 + 2.d0 * u2 + u3) * be2 + (v1 + 2.d0 * v2 + v3) * ga2) * CoeB
			Solver%Heat%LHS_A%Val(indexes(6)) = Solver%Heat%LHS_A%Val(indexes(6)) + Lm * (be2 * be3 + ga2 * ga3) * CoeA &
											  + ((u1 + 2.d0 * u2 + u3) * be3 + (v1 + 2.d0 * v2 + v3) * ga3) * CoeB
			Solver%Heat%LHS_A%Val(indexes(7)) = Solver%Heat%LHS_A%Val(indexes(7)) + Lm * (be3 * be1 + ga3 * ga1) * CoeA &
											  + ((u1 + u2 + 2.d0 * u3) * be1 + (v1 + v2 + 2.d0 * v3) * ga1) * CoeB
			Solver%Heat%LHS_A%Val(indexes(8)) = Solver%Heat%LHS_A%Val(indexes(8)) + Lm * (be3 * be2 + ga3 * ga2) * CoeA &
											  + ((u1 + u2 + 2.d0 * u3) * be2 + (v1 + v2 + 2.d0 * v3) * ga2) * CoeB
			Solver%Heat%LHS_A%Val(indexes(9)) = Solver%Heat%LHS_A%Val(indexes(9)) + Lm * (be3 * be3 + ga3 * ga3) * CoeA	&
											  + ((u1 + u2 + 2.d0 * u3) * be3 + (v1 + v2 + 2.d0 * v3) * ga3) * CoeB
		end do
		! stop
	end subroutine Calc_GM_Diffusion_Advection

    subroutine Assemble_GM_Heat(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver

		if (.not. Solver%isWater) then
			if (Solver%Flags%isTRM) then
				call Calc_GM_Time_TRM(Solver)
				call Calc_GM_Diffusion_TRM(Solver)
				call SpMV(tmpCRS1, Solver%T%old(:), Solver%Heat%Rhs(:))
				Solver%Heat%LHS_A%val(:) = Solver%Heat%LHS_A%val(:) + tmpCRS1%val(:)
			else if (Solver%Flags%isGCC .or. Solver%Flags%isPower) then
				call Calc_GM_Time_Divide(Solver)
				call Calc_GM_Diffusion(Solver)
				call SpMV(tmpCRS2, Solver%T%old(:), Solver%Heat%Rhs(:))
				Solver%Heat%LHS_A%val(:) = Solver%Heat%LHS_A%val(:) + tmpCRS1%val(:)
			end if
		else
			if (Solver%Flags%isTRM) then
				call Calc_GM_Time_TRM(Solver)
				! call Calc_GM_Time_TRM_F(Solver)
				call Calc_GM_Diffusion_Advection_TRM(Solver)
				! call Calc_GM_Diffusion_TRM(Solver)
				call SpMV(tmpCRS1, Solver%T%old(:), Solver%Heat%Rhs(:))
				! print*,Solver%Heat%Rhs(:)
				! stop
				! Solver%Heat%RA(:,:) = Solver%Heat%RA(:,:) + tmpA(:,:)
				Solver%Heat%LHS_A%val(:) = Solver%Heat%LHS_A%val(:) + tmpCRS1%val(:)
			else if (Solver%Flags%isGCC .or. Solver%Flags%isPower) then
				call Calc_GM_Time_Divide(Solver)
				call Calc_GM_Diffusion_Advection(Solver)
				call SpMV(tmpCRS2, Solver%T%old(:), Solver%Heat%Rhs(:))
				Solver%Heat%LHS_A%val(:) = Solver%Heat%LHS_A%val(:) + tmpCRS1%val(:)
			end if
		end if

    end subroutine Assemble_GM_Heat

    subroutine Assemble_GM_Heat_IC(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver

		! if (.not. Solver%isWater) then
			if (Solver%Flags%isTRM) then
				call Calc_GM_Time_TRM(Solver)
				call Calc_GM_Diffusion_TRM(Solver)
				call SpMV(tmpCRS1, Solver%T%old(:), Solver%Heat%Rhs(:))
				Solver%Heat%LHS_A%val(:) = Solver%Heat%LHS_A%val(:) + tmpCRS1%val(:)
			else if (Solver%Flags%isGCC) then
				call Calc_GM_Time_Divide(Solver)
				call Calc_GM_Diffusion(Solver)
				call SpMV(tmpCRS2, Solver%T%old(:), Solver%Heat%Rhs(:))
				Solver%Heat%LHS_A%val(:) = Solver%Heat%LHS_A%val(:) + tmpCRS1%val(:)
			else if (Solver%Flags%isPower) then
				call Calc_GM_Time_Divide(Solver)
				call Calc_GM_Diffusion(Solver)
				call SpMV(tmpCRS2, Solver%T%old(:), Solver%Heat%Rhs(:))
				Solver%Heat%LHS_A%val(:) = Solver%Heat%LHS_A%val(:) + tmpCRS1%val(:)
			end if
		! end if

    end subroutine Assemble_GM_Heat_IC

	subroutine Assemble_GM_Water(Solver)
		implicit none
		type(SolverInfo), intent(inout) :: Solver

		call Calc_GM_Time_Richards(Solver)
		! call Calc_GM_Diffusion_Richards(Solver)
		call Calc_GM_Diffusion_Richards_F(Solver)
		call SpMV(tmpCRS1, Solver%mIce%dif(:), Solver%Water%Rhs(:))
		! print *, Solver%Water%Rhs(:)
		Solver%Water%Rhs(:) = -1.0d0 * Solver%Water%Rhs(:)

	end subroutine Assemble_GM_Water

	subroutine Assemble_GM_Water_IC(Solver)
		implicit none
		type(SolverInfo), intent(inout) :: Solver

		call Calc_GM_Diffusion_Richards_F(Solver)
		Solver%Water%Rhs(:) = 0.0d0

	end subroutine Assemble_GM_Water_IC

end module Matrix_Assemble