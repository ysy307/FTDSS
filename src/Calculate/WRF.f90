module Calculate_WRF
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    private
    real(real64), parameter :: pi = 4 * atan(1.0d0)

    public :: Abstract_WRF
    public :: Type_WRF_BC
    public :: Type_WRF_VG
    public :: Type_WRF_KO
    public :: Type_WRF_MVG
    public :: Type_WRF_Durner
    public :: Type_WRF_DVGCH

    type, abstract :: Abstract_WRF
        real(real64) :: thetaR !! Residual water content
        real(real64) :: thetaS !! Saturated water content
    contains
        procedure(Abstract_Calculate_WRF), deferred :: Calculate_WRF
        procedure(Abstract_Calculate_WRF_Derivative), deferred :: Calculate_WRF_Derivative
    end type Abstract_WRF

    type, extends(Abstract_WRF) :: Type_WRF_BC
        real(real64) :: alpha1 !! Parameter 1
        real(real64) :: n1 !! Parameter 2
    contains
        procedure :: Calculate_WRF => Calculate_WRF_BC
        procedure :: Calculate_WRF_Derivative => Calculate_WRF_BC_Derivative
    end type Type_WRF_BC

    type, extends(Abstract_WRF) :: Type_WRF_VG
        real(real64) :: alpha1 !! Parameter 1
        real(real64) :: n1 !! Parameter 2
        real(real64) :: m1 !! Parameter 3
    contains
        procedure :: Calculate_WRF => Calculate_WRF_VG
        procedure :: Calculate_WRF_Derivative => Calculate_WRF_VG_Derivative
    end type Type_WRF_VG

    type, extends(Abstract_WRF) :: Type_WRF_KO
        real(real64) :: alpha1 !! Parameter 1
        real(real64) :: n1 !! Parameter 2
    contains
        procedure :: Calculate_WRF => Calculate_WRF_KO
        procedure :: Calculate_WRF_Derivative => Calculate_WRF_KO_Derivative
    end type Type_WRF_KO

    type, extends(Abstract_WRF) :: Type_WRF_MVG
        real(real64) :: alpha1 !! Parameter 1
        real(real64) :: n1 !! Parameter 2
        real(real64) :: m1 !! Parameter 3
        real(real64) :: hcrit !! Parameter 4
    contains
        procedure :: Calculate_WRF => Calculate_WRF_MVG
        procedure :: Calculate_WRF_Derivative => Calculate_WRF_MVG_Derivative
    end type Type_WRF_MVG

    type, extends(Abstract_WRF) :: Type_WRF_Durner
        real(real64) :: alpha1 !! Parameter 1
        real(real64) :: n1 !! Parameter 2
        real(real64) :: m1 !! Parameter 3
        real(real64) :: alpha2 !! Parameter 4
        real(real64) :: n2 !! Parameter 5
        real(real64) :: m2 !! Parameter 6
        real(real64) :: w1 !! Parameter 7
        real(real64) :: w2 !! Parameter 8
    contains
        procedure :: Calculate_WRF => Calculate_WRF_Durner
        procedure :: Calculate_WRF_Derivative => Calculate_WRF_Durner_Derivative
    end type Type_WRF_Durner

    type, extends(Abstract_WRF) :: Type_WRF_DVGCH
        real(real64) :: alpha1 !! Parameter 1
        real(real64) :: n1 !! Parameter 2
        real(real64) :: n2 !! Parameter 3
        real(real64) :: m1 !! Parameter 4
        real(real64) :: m2 !! Parameter 5
        real(real64) :: w1 !! Parameter 6
        real(real64) :: w2 !! Parameter 7
    contains
        procedure :: Calculate_WRF => Calculate_WRF_DVGCH
        procedure :: Calculate_WRF_Derivative => Calculate_WRF_DVGCH_Derivative
    end type Type_WRF_DVGCH

    interface
        function Abstract_Calculate_WRF(self, h) result(thetaW)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_WRF
            implicit none
            class(Abstract_WRF), intent(in) :: self
            real(real64), intent(in) :: h !! Water potential
            real(real64) :: thetaW !! Calculated water content
        end function Abstract_Calculate_WRF

        function Abstract_Calculate_WRF_Derivative(self, h) result(Cw)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_WRF
            implicit none
            class(Abstract_WRF), intent(in) :: self
            real(real64), intent(in) :: h !! Water potential
            real(real64) :: Cw !! Calculated derivative of water content
        end function Abstract_Calculate_WRF_Derivative
    end interface
contains

    function Calculate_WRF_BC(self, h) result(thetaW)
        class(Type_WRF_BC), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: thetaW

        if (h < self%alpha1) then
            thetaW = self%thetaR + (self%thetaS - self%thetaR) * (self%alpha1 / h)**self%n1
        else
            thetaW = self%thetaS
        end if

    end function Calculate_WRF_BC

    function Calculate_WRF_BC_Derivative(self, h) result(Cw)
        class(Type_WRF_BC), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: Cw

        !@note alpha1 must be negative

        if (h < self%alpha1) then
            Cw = -(self%thetaS - self%thetaR) * self%n1 * (self%alpha1 / h)**(self%n1 + 1.0d0) / self%alpha1
        else
            Cw = 0.0d0
        end if
    end function Calculate_WRF_BC_Derivative

    function Calculate_WRF_VG(self, h) result(thetaW)
        class(Type_WRF_VG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: thetaW
        !@note alpha1 is must be positive

        if (h < 0.0d0) then
            thetaW = self%thetaR + (self%thetaS - self%thetaR) * (1.0d0 + (-self%alpha1 * h)**self%n1)**(-self%m1)
        else
            thetaW = self%thetaS
        end if

    end function Calculate_WRF_VG

    function Calculate_WRF_VG_Derivative(self, h) result(Cw)
        class(Type_WRF_VG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: Cw
        !@note alpha1 is must be positive

        if (h < 0.0d0) then
            Cw = (self%thetaS - self%thetaR) * &
                 self%alpha1**self%n1 * self%m1 * self%n1 * (-h)**(self%n1 - 1.0d0) &
                 * (1.0d0 + (-self%alpha1 * h)**self%n1)**(-self%m1 - 1.0d0)
        else
            Cw = 0.0d0
        end if

    end function Calculate_WRF_VG_Derivative

    function Calculate_WRF_KO(self, h) result(thetaW)
        class(Type_WRF_KO), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: thetaW

        if (h < 0.0d0) then
            thetaW = self%thetaR + (self%thetaS - self%thetaR) * 0.5d0 * erfc(log(h / self%alpha1) / (self%n1 * sqrt(2.0d0)))
        else
            thetaW = self%thetaS
        end if

    end function Calculate_WRF_KO

    function Calculate_WRF_KO_Derivative(self, h) result(Cw)
        class(Type_WRF_KO), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: Cw

        if (h < 0.0d0) then
            Cw = -(self%thetaS - self%thetaR) * &
                 exp(-(log(h / self%alpha1))**2.0d0 / (2.0d0 * self%n1**2.0d0)) / &
                 (sqrt(2.0d0 * pi) * h * self%n1)
        else
            Cw = 0.0d0
        end if

    end function Calculate_WRF_KO_Derivative

    function Calculate_WRF_MVG(self, h) result(thetaW)
        class(Type_WRF_MVG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: thetaW

        if (h < self%hcrit) then
            thetaW = self%thetaR + (self%thetaS - self%thetaR) * (1.0d0 + abs(self%alpha1 * h)**self%n1)**(-self%m1)
        else
            thetaW = self%thetaS
        end if

    end function Calculate_WRF_MVG

    function Calculate_WRF_MVG_Derivative(self, h) result(Cw)
        class(Type_WRF_MVG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: Cw

        if (h < self%hcrit) then
            Cw = (self%thetaS - self%thetaR) * &
                 self%alpha1**self%n1 * self%m1 * self%n1 * (-h)**(self%n1 - 1.0d0) * &
                 (1.0d0 + (-self%alpha1 * h)**self%n1)**(-self%m1 - 1.0d0)
        else
            Cw = 0.0d0
        end if

    end function Calculate_WRF_MVG_Derivative

    function Calculate_WRF_Durner(self, h) result(thetaW)
        class(Type_WRF_Durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: thetaW

        if (h < 0.0d0) then
            thetaW = self%thetaR + (self%thetaS - self%thetaR) * &
                     (self%w1 * (1.0d0 + abs(self%alpha1 * h)**self%n1)**(-self%m1) &
                      + self%w2 * (1.0d0 + abs(self%alpha2 * h)**self%n2)**(-self%m2))
        else
            thetaW = self%thetaS
        end if

    end function Calculate_WRF_Durner

    function Calculate_WRF_Durner_Derivative(self, h) result(Cw)
        class(Type_WRF_Durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: Cw

        if (h < 0.0d0) then
            Cw = (self%thetaS - self%thetaR) * &
                 (self%w1 * self%alpha1**self%n1 * self%m1 * self%n1 * (-h)**(self%n1 - 1.0d0) * &
                  (1.0d0 + (-self%alpha1 * h)**self%n1)**(-self%m1 - 1.0d0) &
                  + self%w2 * self%alpha2**self%n2 * self%m2 * self%n2 * (-h)**(self%n2 - 1.0d0) * &
                  (1.0d0 + (-self%alpha2 * h)**self%n2)**(-self%m2 - 1.0d0))
        else
            Cw = 0.0d0
        end if

    end function Calculate_WRF_Durner_Derivative

    function Calculate_WRF_DVGCH(self, h) result(thetaW)
        class(Type_WRF_DVGCH), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: thetaW

        if (h < 0.0d0) then
            thetaW = self%thetaR + (self%thetaS - self%thetaR) * &
                     (self%w1 * (1.0d0 + abs(self%alpha1 * h)**self%n1)**(-self%m1) &
                      + self%w2 * (1.0d0 + abs(self%alpha1 * h)**self%n2)**(-self%m2))
        else
            thetaW = self%thetaS
        end if

    end function Calculate_WRF_DVGCH

    function Calculate_WRF_DVGCH_Derivative(self, h) result(Cw)
        class(Type_WRF_DVGCH), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: Cw

        if (h < 0.0d0) then
            Cw = (self%thetaS - self%thetaR) * &
                 (self%w1 * self%alpha1**self%n1 * self%m1 * self%n1 * (-h)**(self%n1 - 1.0d0) * &
                  (1.0d0 + (-self%alpha1 * h)**self%n1)**(-self%m1 - 1.0d0) &
                  + self%w2 * self%alpha1**self%n2 * self%m2 * self%n2 * (-h)**(self%n2 - 1.0d0) * &
                  (1.0d0 + (-self%alpha1 * h)**self%n2)**(-self%m2 - 1.0d0))
        else
            Cw = 0.0d0
        end if

    end function Calculate_WRF_DVGCH_Derivative

end module Calculate_WRF
