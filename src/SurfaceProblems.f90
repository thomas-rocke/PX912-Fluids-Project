! A collection of different young-laplace style equations, with different levels of complexity.
! Each procedure could be used in the Newton iterator to solve for the surface.

! Also holds different boundary conditions that could be used

module SurfaceProblems
    use ISO_FORTRAN_ENV
    use DiffFuncs
    implicit none

    real(kind=REAL64), parameter :: PI = 4.0_REAL64 * atan(1.0_REAL64), OMEGA = 1.0_REAL64, &
                   OMEGA_SQUARE = OMEGA**2

    type :: Data
        ! Axisymmetric bubble model data
        ! h = h(r)
        real(kind=REAL64), dimension(:), allocatable :: h, rad
        real(kind=REAL64) :: Rho, g, sigma, R, dr, P
        integer :: N
        contains
        procedure :: init
    end type

    contains

    subroutine init(this, P, Rho, g, sigma, R, N)
        ! Initialize Data type
        class(Data), intent(out) :: this
        real(kind=REAL64), intent(in) :: Rho, g, sigma, R, P
        integer, intent(in) :: N
        integer :: i

        this%Rho = Rho
        this%g = g
        this%sigma = sigma
        this%R = R
        this%dr = R/(N + 1)
        allocate(this%h(N))
        allocate(this%rad(N))
        this%P = P
        this%N = N

        do i=1, N
            this%rad(i) = (i-1) * this%dr
        end do

    end subroutine

    ! ###################
    ! # ERROR FUNCTIONS #
    ! ###################


    function YoungLaplace(h, N, Dat) result(error)
        ! Basic Young-Laplace equation error function
        real(kind=REAL64), dimension(N), intent(in) :: h
        integer, intent(in) :: N
        type(Data), intent(in) :: Dat
        real(kind=REAL64), dimension(N) :: error, grad

        error = 0.0_REAL64

        grad = NormalGrad(h, Dat%rad, N, Dat%dr)

        error = Dat%P + Dat%rho * Dat%g * h - Dat%sigma * grad
    end function


    function RotationExtension(h, N, Dat) result(error)
        ! Young-Laplace plus rotational term
        real(kind=REAL64), dimension(N), intent(in) :: h
        integer, intent(in) :: N
        type(Data), intent(in) :: Dat
        real(kind=REAL64), dimension(N) :: error

        error = YoungLaplace(h, N, Dat)
        error = error + Dat%rho * Dat%dr * Dat%h * 2.0_REAL64 * PI * OMEGA_SQUARE * Dat%rad
    end function

    ! ######################
    ! # INITIAL CONDITIONS #
    ! ######################

    subroutine ZeroGLowH(Dat)
        ! Analytical solution of a system with no gravity
        ! Under a small h approximation
        ! Modifies Dat%h
        type(Data), intent(inout) :: Dat

        real(kind=REAL64) :: factor, const

        ! h =(P/2sig) * (R^2 - r^2)
        ! = (P/2sig) * R^2 - -(P/2sig) * dr^2 * (i-1)*2
        ! = const - factor * (i-1)^2
        const =  0.5_REAL64 * Dat%P * Dat%R * Dat%R / Dat%sigma
        factor =  0.5_REAL64 * Dat%P / Dat%sigma
        
        Dat%h = const - factor * Dat%rad**2
        

    end subroutine


    subroutine ZeroG(Dat)
        ! Analytical solution of a system with no gravity
        ! Modifies Dat%h
        type(Data), intent(inout) :: Dat

        real(kind=REAL64) :: factor, const

        ! h = - sqrt(4sig^2/P^2 - R^2) + sqrt(4sig^2/P^2 - r^2)
        ! = const + sqrt(factor - r^2)
        factor =  4.0_REAL64 * Dat%sigma**2 / Dat%P ** 2
        const =  sqrt(factor - Dat%R**2)

        Dat%h = (-const + sqrt(factor - Dat%rad**2))

    end subroutine



end module SurfaceProblems