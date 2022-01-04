! A collection of different young-laplace style equations, with different levels of complexity.
! Each procedure could be used in the Newton iterator to solve for the surface.

! Also holds different boundary conditions that could be used

module SurfaceProblems
    use ISO_FORTRAN_ENV
    use DiffFuncs
    implicit none

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


        grad = NormalGrad(h, Dat%rad, N, Dat%dr)

        error = Dat%P + Dat%rho * Dat%g * h - Dat%sigma * grad

    end function


    ! ######################
    ! # INITIAL CONDITIONS #
    ! ######################

    subroutine ZeroG(Dat)
        ! Analytical solution of a system with no gravity
        ! Modifies Dat%h
        type(Data), intent(inout) :: Dat

        integer :: i
        real(kind=REAL64) :: factor, const

        ! h = -(P/2sig) * (R^2 - r^2)
        ! = -(P/2sig) * R^2 - -(P/2sig) * dr^2 * (i-1)*2
        ! = const - factor * (i-1)^2
        const = - 0.5_REAL64 * Dat%P * Dat%R * Dat%R / Dat%sigma
        factor = - 0.5_REAL64 * Dat%P * Dat%dr * Dat%dr / Dat%sigma

        do i=1, Dat%N
            Dat%h(i) = const - factor * (i-1) * (i-1)
        end do

    end subroutine



end module SurfaceProblems