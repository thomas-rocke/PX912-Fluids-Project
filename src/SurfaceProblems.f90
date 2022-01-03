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
        real(kind=REAL64), dimension(:), allocatable :: h
        real(kind=REAL64) :: Rho, g, sigma, R, dr, P
        contains
        procedure :: init
    end type

    contains

    subroutine init(this, P, Rho, g, sigma, R, h_map)
        ! Initialize Data type with a h_map
        class(Data), intent(out) :: this
        real(kind=REAL64), intent(in) :: Rho, g, sigma, R, P
        real(kind=REAL64), dimension(:), intent(in)  :: h_map

        this%Rho = Rho
        this%g = g
        this%sigma = sigma
        this%R = R
        this%dr = R/(size(h_map) + 1)
        allocate(this%h(size(h_map)))
        this%h = h_map
        this%P = P
    end subroutine

    ! ###################
    ! # ERROR FUNCTIONS #
    ! ###################


    function YoungLaplace(r, h, N, Dat) result(error)
        ! Basic Young-Laplace equation error function
        real(kind=REAL64), dimension(N), intent(in) :: h, r
        integer, intent(in) :: N
        type(Data), intent(in) :: Dat
        real(kind=REAL64), dimension(N) :: error, grad

        grad = NormalGrad(h, r, N, Dat%dr)

        error = Dat%P + Dat%rho * Dat%g * h - Dat%sigma * grad

    end function



end module SurfaceProblems