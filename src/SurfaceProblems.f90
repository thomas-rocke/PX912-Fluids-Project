! A collection of different young-laplace style equations, with different levels of complexity.
! Each procedure could be used in the Newton iterator to solve for the surface.

! Also holds different boundary conditions that could be used

module SurfaceProblems
    use ISO_FORTRAN_ENV
    implicit none

    type :: Data
        ! Axisymmetric bubble model data
        ! h = h(r)
        real(kind=REAL64), dimension(:), allocatable :: h
        real(kind=REAL64) :: Rho, g, sigma, R, dr
        contains
        procedure :: init
    end type

    contains

    subroutine init(this, Rho, g, sigma, R, h_map)
        ! Initialize Data type with a h_map
        class(Data), intent(out) :: this
        real(kind=REAL64), intent(in) :: Rho, g, sigma, R
        real(kind=REAL64), dimension(:), intent(in)  :: h_map

        this%Rho = Rho
        this%g = g
        this%sigma = sigma
        this%R = R
        this%dr = R/size(h_map)
        allocate(this%h(size(h_map)))
        this%h = h_map
    end subroutine

    ! ###################
    ! # ERROR FUNCTIONS #
    ! ###################


    function YoungLaplace(x, N, Dat) result(error)
        ! Basic Young-Laplace equation error function
        real(kind=REAL64), dimension(N), intent(in) :: x
        integer, intent(in) :: N
        type(Data), intent(in) :: Dat
        real(kind=REAL64), dimension(N) :: error

        error = 0.0_REAL64

    end function



end module SurfaceProblems