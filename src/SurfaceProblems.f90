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
        real(kind=REAL64), dimension(2) :: ClampHeights = (/0.0_REAL64, 0.0_REAL64/)
        procedure(NullBC), pointer :: BC => NullBC
        contains
        procedure :: init
    end type

    contains

    subroutine init(this, Rho, g, sigma, R, h_map, BC)
        ! Initialize Data type with a h_map
        class(Data), intent(out) :: this
        real(kind=REAL64), intent(in) :: Rho, g, sigma, R
        real(kind=REAL64), dimension(:), intent(in)  :: h_map

        procedure(NullBC) :: BC

        this%Rho = Rho
        this%g = g
        this%sigma = sigma
        this%R = R
        this%dr = R/size(h_map)
        allocate(this%h(size(h_map)))
        this%h = h_map

        this%BC => BC
    end subroutine


    ! #######################
    ! # BOUNDARY CONDITIONS #
    ! #######################

    subroutine NullBC(this)
        ! No Boundary Conditions
        class(Data), intent(inout) :: this
    end subroutine

    subroutine ClampAtEdge(this)
        ! First and last heights should be zero
        class(Data), intent(inout) :: this
        integer :: N
        N = size(this%h)
        this%h(1) = this%ClampHeights(1)
        this%h(N) = this%ClampHeights(2)
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