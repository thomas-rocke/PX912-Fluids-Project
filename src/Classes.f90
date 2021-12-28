! Classes used to contain useful data


module Classes
    use ISO_FORTRAN_ENV
    implicit none

    type :: BaseType
        real(kind=REAL64) :: Rho, g, sigma
    end type

    type, extends(BaseType) :: Case1D
        real(kind=REAL64), dimension(:), allocatable :: h
        contains
        procedure :: create => init1D
    end type

    type, extends(BaseType) :: Case2D
        real(kind=REAL64), dimension(:, :), allocatable :: h
        contains
        procedure :: create => init2D
    end type

    contains

    subroutine init1D(this, Rho, g, sigma, h_map)
        ! Initialize Case1D type with a 1D h_map
        ! h = h_map(x) or h = h_map(r)
        class(Case1D) :: this
        real(kind=REAL64), intent(in) :: Rho, g, sigma
        real(kind=REAL64), dimension(:), intent(in)  :: h_map

        this%Rho = Rho
        this%g = g
        this%sigma = sigma

        allocate(this%h(size(h_map)))
        this%h = h_map
    end subroutine

    subroutine init2D(this, Rho, g, sigma, h_map)
        ! Initialize Case1D type with a 2D h_map
        ! h = h_map(x, y) or h = h_map(r, theta)
        class(Case2D), intent(out) :: this
        real(kind=REAL64), intent(in) :: Rho, g, sigma
        real(kind=REAL64), dimension(:, :), intent(in)  :: h_map

        this%Rho = Rho
        this%g = g
        this%sigma = sigma

        allocate(this%h(size(h_map, 1), size(h_map, 2)))
        this%h = h_map
    end subroutine
end module classes