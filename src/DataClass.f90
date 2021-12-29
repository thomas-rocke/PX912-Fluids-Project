! Classes used to contain useful data


module DataClass
    use ISO_FORTRAN_ENV
    implicit none

    type :: Data
        real(kind=REAL64), dimension(:), allocatable :: h
        real(kind=REAL64) :: Rho, g, sigma
        contains
        procedure :: init
    end type

    contains

    subroutine init(this, Rho, g, sigma, h_map)
        ! Initialize Data type with a h_map
        class(Data), intent(out) :: this
        real(kind=REAL64), intent(in) :: Rho, g, sigma
        real(kind=REAL64), dimension(:), intent(in)  :: h_map

        this%Rho = Rho
        this%g = g
        this%sigma = sigma

        allocate(this%h(size(h_map)))
        this%h = h_map
    end subroutine
end module DataClass