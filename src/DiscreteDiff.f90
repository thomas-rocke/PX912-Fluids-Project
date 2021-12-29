! Functions estimating the differentials of an array given the dx step size

module DiffFuncs
    use ISO_FORTRAN_ENV
    implicit none
    contains


    function OrderOneDiff(x, N, dx) result(diff)
        ! Estimates the first order derivative of x, 
        ! given step size dx
        real(kind=REAL64), dimension(N), intent(in) :: x
        integer, intent(in) :: N
        real(kind=REAL64), intent(in) :: dx
        real(kind=REAL64), dimension(N) :: diff


    end function


    function OrderTwoDiff(x, N, dx) result(diff)
        ! Estimates the 2nd order derivative of x
        ! given step size dx
        real(kind=REAL64), dimension(N), intent(in) :: x
        integer, intent(in) :: N
        real(kind=REAL64), intent(in) :: dx
        real(kind=REAL64), dimension(N) :: diff

    end function

    function NormalGrad(r, N, dr) result(grad)
        ! Estimates grad dot n, where n is a normal vector
        ! given step size dr
        real(kind=REAL64), dimension(N), intent(in) :: r
        integer, intent(in) :: N
        real(kind=REAL64), intent(in) :: dr
        real(kind=REAL64), dimension(N) :: grad, diffOne, diffTwo
        
    end function