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
        real(kind=REAL64), dimension(0:N+1) :: GuardedX
        integer i

        ! Place x into guarded array
        GuardedX(1:N) = x
        ! Boundary conditions
        GuardedX(0) = x(2)
        GuardedX(N+1) = 0

        do i=1, N
            diff(i) = (GuardedX(i+1) - GuardedX(i-1))/(2 * dx)
        end do

    end function


    function OrderTwoDiff(x, N, dx) result(diff)
        ! Estimates the 2nd order derivative of x
        ! given step size dx
        real(kind=REAL64), dimension(N), intent(in) :: x
        integer, intent(in) :: N
        real(kind=REAL64), intent(in) :: dx
        real(kind=REAL64), dimension(N) :: diff
        real(kind=REAL64), dimension(0:N+1) :: GuardedX
        integer :: i

        ! Place x into guarded array
        GuardedX(1:N) = x
        ! Boundary conditions
        GuardedX(0) = x(2)
        GuardedX(N+1) = 0

        do i=1, N
            diff(i) = (GuardedX(i+1) - 2 * GuardedX(i) + GuardedX(i-1))/(dx * dx)
        end do

    end function

    function NormalGrad(h, r, N, dr) result(grad)
        ! Estimates grad dot n, where n is a normal vector
        ! given step size dr
        ! and h, where h = f(r)
        real(kind=REAL64), dimension(N), intent(in) :: h, r
        integer, intent(in) :: N
        real(kind=REAL64), intent(in) :: dr
        real(kind=REAL64), dimension(N) :: grad, diffOne, diffTwo

        diffOne = OrderOneDiff(h, N, dr)
        diffTwo = OrderTwoDiff(h, N, dr)


        ! Add 1E-90 offset to denominator to avoid DIV0 NAN values
        grad = - diffTwo/sqrt(1 + diffOne * diffOne)**3  - diffOne/(r * sqrt(1 + diffOne * diffOne) + 1E-90_REAL64)
        
    end function
end module