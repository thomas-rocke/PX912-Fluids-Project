! Newton-Raphson iterators for 1D and 2D data

module NewtonIteration
    use ISO_FORTRAN_ENV
    implicit none

    interface template
            pure function func_template(x, N)
                use ISO_FORTRAN_ENV
                real(kind=REAL64), intent(in), dimension(N) :: x
                integer, intent(in) :: N
                real(kind=REAL64), dimension(N) :: func
            end function
    end interface


    contains
    
    subroutine NewtonIter(x, f, eps, N)
        ! Perform one iteration of the newton method, updating x
        real(kind=REAL64), intent(inout), dimension(N) :: x ! X data
        procedure(func_template), pointer :: f ! Function pointer (Newton error function), returns array of length M
        real(kind=REAL64), intent(in) :: eps ! Deviation used to approx Jacobian
        integer, intent(in) :: N ! Length of x and of f(x)

        real(kind=REAL64), dimension(N, N) :: Jacobian

    end subroutine



    function GetJacobian(x, f, eps, N) result(Jacobian)
        ! Approximates the jacobian at x using a central differences method
        ! Partial Derivative approximation (df_i/dx_j) uses dx_j = eps*x_k j=k, 0 otherwise

        real(kind=REAL64), intent(in), dimension(N) :: x ! X data
        procedure(func_template), pointer :: f ! Function pointer (Newton error function), returns array of length M
        real(kind=REAL64), intent(in) :: eps ! Deviation used to approx Jacobian
        integer, intent(in) :: N ! Length of x and of f(x)

        real(kind=REAL64), dimension(N, N) :: Jacobian
        real(kind=REAL64), dimension(N) :: dx
        integer, dimension(N) :: indeces

        integer :: i, j

        indeces = 1
        do i=1, N
            indeces(i) = i
        end do

        DO j=1, N
            dx = 0.0_REAL64
            dx(j) = x(j) * eps ! dx_i = eps * x_j for i=j, 0 otherwise
            Jacobian(:, j) = (f(x + dx, N) + f(x - dx, N))/(2 * dx(j)) ! Do df_i/dx_j for all f_is 
        end do
    end function



end module NewtonIteration