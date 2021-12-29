! Newton-Raphson iterators for 1D and 2D data

module NewtonIteration
    use ISO_FORTRAN_ENV
    use SurfaceProblems
    implicit none

    interface template
            pure function func_template(x, N, Dat)
                use ISO_FORTRAN_ENV
                use SurfaceProblems
                real(kind=REAL64), intent(in), dimension(N) :: x
                integer, intent(in) :: N
                type(Data), intent(in) :: Dat
                real(kind=REAL64), dimension(N) :: func
            end function
    end interface


    contains
    
    subroutine Newton(x, f, eps, Dat, tol)
        ! Full Newton-Raphson iteration to solve for x
        ! Given the set of error functions f
        real(kind=REAL64), intent(inout), dimension(:) :: x ! X data
        procedure(func_template), pointer :: f ! Function pointer (Newton error function), returns array of length M
        real(kind=REAL64), intent(in) :: eps, tol ! Deviation used to approx Jacobian, Error tolerance
        type(Data), intent(inout) :: Dat

        integer :: N, i
        integer, parameter :: MAX_ITERATIONS = 10000
        real(kind=REAL64) :: err
        real(kind=REAL64), dimension(size(x)) :: errs

        N = size(x)

        do i=1, MAX_ITERATIONS
            call NewtonIter(x, f, eps, N, Dat)
            call Dat%BC() ! Apply boundary conditions
            errs = f(x, N, Dat)
            err = sum(abs(errs))
            Print *, "Iteration", i, " completed with error", err
            if (err < tol) then
                Print *, "Convergence reached, stopping"
                return
            end if
        end do
        Print *, "Maximum number of iterations reached, stopping"
    end subroutine


    subroutine NewtonIter(x, f, eps, N, Dat)
        ! Perform one iteration of the newton method, updating x
        real(kind=REAL64), intent(inout), dimension(N) :: x ! X data
        procedure(func_template), pointer :: f ! Function pointer (Newton error function), returns array of length M
        real(kind=REAL64), intent(in) :: eps ! Deviation used to approx Jacobian
        integer, intent(in) :: N ! Length of x and of f(x)
        type(Data), intent(in) :: Dat

        real(kind=REAL64), dimension(N, N) :: Jacobian
        real(kind=REAL64), dimension(N) :: dx
        integer :: ipiv, info

        Jacobian = GetJacobian(x, f, eps, N, Dat)

        ! Solve linear equation J_ij(x) dx_i = - f_j(x) 
        dx = - f(x, N, Dat) ! dgesv will modify this to become the correct dx
        call dgesv(N, 1, Jacobian, N, ipiv, dx, N, info)

        if (info /= 0) then
            print *, "Error occurred in linear solving" 
            stop
        end if

        x = x + dx

    end subroutine



    function GetJacobian(x, f, eps, N, Dat) result(Jacobian)
        ! Approximates the jacobian at x using a central differences method
        ! Partial Derivative approximation (df_i/dx_j) uses dx_j = eps*x_k j=k, 0 otherwise

        real(kind=REAL64), intent(in), dimension(N) :: x ! X data
        procedure(func_template), pointer :: f ! Function pointer (Newton error function), returns array of length M
        real(kind=REAL64), intent(in) :: eps ! Deviation used to approx Jacobian
        integer, intent(in) :: N ! Length of x and of f(x)
        type(Data), intent(in) :: Dat

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
            Jacobian(:, j) = (f(x + dx, N, Dat) + f(x - dx, N, Dat))/(2 * dx(j)) ! Do df_i/dx_j for all f_is 
        end do
    end function



end module NewtonIteration