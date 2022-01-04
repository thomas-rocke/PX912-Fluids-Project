program main
use ISO_FORTRAN_ENV
use NewtonIteration
use SurfaceProblems

type(Data) :: Dat
real(kind=REAL64) :: P, Rho, g, sigma, R
integer :: N
procedure(func_template), pointer :: f => YoungLaplace

! Use values for water
P = 1.0_REAL64
Rho = 1000.0_REAL64
g = 0
sigma = 0.07_REAL64
R = 20E-3_REAL64
N = 100


call Dat%init(P, Rho, g, sigma, R, N)
call ZeroG(Dat)

Print *, Dat%h
call Newton(Dat%h, f, 1E-3_REAL64, Dat, 1.0_REAL64)
Print *, Dat%h

Print *, f(Dat%h, N, Dat)

end program main