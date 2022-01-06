program main
use ISO_FORTRAN_ENV
use NewtonIteration
use SurfaceProblems

integer, parameter :: M = 51

type(Data) :: Dat
real(kind=REAL64) :: Rho, g, sigma, R, P, P_min, P_max
integer :: N, i
procedure(func_template), pointer :: f => YoungLaplace

P_max = 3_REAL64
P_min = 0.1_REAL64

! Use values for water
P = 5.0_REAL64
Rho = 1000.0_REAL64
g = -9.81_REAL64
sigma = 0.07_REAL64
R = 1E-2_REAL64
N = 300


open(file="Vcurve.dat", unit=15)
open(file="Pcurve.dat", unit=14)

do i=1, M

    !P = p_max/(i)
    P = i * (P_max - P_min)/M + P_min

    call Dat%init(P, Rho, g, sigma, R, N)
    call ZeroG(Dat)
    call Newton(Dat%h, f, 1E-6_REAL64, Dat, 1E-6_REAL64)

    write(15, *) Volume(Dat)
    write(14, *) P

end do
close(15)
close(14)

end program main