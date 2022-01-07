import numpy as np
import matplotlib.pyplot as plt

n=300

sf = 1E9

R = 1E-4

init = "Initial.dat"
fin = "Final.dat"

in_dat = np.genfromtxt(init, dtype=np.float64)
fin_dat = np.genfromtxt(fin, dtype=np.float64)

x = [i * R/ (n+1) for i in range(n)]


plt.plot(x, sf*in_dat, label="Zero Gravity")
plt.plot(x, sf*fin_dat, label="g=9.81")
plt.xlabel("Radius ($\mu$m)")
plt.ylabel("Surface Height (pm)")

plt.legend()

plt.title("Effect of adding gravity")
plt.show()