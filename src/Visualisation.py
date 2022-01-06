import numpy as np
import matplotlib.pyplot as plt

n=300

sf = 1E3

init = "Initial.dat"
fin = "Final.dat"

in_dat = np.genfromtxt(init, dtype=np.float64)
fin_dat = np.genfromtxt(fin, dtype=np.float64)

x = [i * 10 / (n+1) for i in range(n)]


plt.plot(x, sf*in_dat, label="Zero Gravity")
plt.plot(x, sf*fin_dat, label="g=9.81")
plt.xlabel("Radius (mm)")
plt.ylabel("Surface Height (mm)")

plt.legend()

plt.title("Effect of adding gravity")
plt.show()