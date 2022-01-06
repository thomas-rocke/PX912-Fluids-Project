import numpy as np
import matplotlib.pyplot as plt

n=300

sf = 1E6

init = "Initial.dat"
fin = "Final.dat"

in_dat = np.genfromtxt(init, dtype=np.float64)
fin_dat = np.genfromtxt(fin, dtype=np.float64)

x = [i * 10 / (n+1) for i in range(n)]

fig, ax = plt.subplots(nrows=2, sharex=True)

ax[0].plot(x, sf*in_dat, label="Initial")
ax[0].plot(x, sf*fin_dat, label="Final")
ax[1].plot(x, (in_dat - fin_dat)* sf, label="Relative Difference")

ax[1].set_xlabel("Radius (mm)")
ax[0].set_ylabel("Surface Height ($\mu$m)")

ax[1].set_ylabel("Error ($\mu$m)")
ax[0].legend()

ax[0].set_title("Agreement for P=5Pa")
plt.show()