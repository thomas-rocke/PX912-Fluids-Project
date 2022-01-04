import numpy as np
import matplotlib.pyplot as plt


init = "Initial.dat"
fin = "Final.dat"

in_dat = np.genfromtxt(init, dtype=np.float64)
fin_dat = np.genfromtxt(fin, dtype=np.float64)

x = [i * 20E-3 / 101 for i in range(100)]

plt.plot(x, in_dat, label="Initial")
plt.plot(x, fin_dat, label="Final")
plt.plot(x, in_dat - fin_dat, label="Difference")
plt.legend()
plt.show()