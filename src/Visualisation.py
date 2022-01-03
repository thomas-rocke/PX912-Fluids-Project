import numpy as np
import matplotlib.pyplot as plt


fname = "iteration1.dat"


dat = np.genfromtxt(fname, dtype=np.float64)

print(dat)

plt.plot(dat)
plt.show()