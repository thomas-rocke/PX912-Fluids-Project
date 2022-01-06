import numpy as np
import matplotlib.pyplot as plt

n=300

sf = 1E3

P = np.genfromtxt("Pcurve.dat", dtype=np.float64)
V = np.genfromtxt("Vcurve.dat", dtype=np.float64)


plt.scatter(P, V*1E9)
plt.xlabel("Pressure (Pa)")
plt.ylabel("Volume ($mm^3$)")

plt.legend()

plt.title("PV curve for water")
plt.show()