import numpy as np
import matplotlib.pyplot as plt

a = np.loadtxt('support.csv', delimiter=',')

# [timestep, support] only for node 8
b = a[:, [0, 2]][a[:, 1] == 8]

bx = plt.plot(b[:, 0], b[:, 1])

#plt.show()
