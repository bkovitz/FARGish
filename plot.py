import numpy as np
import tkinter
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt

a = np.loadtxt('support.csv', delimiter=',')

node = 27

# [timestep, support] only for node
b = a[:, [0, 2]][a[:, 1] == node]

bx = plt.plot(b[:, 0], b[:, 1])

plt.show()
