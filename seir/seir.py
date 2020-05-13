import numpy as np
import pandas as pd
import sys

# From https://towardsdatascience.com/social-distancing-to-slow-the-coronavirus-768292f04296

def base_seir_model(init_vals, params, t):
    S_0, E_0, I_0, R_0 = init_vals
    S, E, I, R = [S_0], [E_0], [I_0], [R_0]
    alpha, beta, gamma = params
    dt = t[1] - t[0]
    for _ in t[1:]:
        next_S = S[-1] - (beta*S[-1]*I[-1])*dt
        next_E = E[-1] + (beta*S[-1]*I[-1] - alpha*E[-1])*dt
        next_I = I[-1] + (alpha*E[-1] - gamma*I[-1])*dt
        next_R = R[-1] + (gamma*I[-1])*dt
        S.append(next_S)
        E.append(next_E)
        I.append(next_I)
        R.append(next_R)
    return np.stack([S, E, I, R]).T

# Define parameters
t_max = 156
dt = 1
t = np.linspace(0, t_max, int(t_max/dt) + 1)

init_vals = 1 - float(sys.argv[1]), 0, float(sys.argv[1]), 0
alpha = 1 / 3
gamma = 1 / 17
beta = float(sys.argv[2]) * gamma
params = alpha, beta, gamma

# Run simulation
results = base_seir_model(init_vals, params, t)
np.savetxt('seir' + '_r0=' + sys.argv[2] + '_pi=' + sys.argv[1] + '_' + sys.argv[3] + '.csv', results, delimiter=",")
print('seir' + '_r0 =' + sys.argv[2] + '_pi=' + sys.argv[1] + '_' + sys.argv[3] + '.csv')