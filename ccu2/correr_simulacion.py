import os
import time
import datetime
import numpy as np

# Funciones
def r0_range(x, y, n):
	r0_emp = np.arange(x[0], y[0], n[0])
	r0_pob = np.arange(x[1], y[1], n[1])
	lattice = [(re, rp) for re in r0_emp for rp in r0_pob]
	r0_emp = [r[0] for r in lattice]
	r0_pob = [r[1] for r in lattice]
	return r0_emp, r0_pob



# Parametros

alg =           ['HacerNada'] * 1000 # 
frec_test =     [7] * 1000
ctna_dur =      [14] * 1000
ctna_inic =     [0] * 1000
pob =           [100] * 1000
r0 =            [2.9] 
r0_pob =        [1.6] * 1000
tiempo =        [92] * 1000
iteraciones =   [1000] * 1000
fecha =         ['14-08'] * 1000
p_i =           [0.005, 0.005, 0.005, 0.005]  * 1000
dia_inicial =   '2020-07-14'
archivo =       'input_3.csv'

r0, r0_pob = r0_range([2, 0.8], [4.1, 1.0], [0.2, 0.2])
r0 = np.round(r0, 1)
r0_pob = np.round(r0_pob, 1)

# Correr simulaciones de escenarios

print('\n=============== Simulaciones ===============\n\n')

for n in range(len(alg)):
    t0 = time.time()
    argumentos = alg[n] + ' ' + str(frec_test[n]) + ' ' + str(ctna_dur[n]) + ' ' + str(ctna_inic[n]) + ' ' + str(pob[n]) + ' ' + str(r0[n]) + ' ' + str(tiempo[n]) + ' ' + str(iteraciones[n]) + ' ' + fecha[n] + ' ' + str(p_i[n]) + ' ' + dia_inicial + ' ' + archivo + ' ' + str(r0_pob[n])
    print(argumentos)

    os.system("python simulaciones_eficiente.py " + argumentos)

    print('\n')

    #print('Finalizado en ' + str(datetime.timedelta(seconds=int(time.time() - t0))) + '\n')