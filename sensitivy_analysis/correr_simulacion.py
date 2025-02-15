import os
import time
import datetime

# Parametros

alg =           ['Bios', 'HacerNada'] #* 2 + ['HacerNada']
frec_test =     [3, 0, 0] * 2
ctna_dur =      [14] * 20
ctna_inic =     [0] * 20
pob =           [380] * 20
r0 =            [3] * 20
tiempo =        [30] * 20
iteraciones =   [1] * 20
fecha =         ['27-07'] * 20
p_i =           [0.05]  * 20
sens =          [0.88] * 10#0.2, 0.4, 0.6, 0.8, 0.95, 0.97, 1]
pps =           ['5,10'] * 5

# Correr simulaciones de escenarios

print('\n=============== Simulaciones ===============\n\n')

for n in range(len(alg)):
    t0 = time.time()
    argumentos = alg[n] + ' '\
    + str(frec_test[n]) + ' '\
    + str(ctna_dur[n]) + ' '\
    + str(ctna_inic[n]) + ' '\
    + str(pob[n]) + ' '\
    + str(r0[n]) + ' '\
    + str(tiempo[n]) + ' '\
    + str(iteraciones[n]) + ' '\
    + fecha[n] + ' '\
    + str(p_i[n]) + ' '\
    + str(sens[n]) + ' '\
    + str(pps[n]) 
    print(argumentos)

    os.system("python simulaciones_eficiente.py " + argumentos)

    print('\n')

    #print('Finalizado en ' + str(datetime.timedelta(seconds=int(time.time() - t0))) + '\n')