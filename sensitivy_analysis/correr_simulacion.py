import os
import time
import datetime

# Parametros

alg =           ['HacerNada'] * 2 # 
frec_test =     [0, 0, 3, 5, 7, 10, 14] * 2
ctna_dur =      [14] * 20
ctna_inic =     [0] * 20
pob =           [100] * 20
r0 =            [3] * 20
tiempo =        [156] * 20
iteraciones =   [3000] * 20
fecha =         ['03-06'] * 20
p_i =           [0.0075]  * 20
sens =          [0.97] * 10#0.2, 0.4, 0.6, 0.8, 0.95, 0.97, 1]
pps =           ['0,1'] + ['5,10'] * 5

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