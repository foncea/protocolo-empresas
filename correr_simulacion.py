import os
import time
import datetime

# Parametros

alg =           ['HacerNadaTurnos'] # 
frec_test =     [0, 3, 0, 0] * 2
ctna_dur =      [14] * 12
ctna_inic =     [0] * 6 
pob =           [100] * 20
r0 =            [3] * 20
tiempo =        [156] * 6
iteraciones =   [3000] * 20
fecha =         ['02-06'] * 20
p_i =           [0.0075, 0.02, 0.02, 0.02]  * 2

# Correr simulaciones de escenarios

print('\n=============== Simulaciones ===============\n\n')

for n in range(len(alg)):
    t0 = time.time()
    argumentos = alg[n] + ' ' + str(frec_test[n]) + ' ' + str(ctna_dur[n]) + ' ' + str(ctna_inic[n]) + ' ' + str(pob[n]) + ' ' + str(r0[n]) + ' ' + str(tiempo[n]) + ' ' + str(iteraciones[n]) + ' ' + fecha[n] + ' ' + str(p_i[n])
    print(argumentos)

    os.system("python simulaciones_eficiente.py " + argumentos)

    print('\n')

    #print('Finalizado en ' + str(datetime.timedelta(seconds=int(time.time() - t0))) + '\n')