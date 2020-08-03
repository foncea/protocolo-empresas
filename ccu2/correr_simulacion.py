import os
import time
import datetime

# Parametros

alg =           ['HacerNada', 'Bios', 'Bios', 'Bios'] # 
frec_test =     [0, 1, 7, 14] * 2
ctna_dur =      [14] * 12
ctna_inic =     [0] * 6 
pob =           [100] * 20
r0 =            [2.9] * 20
tiempo =        [92] * 6
iteraciones =   [1000] * 20
fecha =         ['30-08'] * 20
p_i =           [0.005, 0.005, 0.005, 0.005]  * 2
dia_inicial =   '2020-07-14'
archivo =       'input_3.csv'

# Correr simulaciones de escenarios

print('\n=============== Simulaciones ===============\n\n')

for n in range(len(alg)):
    t0 = time.time()
    argumentos = alg[n] + ' ' + str(frec_test[n]) + ' ' + str(ctna_dur[n]) + ' ' + str(ctna_inic[n]) + ' ' + str(pob[n]) + ' ' + str(r0[n]) + ' ' + str(tiempo[n]) + ' ' + str(iteraciones[n]) + ' ' + fecha[n] + ' ' + str(p_i[n]) + ' ' + dia_inicial + ' ' + archivo
    print(argumentos)

    os.system("python simulaciones_eficiente.py " + argumentos)

    print('\n')

    #print('Finalizado en ' + str(datetime.timedelta(seconds=int(time.time() - t0))) + '\n')