import os
import time
import datetime

# Parametros

alg =           ['HacerNada', 'BiosTurnos', 'BiosTurnos', 'BiosTurnos', 'Bios', 'Bios', 'Bios'] # 
frec_test =     [0, 1, 7, 14, 1, 7, 14] * 2
ctna_dur =      [14] * 12
ctna_inic =     [0] * 16 
pob =           [1000] * 20
r0 =            [3] * 20
tiempo =        [92] * 16
iteraciones =   [1000] * 20
fecha =         ['29-07'] * 20
p_i =           [0.005, 0.005, 0.005, 0.005]  * 2
dia_inicial =   '2020-07-27'
# archivo =       'input_3.csv'

# Correr simulaciones de escenarios

print('\n=============== Simulaciones ===============\n\n')

for n in range(len(alg)):
    t0 = time.time()
    argumentos = alg[n] + ' ' + str(frec_test[n]) + ' ' + str(ctna_dur[n]) + ' ' + str(ctna_inic[n]) + ' ' + str(pob[n]) + ' ' + str(r0[n]) + ' ' + str(tiempo[n]) + ' ' + str(iteraciones[n]) + ' ' + fecha[n] + ' ' + str(p_i[n]) + ' ' + dia_inicial 
    print(argumentos)

    os.system("python simulaciones_eficiente.py " + argumentos)

    print('\n')

    #print('Finalizado en ' + str(datetime.timedelta(seconds=int(time.time() - t0))) + '\n')