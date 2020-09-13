import os
import time
import datetime
import itertools

# Parametros

df = pd.DataFrame()
df['protocolo'] = 			['Bios', 'HacerNada']
df['frecuencia_test'] = 	[7, 0]
df['cuarentena_duracion'] = 14
df['cuarentena_inicial'] = 	0
df['tamano_poblacion'] = 	100
df['r0_emp'] = 				3
df['r0_pob'] = 				1.5
df['sensibilidad'] = 		0.88
df['inf_post_sint'] = 		'5,10'
df['tiempo'] = 				150
df['numero_iteraciones'] = 	1000
fecha = 					'21-08'
serie = 					''
df['serial'] = 				fecha + serie


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