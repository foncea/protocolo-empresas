from base.simulador import SimuladorEficiente
from base.algoritmos import AlgoritmoBios, AlgoritmoHacerNada
from base.individuo import Individuo
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import sys
from tqdm import trange
import datetime as dt

# Probabilidades y parametros globales
p_i = [float(sys.argv[10])]
p_s = [1 - p_i[0]]
p_e = [0.00]
p_r = [0.00]
p_sinto = [0.5]
p_m = [0.02]
p_contagio_diaria = [0.00032]       #basal
inf_post_sint = sys.argv[12].split(',')
inf_post_sint = [[int(inf_post_sint[0]) * 24, int(inf_post_sint[1]) * 24]]
def dict_prob(ps, pe, pi, pr, psinto, pm, p_cont):
    return {'susceptible': ps, 'expuesto': pe, 'infeccioso': pi, 'recuperado': pr, 'sintomatico': psinto, 'muerte': pm, 'contagio_diaria': p_cont}

# Parametros y precision maxima del test
esp = [1]
sen = [float(sys.argv[11])]
def dict_prec_test(e, s_max):
    def precision_test(tiempo, tiempo_inicio_infeccion, dias_sintomas):        
        if tiempo < tiempo_inicio_infeccion + dias_sintomas[0] + 6 * 24:
            s = 1#0.2
        elif tiempo < tiempo_inicio_infeccion + dias_sintomas[0] + 14 * 24:
            s = 1#s_max - 0.05
        else:
            s = 1#s_max
        return 1, 1
    return {'precision': precision_test, 'dia_aparicion_ac': 0}

# Parametros poblacion
tamano_poblacion = [int(sys.argv[5])]
info_poblacion = [pd.read_csv('input_3.csv')]
R0 = [{'empresa': float(sys.argv[6]), 'poblacion': 1.9}]   #R0_empresa

# Parametros algoritmo
lista_algoritmos = {'Bios': lambda x: AlgoritmoBios(x[0], x[1]),
                    'HacerNada': lambda x: AlgoritmoHacerNada(x[0], x[1])}
usar_algoritmo = str(sys.argv[1]) #'HacerNadaCerrar'
frecuencia_test = [int(sys.argv[2])] #[0]
cuarentena = [{'duracion': int(sys.argv[3]) * 24, 
                        'inicial': int(sys.argv[4]) * 24}]

# Parametros simulacion
tiempo_simulacion = int(sys.argv[7]) * 24
numero_iteraciones = int(sys.argv[8]) #100
dia_hora_inicial = dt.datetime.strptime('2020-04-05 9:00', '%Y-%m-%d %H:%M')


parametros = [{'info_poblacion': tamano, 
               'probabilidades': dict_prob(ps, pe, pi, pr, psinto, pm, p_cont),
               'precision_test': dict_prec_test(e, s), 
               'frecuencia_test': frec_t, 
               'cuarentena': dc,
               'R0': r0,
               'inf_post_sint': ips}
              for p_cont in p_contagio_diaria
              for tamano in info_poblacion
              for ps in p_s
              for pe in p_e
              for pi in p_i
              for pr in p_r
              for psinto in p_sinto
              for pm in p_m
              for e in esp
              for s in sen
              for frec_t in frecuencia_test
              for dc in cuarentena
              for r0 in R0
              for ips in inf_post_sint]

resultados_simulacion = []

# Simulacion

resultados_df = pd.DataFrame()
numero_tests = {}
numero_infecciosos = {}
u = 1

cv =  lambda x: np.std(x) / np.mean(x) / np.sqrt(len(x))
def umbral_cv(n, df, u):
    aux = df.copy()
    aux[['estado', 'estado_observado', 'actividad', 'sintomas']] = pd.DataFrame(aux.index.tolist(), index=resultados_df.index)
    infecciones = aux[(aux['estado'] == 'infeccioso') & (aux['tiempo'] % 26 == 0)].groupby(['it'])[0].agg('sum')
    trabajando = aux[aux['actividad'] == 'trabajo'].groupby(['it'])[0].agg('mean')
    infecciones_cv = cv(infecciones) < (u / 100)
    trabajando_cv = cv(trabajando) < (u / 100)
    print(cv(infecciones))

    return infecciones_cv and trabajando_cv

for p in parametros:
    alg = lista_algoritmos[usar_algoritmo]([p['cuarentena'], p['frecuencia_test']])
    for n in trange(numero_iteraciones):
        sim = SimuladorEficiente(p['info_poblacion'], p['precision_test'], p['probabilidades'], p['inf_post_sint'], p['R0'], dia_hora_inicial)
        sim.simular(alg, tiempo_simulacion)

        sim.df_estados_actividades_obs['it'] = n
        resultados_df = pd.concat([resultados_df, sim.df_estados_actividades_obs], axis=0)
        numero_tests[n] = sim.historia_numero_tests 
        numero_infecciosos[n] = sim.numero_infectados_totales

        alg.reset()

        if False:#(n>1000) and n % 500 == 0:
            print('Iteración: ' + str(n))
            if umbral_cv(n, resultados_df, u):
                print('Cv = menor a ' + str(u) + '%')
                break

# Guardar resultados en disco
alg = sys.argv[1]
frec = sys.argv[2]
cinic = sys.argv[4]
pob = sys.argv[5]
r0 = sys.argv[6]
iteraciones = sys.argv[8]
fecha = sys.argv[9]
p_i = sys.argv[10]
sens = sys.argv[11]
inf_post_sint = str(sum(inf_post_sint[0])/2)
sufijo = '_' + alg + '_frec=' + frec + '_r0=' + r0 + '_pi=' + p_i + '_iter=' + iteraciones + '_sens=' + sens +'_' + fecha 

resultados_df[['estado', 'estado_observado', 'actividad', 'sintomas']] = pd.DataFrame(resultados_df.index.tolist(), index=resultados_df.index)  
resultados_df.rename(columns={0: 'cantidad'}).to_csv('../../datos_simulaciones/resultados' + sufijo + '.csv', index=False)

pd.DataFrame.from_dict(numero_tests).T.reset_index().to_csv('../../datos_simulaciones/numero_tests' + sufijo + '.csv')
pd.DataFrame.from_dict(numero_infecciosos).T.reset_index().to_csv('../../datos_simulaciones/numero_infecciosos' + sufijo + '.csv')

#print('Resultados guardados en disco')
