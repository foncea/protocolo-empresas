from base.simulador import SimuladorBase, SimuladorEficiente
# from base.seir.simulador_ex import SimuladorBase, SimuladorEficiente, SimuladorRoles, SimuladorFamilias, Simulador2
from base.algoritmos import AlgoritmoBios, AlgoritmoHacerNada
from base.individuo import Individuo
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import sys
from tqdm import trange
import datetime as dt

'''
# TODO:
# + Cambiar duracion de infeccioso (dejarlo como parametro)
# + Cambiar p_m para que de 2% (edad laboral)
# + Prob contagio: peso = numero de horas laborales/16
# + Agregar R_0, es lineal?
# + Agregar tiempo ciego test
# + Agregar dias de infeccion inicial en indiviuos
# + Escenario no hacer nada
# + Agregar tag en la real vs observada
# + Periodo recuperacion unif[24,28]
# + Periodo ciego unif[8,12]
# + Especifidad cambia en 4 y 7
# + Agregar grafico de muertes
# + Cambiar frecuencia test
# + Despues de cuarentena no se testea y se deja como recuperado
# + Cambiar condiciones de test (ya no hay inmune)
# + Precision test no depende del dia
# + Cuarentena 14 dias (desde sinntomas)
# + R0 global y R0 empresa como input
# + Recuperado solo despues de cuarentena
# + Agregar pcr con n dias
# + Agregar plot de infecciosos trabajando
# + Aumentar R0 a 10
# + Arreglar HacerNadaCerrar
# + Disminuir prob contagio para ind en cuarentena
# + optimizar simulacion para calcular df inmediatamente

# - estudiantes son menos probables de tener sintomas
# - arreglar numero de infecciosos acumulados

# Casos
# + Hacer nada
# + Hacer nada pero cerrar si hay un caso
# + Algoritmo BIOS
# + Testear y cerrar si hay un caso
# + Bios con diferente frecuencia de test (1,2,3,4)
# + Bios con turnos
# + HacerNada con turnos
# + Aplicar cuarentena inicial (14, 7) (falta correr - codigo listo)
'''

# Probabilidades y parametros globales
p_i = [float(sys.argv[10])]
p_s = [1 - p_i[0]]
p_e = [0.00]
p_r = [0.00]
p_sinto = [0.5]
p_m = [0.02]
p_contagio_diaria = [0.00032]       #basal
inf_post_sint = [[5, 10]]
def dict_prob(ps, pe, pi, pr, psinto, pm, p_cont):
    return {'susceptible': ps, 'expuesto': pe, 'infeccioso': pi, 'recuperado': pr, 'sintomatico': psinto, 'muerte': pm, 'contagio_diaria': p_cont}

# Parametros y precision maxima del test
esp = [0.99]
sen = [0.88]
def dict_prec_test(e, s_max):
    def precision_test(tiempo, tiempo_inicio_infeccion, dias_sintomas):        
        if tiempo < tiempo_inicio_infeccion + dias_sintomas[0] + 7:
            s = 0.2
        else:
            s = s_max
        return e, s
    return {'precision': precision_test, 'dia_aparicion_ac': 0}

# Parametros poblacion
tamano_poblacion = [int(sys.argv[5])]
input_data = pd.read_csv(sys.argv[12])
input_data = input_data.loc[lambda x: x['tipo_turno'] != 'Fuera de planta'].copy()
print(input_data['tipo_turno'].value_counts())
info_poblacion = [input_data.reset_index()]
R0 = [{'empresa': float(sys.argv[6]), 'poblacion': float(sys.argv[13])}]   #R0_empresa


# Parametros algoritmo
lista_algoritmos = {'Bios': lambda x: AlgoritmoBios(x[0], x[1]),
                    'HacerNada': lambda x: AlgoritmoHacerNada(x[0], x[1]),
                    }
usar_algoritmo = str(sys.argv[1]) #'HacerNadaCerrar'
frecuencia_test = [int(sys.argv[2])] #[0]
cuarentena = [{'duracion': int(sys.argv[3]), 
                        'inicial': int(sys.argv[4])}]

# Parametros simulacion
tiempo_simulacion = int(sys.argv[7])
numero_iteraciones = int(sys.argv[8]) #100

dia_inicial = dt.datetime.strptime(sys.argv[11], '%Y-%m-%d')

parametros = [{'info_poblacion': tamano, 
               'probabilidades': dict_prob(ps, pe, pi, pr, psinto, pm, p_cont),
               'precision_test': dict_prec_test(e, s), 
               'frecuencia_test': frec_t, 
               'cuarentena': dc,
               'R0': r0}
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
              for r0 in R0]

resultados_simulacion = []

# Simulacion

resultados_df = pd.DataFrame()
numero_tests = {}
numero_infecciosos = {}
numero_observado = {}

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
        sim = SimuladorEficiente(p['info_poblacion'], p['precision_test'], p['probabilidades'], 0, p['R0'], dia_inicial)
        sim.simular(alg, tiempo_simulacion)

        sim.df_estados_actividades_obs['it'] = n
        resultados_df = pd.concat([resultados_df, sim.df_estados_actividades_obs], axis=0)
        numero_tests[n] = sim.historia_numero_tests 
        numero_infecciosos[n] = sim.numero_infectados_totales
        numero_observado[n] = sim.numero_observados_totales

        alg.reset()

        # if False:#(n>1000) and n % 500 == 0:
        #     print('Iteración: ' + str(n))
        #     if umbral_cv(n, resultados_df, u):
        #         print('Cv = menor a ' + str(u) + '%')
        #         break

# Guardar resultados en disco
alg = sys.argv[1]
frec = sys.argv[2]
cinic = sys.argv[4]
pob = sys.argv[5]
r0 = sys.argv[6]
iteraciones = sys.argv[8]
fecha = sys.argv[9]
p_i = sys.argv[10]
r0_pob = sys.argv[13]
sufijo = '_' + alg + '_frec=' + frec + '_pob=' + pob + '_r0=' + r0 + '_r0pob=' + r0_pob + '_pi=' + p_i + '_iter=' + iteraciones + '_' + fecha 

resultados_df[['estado', 'estado_observado', 'actividad', 'sintomas']] = pd.DataFrame(resultados_df.index.tolist(), index=resultados_df.index)  
resultados_df.rename(columns={0: 'cantidad'}).to_csv('../../datos_simulaciones/resultados' + sufijo + '.csv', index=False)

pd.DataFrame.from_dict(numero_tests).T.reset_index().to_csv('../../datos_simulaciones/numero_tests' + sufijo + '.csv')
pd.DataFrame.from_dict(numero_infecciosos).T.reset_index().to_csv('../../datos_simulaciones/numero_infecciosos' + sufijo + '.csv')
pd.DataFrame.from_dict(numero_observado).T.reset_index().to_csv('../../datos_simulaciones/numero_observado' + sufijo + '.csv')

#print('Resultados guardados en disco')
