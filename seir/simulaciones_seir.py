from simulador_seir import SimuladorBase, SimuladorEficiente, SimuladorRoles, SimuladorFamilias

from individuo_seir import Individuo
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import sys
from tqdm import trange

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
duracion_infeccion = [[24, 28]]
p_contagio_diaria = [0.00032]       #basal
def dict_prob(ps, pe, pi, pr, psinto, pm, p_cont):
    return {'susceptible': ps, 'expuesto': pe, 'infeccioso': pi, 'recuperado': pr, 'sintomatico': psinto, 'muerte': pm, 'contagio_diaria': p_cont}

# Parametros y precision maxima del test
esp = [1]#[0.97]
sen = [0.93]
dia_aparicion_ac = [[7, 12]]
def dict_prec_test(eg, sg, em, sm, dias_ac):
    def precision_test(tiempo_infeccion, dia_ac):
        if tiempo_infeccion < dia_ac:
            return {'E_G': eg, 'S_G': 0, 'E_M': em, 'S_M': 0}
        else:
            return {'E_G': eg, 'S_G': sg, 'E_M': em, 'S_M': sm}
    return {'precision': precision_test, 'dia_aparicion_ac': dias_ac}
def dict_prec_test(e, s, dias_ac):
    def precision_test(tiempo_infeccion, dia_ac):
        if tiempo_infeccion < dia_ac:
            return {'E': e, 'S': 0}
        else:
            return {'E': e, 'S': s}
    return {'precision': precision_test, 'dia_aparicion_ac': dias_ac}

# Parametros poblacion
tamano_poblacion = [int(sys.argv[5])]
R0 = [float(sys.argv[6])]   #R0_empresa


# Parametros simulacion
tiempo_simulacion = int(sys.argv[7])
numero_iteraciones = int(sys.argv[8]) #100

parametros = [{'duracion_infeccion': df, 
               'tamano': tamano, 
               'probabilidades': dict_prob(ps, pe, pi, pr, psinto, pm, p_cont),
               'precision_test': dict_prec_test(e, s, dac), 
               'R0': r0}
              for df in duracion_infeccion
              for p_cont in p_contagio_diaria
              for tamano in tamano_poblacion
              for ps in p_s
              for pe in p_e
              for pi in p_i
              for pr in p_r
              for psinto in p_sinto
              for pm in p_m
              for e in esp
              for s in sen
              for dac in dia_aparicion_ac
              for r0 in R0]

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
    for n in trange(numero_iteraciones):
        sim = SimuladorEficiente(p['tamano'], p['precision_test'], p['probabilidades'], p['duracion_infeccion'], p['R0'])
        sim.simular(tiempo_simulacion)

        sim.df_estados_actividades_obs['it'] = n
        resultados_df = pd.concat([resultados_df, sim.df_estados_actividades_obs], axis=0)
        numero_tests[n] = sim.historia_numero_tests 
        numero_infecciosos[n] = sim.numero_infectados_totales

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
sufijo = '_r0=' + r0 + '_pi=' + p_i + '_iter=' + iteraciones + '_' + fecha 

resultados_df[['estado', 'estado_observado', 'actividad', 'sintomas']] = pd.DataFrame(resultados_df.index.tolist(), index=resultados_df.index)  
resultados_df.rename(columns={0: 'cantidad'}).to_csv('simulacion_seir' + sufijo + '.csv', index=False)

#print('Resultados guardados en disco')
