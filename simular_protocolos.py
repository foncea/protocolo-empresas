import os
import time
import datetime
import itertools
import pandas as pd
import numpy as np
import random

# Parametros

parametros = pd.DataFrame()
parametros['protocolo'] =       ['ABT', 'Base']
parametros['frecuencia_test'] = [3, 0]

df = {}
df['cuarentena_duracion'] = 14
df['cuarentena_inicial'] =  0
df['tamano_poblacion'] =    100
df['r0_emp'] =              3 #np.linspace(1, 5, num=9)
df['r0_pob'] =              1.5  #np.linspace(0.5, 2.5, num=9)
df['prev_inic'] =           0.0075#[0.005, 0.01, 0.0075, 0.0025]
df['sensibilidad'] =        0.88#[0.58, 0.68, 0.78, 0.88, 0.98]
df['inf_post_sint'] =       ['0,1', '1,5', '5,10', '10,15']
df['tiempo_simulacion'] =   150
df['numero_iteraciones'] =  500
fecha =                     '15-10'
serie =                     'base'
df['id'] =                  fecha + '_' + serie


#######################################################################

parametros['key'] = 1
for col in df:
    aux_df = pd.DataFrame({col: pd.Series(df[col])})
    aux_df['key'] = 1
    parametros = parametros.merge(aux_df, on='key')

print(parametros)

#######################################################################

import base_v2.simulador as simulador
import base_v2.algoritmos as algoritmos
import base_v2.individuo as individuos

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import sys
from tqdm import trange

def prec_test(e=1, s_max=1):
    def precision_test(tiempo, tiempo_inicio_infeccion, dias_sintomas):        
        if tiempo < tiempo_inicio_infeccion + dias_sintomas[0] + 7:
            s = 0.2
        else:
            s = s_max
        return e, s
    return precision_test

def dict_prob(pi, psinto, pm):
    return {'susceptible': 1 - pi, 'expuesto': 0, 'infeccioso': pi, 'recuperado': 0, 'sintomatico': psinto, 'muerte': pm}

lista_algoritmos = {'ABT': algoritmos.AlgoritmoBios,
                    'Base': algoritmos.AlgoritmoHacerNada,
                    'ABT_Turnos' : algoritmos.AlgoritmoBiosTurnos,
                    'Contactos_Estrechos': algoritmos.AlgoritmoContactoEstrechos,
                    }

protocolos_simulados = parametros[['protocolo', 'frecuencia_test']].drop_duplicates()
protocolos_simulados['protocolo_frec'] = protocolos_simulados['protocolo'] + protocolos_simulados['frecuencia_test'].astype(str)

serial = list(parametros['id'])[0]

for index_pf, pf in protocolos_simulados.iterrows():
    parametros_aux = parametros.loc[lambda x: x['protocolo'] == pf['protocolo']][lambda x: x['frecuencia_test'] == pf['frecuencia_test']]

    resultados_df = []
    numero_infecciosos = []

    for index, p in parametros_aux.iterrows():
        print(str(index) + "/" + str(parametros.shape[0]))
        print(p)
        pp = pd.DataFrame(p).T
        pp['key'] = 1

        cuarentena = {'duracion': p['cuarentena_duracion'],
                      'inicial': p['cuarentena_inicial']}
        alg = lista_algoritmos[p['protocolo']](cuarentena, p['frecuencia_test'])

        for n in trange(p['numero_iteraciones']):
            random.seed(n+1)
            np.random.seed(n+1)

            precision_test = prec_test(e=0.99, s_max=p['sensibilidad'])
            probabilidades = dict_prob(p['prev_inic'], 1/2, 0)
            R0 = {'empresa': p['r0_emp'], 'poblacion': p['r0_pob']}
            ips = p['inf_post_sint'].split(',')
            ips = [int(ips[0]), int(ips[1])]

            sim = simulador.SimuladorEficiente(p['tamano_poblacion'], precision_test, probabilidades, ips, R0)
            sim.simular(alg, p['tiempo_simulacion'])

            sim.df_estados_actividades_obs[['estado', 'estado_observado', 'actividad', 'sintomas']] = pd.DataFrame(sim.df_estados_actividades_obs.index.tolist(), index=sim.df_estados_actividades_obs.index)

            sim.df_estados_actividades_obs['it'] = n
            sim.df_estados_actividades_obs['key'] = 1

            aux = pd.merge(sim.df_estados_actividades_obs, pp, on='key')
            resultados_df.append(aux.drop(['key'], axis=1))

            num_inf_aux = pd.DataFrame()
            num_inf_aux['infectados'] = sim.numero_infectados_totales
            num_inf_aux['tiempo'] = range(p['tiempo_simulacion'] + 1)
            num_inf_aux['it'] = n
            num_inf_aux['key'] = 1

            aux = pd.merge(num_inf_aux, pp, on='key')
            numero_infecciosos.append(aux.drop(['key'], axis=1))

            alg.reset()

            # if False:#(n>1000) and n % 500 == 0:
            #     if umbral_cv(n, resultados_df, u):
            #         print('Cv = menor a ' + str(u) + '%')
            #         break

        # if index % 4 == 0:
        #     pd.concat(resultados_df, axis=0).rename(columns={0: 'cantidad'}).to_csv('../datos_simulaciones/resultados_' + serial + '.csv', index=False)
        #     pd.concat(numero_infecciosos, axis=0).to_csv('../datos_simulaciones/numero_infecciosos_' + serial + '.csv', index=False)

    resultados_df = pd.concat(resultados_df, axis=0)
    numero_infecciosos = pd.concat(numero_infecciosos, axis=0)

    # resultados_df[['estado', 'estado_observado', 'actividad', 'sintomas']] = pd.DataFrame(resultados_df.index.tolist(), index=resultados_df.index)
    resultados_df.rename(columns={0: 'cantidad'}).to_csv('../datos_simulaciones/resultados_' + pf['protocolo_frec'] + '_' + serial + '.csv', index=False)

    numero_infecciosos.to_csv('../datos_simulaciones/numero_infecciosos_' + pf['protocolo_frec'] + '_' + serial + '.csv', index=False)

    print('Guardando ' + pf['protocolo_frec'] + '_' + serial)
    