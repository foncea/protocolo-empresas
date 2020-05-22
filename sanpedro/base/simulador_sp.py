import numpy as np
import pandas as pd
from .individuo_sp import IndividuoAdhiere
from collections import Counter, defaultdict
from .simulador import SimuladorEficiente
from .algoritmos import AlgoritmoHacerNada

class SimuladorAdhiere(SimuladorEficiente):
    def __init__(self, tamano_poblacion, precision_tests, probabilidad, duracion_infeccion, R0, adherencia):
        self.adherencia = adherencia
        super().__init__(tamano_poblacion, precision_tests, probabilidad, duracion_infeccion, R0)

    def crear_poblacion(self):
        poblacion = dict()
        for ind in range(self.tamano_poblacion):
            adherencia = ind < self.tamano_poblacion * self.adherencia
            probs = [self.probabilidad[self.S],
                     self.probabilidad[self.E],
                     self.probabilidad[self.I],
                     self.probabilidad[self.R]]
            estado_init = np.random.choice([self.S, self.E, self.I, self.R], 1, p=probs)

            inicio_sintomas = min(np.random.lognormal(np.log(4.1) - 8 / 9, 4 / 3), 20) + 1
            final_sintomas = inicio_sintomas + np.random.randint(7, 10)
            duracion_infeccion = final_sintomas + np.random.randint(5, 10)
            dia_muerte = final_sintomas - 1
            dias_sintomas = [int(inicio_sintomas), int(final_sintomas)]

            dia_aparicion_ac = 0
            sintomatico = False
            muere = False

            if np.random.rand() < self.probabilidad['sintomatico']:
                sintomatico = True
                if np.random.rand() < self.probabilidad['muerte']:
                    muere = True
                
            poblacion[ind] = IndividuoAdhiere(estado_init[0], ind, duracion_infeccion, dias_sintomas, dia_muerte, dia_aparicion_ac, sintomatico, muere, adherencia)

        return poblacion
    
    def simular(self, algoritmo, tiempo_simulacion):
        if algoritmo.algoritmo_id == 'anticuerpos':
            algo_no_adh = AlgoritmoHacerNada(algoritmo.cuarentena, algoritmo.frecuencia_test)
            while self.tiempo < tiempo_simulacion:
                pob_adh = {ind.id:ind for ind in self.poblacion.values() if ind.adhiere}
                pob_no_adh = {ind.id:ind for ind in self.poblacion.values() if  not ind.adhiere}
                pob_adh = algoritmo.decidir(pob_adh, self.tiempo, self.precision_tests)
                pob_no_adh = algo_no_adh.decidir(pob_no_adh, self.tiempo, self.precision_tests)
                self.poblacion = {**pob_adh, **pob_no_adh}
                self.numero_tests = algoritmo.numero_tests
                self.tick()
                algoritmo.tick()
        else:
            while self.tiempo < tiempo_simulacion:
                self.poblacion = algoritmo.decidir(self.poblacion, self.tiempo, self.precision_tests)
                self.numero_tests = algoritmo.numero_tests
                self.tick()
                algoritmo.tick()
        
        self.terminar()  
