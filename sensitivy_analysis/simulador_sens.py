import numpy as np
import pandas as pd
from individuo_sens import IndividuoSensibilidad
from base.simulador import SimuladorEficiente
from collections import Counter, defaultdict

class SimuladorSensibilidad(SimuladorEficiente):
    def __init__(self, tamano_poblacion, precision_tests, probabilidad, R0, periodo_post_sintomas):
        self.pps = periodo_post_sintomas
        super().__init__(tamano_poblacion, precision_tests, probabilidad, 0, R0)

    def crear_poblacion(self):
        poblacion = dict()
        for ind in range(self.tamano_poblacion):
            probs = [self.probabilidad[self.S],
                     self.probabilidad[self.E],
                     self.probabilidad[self.I],
                     self.probabilidad[self.R]]
            estado_init = np.random.choice([self.S, self.E, self.I, self.R], 1, p=probs)

            inicio_sintomas = min(np.random.lognormal(np.log(4.1) - 8 / 9, 4 / 3), 20) + 1
            final_sintomas = inicio_sintomas + np.random.randint(7, 10)
            duracion_infeccion = final_sintomas + np.random.randint(self.pps[0], self.pps[1])
            dia_muerte = final_sintomas - 1
            dias_sintomas = [int(inicio_sintomas), int(final_sintomas)]

            dia_aparicion_ac = 0
            sintomatico = False
            muere = False

            if np.random.rand() < self.probabilidad['sintomatico']:
                sintomatico = True
                if np.random.rand() < self.probabilidad['muerte']:
                    muere = True
                
            poblacion[ind] = IndividuoSensibilidad(estado_init[0], ind, duracion_infeccion, dias_sintomas, dia_muerte, dia_aparicion_ac, sintomatico, muere)

        return poblacion

    
    def simular(self, algoritmo, tiempo_simulacion):
        while self.tiempo < tiempo_simulacion:
            self.poblacion = algoritmo.decidir(self.poblacion, self.tiempo, self.precision_tests)
            self.numero_tests = algoritmo.numero_tests
            self.tick()
            algoritmo.tick()
        
        self.terminar()  
