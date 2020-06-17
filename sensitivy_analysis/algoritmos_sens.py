import numpy as np
from base.algoritmos import AlgoritmoBios

class AlgoritmoBiosSens(AlgoritmoBios):    
    def aplicar_test(self, precision_tests):              
        self.resultado_test = self.individuo.testear_ac(precision_tests)
        
        self.numero_tests += 1
        #self.IgM = self.individuo.testear_IgM(s_m, e_m)
        #self.IgG = self.individuo.testear_IgG(s_g, e_g)
                    
    def decidir(self, poblacion, tiempo, precision_tests):
        self.poblacion = poblacion
        if tiempo < self.cuarentena_inicial:
            for ind in poblacion.values():
                self.individuo = ind
                self.individuo.cuarentena_nacional = True
                self.individuo.aplicar_cuarentena()
                if self.individuo.sintomatico:
                    self.individuo.cambiar_estado_observado('infeccioso')
                self.poblacion[self.individuo.id] = self.individuo
            return self.poblacion 

        for ind in poblacion.values():
            precision_actual_tests = precision_tests['precision']
            self.individuo = ind
            
            if self.individuo.en_trabajo():
                if self.individuo.sintomatico:
                    self.individuo.cambiar_estado_observado('infeccioso')
                elif (tiempo % self.frecuencia_test == 0) and (self.individuo.estado_observado == 'susceptible'):
                    self.aplicar_test(precision_actual_tests)
                    self.asignar_estado_segun_test()
            elif (self.individuo.tiempo_cuarentena > self.tiempo_pcr) and (self.individuo.estado == 'susceptible'):
                self.individuo.cambiar_estado_observado('susceptible')
            elif self.individuo.tiempo_cuarentena > self.duracion_cuarentena:
                if tiempo >= self.individuo.tiempo_inicio_infeccion + self.individuo.dias_sintomas[1] + self.duracion_cuarentena:
                    self.individuo.cambiar_estado_observado('recuperado')
                    
            self.asignar_actividad()
            
            
        return self.poblacion       


class AlgoritmoHacerNadaSens(AlgoritmoBiosSens):
    def __init__(self, cuarentena, frecuencia_test):
        super().__init__(cuarentena, frecuencia_test)
        self.frecuencia_test = float('nan')
        self.algoritmo_id = 'hacer nada'
