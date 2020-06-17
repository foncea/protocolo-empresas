import numpy as np
from numpy import random
from base.individuo import Individuo

class IndividuoSensibilidad(Individuo):
    '''
    Clase de individuo
    '''        
    def testear_ac(self, prec_test):
        e, s = prec_test(self.tiempo, self.tiempo_inicio_infeccion, self.dias_sintomas)
        if (self.estado in ['infeccioso', 'expuesto']) and (self.tiempo_infeccioso >= self.dia_aparicion_ac):
            self.ultimo_test = (s > random.rand())
        else:
            self.ultimo_test = (e < random.rand())
        
        self.fecha_ultimo_test = self.tiempo
        self.historia_test.append(self.ultimo_test)
        return self.ultimo_test
            
    def tick(self, prob_infeccion, ):
        prob_inf = prob_infeccion[self.actividad]
        if self.cuarentena_nacional:
            prob_inf = 0
        
        if self.en_cuarentena():
            self.tiempo_cuarentena += 1
        else:
            self.tiempo_cuarentena = 0
        
        if self.estado == 'infeccioso':
            self.tiempo_infeccioso += 1
            
            if self.tiempo_infeccioso > self.duracion_infeccion:
                self.estado = 'recuperado'

            if self.tiempo_infeccioso == self.dias_sintomas[0] and self.sera_sintomatico:
                self.sintomatico = True
            elif self.tiempo_infeccioso == self.dias_sintomas[1]:
                self.sintomatico = False

            if self.tiempo_infeccioso == self.dia_muerte and self.muere:
                self.morir()
            
        if (self.estado == 'susceptible') and (random.rand() < prob_inf):
            self.enfermar()

        if (self.estado == 'expuesto') and (self.tiempo - self.tiempo_inicio_infeccion >= self.dias_sintomas[0] - 2):
            self.estado = 'infeccioso'

        self.cuarentena_nacional = False
            
        self.tiempo += 1

