import numpy as np
from numpy import random
import datetime as dt

class Individuo:
    '''
    Clase de individuo
    '''
    def __init__(self, estado_inicial, idx, duracion_infeccion, dias_sintomas, dia_muerte, dia_aparicion_ac, sintomatico, muere, dia_inicial, tipo_turno):
        self.id = idx
        self.estado_inicial = estado_inicial
        self.duracion_infeccion = duracion_infeccion
        self.dias_sintomas = dias_sintomas
        self.dia_muerte = dia_muerte
        self.dia_aparicion_ac = dia_aparicion_ac
        self.sera_sintomatico = sintomatico
        self.muere = muere
        self.dia_inicial = dia_inicial
        self.tipo_turno = tipo_turno
        
        self.estado = estado_inicial
        self.estado_observado = 'susceptible'
        
        self.vive = True
        self.sintomatico = False
        
        self.actividad = 'trabajo'
        
        self.ultimo_IgG = None
        self.ultimo_IgM = None
        self.ultimo_test = None
        
        self.fecha_ultimo_IgG = None
        self.fecha_ultimo_IgM = None
        self.fecha_ultimo_test = None

        self.historia_IgG = []
        self.historia_IgM = []
        self.historia_test = []
        
        self.historia_estado = [estado_inicial]
        self.historia_estado_observado = [self.estado_observado]
        self.historia_actividad = [self.actividad]
        
        self.tiempo = 0
        self.dia = self.dia_inicial

        self.tiempo_cuarentena = 0
        self.tiempo_infeccioso = 0
        self.tiempo_inicio_infeccion = 0
        if self.estado_inicial == 'infeccioso':
            self.tiempo_infeccioso = random.randint(1, 14)

        self.cuarentena_nacional = False
        
    def testear_IgM(self, s_m, e_m):
        if (self.estado == 'infeccioso') and (self.tiempo_infeccioso >= self.dia_aparicion_ac):
            self.ultimo_IgM = (s_m > random.rand())
        else:
            self.ultimo_IgM = (e_m < random.rand())
        
        self.fecha_ultimo_IgM = self.tiempo
        self.historia_IgM.append(self.ultimo_IgM)
        return self.ultimo_IgM
    
    def testear_IgG(self, s_g, e_g):
        if (self.estado == 'susceptible') or (self.tiempo_infeccioso < self.dia_aparicion_ac):
            self.ultimo_IgG = (e_g < random.rand())
        else:
            self.ultimo_IgG = (s_g > random.rand())
            
        self.fecha_ultimo_IgG = self.tiempo
        self.historia_IgG.append(self.ultimo_IgM)
        return self.ultimo_IgG

    def testear_ac(self, prec_test):
        e, s = prec_test(self.tiempo, self.tiempo_inicio_infeccion, self.dias_sintomas)
        
        if (self.estado in ['infeccioso', 'expuesto']) and (self.tiempo_infeccioso >= self.dia_aparicion_ac):
            self.ultimo_test = (s > random.rand())
        else:
            self.ultimo_test = (e < random.rand())
        
        self.fecha_ultimo_test = self.tiempo
        self.historia_test.append(self.ultimo_test)
        return self.ultimo_test
    
    def aplicar_cuarentena(self):
        self.actividad = 'cuarentena'
        
    def aplicar_trabajo(self):
        self.actividad = 'trabajo'
        
    def en_cuarentena(self):
        return self.actividad == 'cuarentena'
    
    def en_trabajo(self):
        return self.actividad == 'trabajo'

    def cambiar_estado_observado(self, nuevo_estado_observado):
        self.estado_observado = nuevo_estado_observado
    
    def cambiar_estado(self, nuevo_estado):
        self.estado = nuevo_estado
        
    def enfermar(self):
        self.tiempo_inicio_infeccion = self.tiempo
        self.estado = 'expuesto'
    
    def recuperarse(self):
        self.estado = 'recuperado'
        
    def morir(self):
        self.vive = False
        
    def tick(self, prob_infeccion, ):
        alpha = self.determinar_turno()
        prob_inf = alpha * prob_infeccion[self.actividad] + (1 - alpha) * prob_infeccion['cuarentena']
        
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
        self.dia += dt.timedelta(days=1)
        self.actualizar_turno()

    def determinar_turno(self):
        horario = {'Administrativo': 1/3,
               '12 Horas': 1/2,
               '12 Horas - Dia': 1/2,
               '12 Horas - Noche': 1/2,
               '12 Horas - Backup': 0,
               'Fuera de planta': 0,
               '12 Horas - Licencia': 0,
               '12 Horas - Cuarententa': 0}

        if self.dia.isoweekday() in [6, 7]:
            return 0
        else:
            return horario[self.tipo_turno]

    def actualizar_turno(self):
        shifts = ['12 Horas - Dia', '12 Horas - Noche', '12 Horas - Backup']
        if self.tipo_turno in shifts:
            mod_turno = shifts.index(self.tipo_turno)
            semana = int((self.dia - self.dia_inicial).days / 7)
            nuevo_turno = (mod_turno + semana) % 3
            return shifts[nuevo_turno]
        else:
            return self.tipo_turno   
        
            
        
        
