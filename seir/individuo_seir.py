import numpy as np
from numpy import random

class Individuo:
    '''
    Clase de individuo
    '''
    def __init__(self, estado_inicial, idx, duracion_infeccion, dias_sintomas, dia_muerte, dia_aparicion_ac, sintomatico, muere):
        self.id = idx
        self.estado_inicial = estado_inicial
        self.duracion_infeccion = duracion_infeccion
        self.dias_sintomas = dias_sintomas
        self.dia_muerte = dia_muerte
        self.dia_aparicion_ac = dia_aparicion_ac
        self.sera_sintomatico = sintomatico
        self.muere = muere
        
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

    def testear_ac(self, s, e):
        if self.tiempo < self.tiempo_inicio_infeccion + self.dias_sintomas[0] + 8:
            s = 0.11
        elif self.tiempo < self.tiempo_inicio_infeccion + self.dias_sintomas[0] + 15:
            s = 0.93
        else:
            s = 0.97
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
        prob_inf = prob_infeccion
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

        if (self.estado == 'expuesto') and (self.tiempo - self.tiempo_inicio_infeccion >= self.dias_sintomas[0]):
            self.estado = 'infeccioso'

        self.cuarentena_nacional = False
            
        self.tiempo += 1

class IndividuoRol(Individuo):
    def __init__(self, estado_inicial, idx, duracion_infeccion, dias_sintomas, dia_muerte, dia_aparicion_ac, rol):
        super().__init__(estado_inicial, idx, duracion_infeccion, dias_sintomas, dia_muerte, dia_aparicion_ac)

        self.rol = rol

    def tick(self, prob_infeccion, ):
        #self.historia_actividad.append(self.actividad)
        #self.historia_estado.append(self.estado)
        #self.historia_estado_observado.append(self.estado_observado)

        prob_inf = prob_infeccion[(self.rol, self.actividad)]
        
        if self.en_cuarentena():
            self.tiempo_cuarentena += 1
        else:
            self.tiempo_cuarentena = 0
        
        if self.estado == 'infeccioso':
            self.tiempo_infeccioso += 1
            
            if self.tiempo_infeccioso > self.duracion_infeccion:
                self.estado = 'recuperado'

            if self.tiempo_infeccioso == self.dias_sintomas[0]:
                self.sintomatico = True
            elif self.tiempo_infeccioso == self.dias_sintomas[1]:
                self.sintomatico = False

            if self.tiempo_infeccioso == self.dia_muerte:
                self.morir()
            
        if (self.estado == 'susceptible') and (random.rand() < prob_inf):
            self.enfermar()

        if (self.estado == 'expuesto') and (self.tiempo - self.tiempo_inicio_infeccion >= self.dias_sintomas[0] - 2):
                self.estado == 'infeccioso'
            
        self.tiempo += 1

class IndividuoFamilia(IndividuoRol):
    def __init__(self, estado_inicial, idx, duracion_infeccion, dias_sintomas, dia_muerte, dia_aparicion_ac, rol):
        super().__init__(estado_inicial, idx, duracion_infeccion, dias_sintomas, dia_muerte, dia_aparicion_ac, rol)

        self.id_familia = None

    def tick(self, prob_infeccion, familia):
        #self.historia_actividad.append(self.actividad)
        #self.historia_estado.append(self.estado)
        #self.historia_estado_observado.append(self.estado_observado)

        prob_inf = prob_infeccion[(self.rol, self.actividad)]
        estado_familia = [ind.estado for ind in familia.miembros.values()]
        if 'infeccioso' in estado_familia:
            prob_inf = 0.5
            ###SIMULADOR tiene que agregar familia al arg de tick, indentificando a la familia del individuo
        
        if self.en_cuarentena():
            self.tiempo_cuarentena += 1
        else:
            self.tiempo_cuarentena = 0
        
        if self.estado == 'infeccioso':
            self.tiempo_infeccioso += 1
            
            if self.tiempo_infeccioso > self.duracion_infeccion:
                self.estado = 'recuperado'

            if self.tiempo_infeccioso == self.dias_sintomas[0]:
                self.sintomatico = True
            elif self.tiempo_infeccioso == self.dias_sintomas[1]:
                self.sintomatico = False

            if self.tiempo_infeccioso == self.dia_muerte:
                self.morir()
            
        if (self.estado == 'susceptible') and (random.rand() < prob_inf):
            self.enfermar()
            
        self.tiempo += 1

class Familia:
    def __init__(self, idx, constitucion):
        self.idx = idx
        self.constitucion = constitucion

    def crear_integrante(self, estado_init, ind, duracion_infeccion, dias_sintomas, dia_muerte, dia_aparicion_ac, rol):
        return IndividuoRol(estado_init, idx, duracion_infeccion, dias_sintomas, dia_muerte, dia_aparicion_ac, rol, self.idx)




            
            
        
        
        
        
    
        
        
            
            
        
        
