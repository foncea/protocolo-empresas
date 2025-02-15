import numpy as np
import pandas as pd
from .individuo import Individuo
from collections import Counter, defaultdict
import datetime as dt

class SimuladorBase:
    def __init__(self, info_poblacion, precision_tests, probabilidad, duracion_infeccion, R0, dia_hora_inicial):
        self.tamano_poblacion = info_poblacion.shape[0]
        self.info_poblacion = info_poblacion
        self.precision_tests = precision_tests
        self.probabilidad = probabilidad
        self.S = 'susceptible'
        self.E = 'expuesto'
        self.I = 'infeccioso'
        self.R = 'recuperado'
        self.duracion_infeccion = duracion_infeccion
        self.R0_empresa = R0['empresa']
        self.R0_poblacion = R0['poblacion']
        self.dia_hora_inicial = dia_hora_inicial

        self.alpha = 1 / (3 * 24)  #periodo de incubacion
        self.gamma = 1 / (17.5 * 24) #periodo medio de infeccion
        self.beta_poblacion = self.gamma * self.R0_poblacion
        self.beta_empresa = self.gamma * self.R0_empresa
        dict_p_contagio_trabajo = defaultdict(lambda: 3 / 1000 * self.beta_empresa, {})
        self.p_contagio_diaria = {'trabajo': dict_p_contagio_trabajo,
                                  'cuarentena': defaultdict(lambda: 3 / 1000 * self.beta_poblacion, {})}
        
        self.tiempo = 0
        self.dia_hora = dia_hora_inicial

        self.poblacion = self.crear_poblacion()

        self.historia_numero_tests = [0]
        self.historia_infectados_totales = {ind.id for ind in self.poblacion.values() if ind.estado == self.I}
        self.numero_infectados_totales = [len(self.historia_infectados_totales)]

    def crear_poblacion(self):
        poblacion = {}
        lista_estados = [self.S, self.E, self.I, self.R]
        probs = [self.probabilidad[est] for est in lista_estados]

        for ind in range(self.tamano_poblacion):
            estado_init = self.info_poblacion['estado_inicial'][ind]
            if estado_init == 'random':
                estado_init = np.random.choice(lista_estados, 1, p=probs)[0]

            inicio_sintomas = (min(np.random.lognormal(np.log(4.1) - 8 / 9, 4 / 3), 20) + 10) * 24
            final_sintomas = inicio_sintomas + np.random.randint(7, 10) * 24
            duracion_infeccion = final_sintomas + np.random.randint(15, 20) * 24
            dia_muerte = final_sintomas - 24
            dias_sintomas = [int(inicio_sintomas), int(final_sintomas)]

            dia_aparicion_ac = 0
            sintomatico = False
            muere = False

            if np.random.rand() < self.probabilidad['sintomatico']:
                sintomatico = True
                if np.random.rand() < self.probabilidad['muerte']:
                    muere = True

            area_trabajo = self.info_poblacion['area_trabajo'][ind]
            tipo_turno = self.info_poblacion['tipo_turno'][ind]
                
            poblacion[ind] = Individuo(estado_init, ind, duracion_infeccion, dias_sintomas, dia_muerte, dia_aparicion_ac, sintomatico, muere, area_trabajo, tipo_turno, self.dia_hora_inicial)

        return poblacion

    def actualizar_estados(self):
        self.estados = dict((ind.id, ind.estado) for ind in self.poblacion.values())
        
    def actualizar_estados_observados(self):
        self.estados_observados = dict((ind.id, ind.estado_observado) for ind in self.poblacion.values())
        
    def actualizar_actividades(self):
        self.actividades = dict((ind.id, ind.actividad) for ind in self.poblacion.values())
        
    def actualizar_estados_actividades(self):
        self.estados_actividades = dict((ind.id, (ind.estado, ind.actividad)) for ind in self.poblacion.values())
        
    def historia_estado_poblacion(self):
        return [Counter(estados.values()) for estados in self.historia_estados]
    
    def historia_estado_observado_poblacion(self):
        return [Counter(estados.values()) for estados in self.historia_estados_observados]
    
    def historia_actividad_poblacion(self):
        return [Counter(actividades.values()) for actividades in self.historia_actividades]
    
    def historia_estado_actividad_poblacion(self):
        return [Counter(estados_actividades.values()) for estados_actividades in self.historia_estados_actividades]

    def actualizar_probabilidad(self):
        en_turno = [ind for ind in self.poblacion.values() if ind.en_turno]
        infecciosos = [ind for ind in en_turno if ind.estado == self.I]
        trabajando = [ind for ind in en_turno if ind.actividad == 'trabajo']

        areas_activas = []
        for ind in trabajando:
            areas_activas.append(ind.area_trabajo)
        areas_activas = set(areas_activas)

        infecciosos_area = {}
        total_area = {}
        for area in areas_activas:
            infecciosos_area[area] = [ind for ind in infecciosos if ind.area_trabajo == area]
            trabajando_area = [ind for ind in trabajando if ind.area_trabajo == area]
            trabajando_enfermos = len(set(infecciosos_area) & set(trabajando_area))
            self.p_contagio_diaria['trabajo'][area] = self.beta_empresa * trabajando_enfermos / max(1, len(trabajando_area)) 
            
    def actualizar_probabilidad2(self):
        infecciosos = [ind.id for ind in self.poblacion.values() if ind.estado == self.I]
        susceptibles = [ind.id for ind in self.poblacion.values() if ind.estado == self.S]
        trabajando = [ind.id for ind in self.poblacion.values() if ind.actividad == 'trabajo']
        
        trabajando_enfermos = len(set(infecciosos) & set(trabajando) & set(en_turno))
        trabajando_susceptibles = max(len(set(susceptibles) & set(trabajando) & set(en_turno)), 1)
        
        self.p_contagio_diaria['trabajo'] = self.beta_empresa * trabajando_enfermos / max(1, len(trabajando)) / 2 + 3 / 700 * self.beta_poblacion / 2
        #self.p_contagio_diaria['trabajo'] = self.p_contagio_diaria['trabajo'] if self.p_contagio_diaria['trabajo'] < 1/2 else 1/2 

        
    def tick(self):
        for ind in self.poblacion.values():
            ind.tick(self.p_contagio_diaria)
            
        self.poblacion = dict((ind.id, ind) for ind in self.poblacion.values() if ind.vive)
     
        self.actualizar_probabilidad()
        
        self.historia_numero_tests.append(self.numero_tests)
        self.historia_infectados_totales.update({ind.id for ind in self.poblacion.values() if ind.estado == 'infeccioso'})
        self.numero_infectados_totales.append(len(self.historia_infectados_totales))
            
        self.dia_hora += dt.timedelta(hours=1)
        self.tiempo += 1
        
    def terminar(self):
        pass
    
    def simular(self, algoritmo, tiempo_simulacion):
        while self.tiempo < tiempo_simulacion:
            self.poblacion = algoritmo.decidir(self.poblacion, self.tiempo, self.dia_hora, self.precision_tests)
            self.numero_tests = algoritmo.numero_tests
            self.tick()
            algoritmo.tick()
        
        self.terminar()  

class SimuladorEficiente(SimuladorBase):
    def __init__(self, info_poblacion, precision_tests, probabilidad, duracion_infeccion, R0_poblacion, dia_hora_inicial):
        super().__init__(info_poblacion, precision_tests, probabilidad, duracion_infeccion, R0_poblacion, dia_hora_inicial)

        self.estados_actividades_obs = dict((ind.id, (ind.estado, ind.estado_observado, ind.actividad, ind.sintomatico)) for ind in self.poblacion.values())
        self.df_estados_actividades_obs = pd.DataFrame.from_dict(Counter(self.estados_actividades_obs.values()), orient='index')
        self.df_estados_actividades_obs['tiempo'] = self.tiempo
        
    def contar_estados_actividades_obs(self):
        estado_actividad_actual = pd.DataFrame.from_dict(Counter(self.estados_actividades_obs.values()), orient='index')
        estado_actividad_actual['tiempo'] = self.tiempo
        self.df_estados_actividades_obs = pd.concat([self.df_estados_actividades_obs, estado_actividad_actual], axis=0)

    def actualizar_estados_actividades_obs(self):
        self.estados_actividades_obs =  dict((ind.id, (ind.estado, ind.estado_observado, ind.actividad, ind.sintomatico)) for ind in self.poblacion.values())
        
    def tick(self):
        super().tick()

        self.actualizar_estados_actividades_obs()
        self.contar_estados_actividades_obs()



