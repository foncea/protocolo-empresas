import numpy as np
import pandas as pd
from .individuo import Individuo, IndividuoRol, IndividuoFamilia
from collections import Counter, defaultdict

class SimuladorBase:
    def __init__(self, tamano_poblacion, precision_tests, probabilidad, duracion_infeccion, R0):
        self.tamano_poblacion = tamano_poblacion
        self.precision_tests = precision_tests
        self.probabilidad = probabilidad
        self.S = 'susceptible'
        self.E = 'expuesto'
        self.I = 'infeccioso'
        self.R = 'recuperado'
        self.duracion_infeccion = duracion_infeccion
        self.R0_empresa = R0['empresa']
        self.R0_poblacion = R0['poblacion']

        self.alpha = 1 / 3  #periodo de incubacion
        self.gamma = 1 / 17.5 #periodo medio de infeccion
        self.beta_poblacion = self.gamma * self.R0_poblacion
        self.beta_empresa = self.gamma * self.R0_empresa
        self.p_contagio_diaria = {'trabajo': 3 / 700 * self.beta_empresa,
                                  'cuarentena': 3 / 700 * self.beta_poblacion / 2}
        
        self.tiempo = 0

        self.poblacion = self.crear_poblacion()

        self.historia_numero_tests = [0]
        self.historia_infectados_totales = {ind.id for ind in self.poblacion.values() if ind.estado == self.I}
        self.numero_infectados_totales = [len(self.historia_infectados_totales)]

 def crear_poblacion(self):
        poblacion = {}
        lista_estados = [self.S, self.E, self.I, self.R]
        probs = [self.probabilidad[est] for est in lista_estados]

        for ind in range(self.tamano_poblacion):
            estado_init = np.random.choice(lista_estados, 1, p=probs)[0]
 
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
                
            poblacion[ind] = Individuo(estado_init, ind, duracion_infeccion, dias_sintomas, dia_muerte, dia_aparicion_ac, sintomatico, muere)

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
        trabajando = [ind for ind in self.poblacion.values() if (ind.actividad == 'trabajo') & (ind.determinar_turno() != 0)]
        infecciosos_trabajando = [ind for ind in trabajando if ind.estado == self.I]

        self.p_contagio_diaria['trabajo'] = self.beta_empresa * len(infecciosos_trabajando) / max(1, len(trabajando)) / 2 + 3 / 700 * self.beta_poblacion / 2

        
    def tick(self):
        for ind in self.poblacion.values():
            ind.tick(self.p_contagio_diaria)
            
        self.poblacion = dict((ind.id, ind) for ind in self.poblacion.values() if ind.vive)
     
        self.actualizar_probabilidad()
        
        self.historia_numero_tests.append(self.numero_tests)
        self.historia_infectados_totales.update({ind.id for ind in self.poblacion.values() if ind.estado == 'infeccioso'})
        self.numero_infectados_totales.append(len(self.historia_infectados_totales))
            
        self.tiempo += 1
        
    def terminar(self):
        pass
    
    def simular(self, algoritmo, tiempo_simulacion):
        while self.tiempo < tiempo_simulacion:
            self.poblacion = algoritmo.decidir(self.poblacion, self.tiempo, self.precision_tests)
            self.numero_tests = algoritmo.numero_tests
            self.tick()
            algoritmo.tick()
        
        self.terminar()  

class SimuladorEficiente(SimuladorBase):
    def __init__(self, tamano_poblacion, precision_tests, probabilidad, duracion_infeccion, R0_poblacion):
        super().__init__(tamano_poblacion, precision_tests, probabilidad, duracion_infeccion, R0_poblacion)

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

class SimuladorRoles(SimuladorBase):
    def __init__(self, tamano_poblacion, precision_tests, probabilidad, duracion_infeccion, R0_poblacion):
        self.Tr = 'trabajador'
        self.Es = 'estudiante'
        self.Ot = 'otro'

        self.Ac = 'trabajo'
        self.Cu = 'cuarentena'

        self.proporcion_roles = {self.Tr: 0.45,
                                 self.Es: 0.25,
                                 self.Ot: 0.3}

        super().__init__(tamano_poblacion, precision_tests, probabilidad, duracion_infeccion, R0_poblacion)

        self.p_contagio_diaria = defaultdict(lambda: self.probabilidad['contagio_diaria'])


        self.estados_actividades_roles =  dict((ind.id, (ind.estado, ind.estado_observado, ind.actividad, ind.rol)) for ind in self.poblacion.values())
        self.df_estados_actividades_roles = pd.DataFrame.from_dict(Counter(self.estados_actividades_roles.values()), orient='index')
        self.df_estados_actividades_roles['tiempo'] = self.tiempo

    def crear_poblacion(self):
        poblacion = dict()
        for ind in range(self.tamano_poblacion):
            probs = [self.proporcion_roles[self.Tr],
                     self.proporcion_roles[self.Es],
                     self.proporcion_roles[self.Ot]]
            rol = np.random.choice([self.Tr, self.Es, self.Ot], 1, p=probs)

            probs = [self.probabilidad[self.S],
                     self.probabilidad[self.I],
                     self.probabilidad[self.R]]
            estado_init = np.random.choice([self.S, self.I, self.R], 1, p=probs)

            duracion_infeccion = np.random.randint(self.duracion_infeccion[0], self.duracion_infeccion[1] + 1)

            dia_aparicion_ac = np.random.randint(self.precision_tests['dia_aparicion_ac'][0], self.precision_tests['dia_aparicion_ac'][1] + 1)

            if np.random.rand() < self.probabilidad['sintomatico']:
                inicio = np.random.randint(2, 13)
                final = inicio + np.random.randint(5, 10)
                dias_sintomas = [inicio, final] 
                if np.random.rand() < self.probabilidad['muerte']:
                    dia_muerte = min(dias_sintomas[1] - 1, duracion_infeccion - 1)
                else:
                    dia_muerte = 0
            else:
                dias_sintomas = [0, 0]
                dia_muerte = 0

            poblacion[ind] = IndividuoRol(estado_init[0], ind, duracion_infeccion, dias_sintomas, dia_muerte, dia_aparicion_ac, rol[0])

        return poblacion

    def actualizar_probabilidad(self):
        infecciosos = [ind.id for ind in self.poblacion.values() if ind.estado == self.I]
        activos = [ind.id for ind in self.poblacion.values() if ind.actividad == self.Ac]
        estudiantes = [ind.id for ind in self.poblacion.values() if ind.rol == self.Es]
        trabajadores = [ind.id for ind in self.poblacion.values() if ind.rol == self.Tr]
        otros = [ind.id for ind in self.poblacion.values() if ind.rol == self.Ot]

        num_a_i = len(list(set(infecciosos) & set(activos)))
        num_a_e = len(list(set(activos) & set(estudiantes)))
        num_a_t = len(list(set(activos) & set(trabajadores)))
        num_a_i_e = len(list(set(infecciosos) & set(activos) & set(estudiantes)))
        num_a_i_t = len(list(set(infecciosos) & set(activos) & set(trabajadores)))

        p_defecto = num_a_i / (self.tamano_poblacion - num_a_i)
        p_estudiando = 1 if num_a_e <= num_a_i_e else num_a_i_e / (num_a_e - num_a_i_e)
        p_trabajando = 1 if num_a_t <= num_a_i_t else num_a_i_t / (num_a_t - num_a_i_t)

        self.p_contagio_diaria[(self.Tr, self.Ac)] = (self.R0_poblacion * p_trabajando + 2 * p_defecto) / 2 / 26
        self.p_contagio_diaria[(self.Es, self.Ac)] = (self.R0_poblacion * p_trabajando + 2 * p_defecto) / 2 / 26
        self.p_contagio_diaria = dict((i, self.p_contagio_diaria[i]) if self.p_contagio_diaria[i] < 1/2 else (i, 1/2) for i in self.p_contagio_diaria)
        self.p_contagio_diaria = defaultdict(lambda: p_defecto if p_defecto < 1/2 else 1/2, self.p_contagio_diaria)

    def contar_estado_actividad_rol(self):
        estado_actividad_rol_actual = pd.DataFrame.from_dict(Counter(self.estados_actividades_roles.values()), orient='index')
        estado_actividad_rol_actual['tiempo'] = self.tiempo
        self.df_estados_actividades_roles = pd.concat([self.df_estados_actividades_roles, estado_actividad_rol_actual], axis=0)

    def actualizar_estados_actividades_roles(self):
        self.estados_actividades_roles =  dict((ind.id, (ind.estado, ind.estado_observado, ind.actividad, ind.rol)) for ind in self.poblacion.values())
    
    def tick(self):
        super().tick()

        self.actualizar_estados_actividades_roles()
        self.contar_estado_actividad_rol()

    def simular(self, algoritmo_empresa, algoritmo_colegio, algoritmo_otros, tiempo_simulacion):
        while self.tiempo < tiempo_simulacion:
            trabajadores = dict((ind.id, ind) for ind in self.poblacion.values() if ind.rol == self.Tr)
            estudiantes = dict((ind.id, ind) for ind in self.poblacion.values() if ind.rol == self.Es)
            otros = dict((ind.id, ind) for ind in self.poblacion.values() if ind.rol == self.Ot)
            trabajadores = algoritmo_empresa.decidir(trabajadores, self.tiempo, self.precision_tests)
            estudiantes = algoritmo_colegio.decidir(estudiantes, self.tiempo, self.precision_tests)
            otros = algoritmo_otros.decidir(otros, self.tiempo, self.precision_tests)
            self.poblacion = { **estudiantes, **otros, **trabajadores}

            self.numero_tests = sum([algoritmo_empresa.numero_tests, algoritmo_colegio.numero_tests, algoritmo_otros.numero_tests])
            
            self.tick()
            algoritmo_empresa.tick()
            algoritmo_colegio.tick()
            algoritmo_otros.tick()
        
        self.terminar()  


class SimuladorFamilias(SimuladorRoles):
    def __init__(self, tamano_poblacion, precision_tests, probabilidad, duracion_infeccion, R0_poblacion):
        super().__init__(tamano_poblacion, precision_tests, probabilidad, duracion_infeccion, R0_poblacion)   

        self.asignar_familias()

    def asignar_familias(self):
        poblacion = self.poblacion
        for ind in poblacion.values():
            ind.id_familia = ind.id % int(self.tamano_poblacion / 4)

    def tick(self):
        familias_infectadas = []
        for ind in self.poblacion.values():
            if ind.estado == 'infeccioso':
                familias_infectadas.append(ind.id_familia)
        p_cont_fam_inf = defaultdict(lambda: 0.5)

        for ind in self.poblacion.values():
            if ind.id_familia in familias_infectadas:
                ind.tick(p_cont_fam_inf)
            else:
                ind.tick(self.p_contagio_diaria)
            
        self.poblacion = dict((ind.id, ind) for ind in self.poblacion.values() if ind.vive)
                 
        self.actualizar_probabilidad()

        self.historia_numero_tests.append(self.numero_tests)
            
        self.tiempo += 1

        self.actualizar_estados_actividades_roles()
        self.contar_estado_actividad_rol()


