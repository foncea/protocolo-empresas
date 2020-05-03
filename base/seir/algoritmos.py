import numpy as np

class AlgoritmoBios:
    def __init__(self, cuarentena, frecuencia_test):
        self.frecuencia_test = frecuencia_test
        self.duracion_cuarentena = cuarentena['duracion']
        self.cuarentena_inicial = cuarentena['inicial']

        self.individuo = None

        self.tiempo = 0
        
        self.numero_tests = 0
        
        self.tiempo_pcr = 1  #agregar como precision test
    
    def reset(self):
        self.numero_tests = 0
        self.tiempo = 0
        self.individuo = None
        
    def tick(self):
        self.numero_tests = 0
    
    def aplicar_test(self, precision_tests):        
        #s_m = precision_tests['S_M']
        #e_m = precision_tests['E_M']
        #s_g = precision_tests['S_G']
        #e_g = precision_tests['E_G']
        e = precision_tests['E']
        s = precision_tests['S'] 
        
        self.resultado_test = self.individuo.testear_ac(s, e)
        
        self.numero_tests += 1
        #self.IgM = self.individuo.testear_IgM(s_m, e_m)
        #self.IgG = self.individuo.testear_IgG(s_g, e_g)
        
    def asignar_estado(self):
        if (not self.IgM) and (not self.IgG):
            self.individuo.cambiar_estado_observado('susceptible')
        elif self.IgM:
            self.individuo.cambiar_estado_observado('infeccioso')
        else:
            self.individuo.cambiar_estado_observado('recuperado')
            
    def asignar_estado_segun_test(self):
        if self.resultado_test:
            self.individuo.cambiar_estado_observado('infeccioso')
        else:
            self.individuo.cambiar_estado_observado('susceptible')
            
    def asignar_actividad(self):
        if self.individuo.estado_observado == 'infeccioso':
            self.individuo.aplicar_cuarentena()
        else:
            self.individuo.aplicar_trabajo()
            
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
            precision_actual_tests = precision_tests['precision'](ind.tiempo_infeccioso, ind.dia_aparicion_ac) 
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

class AlgoritmoBiosTurnos(AlgoritmoBios):
    def __init__(self, duracion_cuarentena, frecuencia_test):
        super().__init__(duracion_cuarentena, frecuencia_test)

        self.turno = 0

    def reset(self):
        super().reset()
        self.turnos = 0

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

        tamano_turno = int(len(poblacion) / 2)
        self.id_particion_poblacion = [[i for i in poblacion if i < tamano_turno], [i for i in poblacion if i >= tamano_turno]]

        if tiempo % 14 == 0:
            self.turno = 1 - self.turno
            self.poblacion_activa = dict((i, poblacion[i]) for i in self.id_particion_poblacion[self.turno])
            self.poblacion_espera = dict((i, poblacion[i]) for i in self.id_particion_poblacion[1 - self.turno])

        for ind in self.poblacion_activa.values():
            precision_actual_tests = precision_tests['precision'](ind.tiempo_infeccioso, ind.dia_aparicion_ac) 
            self.individuo = ind
            
            if self.individuo.en_trabajo():
                if self.individuo.estado_observado == 'infeccioso':
                    self.individuo.aplicar_cuarentena()
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
                else:
                    self.individuo.aplicar_cuarentena()
    
            self.asignar_actividad()
            self.poblacion[self.individuo.id] = self.individuo

        for ind in self.poblacion_espera.values():
            self.individuo = ind
            if self.individuo.sintomatico:
                self.individuo.cambiar_estado_observado('infeccioso')
            self.individuo.aplicar_cuarentena()
            self.poblacion[self.individuo.id] = self.individuo

        return self.poblacion        

class AlgoritmoHacerNada(AlgoritmoBios):
    def __init__(self, cuarentena, frecuencia_test):
        super().__init__(cuarentena, frecuencia_test)
        self.frecuencia_test = float('nan')

class AlgoritmoHacerNadaTurnos(AlgoritmoBiosTurnos):
    def __init__(self, cuarentena, frecuencia_test):
        super().__init__(cuarentena, frecuencia_test)
        self.frecuencia_test = float('nan')

class AlgoritmoLiteralmenteHacerNada(AlgoritmoBios):
    def decidir(self, poblacion, tiempo, precision_tests):
        return poblacion

'''    
#class AlgoritmoHacerNada:
    def __init__(self, cuarentena):
        self.numero_tests = 0
        self.duracion_cuarentena = cuarentena['duracion']
        self.cuarentena_inicial = cuarentena['inicial']
    
    def decidir(self, poblacion, tiempo, precision_tests):
        self.poblacion = {}
        for ind in poblacion.values():
            self.individuo = ind
            if self.individuo.en_trabajo():
                if self.individuo.sintomatico:
                    self.individuo.cambiar_estado_observado('infeccioso')
                    self.individuo.aplicar_cuarentena()
            elif tiempo > self.duracion_cuarentena + self.individuo.dias_sintomas[1] + self.individuo.tiempo_inicio_infeccion:
                self.individuo.cambiar_estado_observado('recuperado')
                self.individuo.aplicar_trabajo()
            
            self.poblacion[self.individuo.id] = self.individuo
        return self.poblacion
    
    def tick(self):
        pass
    
    def reset(self):
        self.numero_tests = 0
#class AlgoritmoHacerNadaTurnos(AlgoritmoHacerNada):
    def __init__(self, duracion_cuarentena):
        super().__init__(duracion_cuarentena)
        self.turno = 0


    def reset(self):
        super().reset()
        self.turno = 0

    def decidir(self, poblacion, tiempo, precision_tests):
        tamano_turno = int(len(poblacion) / 2)
        self.id_particion_poblacion = [[i for i in poblacion if i < tamano_turno], [i for i in poblacion if i >= tamano_turno]]

        if tiempo % 14 == 0:
            self.turno = 1 - self.turno
            self.poblacion_activa = dict((i, poblacion[i]) for i in self.id_particion_poblacion[self.turno])
            self.poblacion_espera = dict((i, poblacion[i]) for i in self.id_particion_poblacion[1 - self.turno])

        self.poblacion = poblacion

        for ind in self.poblacion_activa.values():
            self.individuo = ind
            if self.individuo.en_trabajo():
                if self.individuo.estado_observado == 'infeccioso':
                    self.individuo.aplicar_cuarentena()
                if self.individuo.sintomatico:
                    self.individuo.cambiar_estado_observado('infeccioso')
                    self.individuo.aplicar_cuarentena()
            elif tiempo > self.duracion_cuarentena + self.individuo.dias_sintomas[1] + self.individuo.tiempo_inicio_infeccion:
                self.individuo.cambiar_estado_observado('recuperado')
                self.individuo.aplicar_trabajo()
            
            self.poblacion[self.individuo.id] = self.individuo

        for ind in self.poblacion_espera.values():
            self.individuo = ind
            if self.individuo.sintomatico:
                self.individuo.cambiar_estado_observado('infeccioso')
            self.individuo.aplicar_cuarentena()
            self.poblacion[self.individuo.id] = self.individuo

        return self.poblacion
'''

class AlgoritmoHacerNadaCerrar:
    def __init__(self, cuarentena):
        self.tiempo_cuarentena = 0
        self.cerrado = False
        self.numero_tests = 0
        self.duracion_cuarentena = cuarentena['duracion']
        self.cuarentena_inicial = cuarentena['inicial']
        
    def reset(self):
        self.tiempo_cuarentena = 0
        self.tiempo = 0
        self.individuo = None
        self.cerrado = False
        self.numero_tests = 0
        
    def cerrar(self, poblacion):
        self.poblacion = poblacion
        self.cerrado = True
        for ind in poblacion.values():
            self.individuo = ind
            if self.individuo.sintomatico:
                self.individuo.cambiar_estado_observado('infeccioso')
            self.individuo.aplicar_cuarentena()
            self.poblacion[self.individuo.id] = self.individuo
        return self.poblacion        
    
    def abrir(self, poblacion):
        self.tiempo_cuarentena = 0
        self.poblacion = {}
        self.cerrado = False
        for ind in poblacion.values():
            self.individuo = ind
            if not self.individuo.sintomatico:
                self.individuo.cambiar_estado_observado('susceptible')
                self.individuo.aplicar_trabajo()                
            self.poblacion[self.individuo.id] = self.individuo
        return self.poblacion  
        
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

        if self.cerrado:
            self.tiempo_cuarentena += 1
            if self.tiempo_cuarentena > self.duracion_cuarentena:
                return self.abrir(poblacion)
            return self.poblacion
        for ind in poblacion.values():
            self.individuo = ind
            if self.individuo.en_trabajo():
                if self.individuo.sintomatico:
                    return self.cerrar(poblacion)
            elif self.individuo.sintomatico:
                self.individuo.cambiar_estado_observado('infeccioso')
            elif tiempo > self.duracion_cuarentena + self.individuo.dias_sintomas[1] + self.individuo.tiempo_inicio_infeccion:
                    self.individuo.cambiar_estado_observado('recuperado')
                    self.individuo.aplicar_trabajo()
                    self.poblacion[self.individuo.id] = self.individuo
            
        return self.poblacion
    
    def tick(self):
        pass
          
'''
class AlgoritmoBiosCerrar(AlgoritmoBios):
    def __init__(self, duracion_cuarentena, frecuencia_test):
        super().__init__(duracion_cuarentena, frecuencia_test)

        self.tiempo_cuarentena = 0
        self.cerrado = False

    def reset(self)
        self.tiempo_cuarentena = 0
        self.cerrado = False
        self.numero_tests = 0
        self.tiempo = 0
        self.individuo = None
        

    def cerrar(self, poblacion):
        self.poblacion = poblacion
        self.cerrado = True
        for ind in poblacion.values():
            self.individuo = ind
            if self.individuo.sintomatico:
                self.individuo.cambiar_estado_observado('infeccioso')
            self.individuo.aplicar_cuarentena()
            self.poblacion[self.individuo.id] = self.individuo
        return self.poblacion        
    
    def abrir(self, poblacion):
        self.tiempo_cuarentena = 0
        self.poblacion = {}
        self.cerrado = False
        for ind in poblacion.values():
            self.individuo = ind
            if not self.individuo.sintomatico and self.individuo.tiempo_inicio_infeccion == 0:
                self.individuo.cambiar_estado_observado('susceptible')
                self.individuo.aplicar_trabajo()                
            if self.individuo.tiempo_cuarentena > self.duracion_cuarentena:
                if tiempo >= self.individuo.tiempo_inicio_infeccion + self.individuo.dias_sintomas[1] + self.duracion_cuarentena:
                    self.individuo.cambiar_estado_observado('recuperado')
                    self.individuo.aplicar_trabajo()
            self.poblacion[self.individuo.id] = self.individuo
        return self.poblacion  

    def decidir(self, poblacion, tiempo, precision_tests):
        self.poblacion = poblacion
        if self.cerrado:
            self.tiempo_cuarentena =+ 1
            for ind in poblacion.values():
                self.individuo = ind
                if self.individuo.sintomatico:
                    self.individuo.cambiar_estado_observado('infeccioso')
            if self.tiempo_cuarentena > self.duracion_cuarentena:
                return self.abrir(poblacion)
            return self.poblacion

        for ind in poblacion.values():
            precision_actual_tests = precision_tests['precision'](ind.tiempo_infeccioso, ind.dia_aparicion_ac) 
            self.individuo = ind
            
            if self.individuo.en_trabajo():
                if self.individuo.sintomatico:
                    self.individuo.cambiar_estado_observado('infeccioso')
                    return self.cerrar(poblacion)
                elif (tiempo % self.frecuencia_test == 0) and (self.individuo.estado_observado == 'susceptible'):
                    self.aplicar_test(precision_actual_tests)
                    self.asignar_estado_segun_test()
                    if self.individuo.estado_observado == 'infeccioso' and self.individuo.estado == 'infeccioso':
                        return self.cerrar(poblacion)
            #elif (self.individuo.tiempo_cuarentena > self.tiempo_pcr) and (self.individuo.estado == 'susceptible'):
             #   self.individuo.cambiar_estado_observado('susceptible')
            elif self.individuo.tiempo_cuarentena > self.duracion_cuarentena:
                if tiempo >= self.individuo.tiempo_inicio_infeccion + self.individuo.dias_sintomas[1] + self.duracion_cuarentena:
                    self.individuo.cambiar_estado_observado('recuperado')
                    
            self.asignar_actividad()
            self.poblacion[self.individuo.id] = self.individuo
            
        return self.poblacion    
'''      
                
            
            
            
        
                
            
        