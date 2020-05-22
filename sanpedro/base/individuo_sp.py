import numpy as np
from numpy import random
from .individuo import Individuo

class IndividuoAdhiere(Individuo):
    '''
    Clase de individuo
    '''
    def __init__(self, estado_inicial, idx, duracion_infeccion, dias_sintomas, dia_muerte, dia_aparicion_ac, sintomatico, muere, adhiere):
        super().__init__(estado_inicial, idx, duracion_infeccion, dias_sintomas, dia_muerte, dia_aparicion_ac, sintomatico, muere)
        self.adhiere = adhiere
