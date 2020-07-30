
# Parametros simulacion
    
alg =           c('Bios')
frec_test =     c(3, 5, 7, 10, 14)#rep(c(3, 0), each=8)#rep(c(0, 3, 0, 0), 20) 
ctna_dur =      rep(14, 60)
ctna_inic =     rep(0, 60)
pob =           rep(100, 60)
r0 =            rep(3, 50)
tiempo =        rep(30, 60)
iteraciones =   rep(30, 60)
fecha =         rep('19-07', 60)
p_inic =        rep(0.0075, 16)
ips =           rep(c('180.0'), 10)#rep(7.5, 40)
sens =          rep(0.97, 69) #rep(c(0, 0.2, 0.4, 0.6, 0.8, 0.95, 0.97, 1), 2)
nombre =        rep(c('No Protocol', 'ABT 3', 'Shift 14', 'Shut Down'), 20)
pcr =           c('1. Sin-PCR', '1. Sin-PCR', '1. Sin-PCR', 
                  '2. PCR Inicial', '2. PCR Inicial', '2. PCR Inicial',
                  '3. PCR Inicial y Contagio', '3. PCR Inicial y Contagio', '3. PCR Inicial y Contagio')

################################################################################

library(ggplot2)
library(tidyr)
library(dplyr)
library(rlang)
#library(formattable)
library(gridExtra)
library(viridis)
library(latex2exp)
library(forecast)
library(xtable)

################################################################################
path_sim = function(n){
    return(paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_ips=' , ips[n], '_sens=' , sens[n], '_' , fecha[n] , sep=''))
}

numero_infecciosos = function(sim){
    X = 0:3000
    aux2 = data.frame(X)
    aux2$infecciosos = 0
    
    out = read.csv(paste('../../datos_simulaciones/numero_infecciosos', sim, '.csv', sep='')) %>%
        gather(tiempo, infecciosos, X0:X156) %>%
        mutate(tiempo = as.numeric(substr(tiempo, 2, 100))) %>%
        filter(tiempo == max(tiempo)) %>%
        select(X, infecciosos) %>%
        rbind(aux2) %>%
        group_by(X) %>%
        summarize(infecciosos = sum(infecciosos)) %>%
        mutate(Protocolo = alg[n], Frecuencia = frec_test[n], R0 = r0[n], Sensibilidad = sens[n], PPS = ips[n])
    return(out)
}

numero_dias_infecciosos = function(sim){
    resultados = read.csv(paste('../../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    out = resultados %>%
        filter(estado == 'infeccioso' & actividad == 'trabajo') %>%
        summarize(Dias_Inf = sum(cantidad) / iteraciones[n]) %>% 
        mutate(Protocolo = alg[n],
               Frecuencia = frec_test[n], R0 = r0[n], Sensibilidad = sens[n], PPS = ips[n])
    return(out)
}

fraccion_tiempo_sano = function(sim){
    resultados = read.csv(paste('../../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    out = resultados %>%
        filter(estado == 'infeccioso' & actividad == 'trabajo') %>%
        group_by(it) %>%
        summarize(inf = NROW(tiempo) / 156) %>%
        summarize(fraccion_tiempo = 1 - (sum(inf) / iteraciones[n])) %>%
        mutate(Protocolo = alg[n],  Frecuencia = frec_test[n], R0 = r0[n], Sensibilidad = sens[n], PPS = ips[n])
    return(out)
}

poblacion_inf_trabajando = function(sim){
    resultados = read.csv(paste('../../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    
    out = resultados %>%
        filter(actividad == 'trabajo') %>%
        group_by(it, tiempo) %>%
        mutate(trabajando = sum(cantidad)) %>%
        filter(estado == 'infeccioso') %>%
        group_by(tiempo) %>%
        summarize(Inf_Trab = sum(cantidad / trabajando), trabajando = first(trabajando)) %>%
        mutate(Protocolo = alg[n], Frecuencia = frec_test[n], R0 = r0[n] , Sensibilidad = sens[n], PPS = ips[n])
    
    return(out)
}

################################################################################

# Coeficiente de Variacion
{
    n=1
    resultados = numero_infecciosos(path_sim(n))
    for(n in 2:length(alg)){
        aux = numero_infecciosos(path_sim(n))
        resultados = rbind(resultados, aux)
    }
    
    cv_it = resultados %>%
        group_by(Frecuencia, Sensibilidad, PPS, Protocolo) %>%
        summarize(MEAN = mean(infecciosos), STD = sd(infecciosos), cv = MEAN / STD / sqrt(iteraciones[1])) 
    cv_it[cv_it$Protocolo == 'HN', 'Protocolo'] = 'HacerNada'
    
    #cv_it %>% mutate(cv = scales::label_percent(accuracy=0.01)(cv)) %>% spread(key=Protocolo, value=cv) %>% xtable()
}
################################################################################

# Numero de Dias infecciosos en el trabajo
{
    n=1
    dias_infecciosos = numero_dias_infecciosos(path_sim(n))
    
    for(n in 2:length(alg)){
        aux = numero_dias_infecciosos(path_sim(n))
        dias_infecciosos = rbind(dias_infecciosos, aux)
    }
    
    dias_infecciosos
    
   d_fin = merge(cv_it, dias_infecciosos, by=c('Frecuencia', 'Sensibilidad', 'PPS', 'Protocolo')) %>%
        mutate(Dias_Promedio_Extraccion = Dias_Inf / MEAN) 
}

################################################################################
# Fraccion de tiempo sin empleados enfermos
{
    n=1
    fraccion_tiempo = fraccion_tiempo_sano(path_sim(n))
    
    for(n in 2:length(alg)){
        aux =  fraccion_tiempo_sano(path_sim(n))
        fraccion_tiempo = rbind(fraccion_tiempo, aux)
    }
    
    fraccion_tiempo[order(fraccion_tiempo$Frecuencia),]
}

################################################################################
# Poblacion infectada trabajando
{
    n=1
    trabajando_inf = poblacion_inf_trabajando(path_sim(n))
    for(n in 2:length(alg)){
        aux = poblacion_inf_trabajando(path_sim(n))
        trabajando_inf = rbind(trabajando_inf, aux)
    }
    
    trabajando_inf 
    tabla = trabajando_inf %>%
        group_by(Frecuencia, Sensibilidad, PPS, Protocolo) %>%
        summarize(Trabajando = mean(trabajando), Infectados = mean(Inf_Trab))
    tabla
    
}

##############################################################################
cv_it %>% 
    select(Sensibilidad, MEAN) %>%
    cbind(dias_infecciosos %>%
              select(Dias_Inf)) %>%
    cbind(fraccion_tiempo %>%
              select(fraccion_tiempo)) %>%
    cbind(tabla %>% ungroup %>%
              select(Trabajando, Infectados)) %>%
    xtable()

cv_it %>% 
    select(PPS, MEAN) %>%
    cbind(dias_infecciosos[order(dias_infecciosos$PPS), ] %>%
              select(Dias_Inf)) %>%
    cbind(fraccion_tiempo[order(fraccion_tiempo$PPS),] %>%
              select(fraccion_tiempo)) %>%
    cbind(tabla %>% ungroup %>%
              select(Trabajando, Infectados)) %>%
    xtable()
    
cv_it %>% 
    select(Frecuencia, MEAN) %>%
    cbind(dias_infecciosos[order(dias_infecciosos$Frecuencia), ] %>%
              select(Dias_Inf)) %>%
    cbind(fraccion_tiempo[order(fraccion_tiempo$Frecuencia),] %>%
              select(fraccion_tiempo)) %>%
    cbind(tabla %>% ungroup %>%
              select(Trabajando, Infectados)) %>%
    xtable()    


################################################################################
# CV por numero de iteraciones
{
    n=1
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
    
    X = 0:9999
    aux2 = data.frame(X)
    aux2$infecciosos = 0

    resultados = read.csv(paste('../datos_simulaciones/numero_infecciosos', sim, '.csv', sep='')) %>%
        gather(tiempo, infecciosos, X0:X156) %>%
        mutate(tiempo = as.numeric(substr(tiempo, 2, 100))) %>%
        filter(tiempo == max(tiempo)) %>%
        select(X, infecciosos) %>%
        rbind(aux2) %>%
        group_by(X) %>%
        summarize(infecciosos = sum(infecciosos)) %>%
        mutate(Protocolo = nombre[n], R0 = r0[n])
    
    for(n in 2:length(alg)){
        sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
        
        X = 0:9999
        aux2 = data.frame(X)
        aux2$infecciosos = 0
        
        aux = read.csv(paste('../datos_simulaciones/numero_infecciosos', sim, '.csv', sep='')) %>%
            gather(tiempo, infecciosos, X0:X156) %>%
            mutate(tiempo = as.numeric(substr(tiempo, 2, 100))) %>%
            filter(tiempo == max(tiempo)) %>%
            select(X, infecciosos) %>%
            rbind(aux2) %>%
            group_by(X) %>%
            summarize(infecciosos = sum(infecciosos)) %>%
            mutate(Protocolo = nombre[n], R0 = r0[n])
        
        resultados = rbind(resultados, aux)
    }
    
    cv_it = resultados %>%
        filter(X < 1000) %>%
        group_by(Protocolo) %>%
        summarize(cv = mean(infecciosos) / sd(infecciosos) / sqrt(1000)) %>%
        mutate(it = 1000)
    
    for(it in seq(2000, 10000, 1000)){
        aux = resultados %>%
            filter(X < it) %>%
            group_by(Protocolo) %>%
            summarize(cv = mean(infecciosos) / sd(infecciosos) / sqrt(it)) %>%
            mutate(it = it)
        
        cv_it = rbind(cv_it, aux)
    
    }    
    
    cv_it %>% mutate(cv = scales::label_percent(accuracy=0.01)(cv)) %>% spread(key=Protocolo, value=cv) %>% xtable()
    
}

################################################################################
# CV por numero de iteraciones
{
    n=1
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
    
    resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) %>%
        select(it, tiempo, estado, estado_observado, cantidad) %>%
        filter(estado == 'infeccioso' | estado_observado == 'infeccioso') %>%
        mutate(infeccioso_identificado = ifelse(estado_observado == 'infeccioso' & estado == 'infeccioso', cantidad, 0),
               infeccioso_real = ifelse(estado == 'infeccioso', cantidad, 0)) %>%
        group_by(tiempo) %>%
        summarize(infeccioso_identificado = mean(infeccioso_identificado), infeccioso_real = mean(infeccioso_real)) %>%
        summarize(infeccioso_identificado = sum(infeccioso_identificado), infeccioso_real = sum(infeccioso_real)) %>%
        mutate(ratio_precision = infeccioso_identificado / infeccioso_real) %>%
        mutate(Protocolo = nombre[n], R0 = r0[n])
    
    for(n in 2:length(alg)){
        sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
        
        X = 0:9999
        aux2 = data.frame(X)
        aux2$infecciosos = 0
        
        aux =  read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) %>%
            select(it, tiempo, estado, estado_observado, cantidad) %>%
            filter(estado == 'infeccioso' | estado_observado == 'infeccioso') %>%
            mutate(infeccioso_identificado = ifelse(estado_observado == 'infeccioso' & estado == 'infeccioso', cantidad, 0),
                   infeccioso_real = ifelse(estado == 'infeccioso', cantidad, 0)) %>%
            group_by(tiempo) %>%
            summarize(infeccioso_identificado = mean(infeccioso_identificado), infeccioso_real = mean(infeccioso_real)) %>%
            summarize(infeccioso_identificado = sum(infeccioso_identificado), infeccioso_real = sum(infeccioso_real)) %>%
            mutate(ratio_precision = infeccioso_identificado / infeccioso_real) %>%
            mutate(Protocolo = nombre[n], R0 = r0[n])
        
        resultados = rbind(resultados, aux)
    }
    
    cv_it = resultados %>%
        filter(X < 1000) %>%
        group_by(Protocolo) %>%
        summarize(cv = mean(infecciosos) / sd(infecciosos) / sqrt(1000)) %>%
        mutate(it = 1000)
    
    for(it in seq(2000, 10000, 1000)){
        aux = resultados %>%
            filter(X < it) %>%
            group_by(Protocolo) %>%
            summarize(cv = mean(infecciosos) / sd(infecciosos) / sqrt(it)) %>%
            mutate(it = it)
        
        cv_it = rbind(cv_it, aux)
        
    }    
    
    cv_it %>% mutate(cv = scales::label_percent(accuracy=0.01)(cv)) %>% spread(key=Protocolo, value=cv) %>% xtable()
    
}





ggsave(paste('../plots/resultados_28-04/Infectados totales (no porcentajes) iter=200.pdf', sep=''), p)




infecciones_2 %>%
    ggplot(aes(x=tiempo, y=1 - (total_inf/1000), color=Protocolo)) +
    geom_line() +
    facet_wrap(~factor(Inf_Inic))

infecciones_2 %>%
    ggplot(aes(x=tiempo, y=1 - total_inf/1000, fill=Protocolo)) +
    geom_bar(stat='identity', position='dodge') +
    facet_wrap(~factor(Inf_Inic))




resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
resultados$X = NULL
resultados$actividad = as.character(resultados$actividad)
resultados$actividad[resultados$actividad == 'trabajo'] = 'activo'
resultados$actividad = as.factor(resultados$actividad)

estado = resultados[c('estado', 'cantidad', 'tiempo', 'it')] %>%
    sumario_tabla(c('Estado'))
estado$alg = nombre[n]
estado$ctna_inic = ctna_inic[n] > 0
estado$p_inic = p_inic[n]

actividad = resultados[c('actividad', 'cantidad', 'tiempo', 'it')] %>%
    sumario_tabla(c('Actividad'))
actividad$alg = nombre[n]
actividad$ctna_inic = ctna_inic[n] > 0
actividad$p_inic = p_inic[n]

for(n in 2:length(alg)){
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
    
    resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    resultados$actividad = as.character(resultados$actividad)
    resultados$actividad[resultados$actividad == 'trabajo'] = 'activo'
    resultados$actividad = as.factor(resultados$actividad)
    
    aux = resultados[c('estado', 'cantidad', 'tiempo', 'it')] %>%
        sumario_tabla(c('Estado'))
    aux$alg = nombre[n]
    aux$ctna_inic = ctna_inic[n] > 0
    aux$p_inic = p_inic[n]
    
    estado = rbind(estado, aux)
    
    aux = resultados[c('actividad', 'cantidad', 'tiempo', 'it')] %>%
        sumario_tabla(c('Actividad'))
    aux$alg = nombre[n]
    aux$ctna_inic = ctna_inic[n] > 0
    aux$p_inic = p_inic[n]
    
    actividad = rbind(actividad, aux)
    #estado_actividad = resultados[c('estado', 'actividad', 'cantidad', 'tiempo', 'it')]
}

estado_inf = estado %>%
    filter(Estado == 'infeccioso' & Dias %% 26 == 0) %>%
    group_by(alg, ctna_inic, p_inic) %>%
    summarise(Total_Infecciosos = percent(sum(Poblacion) / pob[1])) %>%
    rename(Protocolo = alg, Cuarentena_Inicial = ctna_inic)

estado_inf2 = estado %>%
    filter(Estado == 'infeccioso') %>%
    group_by(alg, ctna_inic, p_inic) %>%
    summarise(Total_Infecciosos = sum(Poblacion) / 26)

actividad_act = actividad %>%
    filter(Actividad == 'activo') %>%
    group_by(alg, ctna_inic, p_inic) %>%
    summarise(Promedio_Trabajando = percent(mean(Poblacion) / pob[1])) %>% 
    rename(Protocolo = alg, Cuarentena_Inicial = ctna_inic)

estado_sum = data.frame(t(spread(estado_inf, key=p_inic, value=Total_Infecciosos)))
actividad_sum = data.frame(t(spread(actividad_act, key=p_inic, value=Promedio_Trabajando)))
formattable(estado_sum)
formattable(actividad_sum)

