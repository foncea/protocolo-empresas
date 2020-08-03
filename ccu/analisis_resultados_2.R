
# Parametros simulacion
    
alg =           c('HacerNada', 'Bios', 'Bios', 'Bios')
frec_test =     c(0, 1, 7, 14)#rep(c(3, 0), each=8)#rep(c(0, 3, 0, 0), 20) 
ctna_dur =      rep(14, 60)
ctna_inic =     rep(0, 60)
pob =           rep(100, 60)
r0 =            rep(2.9, 50)
tiempo =        rep(92, 60)
iteraciones =   rep(1000, 4)
fecha =         rep('30-08', 60)
p_inic =        rep(0.005, 16)
ips =           rep(c('180.0'), 10)#rep(7.5, 40)
sens =          rep(0.88, 2) #rep(c(0, 0.2, 0.4, 0.6, 0.8, 0.95, 0.97, 1), 2)
nombre =        rep(c('Sin AC', 'AC 2/Semana', 'AC 1/Semana', 'AC 14 Dias'), 20)
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
    return(paste('_' , alg[n] , '_frec=' , frec_test[n] , '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n] , '_sens=', sens[n], '_', fecha[n] , sep=''))
}

path_sim2 = function(n){
    return(paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=', pob[n], '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_', fecha[n] , sep=''))
}

numero_infecciosos = function(sim){
    X = 0:iteraciones[1]
    aux2 = data.frame(X)
    aux2$infecciosos = 0
    
    out = read.csv(paste('../../datos_simulaciones/numero_infecciosos', sim, '.csv', sep='')) %>%
        gather(tiempo, infecciosos, X0:X92) %>%
        mutate(tiempo = as.numeric(substr(tiempo, 2, 100))) %>%
        filter(tiempo == max(tiempo)) %>%
        select(X, infecciosos) %>%
        rbind(aux2) %>%
        group_by(X) %>%
        summarize(infecciosos = sum(infecciosos)) %>%
        mutate(Protocolo = nombre[n], Frecuencia = frec_test[n], R0 = r0[n], Sensibilidad = sens[n], PPS = ips[n])
    return(out)
}

numero_infecciosos_cum = function(sim){
    X = 0:iteraciones[1]
    aux2 = data.frame(X)
    aux2$infecciosos = 0
    
    out = read.csv(paste('../../datos_simulaciones/numero_infecciosos', sim, '.csv', sep='')) %>%
        gather(tiempo, infecciosos, X0:X92) %>%
        mutate(tiempo = as.numeric(substr(tiempo, 2, 100))) %>%
        group_by(X, tiempo) %>%
        summarize(infecciosos = sum(infecciosos)) %>%
        group_by(tiempo) %>%
        summarize(intervalo = sd(infecciosos), infecciosos = mean(infecciosos)) %>%
        mutate(Protocolo = nombre[n])
    return(out)
}

numero_dias_infecciosos = function(sim){
    resultados = read.csv(paste('../../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    out = resultados %>%
        filter(estado == 'infeccioso' & actividad == 'trabajo') %>%
        filter(tiempo %% 7 %in% c(0, 1, 2, 3, 4)) %>%
        summarize(Dias_Inf = sum(cantidad) / iteraciones[n]) %>% 
        mutate(Protocolo = nombre[n],
               Frecuencia = frec_test[n], R0 = r0[n], Sensibilidad = sens[n], PPS = ips[n])
    return(out)
}

fraccion_tiempo_sano = function(sim){
    resultados = read.csv(paste('../../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    out = resultados %>%
        filter(estado == 'infeccioso' & actividad == 'trabajo') %>%
        group_by(it) %>%
        summarize(inf = NROW(tiempo) / 92) %>%
        summarize(fraccion_tiempo = 1 - (sum(inf) / iteraciones[n])) %>%
        mutate(Protocolo = nombre[n],  Frecuencia = frec_test[n], R0 = r0[n], Sensibilidad = sens[n], PPS = ips[n])
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
        mutate(Protocolo = nombre[n], Frecuencia = frec_test[n], R0 = r0[n] , Sensibilidad = sens[n], PPS = ips[n])
    
    return(out)
}

seir = function(sim){
    resultados = read.csv(paste('../../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    
    out = resultados %>%
        group_by(it, tiempo, estado) %>%
        mutate(cantidad = sum(cantidad)) %>%
        group_by(tiempo, estado) %>%
        summarize(inter = sd(cantidad), cantidad = mean(cantidad)) %>%
        mutate(Protocolo = nombre[n])
    
    return(out)
}

trabajando_tiempo = function(sim){
    resultados = read.csv(paste('../../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    
    out = resultados %>%
        group_by(it, tiempo, actividad) %>%
        mutate(cantidad = sum(cantidad)) %>%
        group_by(tiempo, actividad) %>%
        summarize(inter = sd(cantidad), cantidad = mean(cantidad)) %>%
        mutate(Protocolo = nombre[n])
    
    return(out)
}


################################################################################

# Coeficiente de Variacion
{
    n=1
    resultados = numero_infecciosos(path_sim2(n))
    for(n in 2:length(alg)){
        aux = numero_infecciosos(path_sim2(n))
        resultados = rbind(resultados, aux)
    }
    
    cv_it = resultados %>%
        group_by(Protocolo) %>%
        summarize(MEAN = mean(infecciosos), STD = sd(infecciosos), cv = MEAN / STD / sqrt(iteraciones[1])) 
    cv_it[order(cv_it$MEAN), ]
    cv_it[cv_it$Protocolo == 'HN', 'Protocolo'] = 'HacerNada'
    
    #cv_it %>% mutate(cv = scales::label_percent(accuracy=0.01)(cv)) %>% spread(key=Protocolo, value=cv) %>% xtable()
}
################################################################################

# Numero de Dias infecciosos en el trabajo
{
    n=1
    dias_infecciosos = numero_dias_infecciosos(path_sim2(n))
    
    for(n in 2:length(alg)){
        aux = numero_dias_infecciosos(path_sim2(n))
        dias_infecciosos = rbind(dias_infecciosos, aux)
    }
    
    dias_infecciosos
    
   d_fin = merge(cv_it, dias_infecciosos, by=c( 'Protocolo')) %>%
        mutate(Dias_Promedio_Extraccion = Dias_Inf / MEAN) 
   
   p1 = dias_infecciosos %>%
       mutate(Protocolo = factor(Protocolo, levels=nombre[c(2,3,1)])) %>%
       ggplot(aes(x=Protocolo, y=Dias_Inf, fill=Protocolo)) +
       geom_bar(stat='identity', position='dodge', color='black') +
       ggtitle('Working-Days Infected at Work (5 Months, 100 Workers)') +
       ylab('') + 
       xlab('') +
       #xlab(TeX("$R_0$")) + 
       theme_bw() + 
       scale_fill_brewer(palette='Set3') +
       geom_text(aes(label=round(Dias_Inf, digits=1)), position=position_dodge(width=0.9), vjust=-0.25) 
   p1
   p = merge(cv_it, dias_infecciosos, by=c('Protocolo')) %>%
       mutate(Dias_Promedio_Extraccion = Dias_Inf / MEAN) %>%
   #    mutate(Protocolo = factor(Protocolo, levels=nombre[c(2,3,1)])) %>%
       ggplot(aes(x=Protocolo, y=Dias_Promedio_Extraccion, fill=Protocolo)) +
       geom_bar(stat='identity', position='dodge', color='black') +
       ggtitle('Average number of days between infection and quarantine') +
       ylab('') + 
       xlab('') +
       #xlab(TeX('$R_0$')) +
       theme_bw() + 
       scale_fill_brewer(palette='Set3') +
       geom_text(aes(label=round(Dias_Promedio_Extraccion, digits=1)), position=position_dodge(width=0.9), vjust=-0.25) 
   p
   
   p = merge(cv_it, dias_infecciosos, by=c('Protocolo')) %>%
       mutate(Dias_Promedio_Extraccion = Dias_Inf / MEAN) %>%
       mutate(Protocolo = factor(Protocolo, levels=nombre[c(2, 3,4,1)])) %>%
       ggplot(aes(x=Protocolo, y=2.9 * Dias_Promedio_Extraccion / 17.5, fill=Protocolo)) +
       geom_bar(stat='identity', position='dodge', color='black') +
       ggtitle(TeX('$R_e$ Ajustado')) +
       ylab('') + 
       xlab('') +
       #xlab(TeX('$R_0$')) +
       theme_bw() + 
       scale_fill_brewer(palette='Set3') +
       geom_text(aes(label=round(2.9 * Dias_Promedio_Extraccion / 17.5, digits=1)), position=position_dodge(width=0.9), vjust=-0.25) 
   p
   
   
}
ggsave(paste('../plots/ABT_1_semana_07_07/Dias-hombre Infectados en Trabajo.pdf', sep=''), p1)
ggsave(paste('../plots/ABT_1_semana_07_07/Dias promedio entre infeccion y cuarentena.pdf', sep=''), p)


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
    
    p = fraccion_tiempo %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[c(2,3,1)])) %>%
        ggplot(aes(x=Protocolo, y=fraccion_tiempo, fill=Protocolo)) +
        geom_bar(stat='identity', position='dodge', color='black') +
        ggtitle('Fraction of days with no infected workers at work') +
        ylab('') + 
        scale_y_continuous(labels = scales::percent) +
        xlab('') + 
        coord_cartesian(ylim=c(0,1)) + 
        scale_fill_brewer(palette='Set3') + 
        #scale_fill_viridis(option='viridis', discrete=TRUE) + 
        geom_text(aes(label=scales::label_percent(accuracy=0.1)(fraccion_tiempo)), position=position_dodge(width=0.9), vjust=-0.25) +
        theme_bw()
    p
        }
ggsave(paste('../plots/ABT_1_semana_07_07/Fraccion dias sin trabajadores infectados.pdf', sep=''), p)

################################################################################
# Poblacion infectada trabajando
{
    n=1
    trabajando_inf = poblacion_inf_trabajando(path_sim2(n))
    for(n in 2:length(alg)){
        aux = poblacion_inf_trabajando(path_sim2(n))
        trabajando_inf = rbind(trabajando_inf, aux)
    }
    
    trabajando_inf 
    tabla = trabajando_inf %>%
        group_by(Frecuencia, Sensibilidad, PPS, Protocolo) %>%
        summarize(Trabajando = mean(trabajando), Infectados = mean(Inf_Trab))
    tabla
    
    trabajando_inf[trabajando_inf$tiempo == 0, 'Inf_Trab'] = 0.005 * iteraciones[1]
    trabajando_inf['dias'] = trabajando_inf['tiempo'] / 24
    
    p = trabajando_inf %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[rev(c(2,3, 1))])) %>%
        ggplot(aes(x=tiempo, y=ma(Inf_Trab / iteraciones[1], order=7) , color=Protocolo)) +
        geom_line(size=1) +
        ggtitle('Procentaje de trabajadores infectados en un dia dado en la empresa') +
        ylab('') + 
        xlab('Days') +
        #geom_ribbon(aes(ymin=100*(Inf_Trab - desv/8), ymax=100*(Inf_Trab + desv/8)),alpha=0.3) +
        theme_bw() +
        #facet_wrap(~factor(R0)) + 
        scale_y_continuous(labels = scales::percent) 
    
    p
    
    trabajando_inf['semana'] = 0
    for(i in 1:nrow(trabajando_inf)){
        trabajando_inf[i, 'semana'] = floor(trabajando_inf[i, 'dias'] / 7)
    }
    
    trabajando_inf %>%
        group_by(semana, Protocolo) %>%
        summarize(inf_trab = mean(Inf_Trab)) %>%
        ggplot(aes(x=semana, y=inf_trab, order=7 / iteraciones[1] , color=Protocolo)) +
        geom_line(size=1) +
        ggtitle('Procentaje de trabajadores infectados') +
        ylab('') + 
        xlab('Semanas') +
        #geom_ribbon(aes(ymin=100*(Inf_Trab - desv/8), ymax=100*(Inf_Trab + desv/8)),alpha=0.3) +
        theme_bw() +
        #facet_wrap(~factor(R0)) + 
        scale_y_continuous(labels = scales::percent) 
    
    aux = data.frame(dias=c(1, 79, 59, 63, 74, 74, 78, 67, 105))
    aux$Protocolo = 'Realidad'
    aux = aux[order(aux$dias), ]
    aux$infecciosos = 1:nrow(aux)
    aux['tiempo'] = aux['dias'] * 24
    aux['infecciosos_sintomaticos'] = aux['infecciosos']
    aux['intervalo'] = 0
    
    
        
        
    
}
ggsave(paste('../plots/ABT_1_semana_07_07/Infectados en el trabajo.pdf', sep=''), p)


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
{
    n=1
    resultados = numero_infecciosos_cum(path_sim2(n))
    for(n in 2:length(alg)){
        aux = numero_infecciosos_cum(path_sim2(n))
        resultados = rbind(resultados, aux)
    }
    #resultados['dias'] = resultados['tiempo'] / 24
    #resultados['infecciosos_sintomaticos'] = resultados['infecciosos'] / 2
    
    #resultados['Protocolo'] = 'Simulacion'
    
    
    aux = data.frame(tiempo=c(1, 79, 59, 63, 74, 74, 78, 67, 102))
    aux$Protocolo = 'Realidad'
    aux = aux[order(aux$tiempo), ]
    aux$infecciosos = 1:nrow(aux)
    aux['tiempo'] = aux['dias'] * 24
    aux['infecciosos_sintomaticos'] = aux['infecciosos']
    aux['intervalo'] = 0
    
    p = resultados %>% 
        mutate(Protocolo = factor(Protocolo, levels=nombre[c(1,4,3,2)])) %>%
    #    rbind(aux) %>%
            ggplot(aes(x=tiempo, y=infecciosos, color=Protocolo)) +
                geom_line(size=1.5) +
                #geom_point(data=resultados[resultados$Protocolo == 'Realidad', ]) +
                #geom_line(data=resultados[resultados$Protocolo == 'Simulacion', ]) + 
                ggtitle('Numero total de infecciosos acumulados a traves del tiempo') 
                #geom_ribbon(aes(ymin = infecciosos_sintomaticos - intervalo/4, ymax = infecciosos_sintomaticos + intervalo/4), alpha=0.2)
    p
    
    ggplot(df, aes(x, y))+
        geom_point(data=df[df$group==2, ])+
        geom_line(data=df[df$group==1, ])
}

seir[seir['estado'] == 'i', 'estado'] = 'infeccioso'

seir %>%
    ggplot(aes(x=tiempo/24, y=cantidad, color=estado)) +
    geom_line()

################################################################################
{
    n=1
    resultados = trabajando_tiempo(path_sim2(n))
    for(n in 2:length(alg)){
        aux = trabajando_tiempo(path_sim2(n))
        resultados = rbind(resultados, aux)
    }
    N=345
    
    resultados %>%
        filter(actividad == 'cuarentena') %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[rev(c(2,3, 4, 1))])) %>%
        mutate(cantidad = ifelse(Protocolo == 'Con AC', cantidad - 7 , cantidad)) %>% 
        group_by(Protocolo) %>%
        summarize(mean_cuarentena = mean(cantidad/N)) %>%
        ggplot(aes(x=Protocolo, y=mean_cuarentena, fill=Protocolo)) +
            geom_bar(stat='identity', position='dodge', color='black') +
            ggtitle('Porcentaje de empleados en cuarentena promedio a traves del tiempo') +
            ylab('') + 
            xlab('') +
            #xlab(TeX('$R_0$')) +
            theme_bw() + 
            scale_fill_brewer(palette='Set3') +
            geom_text(aes(label=scales::label_percent(accuracy=0.1)(round(mean_cuarentena, 4))), position=position_dodge(width=0.9), vjust=-0.25) +
            scale_y_continuous(labels = scales::percent) 
    p
    
    resultados %>%
        filter(actividad == 'cuarentena') %>%c
        ggplot(aes(x=tiempo, y=ma(cantidad, order=10)/N, color=Protocolo)) +
            geom_line() +
        ggtitle('Porcentaje de trabajadores en cuarentena') +
        scale_y_continuous(labels = scales::percent) 
    
    resultados['dias'] = resultados['tiempo'] / 24
    resultados['infecciosos_sintomaticos'] = resultados['infecciosos'] / 2
    
    resultados['Protocolo'] = 'Simulacion'
    
    
    aux = data.frame(tiempo=c(1, 79, 59, 63, 74, 74, 78, 67, 102))
    aux$Protocolo = 'Realidad'
    aux = aux[order(aux$tiempo), ]
    aux$infecciosos = 1:nrow(aux)
    aux['tiempo'] = aux['dias'] * 24
    aux['infecciosos_sintomaticos'] = aux['infecciosos']
    aux['intervalo'] = 0
    
    p = resultados %>% 
        #    rbind(aux) %>%
        ggplot(aes(x=tiempo, y=infecciosos, color=Protocolo)) +
        geom_point(size=3)+
        #geom_point(data=resultados[resultados$Protocolo == 'Realidad', ]) +
        #geom_line(data=resultados[resultados$Protocolo == 'Simulacion', ]) + 
        ggtitle('Numero total de infecciosos acumulados a traves del tiempo') 
    #geom_ribbon(aes(ymin = infecciosos_sintomaticos - intervalo/4, ymax = infecciosos_sintomaticos + intervalo/4), alpha=0.2)
    p
    
    ggplot(df, aes(x, y))+
        geom_point(data=df[df$group==2, ])+
        geom_line(data=df[df$group==1, ])
}

seir[seir['estado'] == 'i', 'estado'] = 'infeccioso'

seir %>%
    ggplot(aes(x=tiempo/24, y=cantidad, color=estado)) +
    geom_line()





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

