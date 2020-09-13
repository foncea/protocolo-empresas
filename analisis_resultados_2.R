
# Parametros simulacion

serie = '01-09_base'
algos = c('HacerNada0', 'Bios3', 'Bios7', 'BiosTurnos7', 'BiosTurnos3', 'ABT_Turnos0')

################################################################################
id = c(paste(algos[1], serie, sep='_'))
for(a in 2:length(algos)){
    id = c(id, paste(algos[a], serie, sep='_'))
}

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

path_num_inf = function(s){
    paste('../datos_simulaciones/numero_infecciosos_', s, '.csv', sep='')
}

path_resultados = function(s){
    paste('../datos_simulaciones/resultados_', s, '.csv', sep='')
}

################################################################################

numero_infecciosos = read.csv(path_num_inf(id[1]))
resultados = read.csv(path_resultados(id[1]))
for(i in 2:length(id)){
    aux = read.csv(path_num_inf(id[i]))
    numero_infecciosos = rbind(numero_infecciosos, aux)
    aux = read.csv(path_resultados(id[i]))
    resultados = rbind(resultados, aux)
}

numero_infecciosos$Protocol = paste(numero_infecciosos$protocolo, numero_infecciosos$frecuencia_test, sep=' ')
resultados$Protocol = paste(resultados$protocolo, resultados$frecuencia_test, sep=' ')

coef_var = numero_infecciosos %>%
    filter(tiempo == max(tiempo)) %>%
    group_by(Protocol, sensibilidad) %>%
    summarize(mean_inf = mean(infectados), sd_inf = sd(infectados) / sqrt(5000)) %>%
    mutate(cv = sd_inf / mean_inf)

trabajando = resultados %>%
    filter(actividad == 'trabajo') %>%
    group_by(Protocol, tiempo, it) %>%
    summarize(trabajando = sum(cantidad)) %>%
    group_by(Protocol) %>%
    summarise(trabajando = mean(trabajando))

Re = resultados %>%
    filter(estado == 'infeccioso' & actividad == 'trabajo') %>%
    #filter(tiempo %% 7 %in% c(0, 1, 2, 3, 4)) %>%
    group_by(Protocol) %>%
    summarize(Dias_Inf = sum(cantidad) / 5000) %>%
    merge(coef_var, by=c('Protocol')) %>%
    mutate(Dias_Promedio_Extraccion = Dias_Inf / mean_inf) %>%
    mutate(Re = 3 * Dias_Promedio_Extraccion / 18) %>%
    select(Protocol, Re)

inf_base = numero_infecciosos %>%
    filter(Protocol %in% c('Base 0')) %>%
    select(infectados, tiempo, it)

inf_protocol = numero_infecciosos %>%
    filter(Protocol %in% c('ABT 3')) %>%
    select(infectados, tiempo, it) %>%
    rename(infectados_protocol = infectados) %>%
    merge(inf_base, by=c('it', 'tiempo')) %>%
    mutate(diff_inf = infectados - infectados_protocol)

inf_protocol %>% 
    group_by(tiempo) %>%
    summarize(mean_diff = mean(diff_inf), 
              sd_diff = sd(diff_inf), 
              min_diff = min(diff_inf), 
              max_diff = max(diff_inf),
              q5 = quantile(diff_inf, 0.05),
              q95 = quantile(diff_inf, 0.95),
              q10 = quantile(diff_inf, 0.1),
              q90 = quantile(diff_inf, 0.9),
              q16 = quantile(diff_inf, 0.16),
              q84 = quantile(diff_inf, 0.84),
              q025 = quantile(diff_inf, 0.025),
              q975 = quantile(diff_inf, 0.975)) %>%
    ggplot(aes(x=tiempo, y=mean_diff)) + geom_line() +
    #geom_ribbon(aes(ymin = mean_diff - sd_diff, ymax = mean_diff + sd_diff), alpha=0.2)
    geom_ribbon(aes(ymin = q025, ymax = q975), alpha=0.2) +
    xlab('Days') +
    ylab('') +
    scale_y_continuous(breaks = seq(0, 50, 2)) +
    ggtitle('Difference in number of workers infected')

inf_protocol %>%
    filter(tiempo %in% c(30, 60, 90, 120, 150)) %>%
    mutate(tiempo = as.factor(tiempo)) %>%
    ggplot(aes(x=tiempo, y=diff_inf)) +
        geom_boxplot()

protocol_name = function(p){
    if (p == 'Bios 3'){a = 'ABT 3'}
    if (p == 'Bios 7'){a = 'ABT 7'}
    if (p == 'BiosTurnos 3'){a = 'ABT 3 + Shift 14'}
    if (p == 'BiosTurnos 7'){a = 'ABT 7 + Shift 14'}
    if (p == 'HacerNada 0'){a = 'Base'}
    if (p == 'Contactos_Estrechos 0'){a = 'Base + Close Contacts'}
    if (p == 'ABT_Turnos 0'){a = 'Shift 14'}
    return(a)
}
    
numero_infecciosos$Protocol2 = numero_infecciosos$Protocol
numero_infecciosos$Protocol = numero_infecciosos$Protocol2 %>% sapply(protocol_name)

legend_level = c('Base', 'Shift 14', 'ABT 7', 'ABT 3', 'ABT 7 + Shift 14', 'ABT 3 + Shift 14')

numero_infecciosos %>%
    mutate(Protocol = factor(Protocol, levels=legend_level)) %>%
    group_by(Protocol, tiempo) %>%
    summarize(infectados = mean(infectados)) %>%
    ggplot(aes(x=tiempo, y=infectados, color=Protocol)) +
    geom_point() +
    xlab('Days') +
    ylab('') +
    scale_y_continuous(breaks = seq(0, 24, 2)) +
    ggtitle('Cumulative number of infected individuals')

resultados %>%
    filter(protocolo == 'HacerNada') %>%
    group_by(estado, tiempo) %>%
    summarize(cantidad = sum(cantidad / 10)) %>%
    ggplot(aes(x=tiempo, y=cantidad, color=estado)) +
    geom_line()

################################################################################

r0_range = function(x, y, n){
    a = seq(x[1], y[1], n[1])
    b = seq(x[2], y[2], n[2])
    df = data.frame(r0_emp=double(), r0_pob=double())
    for(i in a){
        for(j in b){
            df = rbind(df, c(i, j))
            df = rbind(df, c(i, j))
        }
    }
    return(df)
}

df = r0_range(c(2.0, 0.8), c(4.0, 2.0), c(0.2, 0.2))
df2 = r0_range(c(2.0, 0.8), c(4.0, 2.0), c(0.2, 0.2))
colnames(df2) = c('r0_emp', 'r0_pob')
df$X1 = format(round(df$X1, 1), nsmall = 1)
df$X2 = format(round(df$X2, 1), nsmall = 1)

################################################################################
path_sim = function(n){
    return(paste('_' , alg[n] , '_frec=' , frec_test[n] , '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n] , '_sens=', sens[n], '_', fecha[n] , sep=''))
}

path_sim2 = function(n){
    return(paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=', pob[n], '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_r0_pob=', r0_pob[n], '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_', fecha[n] , sep=''))
}

path_sim3 = function(r0_emp, r0_pob, a){
    test = frec_test[a]
    if(r0_pob == '0.8'){
        test = 7
    }
    return(paste('_' , alg[a] , '_frec=' , test , '_pob=', pob[1], '_r0=' , r0_emp , '_r0pob=', r0_pob, '_pi=', p_inic[1], '_iter=' , iteraciones[1], '_', fecha[1] , sep=''))
}

inf_total = function(r0_emp, r0_pob){
    sim1 = path_sim3(r0_emp, r0_pob, 1)
    sim2 = path_sim3(r0_emp, r0_pob, 2)
    X = 0:iteraciones[1]
    aux2 = data.frame(X)
    aux2$infecciosos = 0
    
    out1 = read.csv(paste('../../datos_simulaciones/numero_infecciosos', sim1, '.csv', sep='')) %>%
        gather(tiempo, infecciosos, X0:X92) %>%
        mutate(tiempo = as.numeric(substr(tiempo, 2, 100))) %>%
        filter(tiempo == max(tiempo)) %>%
        select(X, infecciosos) %>%
        rbind(aux2) %>%
        group_by(X) %>%
        summarize(infecciosos = sum(infecciosos))%>%
        summarize(infecciosos = mean(infecciosos))
    
    out2 = read.csv(paste('../../datos_simulaciones/numero_infecciosos', sim2, '.csv', sep='')) %>%
        gather(tiempo, infecciosos, X0:X92) %>%
        mutate(tiempo = as.numeric(substr(tiempo, 2, 100))) %>%
        filter(tiempo == max(tiempo)) %>%
        select(X, infecciosos) %>%
        rbind(aux2) %>%
        group_by(X) %>%
        summarize(infecciosos = sum(infecciosos)) %>%
        summarize(infecciosos = mean(infecciosos))
    
    return(c(out1$infecciosos, out2$infecciosos))
}  
     
results = apply(df, 1, function(x) inf_total(as.character(x[1]), as.character(x[2]))) %>%
    t() %>%
    data.frame() %>%
    cbind(df2) %>%
    mutate(diff = X1 - X2, ratio = diff / X1, percentage = diff / 345 * 100)

results$semaforo = '#FFFF99'
results[results$ratio < 0.17, 'semaforo'] = 'green'
results[results$ratio > 0.24, 'semaforo'] = 'red'

results$semaforo2 = 'yellow'
results[results$percentage < 1.25, 'semaforo2'] = 'green'
results[results$percentage > 2.5, 'semaforo2'] = 'red'

results$Porcentaje = '1.25% - 2.5%'
results[results$percentage < 1.25, 'Porcentaje'] = '< 1.25%'
results[results$percentage > 2.5, 'Porcentaje'] = '> 2.5%'



results %>%
    ggplot(aes(x=r0_emp, y=r0_pob, color=diff)) +
        geom_point(size=10) +
        scale_color_gradient(low="green", high="red")

results %>%
    ggplot(aes(x=r0_emp, y=r0_pob, color=ratio)) +
    geom_point(size=10) +
    scale_color_gradient(low="green", high="red")

results %>%
    ggplot(aes(x=r0_emp, y=r0_pob)) +
    #geom_point(size=0.01) +
    theme_bw() +
    scale_y_continuous(breaks=seq(0.8,2,0.1)) +
    scale_x_continuous(breaks=seq(2,4,0.2))
    scale_color_gradien(low="green", high="red")

results %>%
    ggplot(aes(x=r0_emp, y=r0_pob, color=percentage)) +
    geom_point(size=10) +
    scale_color_gradient(low="green", high="red")

col <- as.character(results$semaforo2)
names(col) <- results$Porcentaje

results %>%
    mutate(Porcentaje = factor(Porcentaje, levels=c('< 1.25%', '1.25% - 2.5%', '> 2.5%'))) %>%
    ggplot(aes(x=r0_emp / 17.5 * 7.8, y=r0_pob, color=Porcentaje)) +
    theme_bw() +
    scale_y_continuous(breaks=seq(0.8,2,0.2)) +
    scale_x_continuous(breaks=seq(2/17.5 * 7.8,4/17.5 * 7.8,0.2/17.5 * 7.8), labels=round(seq(2/17.5 * 7.8,4/17.5 * 7.8,0.2/17.5 * 7.8), 2)) +
    scale_color_manual(values=col) +
    geom_point(size=10) #+
    #scale_color_gradient(low="green", high="red")











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

numero_observados = function(sim){
    X = 0:iteraciones[1]
    aux2 = data.frame(X)
    aux2$infecciosos = 0
    
    out = read.csv(paste('../../datos_simulaciones/numero_observado', sim, '.csv', sep='')) %>%
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

numero_infecciosos_mes = function(sim){
    X = 0:iteraciones[1]
    aux2 = data.frame(X)
    aux2$infecciosos = 0
    
    out = read.csv(paste('../../datos_simulaciones/numero_infecciosos', sim, '.csv', sep='')) %>%
        gather(tiempo, infecciosos, X0:X25) %>%
        mutate(tiempo = as.numeric(substr(tiempo, 2, 100))) %>%
        filter(tiempo == max(tiempo)) %>%
        select(X, infecciosos) %>%
        rbind(aux2) %>%
        group_by(X) %>%
        summarize(infecciosos = sum(infecciosos)) %>%
        mutate(Protocolo = nombre[n], Frecuencia = frec_test[n], R0 = r0[n], Sensibilidad = sens[n], PPS = ips[n])
    return(out)
}

numero_observados_mes = function(sim){
    X = 0:iteraciones[1]
    aux2 = data.frame(X)
    aux2$infecciosos = 0
    
    out = read.csv(paste('../../datos_simulaciones/numero_observado', sim, '.csv', sep='')) %>%
        gather(tiempo, infecciosos, X0:X25) %>%
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

numero_observados_cum = function(sim){
    X = 0:iteraciones[1]
    aux2 = data.frame(X)
    aux2$infecciosos = 0
    
    out = read.csv(paste('../../datos_simulaciones/numero_observado', sim, '.csv', sep='')) %>%
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

infecciosos_observados = function(sim){
    resultados = read.csv(paste('../../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    
    out = resultados %>%
        filter(estado_observado == 'infeccioso' & estado == 'infeccioso') %>%
        group_by(tiempo) %>%
        mutate(cantidad = sum(cantidad) / 1000) %>%
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
    
    
    n=1
    resultados = numero_observados(path_sim2(n))
    for(n in 2:length(alg)){
        aux = numero_observados(path_sim2(n))
        resultados = rbind(resultados, aux)
    }
    
    cv_it = resultados %>%
        group_by(Protocolo) %>%
        summarize(MEAN = mean(infecciosos), STD = sd(infecciosos), cv = MEAN / STD / sqrt(iteraciones[1])) 
    cv_it[order(cv_it$MEAN), ]
    
    n=2
    resultados = numero_observados_mes(path_sim2(n))
    
    resultados %>%
        filter(infecciosos < 2.5) %>%
        nrow()
    
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
       geom_text(aes(label=round(Dias_Promedio_Extraccion, digits=2)), position=position_dodge(width=0.9), vjust=-0.25) 
   p
   
   p = merge(cv_it, dias_infecciosos, by=c('Protocolo')) %>%
       mutate(Dias_Promedio_Extraccion = Dias_Inf / MEAN) %>%
       mutate(Protocolo = factor(Protocolo, levels=nombre[c(1,3,2)])) %>%
       ggplot(aes(x=Protocolo, y=2.9 * Dias_Promedio_Extraccion / 17.5, fill=Protocolo)) +
       geom_bar(stat='identity', position='dodge', color='black') +
       ggtitle(TeX('$R_e$ Ajustado (Dias Efectivos)')) +
       ylab('') + 
       xlab('') +
       #xlab(TeX('$R_0$')) +
       theme_bw() + 
       scale_fill_brewer(palette='Set3') +
       geom_text(aes(label=c('0.8 (5 dias)', '0.7 (4.1 dias)',  '1.3 (7.7 dias)')), position=position_dodge(width=0.9), vjust=-0.25) 
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
    
    resultados['Protocolo'] = 'Simulacion'
    
    
    aux = data.frame(tiempo=c(1, 79, 59, 63, 74, 74, 78, 67))
    aux = data.frame(tiempo=c(0,25))
    aux$Protocolo = 'Realidad'
    aux = aux[order(aux$tiempo), ]
    aux$infecciosos = c(2, 2)#1:nrow(aux)#c(2, 2)
    aux['tiempo'] = aux['dias'] * 24
    aux['infecciosos_sintomaticos'] = aux['infecciosos']
    aux['intervalo'] = 0
    
    p = resultados %>% 
        #filter(tiempo < 26 & tiempo > 0) %>%
        #mutate(infecciosos = ifelse(Protocolo == 'Sin AC', infecciosos - 1.25, infecciosos)) %>%
       #mutate(Protocolo = factor(Protocolo, levels=nombre[c(1,3,2)])) %>%
    #   rbind(aux) %>%
            ggplot(aes(x=tiempo, y=infecciosos, color=Protocolo)) +
                geom_point(size=3) +
                xlab("Días") + 
                ylab("Infectados (Acumulados)") +
                scale_y_continuous(breaks=seq(0,28,4), labels=seq(0,28,4)) +
                #geom_point(data=resultados[resultados$Protocolo == 'Realidad', ]) +
                #geom_line(data=resultados[resultados$Protocolo == 'Simulacion', ]) + 
                ggtitle('Número de Infectados Acumulados') #+
                #geom_ribbon(aes(ymin = infecciosos - intervalo, ymax = infecciosos + intervalo), alpha=0.2)
    p
    
    ggplot(df, aes(x, y))+
        geom_point(data=df[df$group==2, ])+
        geom_line(data=df[df$group==1, ])
}

{
    n=1
    resultados = infecciosos_observados(path_sim2(n))
    for(n in 2:length(alg)){
        aux = infecciosos_observados(path_sim2(n))
        resultados = rbind(resultados, aux)
    }
    #resultados['dias'] = resultados['tiempo'] / 24
    #resultados['infecciosos_sintomaticos'] = resultados['infecciosos'] / 2
    
    resultados['Protocolo'] = 'Simulacion'
    resultados
    
    aux = data.frame(tiempo=c(1, 79, 59, 63, 74, 74, 78, 67))
    aux = data.frame(tiempo=c(0,25))
    aux$Protocolo = 'Realidad'
    aux = aux[order(aux$tiempo), ]
    aux$infecciosos = c(2, 2)#1:nrow(aux)#c(2, 2)
    aux['tiempo'] = aux['dias'] * 24
    aux['infecciosos_sintomaticos'] = aux['infecciosos']
    aux['intervalo'] = 0
    
    p = resultados %>% 
        filter(tiempo < 26) %>%
        #mutate(infecciosos = ifelse(Protocolo == 'Sin AC', infecciosos - 1.25, infecciosos)) %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[c(1,3,2)])) %>%
        #rbind(aux) %>%
        ggplot(aes(x=tiempo, y=cantidad, color=Protocolo)) +
        geom_point(size=3) +
        xlab("Días") + 
        ylab("Infectados Sintomáticos Acumulados") +
        scale_y_continuous(breaks=seq(0,28,2), labels=seq(0,28,2)) +
        #geom_point(data=resultados[resultados$Protocolo == 'Realidad', ]) +
        #geom_line(data=resultados[resultados$Protocolo == 'Simulacion', ]) + 
        ggtitle('Número de Infectados Sintomáticos Acumulados') 
       # geom_ribbon(aes(ymin = infecciosos - intervalo, ymax = infecciosos + intervalo), alpha=0.2)
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
    
    p = resultados %>%
        filter(actividad == 'cuarentena') %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[rev(c(2,3, 4, 1))])) %>%
        mutate(cantidad = ifelse(Protocolo == 'AC 14 Dias', cantidad - 0.5, cantidad)) %>% 
        mutate(cantidad = ifelse(Protocolo == 'AC 1/Semana', cantidad - 1, cantidad)) %>% 
        mutate(cantidad = ifelse(Protocolo == 'AC 2/Semana', cantidad - 2, cantidad)) %>% 
        group_by(Protocolo) %>%
        summarize(mean_cuarentena = mean(cantidad/N)) %>%
        ggplot(aes(x=Protocolo, y=mean_cuarentena, fill=Protocolo)) +
            geom_bar(stat='identity', position='dodge', color='black') +
            ggtitle('Porcentaje de empleados en cuarentena (Promedio Diario)') +
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

###############################################################################





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

