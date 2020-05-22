
# Parametros simulacion
    
alg =           c('HacerNada', 'Bios', 'Bios', 'Bios', 'Bios')
                 # 'HacerNada', 'Bios', 'HacerNadaTurnos', 'HacerNadaCerrar')#
                  # 'HacerNada', 'Bios', 'HacerNadaTurnos')
                  #'HacerNadaPCR', 'BiosPCR', 'HacerNadaTurnosPCR') 
frec_test =     rep(c(0, 3, 3, 3, 3), 20) 
ctna_dur =      rep(14, 60)
ctna_inic =     rep(0, 12)
pob =           rep(100, 60)
r0 =            c(3, 3,3,3,3)#rep(1, 60)
tiempo =        rep(156, 60)
iteraciones =   rep(3000, 60)
fecha =         rep('14-05', 60)
p_inic =        c(0.0075, 0.0075,  0.0075, 0.0075, 0.0075,  0.0075, 0, 0, 0, 0, 0, 0)
adh =           c(1, 0.3, 0.5, 0.8, 1)
nombre =        rep(c('30%', '50%', '80%', '100%'), 20)
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

sumario_tabla = function(df, categorias=c('Grupo')){
    aux = df
    num_categorias = length(categorias)
    switch(num_categorias,
           {colnames(aux) = c(categorias[1], 'cantidad', 'Dias', 'it')
           aux = complete(aux, (!!sym(categorias[1])), it, Dias = full_seq(Dias, period = 1), fill = list(cantidad = 0))
           aux = aux %>% group_by((!!sym(categorias[1])), Dias, it) %>% summarize (Cantidad = sum(cantidad))
           aux = aux %>% group_by((!!sym(categorias[1])), Dias) %>% summarize(Poblacion = mean(Cantidad),
                                                                              std = sd(Cantidad))
           },
           {cat1 = sym(categorias[1])
           cat2 = sym(categorias[2])
           colnames(aux) = c(categorias[1], categorias[2], 'cantidad', 'Dias', 'it')
           
           aux = complete(aux, !!cat1, !!cat2, it, Dias = full_seq(Dias, period = 1), fill = list(cantidad = 0))
           aux = aux %>% group_by(!!cat1, !!cat2, Dias, it) %>% summarize (Cantidad = sum(cantidad))
           aux = aux %>% group_by(!!cat1, !!cat2, Dias) %>% summarize(Poblacion = mean(Cantidad),
                                                                      std = sd(Cantidad))
           })
    
    aux$Dias = as.numeric(aux$Dias)
    return(aux)
}

################################################################################
# Trayectorias sin infectados
{
n=1
sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')

resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
resultados$X = NULL
infecciones = resultados %>%
    filter(estado == 'infeccioso') %>%
    group_by(it, tiempo) %>%
    summarize(num_inf = sum(cantidad)) %>%
    group_by(tiempo) %>%
    summarize(inf_1 = sum(num_inf == 1),
              inf_2 = sum(num_inf == 2),
              inf_3 = sum(num_inf == 3),
              inf_4 = sum(num_inf == 4),
              inf_5 = sum(num_inf == 5),
              inf_6_mas = sum(num_inf >= 6),
              inf_1_mas = sum(num_inf >= 1))
infecciones$inf_0 = iteraciones[n] - infecciones$inf_1_mas
infecciones$Protocolo = nombre[n]
infecciones$CI = ctna_inic[n] == 14
infecciones$Inf_Inic = percent(p_inic[n])

for(n in 2:length(alg)){
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
    resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    aux = resultados %>%
            filter(estado == 'infeccioso') %>%
            group_by(it, tiempo) %>%
            summarize(num_inf = sum(cantidad)) %>%
            group_by(tiempo) %>%
            summarize(inf_1 = sum(num_inf == 1),
                      inf_2 = sum(num_inf == 2),
                      inf_3 = sum(num_inf == 3),
                      inf_4 = sum(num_inf == 4),
                      inf_5 = sum(num_inf == 5),
                      inf_6_mas = sum(num_inf >= 6),
                      inf_1_mas = sum(num_inf >= 1))
    aux$inf_0 = iteraciones[n] - aux$inf_1_mas
    aux$Protocolo = nombre[n]
    aux$CI = ctna_inic[n] == 14
    aux$Inf_Inic = percent(p_inic[n])
    colnames(aux) = colnames(infecciones)
    infecciones = rbind(infecciones, aux)
}

infecciones %>%
    ggplot(aes(x=tiempo, y=percent(inf_0 / 1000))) + 
        geom_line(aes(color=Protocolo)) +
        facet_wrap(~factor(Inf_Inic))
}    
################################################################################
{
n=1
sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')

resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
resultados$X = NULL

p1=resultados %>%
    group_by(tiempo, estado) %>%
    summarize(poblacion = sum(cantidad)) %>%
    ggplot(aes(x=tiempo, y=poblacion, color=estado)) + 
        geom_line()
n=2
sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')

resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
resultados$X = NULL

p2=resultados %>%
    group_by(tiempo, estado) %>%
    summarize(poblacion = sum(cantidad)) %>%
    ggplot(aes(x=tiempo, y=poblacion, color=estado)) + 
    geom_line()
n=3
sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')

resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
resultados$X = NULL

p3=resultados %>%
    group_by(tiempo, estado) %>%
    summarize(poblacion = sum(cantidad)) %>%
    ggplot(aes(x=tiempo, y=poblacion, color=estado)) + 
    geom_line()
}
################################################################################
# Primer Infectado
{
n=1
sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')

resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
resultados$X = NULL

primer_infectado = resultados %>%
    filter(estado == 'infeccioso' & actividad == 'trabajo') %>%
    select(cantidad, tiempo, it) %>%
    complete(it=0:999, tiempo=0:156, fill = list(cantidad = 0)) %>%
    filter(cantidad > 0) %>%
    group_by(it) %>%
    summarize(Dia = first(tiempo)) %>% 
    group_by(Dia) %>%
    summarize(Num_It = NROW(it)) %>%
    complete(Dia = 0:154, fill=list(Num_It = 0)) %>%
    mutate(Prob_0_Inf = 1 - (cumsum(Num_It) / 1000),
           Protocolo = nombre[n],
           PCR = pcr[n])


for(n in 2:length(alg)){
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
    resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    aux = resultados %>%
        filter(estado == 'infeccioso' & cantidad > 0 & actividad == 'trabajo') %>%
        select(cantidad, tiempo, it) %>%
        complete(it=0:999, tiempo=0:156, fill = list(cantidad = 0)) %>%
        filter(cantidad > 0) %>%
        group_by(it) %>%
        summarize(Dia = first(tiempo)) %>% 
        group_by(Dia) %>%
        summarize(Num_It = NROW(it)) %>%
        complete(Dia = 0:154, fill=list(Num_It = 0)) %>%
        mutate(Prob_0_Inf = 1 - (cumsum(Num_It) / 1000),
               Protocolo = nombre[n],
               PCR = pcr[n])

    primer_infectado = rbind(primer_infectado, aux)
}

p = primer_infectado %>%
    #filter(Protocolo != 'Turnos') %>%
    ggplot(aes(x=Dia, y=Prob_0_Inf,  color=Protocolo)) + 
    geom_line() + 
    facet_wrap(~factor(PCR)) + 
    ggtitle('Probabilidad de que ning?n trabajador se haya contagiado - R0(emp) = 1') +
    ylab('')
p
}
ggsave(paste('plots/senama_24-04/Probabilidad primer contagio.pdf', sep=''), p)

################################################################################
# Segundo Infectado
{
n=1
sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')

resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
resultados$X = NULL

segundo_infectado = resultados %>%
    filter(estado == 'infeccioso' & cantidad > 1 & actividad == 'trabajo') %>%
    group_by(it) %>%
    summarize(Dia = first(tiempo)) %>% 
    group_by(Dia) %>%
    summarize(Num_It = NROW(it)) %>%
    complete(Dia = 0:154, fill=list(Num_It = 0))
segundo_infectado$prob_1_inf = 1 - (cumsum(segundo_infectado$Num_It) / 1000)
segundo_infectado$Protocolo = nombre[n]
segundo_infectado$PCR = pcr[n]

for(n in 2:length(alg)){
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
    resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    aux = resultados %>%
        filter(estado == 'infeccioso'& cantidad > 1 & actividad == 'trabajo') %>%
        group_by(it) %>%
        summarize(Dia = first(tiempo)) %>% 
        group_by(Dia) %>%
        summarize(Num_It = NROW(it)) %>%
        complete(Dia = 0:154, fill=list(Num_It = 0))
    aux$prob_1_inf = 1 - (cumsum(aux$Num_It) / 1000)
    aux$Protocolo = nombre[n]
    aux$PCR = pcr[n]
    segundo_infectado = rbind(segundo_infectado, aux)
}

p = segundo_infectado %>%
    mutate(Protocolo = factor(Protocolo, levels=nombre[c(3,2, 1)])) %>%
    ggplot(aes(x=Dia, y=prob_1_inf,  color=Protocolo)) + 
    geom_line() + 
    facet_wrap(~PCR) + 
    ggtitle('Probabilidad de que menos de 1 trabajador se haya contagiado - R0(emp) = 1') +
    ylab('') 
p
}
ggsave(paste('plots/senama_24-04/Probabilidad segundo contagio.pdf', sep=''), p)

################################################################################

# Coeficiente de Variacion
{
    n=1
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , adh[n], '_', fecha[n] , sep='')
    
    X = 0:2999
    aux2 = data.frame(X)
    aux2$infecciosos = 0
    
    resultados = read.csv(paste('../../datos_simulaciones/numero_infecciosos_sanpedro', sim, '.csv', sep='')) %>%
        gather(tiempo, infecciosos, X0:X156) %>%
        mutate(tiempo = as.numeric(substr(tiempo, 2, 100))) %>%
        filter(tiempo == max(tiempo)) %>%
        select(X, infecciosos) %>%
        rbind(aux2) %>%
        group_by(X) %>%
        summarize(infecciosos = sum(infecciosos)) %>%
        mutate(Protocolo = nombre[n], R0 = r0[n], Adh = adh[n])
    
    for(n in 2:length(alg)){
        sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , adh[n], '_', fecha[n] , sep='')
        
        X = 0:2999
        aux2 = data.frame(X)
        aux2$infecciosos = 0
        
        aux = read.csv(paste('../../datos_simulaciones/numero_infecciosos_sanpedro', sim, '.csv', sep='')) %>%
            gather(tiempo, infecciosos, X0:X156) %>%
            mutate(tiempo = as.numeric(substr(tiempo, 2, 100))) %>%
            filter(tiempo == max(tiempo)) %>%
            select(X, infecciosos) %>%
            rbind(aux2) %>%
            group_by(X) %>%
            summarize(infecciosos = sum(infecciosos)) %>%
            mutate(Protocolo = nombre[n], R0 = r0[n], Adh = adh[n])
        
        resultados = rbind(resultados, aux)
    }
    
    cv_it = resultados %>%
        group_by(Protocolo) %>%
        summarize(MEAN = mean(infecciosos), STD = sd(infecciosos), cv = MEAN / STD / sqrt(iteraciones[1])) 
    
    cv_it %>% mutate(cv = scales::label_percent(accuracy=0.01)(cv)) %>% spread(key=Protocolo, value=cv) %>% xtable()
    
}

{
n=1
sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')

resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
resultados$X = NULL

tot_infecciones = resultados %>%
    filter(estado == 'infeccioso' & tiempo %% 22 == 0) %>%
    group_by(it) %>%
    summarize(num_inf = sum(cantidad)) %>%
    complete(it=0:999, fill=list(num_inf = 0)) %>%
    summarize(Total_Inf = mean(num_inf), STD = sd(num_inf)) %>%
    mutate(Protocolo = nombre[n], 
           PCR = pcr[n],
           R0 = r0[n],
           CV = STD / Total_Inf / sqrt(iteraciones[n]))

for(n in 2:length(alg)){
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
    
    resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    
    aux = resultados %>%
        filter(estado == 'infeccioso' & tiempo %% 22 == 0) %>%
        group_by(it) %>%
        summarize(num_inf = sum(cantidad)) %>%
        complete(it=0:999, fill=list(num_inf = 0)) %>%
        summarize(Total_Inf = mean(num_inf), STD = sd(num_inf)) %>%
        mutate(Protocolo = nombre[n], 
               PCR = pcr[n],
               R0 = r0[n],
               CV = STD / Total_Inf / sqrt(iteraciones[n]))
    
    tot_infecciones = rbind(tot_infecciones, aux)
}

tot_infecciones %>%
    select(Protocolo, Total_Inf, STD, CV) %>%
    formattable(digits=2)

p = tot_infecciones %>%
    mutate(Protocolo = factor(Protocolo, levels=nombre[c(3,2,4,1)])) %>%
    ggplot(aes(x=Protocolo, y=Total_Inf / pob[1], fill=Protocolo)) +
    geom_bar(stat='identity', position='dodge', color='black') +
    ggtitle('Porcentaje de Enfermos Totales en 5 Meses') +
    ylab('') + 
    xlab('') +
    theme_bw() + 
    scale_y_continuous(labels = scales::percent) +
    scale_fill_brewer(palette='Set3') +
    geom_text(aes(label=percent(Total_Inf / pob[1], digits=1)), position=position_dodge(width=0.9), vjust=-0.25) 
p
}
ggsave(paste('../plots/resultados_04-05/Porcentaje infecciosos totales.pdf', sep=''), p)

################################################################################

# Numero de Dias infecciosos en el trabajo
{
n=1
sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_', adh[n], '_' , fecha[n] , sep='')

resultados = read.csv(paste('../../datos_simulaciones/resultados_sanpedro', sim, '.csv', sep='')) 
resultados$X = NULL

dias_infecciosos = resultados %>%
    filter(estado == 'infeccioso' & actividad == 'trabajo') %>%
    summarize(Dias_Inf = sum(cantidad) / iteraciones[n]) %>% 
    mutate(Protocolo = nombre[n],
           PCR = pcr[n],
           R0 = r0[n],
           Adh = 'Hacer Nada')


for(n in 2:length(alg)){
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_', adh[n], '_' , fecha[n] , sep='')
    resultados = read.csv(paste('../../datos_simulaciones/resultados_sanpedro', sim, '.csv', sep='')) 
    resultados$X = NULL
    aux = resultados %>%
        filter(estado == 'infeccioso' & actividad == 'trabajo') %>%
        summarize(Dias_Inf = sum(cantidad) / iteraciones[n]) %>% 
        mutate(Protocolo = nombre[n],
               PCR = pcr[n],
               R0 = r0[n],
               Adh = adh[n])
    
    dias_infecciosos = rbind(dias_infecciosos, aux)
}

p1 = dias_infecciosos %>%
    mutate(Protocolo = factor(Protocolo, levels=nombre[c(3,2,4,1)])) %>%
    ggplot(aes(x=as.factor(Adh), y=Dias_Inf, fill=as.factor(Adh))) +
    geom_bar(stat='identity', position='dodge', color='black') +
    ggtitle('Dias-Hombre infectados (5 Meses, 100 Trabajadores, R0(emp)=3, R0=1.6, p_i=0.75%)') +
    ylab('') + 
    xlab('') +
    #xlab(TeX("$R_0$")) + 
    theme_bw() + 
    scale_fill_brewer(palette='Set3') +
    geom_text(aes(label=round(Dias_Inf, digits=1)), position=position_dodge(width=0.9), vjust=-0.25) 
p1

p = merge(cv_it, dias_infecciosos, by=c('Protocolo')) %>%
    mutate(Dias_Promedio_Extraccion = Dias_Inf / MEAN) %>%
    #mutate(Adh2 = factor(Adh, levels=c('Hacer Nada', '30%', '50%', '80%', '100%'))) %>%
    ggplot(aes(x=as.factor(Adh), y=Dias_Promedio_Extraccion, fill=as.factor(Adh))) +
    geom_bar(stat='identity', position='dodge', color='black') +
    ggtitle('Número de días promedio entre infección y cuarentena (5 Meses, 100 Trabajadores, R0(emp)=3, R0=1.6, p_i=0.75%)') +
    ylab('') + 
    xlab('') +
    #xlab(TeX('$R_0$')) +
    theme_bw() + 
    scale_fill_brewer(palette='Set3') +
    geom_text(aes(label=round(Dias_Promedio_Extraccion, digits=1)), position=position_dodge(width=0.9), vjust=-0.25) 
p
}
ggsave(paste('../../plots/sanpedro_15-05/Dias promedio entre infeccion y cuarentena.pdf', sep=''), p)
ggsave(paste('../../plots/sanpedro_15-05/Dias-hombre Infectados en Trabajo.pdf', sep=''), p1)

################################################################################
# Fraccion de tiempo sin empleados enfermos
{
    n=1
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , adh[n], '_', fecha[n] , sep='')
    
    resultados = read.csv(paste('../../datos_simulaciones/resultados_sanpedro', sim, '.csv', sep='')) 
    resultados$X = NULL
    
    fraccion_tiempo = resultados %>%
        filter(estado == 'infeccioso' & actividad == 'trabajo') %>%
        group_by(it) %>%
        summarize(inf = NROW(tiempo) / 156) %>%
        summarize(fraccion_tiempo = 1 - (sum(inf) / iteraciones[n])) %>%
        mutate(Protocolo = nombre[n], PCR = pcr[n], R0 = r0[n], Adh='Hacer Nada')
    
    for(n in 2:length(alg)){
        sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_', adh[n], '_' , fecha[n] , sep='')
        resultados = read.csv(paste('../../datos_simulaciones/resultados_sanpedro', sim, '.csv', sep='')) 
        resultados$X = NULL
        
        aux =  resultados %>%
            filter(estado == 'infeccioso' & actividad == 'trabajo') %>%
            group_by(it) %>%
            summarize(inf = NROW(tiempo) / 156) %>%
            summarize(fraccion_tiempo = 1 - (sum(inf) / iteraciones[n])) %>%
            mutate(Protocolo = nombre[n], PCR = pcr[n], R0 = r0[n], Adh=adh[n])
        
        fraccion_tiempo = rbind(fraccion_tiempo, aux)
    }
    
p = fraccion_tiempo %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[c(3,2,4,1)])) %>%
        ggplot(aes(x=as.factor(Adh), y=fraccion_tiempo, fill=as.factor(Adh))) +
        geom_bar(stat='identity', position='dodge', color='black') +
        ggtitle('Fracción de días sin trabajadores enfermos (5 Meses, 100 Trabajadores, R0(emp)=3, R0=1.6, p_i=0.75%)') +
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
ggsave(paste('../../plots/sanpedro_15-05/Fraccion dias sin trabajadores infectados.pdf', sep=''), p)

################################################################################
# Poblacion trajando
{
    n=1
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_', adh[n], '_' , fecha[n] , sep='')
    resultados = read.csv(paste('../../datos_simulaciones/resultados_sanpedro', sim, '.csv', sep='')) 
    resultados$X = NULL
    
    trabajando = resultados %>%
        filter(actividad == 'trabajo') %>%
        group_by(it, tiempo) %>%
        summarize(inf = sum(cantidad)) %>%
        group_by(tiempo) %>%
        summarize(Trabajando = sum(inf) / iteraciones[n] / pob[n], desv = sd(inf)) %>%
        mutate(Protocolo = nombre[n], PCR = pcr[n], Adh='Hacer Nada')
    
    for(n in 2:length(alg)){
        sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_', adh[n], '_' , fecha[n] , sep='')
        resultados = read.csv(paste('../../datos_simulaciones/resultados_sanpedro', sim, '.csv', sep='')) 
        resultados$X = NULL
        
        aux =  resultados %>%
            filter(actividad == 'trabajo') %>%
            group_by(it, tiempo) %>%
            summarize(inf = sum(cantidad)) %>%
            group_by(tiempo) %>%
            summarize(Trabajando = sum(inf) / iteraciones[n] / pob[n], desv = sd(inf)) %>%
            mutate(Protocolo = nombre[n], PCR = pcr[n], Adh=adh[n])
        
        trabajando = rbind(trabajando, aux)
    }
    
    p = trabajando %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[rev(c(3,2, 1))])) %>%
        ggplot(aes(x=tiempo, y=Trabajando, color=as.factor(Adh))) +
        geom_line() +
        ggtitle('Porcentaje de Trabajadores Activos') +
        ylab('') + 
        xlab('Dias') + 
        #facet_wrap(~factor(R0)) + 
        scale_y_continuous(labels = scales::percent) 
    p
    
    a = trabajando %>%
        group_by(Adh) %>%
        summarize(Trabajando = mean(Trabajando)) %>%
        merge(cv_it %>%
                  select(Adh, MEAN), by='Adh') %>% 
        mutate(Working = scales::label_percent(accuracy=0.1)(Trabajando),
               Total_Infected = scales::label_percent(accuracy=0.1)(MEAN / 100)) %>%
        select(Protocolo, Total_Infected, Working) %>%
        arrange(Total_Infected)# %>%
        #rename(Adherencia = Protocolo)
    
        xtable()
            # formattable(digits=0)
}
ggsave(paste('../../plots/sanpedro_15-05/Porcentaje de trabajadores.pdf', sep=''), p)

################################################################################
# Poblacion infectada trabajando
{
    n=1
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
    
    resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    
    tiempo = rep(0:156, 3000)
    it = rep(0:2999, each=157)
    aux2 = data.frame(tiempo, it)
    aux2$actividad = 'trabajo'
    aux2$estado = 'infeccioso'
    aux2$cantidad = 0
    
    trabajando_inf = resultados %>%
        select(it, tiempo, estado, actividad, cantidad) %>%
        filter(actividad == 'trabajo') %>%
        rbind(aux2) %>%
        group_by(it, tiempo) %>%
        mutate(trabajando = sum(cantidad)) %>%
        filter(estado == 'infeccioso') %>%
        group_by(tiempo, it) %>%
        summarize(cantidad = sum(cantidad), trabajando = first(trabajando)) %>%
        mutate(trabajando = max(trabajando, 1)) %>%
        group_by(tiempo) %>%
        summarize(Inf_Trab = mean(cantidad / trabajando), 
                  desv = sd(cantidad / trabajando), 
                  q25 = quantile(cantidad / trabajando, 0.25),
                  q75 = quantile(cantidad / trabajando, 0.75),
                  q05 = quantile(cantidad / trabajando, 0.05),
                  q95 = quantile(cantidad / trabajando, 0.95),
                  q10 = quantile(cantidad / trabajando, 0.1),
                  q90 = quantile(cantidad / trabajando, 0.9)) %>%
        mutate(Protocolo = nombre[n], PCR = pcr[n], R0 = r0[n])
    
    for(n in 2:length(alg)){
        sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
        
        resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
        resultados$X = NULL
        
        aux =  resultados %>%
            filter(actividad == 'trabajo') %>%
            select(it, tiempo, estado, actividad, cantidad) %>%
            rbind(aux2) %>%
            group_by(it, tiempo) %>%
            mutate(trabajando = sum(cantidad)) %>%
            filter(estado == 'infeccioso') %>%
            group_by(tiempo, it) %>%
            summarize(cantidad = sum(cantidad), trabajando = first(trabajando)) %>%
            mutate(trabajando = max(trabajando, 1)) %>%
            group_by(tiempo) %>%
            summarize(Inf_Trab = mean(cantidad / trabajando), 
                      desv = sd(cantidad / trabajando), 
                      q25 = quantile(cantidad / trabajando, 0.25),
                      q75 = quantile(cantidad / trabajando, 0.75),
                      q05 = quantile(cantidad / trabajando, 0.05),
                      q95 = quantile(cantidad / trabajando, 0.95),
                      q10 = quantile(cantidad / trabajando, 0.1),
                      q90 = quantile(cantidad / trabajando, 0.9)) %>%
            mutate(Protocolo = nombre[n], PCR = pcr[n], R0 = r0[n])
        
        trabajando_inf = rbind(trabajando_inf, aux)
    }
    
    trabajando_inf[trabajando_inf$tiempo == 0, 'Inf_Trab'] = 0.01
    
    p = trabajando_inf %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[rev(c(2, 3, 4, 1))])) %>%
        ggplot(aes(x=tiempo, y=Inf_Trab * 100, color=Protocolo)) +
        geom_line(size=1) +
        ggtitle('Number of infected working at a given day at as a percentage of total workers') +
        ylab('') + 
        xlab('Days') +
        #geom_ribbon(aes(ymin=100*(Inf_Trab - desv/8), ymax=100*(Inf_Trab + desv/8)),alpha=0.3) +
        theme_bw() +
        #facet_wrap(~factor(R0)) + 
        scale_y_continuous(labels = scales::percent) 
    p
}
ggsave(paste('../plots/resultados_04-05/Infectados en el trabajo cuarto desv iter=5000.pdf', sep=''), p)
################################################################################
# Poblacion infectada trabajando (SOLO EL PROMEDIO)
{
    n=1
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_', adh[n], '_' , fecha[n] , sep='')
    resultados = read.csv(paste('../../datos_simulaciones/resultados_sanpedro', sim, '.csv', sep='')) 
    resultados$X = NULL
    
    trabajando_inf = resultados %>%
        select(it, tiempo, estado, actividad, cantidad) %>%
        filter(actividad == 'trabajo') %>%
        group_by(it, tiempo) %>%
        mutate(trabajando = sum(cantidad)) %>%
        filter(estado == 'infeccioso') %>%
        group_by(tiempo, it) %>%
        summarize(cantidad = sum(cantidad), trabajando = first(trabajando)) %>%
        mutate(trabajando = max(trabajando, 1)) %>%
        group_by(tiempo) %>%
        summarize(Inf_Trab = sum(cantidad / trabajando) / iteraciones[n]) %>%
        mutate(Protocolo = nombre[n], PCR = pcr[n], R0 = r0[n], Adh= 'Hacer Nada')
    
    for(n in 2:length(alg)){
        sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_', adh[n], '_' , fecha[n] , sep='')
        resultados = read.csv(paste('../../datos_simulaciones/resultados_sanpedro', sim, '.csv', sep='')) 
        resultados$X = NULL
        
        aux = resultados %>%
            select(it, tiempo, estado, actividad, cantidad) %>%
            filter(actividad == 'trabajo') %>%
            group_by(it, tiempo) %>%
            mutate(trabajando = sum(cantidad)) %>%
            filter(estado == 'infeccioso') %>%
            group_by(tiempo, it) %>%
            summarize(cantidad = sum(cantidad), trabajando = first(trabajando)) %>%
            mutate(trabajando = max(trabajando, 1)) %>%
            group_by(tiempo) %>%
            summarize(Inf_Trab = sum(cantidad / trabajando) / iteraciones[n]) %>%
            mutate(Protocolo = nombre[n], PCR = pcr[n], R0 = r0[n], Adh=adh[n])
        
        trabajando_inf = rbind(trabajando_inf, aux)
    }
    
    trabajando_inf[trabajando_inf$tiempo == 0, 'Inf_Trab'] = 0.0075
    
    p = trabajando_inf %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[rev(c(2, 4, 3, 1))])) %>%
        ggplot(aes(x=tiempo, y=Inf_Trab, color=as.factor(Adh))) +
        geom_line(size=1) +
        ggtitle('Porcentaje de trabajadores trabajando que están infecciosos (5 Meses, 100 Trabajadores, R0(emp)=3, R0=1.6, p_i=0.75%) ') +
        ylab('') + 
        xlab('Days') +
        #geom_ribbon(aes(ymin=100*(Inf_Trab - desv/8), ymax=100*(Inf_Trab + desv/8)),alpha=0.3) +
        theme_bw() +
        #facet_wrap(~factor(R0)) + 
        scale_y_continuous(labels = scales::percent) 
    p
}
ggsave(paste('../../plots/sanpedro_15-05/Infectados en el trabajo.pdf', sep=''), p)
################################################################################
# Poblacion infectada trabajando
{
    n=1
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
    
    resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) %>%
        filter(tiempo %in% c(60, 90, 120,150))
        
    resultados$X = NULL
    
    tiempo = rep(c(60,90,120,150), iteraciones[n])
    it = rep(0:(iteraciones[n]-1), each=4)
    aux2 = data.frame(tiempo, it)
    aux2$actividad = 'trabajo'
    aux2$estado = 'infeccioso'
    aux2$cantidad = 0
    
    trabajando_inf = resultados %>%
        select(it, tiempo, estado, actividad, cantidad) %>%
        filter(actividad == 'trabajo') %>%
        rbind(aux2) %>%
        group_by(it, tiempo) %>%
        mutate(trabajando = sum(cantidad)) %>%
        filter(estado == 'infeccioso') %>%
        group_by(tiempo, it) %>%
        summarize(cantidad = sum(cantidad), trabajando = first(trabajando)) %>%
        mutate(trabajando = max(trabajando, 1), Inf_Trab = cantidad / trabajando) %>%
        # group_by(tiempo) %>%
        # summarize(Inf_Trab = mean(cantidad / trabajando), 
        #            desv = sd(cantidad / trabajando), 
        #           # q25 = quantile(cantidad / trabajando, 0.25),
        #           # q75 = quantile(cantidad / trabajando, 0.75),
        #           # q05 = quantile(cantidad / trabajando, 0.05),
        #           # q95 = quantile(cantidad / trabajando, 0.95),
        #           # q10 = quantile(cantidad / trabajando, 0.1),
        #           # q90 = quantile(cantidad / trabajando, 0.9)
        #           ) %>%
        mutate(Protocolo = nombre[n], PCR = pcr[n], R0 = r0[n])
    
    for(n in 2:length(alg)){
        sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
        
        resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) %>%
            filter(tiempo %in% c(60, 90, 120,150))
        resultados$X = NULL
        
        aux =  resultados %>%
            filter(actividad == 'trabajo') %>%
            select(it, tiempo, estado, actividad, cantidad) %>%
            rbind(aux2) %>%
            group_by(it, tiempo) %>%
            mutate(trabajando = sum(cantidad)) %>%
            filter(estado == 'infeccioso') %>%
            group_by(tiempo, it) %>%
            summarize(cantidad = sum(cantidad), trabajando = first(trabajando)) %>%
            mutate(trabajando = max(trabajando, 1), Inf_Trab = cantidad / trabajando) %>%
            # group_by(tiempo) %>%
            # summarize(Inf_Trab = mean(cantidad / trabajando), 
            #           desv = sd(cantidad / trabajando), 
            #           # q25 = quantile(cantidad / trabajando, 0.25),
            #           # q75 = quantile(cantidad / trabajando, 0.75),
            #           # q05 = quantile(cantidad / trabajando, 0.05),
            #           # q95 = quantile(cantidad / trabajando, 0.95),
            #           # q10 = quantile(cantidad / trabajando, 0.1),
            #           # q90 = quantile(cantidad / trabajando, 0.9)
            #           ) %>%
            mutate(Protocolo = nombre[n], PCR = pcr[n], R0 = r0[n])
        
        trabajando_inf = rbind(trabajando_inf, aux)
    }
    
    trabajando_inf[trabajando_inf$tiempo == 0, 'Inf_Trab'] = 0.01
    
    trabajando_inf %>%
        filter(tiempo %in% c(60, 90, 120,150)) %>%
        group_by(Protocolo, tiempo, Inf_Trab) %>%
        summarize(count = n()) %>%
        group_by(Protocolo, tiempo) %>%
        arrange(Inf_Trab) %>%
        mutate(cumulative = cumsum(count)) %>%
        mutate(cumulative = cumulative / max(cumulative)) %>%
        ungroup %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[rev(c(2, 3, 4, 1))])) %>%
        ggplot(aes(x=Inf_Trab, y=cumulative, color=Protocolo)) + 
            geom_line(size=1) +
            #geom_histogram(aes(y=count), binwidth=0.001,  position='dodge') +
        ylab('Cumulative Density Function') + 
        xlab('Infected as a Percentage of Individuals at Work') +
            #xlim(c(0,0.16)) +
            theme_bw() +
            scale_fill_brewer(palette='Dark2') +
            theme(legend.title = element_blank()) +
            facet_wrap(~factor(tiempo)) +
            scale_x_continuous(labels = scales::percent, limits=c(0,0.16))
    
    table = trabajando_inf %>%
        group_by(Protocolo, tiempo) %>%
        summarize(Mean = scales::label_percent(accuracy=0.01)(mean(Inf_Trab)), SD = scales::scientific(sd(Inf_Trab), num=3)) %>%
        gather(stat, value, Mean:SD) %>%
        rename(Days = tiempo, Protocol = Protocolo) %>%
        arrange(Protocol, Days) %>% 
        #unite(Stats, Mean, SD) %>%
        spread(key=Days, value=value)
    table %>% xtable()
    
    trabajando_inf %>%
        group_by(Protocolo, tiempo) %>%
        summarize(Mean = mean(Inf_Trab), SD = sd(Inf_Trab)) %>%
        gather(stat, value, Mean:SD) %>%
        rename(Days = tiempo, Protocol = Protocolo) %>%
        arrange(Protocol, Days) %>% 
        #unite(Stats, Mean, SD) %>%
        spread(key=Days, value=value)
        
    p = trabajando_inf %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[rev(c(1,4,2,3))])) %>%
        ggplot(aes(x=scales::label_percent()(Inf_Trab), color=factor(Protocolo))) +
        #geom_histogram(aes(y=..density..), binwidth=0.001,  position='dodge') +
        #geom_step(aes(len=len,y=..y.. * len),stat="ecdf") +
        geom_density(alpha=0.3, size=1) +
        #ggtitle('Infectados Total al día por cada 100 Trabajadores ') +
        ylab('Cumulative Density Function') + 
        xlab('Infected as a Percentage of Individuals at Work') +
        # lim(c(0,60)) +
        theme_bw() +
        scale_fill_brewer(palette='Dark2') +
        theme(legend.title = element_blank()) +
        facet_wrap(~factor(tiempo)) 
    p
    
    p = trabajando_inf %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[rev(c(2, 3, 4, 1))])) %>%
        ggplot(aes(x=tiempo, y=Inf_Trab * 100, color=Protocolo)) +
        geom_line(size=1) +
        ggtitle('Trabajadores Infectados al día por cada 100 Trabajadores') +
        ylab('') + 
        xlab('Días') +
        geom_ribbon(aes(ymin=100*(Inf_Trab - desv/8), ymax=100*(Inf_Trab + desv/8)),alpha=0.3) +
        theme_bw()
    #facet_wrap(~factor(R0)) + 
    #scale_y_continuous(labels = scales::percent) 
    p
}
ggsave(paste('../plots/resultados_04-05/Infectados en el trabajo cuarto desv iter=5000.pdf', sep=''), p)

################################################################################
# Numero de PCR

{
    n=7
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
    
    resultados = read.csv(paste('../datos_simulaciones/numero_pcr', sim, '.csv', sep='')) %>%
        mutate(Protocolo = nombre[n], PCR = pcr[n])
    
    for(n in 8:length(alg)){
        sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
        
        aux = read.csv(paste('../datos_simulaciones/numero_pcr', sim, '.csv', sep='')) %>%
            mutate(Protocolo = nombre[n], PCR = pcr[n])
        resultados = rbind(resultados, aux)
    }
    
    p = resultados %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[c(3,2,1)])) %>%
        group_by(Protocolo) %>%
        summarize(cantidad = last(cantidad) / 1000) %>%
        ggplot(aes(x=factor(Protocolo), y=cantidad)) +
        geom_bar(stat='identity', position='dodge', color='black') +
        ggtitle('N?mero Test PCR - R0(emp) = 1') +
        ylab('') +
        xlab('') + 
        scale_fill_brewer(palette='Set3') + 
        #scale_fill_viridis(option='viridis', discrete=TRUE) + 
        geom_text(aes(label=round(cantidad, 0)), position=position_dodge(width=0.9), vjust=-0.25) +
        theme_bw()
    p
}
ggsave(paste('plots/senama_24-04/Numero Test PCR.pdf', sep=''), p)
################################################################################

# Numero de Dias infecciosos en el trabajo
{
    n=1
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
    
    resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    
    dias_infecciosos = resultados %>%
        filter(estado == 'infeccioso' & actividad == 'trabajo') %>%
        complete(tiempo=0:156, it=0:999, fill=list(cantidad=0)) %>%
        group_by(it) %>%
        summarize(Dias_Inf = sum(cantidad)) %>% 
        mutate(Protocolo = nombre[n],
               Inf_Inic = percent(p_inic[n]))
    
    
    for(n in 2:length(alg)){
        sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
        resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
        resultados$X = NULL
        aux =  resultados %>%
            filter(estado == 'infeccioso' & actividad == 'trabajo') %>%
            complete(tiempo=0:156, it=0:999, fill=list(cantidad=0)) %>%
            group_by(it) %>%
            summarize(Dias_Inf = sum(cantidad)) %>% 
            mutate(Protocolo = nombre[n],
                   Inf_Inic = percent(p_inic[n]))
        
        dias_infecciosos = rbind(dias_infecciosos, aux)
    }
    
    p = dias_infecciosos %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[c(3,2,1)])) %>%
        ggplot(aes(x=factor(Inf_Inic), y=Dias_Inf, fill=Protocolo)) +
        geom_boxplot(outlier.shape=NA) +
        ggtitle('Dias-Hombre Infectados en el Trabajo') +
        ylab('') + 
        xlab('Porcentaje Inicial de la Poblaci?n Infectada') + 
        theme_bw() + 
        scale_fill_brewer(palette='Set3') +
        coord_cartesian(ylim = c(0, 50))
    
    ylim1 = boxplot.stats(df$y)$stats[c(1, 5)]
    
    # scale y limits based on ylim1
    p1 = p0 + coord_cartesian(ylim = ylim1*1.05)
    p
}
#ggsave(paste('plots/senama_senama_23-04/Dias-hombre Infectados en Trabajo - Boxplot.pdf', sep=''), p)

################################################################################
# Poblacion infectada trabajando
{
    n=1
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
    
    resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    
    trabajando_inf = resultados %>%
        filter(actividad == 'trabajo') %>%
        group_by(it, tiempo) %>%
        mutate(trabajando = sum(cantidad)) %>%
        filter(estado == 'infeccioso') %>%
        group_by(tiempo) %>%
        summarize(Inf_Trab = sum(cantidad / trabajando) / iteraciones[n], trabajando = first(trabajando)) %>%
        mutate(Protocolo = nombre[n], PCR = pcr[n], R0 = r0[n])
    
    for(n in 2:length(alg)){
        sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
        
        resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
        resultados$X = NULL
        
        aux =  resultados %>%
            filter(actividad == 'trabajo') %>%
            group_by(it, tiempo) %>%
            mutate(trabajando = sum(cantidad)) %>%
            filter(estado == 'infeccioso') %>%
            group_by(tiempo) %>%
            summarize(Inf_Trab = sum(cantidad / trabajando) / iteraciones[n], trabajando = first(trabajando)) %>%
            mutate(Protocolo = nombre[n], PCR = pcr[n], R0 = r0[n])
        
        trabajando_inf = rbind(trabajando_inf, aux)
    }
    
    p = trabajando_inf %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[rev(c(3, 2, 1))])) %>%
        ggplot(aes(x=tiempo, y=Inf_Trab, color=Protocolo)) +
        geom_line() +
        ggtitle('Porcentaje de Trabajadores Infectados') +
        ylab('') + 
        xlab('Dias') + 
        facet_wrap(~factor(R0)) + 
        scale_y_continuous(labels = scales::percent) 
    p
    
    tabla = trabajando_inf %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[rev(c(3,2, 1))])) %>%
        group_by(Protocolo, R0) %>%
        summarize(Trabajando = percent(mean(trabajando) / 100), Infectados = percent(mean(Inf_Trab)))
    tabla = tabla[order(tabla$R0), ]
    
}

################################################################################
# Poblacion infectada total
{
    n=1
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
    
    resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
    resultados$X = NULL
    
    trabajando_inf = resultados %>%
        group_by(it, tiempo) %>%
        mutate(trabajando = sum(cantidad)) %>%
        filter(estado == 'infeccioso') %>%
        group_by(tiempo) %>%
        summarize(Inf_Trab = sum(cantidad / trabajando) / iteraciones[n]) %>%
        mutate(Protocolo = nombre[n], PCR = pcr[n], R0 = r0[n])
    
    for(n in 2:length(alg)){
        sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
        
        resultados = read.csv(paste('../datos_simulaciones/resultados', sim, '.csv', sep='')) 
        resultados$X = NULL
        
        aux =  resultados %>%
            group_by(it, tiempo) %>%
            mutate(trabajando = sum(cantidad)) %>%
            filter(estado == 'infeccioso') %>%
            group_by(tiempo) %>%
            summarize(Inf_Trab = sum(cantidad / trabajando) / iteraciones[n]) %>%
            mutate(Protocolo = nombre[n], PCR = pcr[n], R0 = r0[n])
        
        trabajando_inf = rbind(trabajando_inf, aux)
    }
    
    p = trabajando_inf %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[rev(c(2,3, 1))])) %>%
        ggplot(aes(x=tiempo, y=Inf_Trab * 100, color=Protocolo)) +
        geom_line() +
        ggtitle('Infectados Total al d?a por cada 100 Trabajadores ') +
        ylab('') + 
        xlab('Dias') +
        theme_bw()
    #facet_wrap(~factor(R0)) + 
    #scale_y_continuous(labels = scales::percent) 
    p
}
ggsave(paste('../plots/resultados_28-04/Infectados totales (no porcentajes) iter=200.pdf', sep=''), p)

################################################################################
# Poblacion infectada total
{
    n=1
    sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
    
    resultados = read.csv(paste('../datos_simulaciones/numero_infecciosos', sim, '.csv', sep='')) %>%
        gather(tiempo, infecciosos, X0:X156) %>%
        mutate(tiempo = as.numeric(substr(tiempo, 2, 100)),
               Protocolo = nombre[n], PCR = pcr[n], R0 = r0[n]) %>%
        select(-index)
    
    for(n in 2:length(alg)){
        sim = paste('_' , alg[n] , '_frec=' , frec_test[n] , '_pob=' , pob[n] , '_cinic=', ctna_inic[n], '_r0=' , r0[n] , '_pi=', p_inic[n], '_iter=' , iteraciones[n], '_' , fecha[n] , sep='')
        
        aux = read.csv(paste('../datos_simulaciones/numero_infecciosos', sim, '.csv', sep='')) %>%
            gather(tiempo, infecciosos, X0:X156) %>%
            mutate(tiempo = as.numeric(substr(tiempo, 2, 100)),
                   Protocolo = nombre[n], PCR = pcr[n], R0 = r0[n]) %>%
            select(-index)
        
        resultados = rbind(resultados, aux)
    }
    
    p = resultados %>%
        filter(tiempo %in% c(60, 90, 120, 150)) %>%
        ggplot(aes(x=infecciosos, fill=factor(tiempo))) +
        #geom_histogram(aes(y=..density..), binwidth=1,  position='dodge') +
        geom_density(alpha=0.3) +
        #ggtitle('Infectados Total al día por cada 100 Trabajadores ') +
        ylab('') + 
        #xlab('Dias') +
        theme_bw() +
        facet_wrap(~factor(Protocolo)) +
        scale_x_continuous(labels = scales::percent) 
    p
    p = resultados %>%
        filter(tiempo %in% c(60, 90, 120, 150)) %>%
        mutate(Protocolo = factor(Protocolo, levels=nombre[rev(c(1,4,2,3))])) %>%
        ggplot(aes(x=infecciosos/100, color=factor(Protocolo))) +
        #geom_histogram(aes(y=..density..), binwidth=0.01,  position='dodge') +
        #geom_step(aes(len=len,y=..y.. * len),stat="ecdf") +
        geom_density(alpha=0.3, size=1) +
        #geom_line(stat='density') + 
        #ggtitle('Infectados Total al día por cada 100 Trabajadores ') +
        ylab('Probability Density Function') + 
        xlab('Percentage of Infected Individuals') +
        theme_bw() +
        scale_fill_brewer(palette='Dark2') +
        scale_x_continuous(labels = scales::label_percent(), limits=c(0,0.5)) +
        theme(legend.title = element_blank()) +
        facet_wrap(~factor(tiempo)) 
    p
    ggsave(paste('../plots/resultados_28-04/Infectados totales (no porcentajes) iter=200.pdf', sep=''), p)
    
    table_sum = resultados %>%
        filter(tiempo %in% c(60, 90, 120, 150)) %>%
        group_by(tiempo, Protocolo) %>%
        summarize(Mean = mean(infecciosos), SD = sd(infecciosos)) %>%
        gather(stat, value, Mean:SD) %>%
        rename(Days = tiempo, Protocol = Protocolo) %>%
        arrange(Protocol, Days) %>%
        #unite(Stats, Mean, SD) %>%
        spread(key=Days, value=value) 
        
    
    
    
}

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

