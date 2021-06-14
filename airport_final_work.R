# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
#
# Amauri Silva      - Matrícula: A58330068
# Caio Ramos        - Matrícula: A58338177
# Isabela Massaro   - Matrícula: A58337886
# Márcia Magalhães  - Matrícula: A58331043
# Rafael Ciaramicoli- Matrícula: A57445190
#
# Turma: T10
# Curso: MBA EM BUSINESS ANALYTICS E BIG DATA
# Matéria: INFERÊNCIA ESTATÍSTICA
# --------------------------------------------------------------------------------------------------
# ==================================================================================================


install.packages('dplyr')
install.packages("tidyr")
install.packages("stringr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("Rmisc")

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(Rmisc)

#_______________________________________________________________________________________________________________________________

#1.1 Apresentação e descrição da Base de Dados

#1.1. - Resumo e descrição bases de dados originais

# Leitura e inspeção do dataframe
path = 'D:/Rafael/OneDrive/MBA - Business Analytics Big Data/Inferência Estatística/'
nyc_airlines = read.csv(str_c(path, 'Trabalho em Grupo/nyc_airlines.csv', sep = ''),
                        sep = ',', stringsAsFactors = TRUE)
nyc_airports = read.csv(str_c(path, 'Trabalho em Grupo/nyc_airports.csv', sep = ''),
                        sep = ',', stringsAsFactors = TRUE)
nyc_flights = read.csv(str_c(path, 'Trabalho em Grupo/nyc_flights.csv', sep = ''),
                       sep = ',', stringsAsFactors = TRUE)
nyc_planes = read.csv(str_c(path, 'Trabalho em Grupo/nyc_planes.csv', sep = ''),
                      sep = ',', stringsAsFactors = TRUE)
nyc_weather = read.csv(str_c(path, 'Trabalho em Grupo/nyc_weather.csv', sep = ''),
                       sep = ',', stringsAsFactors = TRUE)


str(nyc_airlines); summary(nyc_airlines)
str(nyc_airports); summary(nyc_airports)
str(nyc_flights); summary(nyc_flights)
str(nyc_planes); summary(nyc_planes)
str(nyc_weather); summary(nyc_weather)

#1.2. Diagrama relacional e construção da base principal.

#Criação de variável chave entre a base de dados nyc_flights e nyc_weather.

nyc_flights$time_hour_key <- str_c(nyc_flights$year, nyc_flights$month,
                                   nyc_flights$day, nyc_flights$hour,
                                   nyc_flights$origin, sep = '-')

nyc_weather$time_hour_key <- str_c(nyc_weather$year, nyc_weather$month,
                                   nyc_weather$day, nyc_weather$hour,
                                   nyc_weather$origin, sep = '-')


#Para realizar o join entre os dts flight e airports, substituir label da variável faa do dt nyc_airports para origin. 
colnames(nyc_airports)[which(names(nyc_airports) == 'faa')] <- 'origin'

#Iniciar leftjoins das bases. Left join escolhido para trazer as informações coincidentes com a base de dados principal, nyc_flights.

nyc_fli_airl <- left_join(nyc_flights, nyc_airlines, by = 'carrier')

nyc_fli_airl_airp <- left_join(nyc_fli_airl, nyc_airports, by = 'origin')

nyc_fli_airl_aip_pl <- left_join(nyc_fli_airl_airp, nyc_planes, by = 'tailnum')

nyc_complete <- left_join(nyc_fli_airl_aip_pl, nyc_weather, by = 'time_hour_key')

#analisar descrição e resumo dos dados
str(nyc_complete); summary(nyc_complete)

#excluir colunas duplicadas 
nyc_complete <- nyc_complete %>% select(-year, -month.y, -day.y, -hour.y, -origin.y, -time_hour.y)

#Outras alterações de labels para evitar duplicidade de nome de variável quando realizar o join.

colnames(nyc_complete)[which(names(nyc_complete) == 'year.x')] <- 'year'
colnames(nyc_complete)[which(names(nyc_complete) == 'month.x')] <- 'month'
colnames(nyc_complete)[which(names(nyc_complete) == 'day.x')] <- 'day'
colnames(nyc_complete)[which(names(nyc_complete) == 'hour.x')] <- 'hour'
colnames(nyc_complete)[which(names(nyc_complete) == 'time_hour.x')] <- 'time_hour'
colnames(nyc_complete)[which(names(nyc_complete) == 'name.x')] <- 'name_airl'
colnames(nyc_complete)[which(names(nyc_complete) == 'name.y')] <- 'name_airp'
colnames(nyc_complete)[which(names(nyc_complete) == 'year.y')] <- 'year_manuf'
colnames(nyc_complete)[which(names(nyc_complete) == 'origin.x')] <- 'origin'

#converter variável time_hour em data
nyc_flights$time_hour <- ymd_hms(nyc_flights$time_hour)

#criar variáveis month_label, wday e name_origin
nyc_complete$month_label <- month(nyc_complete$time_hour, label = T, abbr = T, locale = 'English')
nyc_complete$wday <- wday(nyc_complete$time_hour, label = T, abbr = T, locale = 'English')
nyc_complete$name_origin = ifelse(nyc_complete$origin %in% 'EWR','Newark Liberty',
                                  
                                  ifelse(nyc_complete$origin %in% 'JFK','John F Kennedy','La Guardia'))

#converter variável name_origin em fator.
#nyc_complete$name_origin <- as.factor(nyc_complete$name_origin)

#analisar descrição e resumo dos dados.
str(nyc_complete); summary(nyc_complete)

#_______________________________________________________________________________________________________________________________

#1.	Análise Exploratória da dinâmica de vôos 

#1.1.	Panorama Geral Voos Aeroportos

barplot(table(nyc_complete$name_origin),
        
        main = 'Volume anual de voos por Aeroportos', cex.main = 1.4,
        
        cex.names = 1.4,
        
        xlab = "Aeroporto", 
        
        ylab = "Frequência voos (#)", cex.axis = 1.4,
        
        ylim = c(0,150000), col= 'dodgerblue', border= 'dodgerblue4')


group_by(nyc_complete,name_origin) %>% dplyr::summarise(count = n())

#1.1.1 Newark Liberty Airport


flights_EWR <- nyc_complete %>% filter(name_origin %in% ('Newark Liberty'))


barplot(table(flights_EWR$month_label),        
        main = 'Volume Mensal de voos - Newark Liberty', cex.main = 1.4,
        
        cex.names = 0.9,
        
        xlab = "Meses", 
        
        ylab = "Frequência voos (#)", cex.axis = 1.2,
        
        ylim = c(0,11000), col= 'dodgerblue', border= 'dodgerblue4')  


group_by(flights_EWR,month_label) %>% dplyr::summarise(count = n())

barplot(table(flights_EWR$day),
        
        main = 'Volume voos por dia do mês - Newark Liberty', cex.main = 1.4,
        
        cex.names = 0.9,
        
        xlab = "Dia do Mês", 
        
        ylab = "Frequência voos (#)", cex.axis = 1.2,
        
        ylim = c(0,4000), col= 'dodgerblue', border= 'dodgerblue4')  

flights_EWR_day <- group_by(flights_EWR,day) %>% summarise(count = n())

flights_EWR_day$count2 = ifelse(flights_EWR_day$day ==31,flights_EWR_day$count/7,
                                ifelse(flights_EWR_day$day ==30,flights_EWR_day$count/11, 
                                       ifelse(flights_EWR_day$day ==29,flights_EWR_day$count/11,
                                              flights_EWR_day$count/12)))


barplot(table(flights_EWR$wday),      
        main = 'Volume voos por dia da semana - Newark Liberty', cex.main = 1.4,
        
        cex.names = 0.9,
        
        xlab = "Dia da Semana", 
        
        ylab = "Frequência voos (#)", cex.axis = 1.2,
        
        ylim = c(0,20000), col= 'dodgerblue', border= 'dodgerblue4') 


group_by(flights_EWR,wday) %>% dplyr::summarise(count = n())

barplot(table(flights_EWR$hour),
        
        main = 'Volume voos por hora do dia - Newark Liberty', cex.main = 1.4,
        
        cex.names = 0.9,
        
        xlab = "Hora do dia", 
        
        ylab = "Frequência voos (#)", cex.axis = 1.2,
        
        ylim = c(0,15000), col= 'dodgerblue', border= 'dodgerblue4') 


IQR(flights_EWR$hour)

quantile(flights_EWR$hour,0.25)

quantile(flights_EWR$hour,0.75)


par(mar = rep(2, 4))

boxplot(flights_EWR$hour,
        
        main = 'Voos por hora - Newark Liberty', cex.main = 1.4,
        
        xlab = 'Dia da Semana', cex.axis = 1.2,
        
        horizontal = F, cex.lab = 1.2,
        
        ylim= c(0,24), col = 'dodgerblue', border= 'dodgerblue4' )


boxplot(hour ~ wday, data = flights_EWR,
        
        main = 'voos por Hora/dia da semana - Newark Liberty', cex.main = 1.4,
        
        xlab = 'Dia da Semana',
        
        ylab = 'Hora do Dia', cex.axis = 1.0,
        
        horizontal = F, cex.lab = 1.2,
        
        ylim= c(0,24), col = 'dodgerblue', border= 'dodgerblue4' )


flights_EWR_wknd <- flights_EWR %>% filter(wday %in% c("Sat","Sun"))

IQR(flights_EWR_wknd$hour)

quantile(flights_EWR_wknd$hour,0.25)

quantile(flights_EWR_wknd$hour,0.75)

min(flights_EWR_wknd$hour)


flights_EWR_du <- flights_EWR %>% filter(wday %in% c("Mon","Tue","Wed","Thu","Fri"))

IQR(flights_EWR_du$hour)

quantile(flights_EWR_du$hour,0.25)

quantile(flights_EWR_du$hour,0.75)

min(flights_EWR_du$hour)

max(flights_EWR_du$hour)


#1.1.2. John F Kennedy Airport


flights_JFK <- nyc_complete %>% filter(name_origin %in% ('John F Kennedy'))

barplot(table(flights_JFK$month_label),
        
        main = 'Volume Mensal de voos - John F Kennedy', cex.main = 1.4,
        
        cex.names = 1.2,
        
        xlab = "Meses", 
        
        ylab = "Frequência voos (#)", cex.axis = 1.2,
        
        ylim = c(0,11000), col= 'dodgerblue', border= 'dodgerblue4')   

group_by(flights_JFK,month_label) %>% dplyr::summarise(count = n())

barplot(table(flights_JFK$day),
        
        main = 'Volume voos por dia do mês - John F Kennedy', cex.main = 1.4,
        
        cex.names = 0.9,
        
        xlab = "Dia do Mês",
        
        ylab = "Frequência voos (#)", cex.axis = 1.2,
        
        ylim = c(0,4000), col= 'dodgerblue', border= 'dodgerblue4')    


flights_JFK_day <- group_by(flights_JFK,day) %>% dplyr::summarise(count = n())


flights_JFK_day$count2 = ifelse(flights_JFK_day$day ==31,flights_JFK_day$count/7,
                                
                                ifelse(flights_JFK_day$day ==30,flights_JFK_day$count/11,
                                       
                                       ifelse(flights_JFK_day$day ==29,flights_JFK_day$count/11,
                                              
                                              flights_JFK_day$count/12)))


barplot(table(flights_JFK$wday),
        main = 'Volume voos por dia da semana - John F Kennedy', cex.main = 1.4,
        
        cex.names = 1.2,
        
        xlab = "Dia da Semana", 
        
        ylab = "Frequência voos (#)", cex.axis = 1.2,
        
        ylim = c(0,20000), col= 'dodgerblue', border= 'dodgerblue4') 

group_by(flights_JFK,wday) %>% dplyr::summarise(count = n())

barplot(table(flights_JFK$hour),
        
        main = 'Volume voos por hora do dia - John F Kennedy', cex.main = 1.4,
        
        cex.names = 0.9,
        
        xlab = "Hora do dia", 
        
        ylab = "Frequência voos (#)", cex.axis = 1.2,
        
        ylim = c(0,15000), col= 'dodgerblue', border= 'dodgerblue4') 


boxplot(hour ~ wday, data = flights_JFK,
        
        main = 'Voos por Hora/dia da semana - John F Kennedy', cex.main = 1.4,
        
        xlab = 'Dia da Semana',
        
        ylab = 'Hora do Dia', cex.axis = 1.2,
        
        horizontal = F, cex.lab = 1.2,
        
        ylim= c(0,24), col = 'dodgerblue', border= 'dodgerblue4' )


IQR(flights_JFK$hour)

quantile(flights_JFK$hour,0.25)

quantile(flights_JFK$hour,0.75)

min(flights_JFK$hour)

median(flights_JFK$hour)



#1.1.3 La Guardia Airport


flights_LGA <- nyc_complete %>% filter(name_origin %in% ('La Guardia'))

barplot(table(flights_LGA$month_label),        
        main = 'Volume Mensal de voos - La Guardia', cex.main = 1.4,
        
        cex.names = 0.9,
        
        xlab = "Meses", 
        
        ylab = "Frequência voos (#)", cex.axis = 1.2,
        
        ylim = c(0,11000), col= 'dodgerblue', border= 'dodgerblue4')   


group_by(flights_LGA,month_label) %>% dplyr::summarise(count = n())

barplot(table(flights_LGA$day),
        
        main = 'Volume voos por dia do mês - La Guardia', cex.main = 1.4,
        
        cex.names = 0.9,
        
        xlab = "Dia do Mês", 
        
        ylab = "Frequência voos (#)", cex.axis = 1.2,
        
        ylim = c(0,4000), col= 'dodgerblue', border= 'dodgerblue4')         


flights_LGA_day <- group_by(flights_LGA,day) %>% dplyr::summarise(count = n())


flights_LGA_day$count2 = ifelse(flights_LGA_day$day ==31,flights_LGA_day$count/7,
                                
                                ifelse(flights_LGA_day$day ==30,flights_LGA_day$count/11,
                                       
                                       ifelse(flights_LGA_day$day ==29,flights_LGA_day$count/11,
                                              
                                              flights_LGA_day$count/12)))

barplot(table(flights_LGA$wday),       
        main = 'Volume voos por dia da semana - La Guardia', cex.main = 1.4,
        
        cex.names = 0.9,
        
        xlab = "Dia da Semana", 
        
        ylab = "Frequência voos (#)", cex.axis = 1.2,
        
        ylim = c(0,20000), col= 'dodgerblue', border= 'dodgerblue4') 


group_by(flights_LGA,wday) %>% dplyr::summarise(count = n())

barplot(table(flights_LGA$hour),
        
        main = 'Volume voos por hora do dia - La Guardia', cex.main = 1.4,
        
        cex.names = 0.9,
        
        xlab = "Hora do dia", 
        
        ylab = "Frequência voos (#)", cex.axis = 1.2,
        
        ylim = c(0,15000), col= 'dodgerblue', border= 'dodgerblue4') 

boxplot(hour ~ wday, data = flights_LGA,       
        main = 'voos por Hora/dia da semana - La Guardia', cex.main = 1.4,
        
        xlab = 'Dia da Semana',
        
        ylab = 'Hora do Dia', cex.axis = 1.0,
        
        horizontal = F, cex.lab = 1.2,
        
        ylim= c(0,24), col = 'dodgerblue', border= 'dodgerblue4' )


flights_LGA_wknd <- flights_LGA %>% filter(wday %in% c("Sun"))

IQR(flights_LGA_wknd$hour)

quantile(flights_LGA_wknd$hour,0.25)

quantile(flights_LGA_wknd$hour,0.75)

min(flights_LGA_wknd$hour)

max(flights_LGA_wknd$hour)


flights_LGA_du <- flights_LGA %>% filter(wday %in% c("Mon","Tue","Wed","Thu","Fri","Sat"))

IQR(flights_LGA_du$hour)

quantile(flights_LGA_du$hour,0.25)

quantile(flights_LGA_du$hour,0.75)

min(flights_LGA_du$hour)

max(flights_LGA_du$hour)


#1.2 Análise dos voos por companhia aérea


barplot(table(nyc_complete$name_airl),        
        main = 'Volume anual de voos por Companhia Aérea', cex.main = 1.4,
        
        cex.names = 0.66,
        
        xlab = , 
        
        ylab = , cex.axis = 1.0,
        
        ylim = c(0,60000), col= 'dodgerblue', border= 'dodgerblue4',
        
        las=2,angle=90)


par(mar=c(9,4,2,2))

boxplot(distance ~ name_airl, data = nyc_complete,        
        main = 'Distância voos por Companhias Aéreas', cex.main = 1.3,
        
        xlab = '',
        
        ylab = 'Distância (milhas)', cex.axis = 0.7,
        
        horizontal = F, cex.lab = 1.0,
        
        ylim= c(0,5400), col = 'dodgerblue', border= 'dodgerblue4',
        
        las=2,angle=90)


group_by(nyc_complete,name_airl) %>% dplyr::summarise(count = n(),                                              
                                               mean = mean(distance, na.rm = TRUE),
                                               
                                               sd = sd(distance, na.rm = TRUE),
                                               
                                               var = var(distance, na.rm = TRUE),
                                               
                                               min = min(distance, na.rm = TRUE),
                                               
                                               max = max(distance, na.rm = TRUE))


ggplot(data = nyc_complete) + 
  geom_point(mapping = aes(x = distance, y = air_time, color = name_airl))

boxplot(air_time ~ name_airl, data = nyc_complete,        
        main = 'Duração voos por Companhias Aéreas', cex.main = 1.3,
        
        xlab = '',
        
        ylab = 'Duração (minutos)', cex.axis = 0.7,
        
        horizontal = F, cex.lab = 1.0,
        
        ylim= c(0,800), col = 'dodgerblue', border= 'dodgerblue4',
        
        las=2,angle=90)


group_by(nyc_complete,name_airl) %>% dplyr::summarise(count = n(),
                                               
                                               mean = mean(air_time, na.rm = TRUE),
                                               
                                               sd = sd(air_time, na.rm = TRUE),
                                               
                                               var = var(air_time, na.rm = TRUE),
                                               
                                               min = min(air_time, na.rm = TRUE),
                                               
                                               max = max(air_time, na.rm = TRUE))

#_______________________________________________________________________________________________________________________________

#2.Análise dos Atrasos em vôos


#Tratamentos pré-análise 


#criação variável atrasos e transformação para factor

nyc_complete$atrasos = ifelse(nyc_complete$dep_delay >= 60, "atrasado", 'pontual')
nyc_complete <- nyc_complete %>% transform(atrasos = as.factor(atrasos))

#eliminação de NAs
flights_atr_pont <- nyc_complete %>% filter(atrasos == 'atrasado' | atrasos == 'pontual')

#filtro no dataset mantendo apenas os vôos atrasados
flights_atr <- nyc_complete %>% filter(atrasos == 'atrasado')


#2.1 Por tempo

#2.1.1 quantidade de voos

ggplot(data = flights_atr, aes(x = dep_delay)) +  
  geom_histogram(binwidth = 30,color = 'dodgerblue4', fill = 'dodgerblue1') +
  xlab("Tempo de atraso (minutos)") +
  ylab("Voos") +
  ggtitle("Frequência de voos atrasados por tempo")+
  scale_x_continuous(limits = c(30,500), breaks = seq(30,500, by = 30))

#2.1.2 Percentual de vôos


ggplot(data = flights_atr, aes(x = dep_delay)) + 
  geom_boxplot(fill = 'dodgerblue1') +
  xlab("Tempo de atraso") +
  ylab("Voos") +
  ggtitle("Distribuição voos atrasados por tempo de atraso")+
  scale_x_continuous(limits = c(30,500), breaks = seq(30,500, by = 30))+
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())



#2.2 Atrasos por cia aérea

#flights_weather3 <- left_join(flights_weather3, airlines, by = 'carrier')
#join com a base de airines para obter os nomes das cias aéreas

#Quantidade

ggplot(data = flights_atr) +  
  geom_bar(mapping = aes(x = name_airl),color = 'dodgerblue4', fill = 'dodgerblue1', position = "dodge") +
  xlab("Cia Aérea") +
  ylab("Atrasos") +
  ggtitle("Quantidade de atrasos por Cia Aérea") +
  theme(axis.text.x = element_text(angle = 90, size = 12))


#Percentual

#ggplot(data = flights_weather3) + 
#geom_bar(mapping = aes(x = name, fill = atrasos), color = 'dodgerblue4', position = "fill") +
ggplot(data = flights_atr_pont) +  
  geom_bar(mapping = aes(x = name_airl, fill = atrasos), color = 'dodgerblue4', position = "fill") +
  xlab("Cia Aérea") +
  ylab("Atrasos") +
  ggtitle("Percentual de voos atrasados por Cia Aérea") +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c('atrasado' = 'orange', 'pontual' = 'dodgerblue1'))+
  theme(axis.text.x = element_text(angle = 90, size = 12))

#2.3 Atrasos por período

#2.3.1 Por mês

#variável month_label ordenada corretamente. 
levels(flights_atr_pont$month_label)


#Por quantidade

ggplot(data = flights_atr) + 
  geom_bar(mapping = aes(x = month_label),color = 'dodgerblue4', fill = 'dodgerblue1', position = "dodge") +  
  xlab("Mês") +
  ylab("Atrasos") +
  ggtitle("Quantidade de atrasos por Mês") +
  theme(axis.text.x = element_text(angle = 90, size = 12))

#Percentual

ggplot(data = flights_atr_pont) + 
  geom_bar(mapping = aes(x = month_label, fill = atrasos), color = 'dodgerblue4', position = "fill") + 
  xlab("Mês") +
  ylab("Atrasos") +
  ggtitle("Percentual de voos atrasados por Mês") +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c('atrasado' = 'orange', 'pontual' = 'dodgerblue1'))+
  theme(axis.text.x = element_text(angle = 90, size = 12))

#2.3.2 Dia do mes

#Por quantidade

ggplot(data = flights_atr) + 
  geom_bar(mapping = aes(x = factor(day)),color = 'dodgerblue4', fill = 'dodgerblue1', position = "dodge") +
  xlab("Dia") +
  ylab("Atrasos") +
  ggtitle("Quantidade de voos atrasados por dia do mês") +
  theme(axis.text.x = element_text(angle = 90, size = 12))

#Percentual

#ggplot(data = flights_weather3) + 
ggplot(data = flights_atr_pont) + 
  geom_bar(mapping = aes(x = factor(day), fill = atrasos), color = 'dodgerblue4', position = "fill") +
  xlab("Dia") +
  ylab("Atrasos") +
  ggtitle("Percentual de voos atrasados por dia do mês") +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c('atrasado' = 'orange', 'pontual' = 'dodgerblue1'))+
  theme(axis.text.x = element_text(angle = 90, size = 12))

#2.3.3 Dia da semana


#Por quantidade

ggplot(data = flights_atr) +  
  geom_bar(mapping = aes(x = wday),color = 'dodgerblue4', fill = 'dodgerblue1', position = "dodge") +
  xlab("Dia") +
  ylab("Atrasos") +
  ggtitle("Quantidade de voos atrasados por dia da semana") +
  theme(axis.text.x = element_text(angle = 90, size = 10))

#Percentual

ggplot(data = flights_atr_pont) + 
  geom_bar(mapping = aes(x = wday, fill = atrasos), color = 'dodgerblue4', position = "fill") +
  xlab("Dia") +
  ylab("Atrasos") +
  ggtitle("Percentual de voos atrasados por dia da semana") +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c('atrasado' = 'orange', 'pontual' = 'dodgerblue1'))+
  theme(axis.text.x = element_text(angle = 90, size = 12))


#2.4 Aeroportos

#Por quantidade

ggplot(data = flights_atr) +
  geom_bar(mapping = aes(x = origin),color = 'dodgerblue4', fill = 'dodgerblue1', position = "dodge") +
  xlab("Aeroporto") +
  ylab("Atrasos") +
  ggtitle("Quantidade de voos atrasados por aeroporto") +
  theme(axis.text.x = element_text(size = 10))

#Percentual

#ggplot(data = flights_weather5) + 
ggplot(data = flights_atr_pont) + 
  geom_bar(mapping = aes(x = origin, fill = atrasos), color = 'dodgerblue4', position = "fill") +
  xlab("Aeroporto") +
  ylab("Atrasos") +
  ggtitle("Percentual de voos atrasados por aeroporto") +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c('atrasado' = 'orange', 'pontual' = 'dodgerblue1'))+
  theme(axis.text.x = element_text(size = 12))

#_______________________________________________________________________________________________________________________________

#3. Panorama geral dos voos realizados para as top 5 cia aéreas

#3.1 Análise do panorama de voos.

# Obtendo o nome e afrequência de voos das Top 5 CIA AÉREA :::::::::::::::::::::::::::::::::::::::::

flight_per_airline <- table(nyc_complete$carrier)
flight_per_airline <- sort(flight_per_airline, decreasing = TRUE)

View(flight_per_airline)

top_5_airlines <- head(flight_per_airline, 5)
top_5_airlines <- data.frame(top_5_airlines)
top_5_airlines <- rename(top_5_airlines, carrier=Var1, total_flight=Freq)

View(top_5_airlines)

# Novo dataframe contendo apenas os dados das top 5 CIA aéreas

t5_airline <- filter(nyc_complete, carrier %in% c('UA', 'B6', 'EV', 'DL', 'AA'))
View(t5_airline)

# Análise do panorama de voos por CIA ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# United Air Lines Inc.-----------------------------------------------------------------------------

UA <- filter(t5_airline, carrier == 'UA')
View(UA)

UAA <- ggplot(UA, aes(x = origin))+
  geom_bar(fill = 'dodgerblue')+
  xlab('Origin departure - United Air Lines ')

B6 <- filter(t5_airline, carrier == 'B6')
View(B6)

BB6 <- ggplot(B6, aes(x = origin))+
  geom_bar(fill = 'dodgerblue')+
  xlab('Origin departure - JetBlue Airways')

EV <- filter(t5_airline, carrier == 'EV')
View(EV)

EVV <- ggplot(EV, aes(x = origin))+
  geom_bar(fill = 'dodgerblue')+
  xlab('Origin departure - ExpressJet Airlines')

DL <- filter(t5_airline, carrier == 'DL')
View(DL)

DLL <- ggplot(DL, aes(x = origin))+
  geom_bar(fill = 'dodgerblue')+
  xlab('Origin departure - Delta Air Lines')

AA <- filter(t5_airline, carrier == 'AA')
View(AA)

AAA <- ggplot(AA, aes(x = origin))+
  geom_bar(fill = 'dodgerblue')+
  xlab('Origin departure - American Airlines')


multiplot(UAA, BB6, EVV, DLL, AAA, cols = 2)


#3.2 Análise de distâncias

ggplot(UA, aes(x = distance, dest, color = origin))+
  geom_point(aes(size = distance))+
  xlab('Distance in miles')+
  ylab('Final destinantion')+
  ggtitle('United Air Lines')


ggplot(B6, aes(x = distance, dest, color = origin))+
  geom_point(aes(size = distance))+
  xlab('Distance in miles')+
  ylab('Final destinantion')+
  ggtitle('JetBlue Airways')


ggplot(EV, aes(x = distance, dest, color = origin))+
  geom_point(aes(size = distance))+
  xlab('Distance in miles')+
  ylab('Final destinantion')+
  ggtitle('ExpressJet Airlines')


ggplot(DL, aes(x = distance, dest, color = origin))+
  geom_point(aes(size = distance))+
  xlab('Distance in miles')+
  ylab('Final destinantion')+
  ggtitle('Delta Air Lines')


ggplot(AA, aes(x = distance, dest, color = origin))+
  geom_point(aes(size = distance))+
  xlab('Distance in miles')+
  ylab('Final destinantion')+
  ggtitle('American Airlines')


#3.3 durações típicas 

ggplot(t5_airline, aes(x = distance, y= air_time, color = carrier))+
  geom_point()+
  xlab('Distance in miles')+
  ylab('Air time duration (minutes)')


ggplot(t5_airline, aes(x = carrier, y = air_time, ))+
  geom_boxplot(color = 'dodgerblue',  border= 'dodgerblue4')+
  xlab('Carrier')+
  ylab('Air time duration (minutes)')



# voos que não partiram (cancelados por algum motivo) ----------------------------------------------
canceled_flight_UA <- filter(UA,  is.na(dep_time))
View(canceled_flight_UA)
str(canceled_flight_UA)

nrow(canceled_flight_UA)
# 686
nrow(filter(canceled_flight_UA, origin == 'EWR'))
# 435
nrow(filter(canceled_flight_UA, origin == 'JFK'))
# 44
nrow(filter(canceled_flight_UA, origin == 'LGA'))
# 207

can_UA <- ggplot(canceled_flight_UA, aes(x = origin))+
geom_bar(fill = 'dodgerblue')+
  xlab('Origin departure')+
  ylab('Canceled flights - United Air Lines')


canceled_flight_B6 <- filter(B6,  is.na(dep_time))

nrow(canceled_flight_B6)
# 466
nrow(filter(canceled_flight_B6, origin == 'EWR'))
# 74
nrow(filter(canceled_flight_B6, origin == 'JFK'))
# 315
nrow(filter(canceled_flight_B6, origin == 'LGA'))
# 77

can_B6 <- ggplot(canceled_flight_B6, aes(x = origin))+
 geom_bar(fill = 'dodgerblue')+
  xlab('Origin departure')+
  ylab('Canceled flights - JetBlue Airways')



canceled_flight_EV <- filter(EV,  is.na(dep_time))
nrow(canceled_flight_EV)
# 2817
nrow(filter(canceled_flight_EV, origin == 'EWR'))
# 2164
nrow(filter(canceled_flight_EV, origin == 'JFK'))
# 82
nrow(filter(canceled_flight_EV, origin == 'LGA'))
# 571

can_EV <- ggplot(canceled_flight_EV, aes(x = origin))+
  geom_bar(fill = 'dodgerblue')+
  xlab('Origin departure')+
  ylab('Canceled flights - ExpressJet Airlines')


canceled_flight_DL <- filter(DL,  is.na(dep_time))

nrow(canceled_flight_DL)
# 349
nrow(filter(canceled_flight_DL, origin == 'EWR'))
# 39
nrow(filter(canceled_flight_DL, origin == 'JFK'))
# 100
nrow(filter(canceled_flight_DL, origin == 'LGA'))
# 210

can_DL <- ggplot(canceled_flight_DL, aes(x = origin))+
  geom_bar(fill = 'dodgerblue')+
  xlab('Origin departure')+
  ylab('Canceled flights - Delta Air Lines')



canceled_flight_AA <- filter(AA,  is.na(dep_time))

nrow(canceled_flight_AA)
# 636
nrow(filter(canceled_flight_AA, origin == 'EWR'))
# 99
nrow(filter(canceled_flight_AA, origin == 'JFK'))
# 141
nrow(filter(canceled_flight_AA, origin == 'LGA'))
# 396

can_AA <- ggplot(canceled_flight_AA, aes(x = origin))+
  geom_bar(fill = 'dodgerblue')+
  xlab('Origin departure')+
  ylab('Canceled flights - American Airlines')


multiplot(can_UA, can_B6, can_EV, can_DL, can_AA, cols = 2)


# Voos com atraso maior ou igual a 1h----------------------------------------------------------------
delay_flight <- filter(UA, dep_delay >= 60)
View(delay_flight)

nrow(delay_flight)
# 3899

# media de atraso por mês---------------------------------------------------------------------------
(filter(delay_flight, month == 1))
nrow(filter(delay_flight, month == 2))
nrow(filter(delay_flight, month == 3))
nrow(filter(delay_flight, month == 4))
nrow(filter(delay_flight, month == 5))
nrow(filter(delay_flight, month == 6))
nrow(filter(delay_flight, month == 7))
nrow(filter(delay_flight, month == 8))
nrow(filter(delay_flight, month == 9))
nrow(filter(delay_flight, month == 10))
nrow(filter(delay_flight, month == 11))
nrow(filter(delay_flight, month == 12))

delay_flight$month <- month.abb[delay_flight$month]
delay_flight$month <- toupper(delay_flight$month)
View(delay_flight)


ggplot(delay_flight, aes(x = month , y = dep_delay))+
  geom_boxplot(fill = 'dodgerblue')


ggplot(delay_flight, aes(x = month))+
  geom_bar()


#3.4 Tipo e idade da frota. 


UA_AIRPLANE <- select(UA, tailnum, year_manuf, type, manufacturer, model, engines, seats, speed, engine)
View(UA_AIRPLANE)

UA1 <- ggplot(UA_AIRPLANE, aes(x = manufacturer, y = year_manuf))+
  geom_boxplot(col = 'dodgerblue', border = 'dodgerblue4')


# Airbus Industrie (1970–2001)
# fonte: https://en.wikipedia.org/wiki/Airbus

# A frota utilizada pela United Air Lines  é predominantemente da Boeing

ggplot(UA_AIRPLANE, aes(x = model, y =year_manuf, color = manufacturer))+
  geom_point(size = 4) +
  xlab('Air plane model')+
  ylab(' Year facture')


ggplot(UA_AIRPLANE, aes(x = model, y =year_manuf, color = manufacturer, label = seats))+
  geom_text()+
  xlab('Air plane model')+
  ylab(' Year facture')



# --------------------------------------------------------------------------------------------------


B6_AIRPLANE <- select(B6, tailnum, year_manuf, type, manufacturer, model, engines, seats, speed, engine)
View(B6_AIRPLANE)


ggplot(B6_AIRPLANE, aes(x = manufacturer, y =year_manuf))+
  geom_boxplot(col = 'dodgerblue', border= 'dodgerblue4')

# Airbus Industrie (1970–2001)
# fonte: https://en.wikipedia.org/wiki/Airbus


ggplot(B6_AIRPLANE, aes(x = model, y =year_manuf, color = manufacturer))+
  geom_point(size = 4) +
  xlab('Air plane model')+
  ylab(' Year facture')


ggplot(B6_AIRPLANE, aes(x = model, y =year_manuf, color = manufacturer, label = seats))+
  geom_text()+
  xlab('Air plane model')+
  ylab(' Year facture')


# --------------------------------------------------------------------------------------------------


EV_AIRPLANE <- select(EV, tailnum, year_manuf, type, manufacturer, model, engines, seats, speed, engine)
View(EV_AIRPLANE)


ggplot(EV_AIRPLANE, aes(x = manufacturer, y =year_manuf))+
  geom_boxplot(col = 'dodgerblue', border= 'dodgerblue4')

# Airbus Industrie (1970–2001)
# fonte: https://en.wikipedia.org/wiki/Airbus


ggplot(EV_AIRPLANE, aes(x = model, y =year_manuf, color = manufacturer))+
  geom_point(size = 4) +
  xlab('Air plane model')+
  ylab(' Year facture')


ggplot(EV_AIRPLANE, aes(x = model, y =year_manuf, color = manufacturer, label = seats))+
  geom_text()+
  xlab('Air plane model')+
  ylab(' Year facture')


# --------------------------------------------------------------------------------------------------


DL_AIRPLANE <- select(DL, tailnum, year_manuf, type, manufacturer, model, engines, seats, speed, engine)
View(DL_AIRPLANE)


ggplot(DL_AIRPLANE, aes(x = manufacturer, y =year_manuf))+
  geom_boxplot(col = 'dodgerblue', border= 'dodgerblue4')

# Airbus Industrie (1970–2001)
# fonte: https://en.wikipedia.org/wiki/Airbus

ggplot(DL_AIRPLANE, aes(x = model, y =year_manuf, color = manufacturer))+
  geom_point(size = 4) +
  xlab('Air plane model')+
  ylab(' Year facture')


ggplot(DL_AIRPLANE, aes(x = model, y =year_manuf, color = manufacturer, label = seats))+
  geom_text()+
  xlab('Air plane model')+
  ylab(' Year facture')


# --------------------------------------------------------------------------------------------------


AA_AIRPLANE <- select(AA, tailnum, year_manuf, type, manufacturer, model, engines, seats, speed, engine)
View(AA_AIRPLANE)

ggplot(AA_AIRPLANE, aes(x = manufacturer, y =year_manuf))+
  geom_boxplot(col = 'dodgerblue', border= 'dodgerblue4')

# Airbus Industrie (1970–2001)
# fonte: https://en.wikipedia.org/wiki/Airbus

ggplot(AA_AIRPLANE, aes(x = model, y =year_manuf, color = manufacturer))+
  geom_point(size = 4) +
  xlab('Air plane model')+
  ylab(' Year facture')


ggplot(AA_AIRPLANE, aes(x = year_manuf, y =model, color = manufacturer, label = seats))+
  geom_text()+
  xlab('Air plane model')+
  ylab(' Year facture')

#_______________________________________________________________________________________________________________________________

#4. Outras Análises


summary(nyc_weather)

# transformando a variável month em factor
nyc_weather2 <- nyc_weather %>% transform(month = as.factor(month))
str(nyc_weather2)


# renomeando os fatores mês
levels(nyc_weather2$month) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
levels(nyc_weather2$month)
View (nyc_weather2)


nyc_weather2$precip <- as.numeric(nyc_weather2$precip)
nyc_weather2$visib <- as.numeric(nyc_weather2$visib)

#média global precipitação
nyc_weather2 %>% dplyr::summarise(average_precip = mean(precip))
nyc_weather2 %>% dplyr::summarise(average_precip = mean(visib))

#4.1. Análise da Precipitação Média por mês

nyc_weather2 %>% group_by(origin, month) %>% dplyr::summarise(average_precip_month = mean(precip), average_visib_month = mean(visib)) -> nyc_weather4
View(nyc_weather4)


# Gráficos Precipitação por aeroporto

nyc_weather4 %>% filter(origin == 'EWR') -> nyc_weather5
View(nyc_weather5)

ggplot(nyc_weather5, aes(month, average_precip_month, group = 1)) +  geom_line(color = 'dodgerblue', lwd = 1.1) +
  labs(x="Mês",
       y = "Precipitação Média ",
       title = "Precipitação Média por Mês - Newark Liberty Airport")


nyc_weather4 %>% filter(origin == 'JFK') -> nyc_weather6
View(nyc_weather6)

ggplot(nyc_weather6, aes(month, average_precip_month, group = 1)) +  geom_line(color = 'dodgerblue', lwd = 1.1) +
  labs(x="Mês",
       y = "Precipitação Média ",
       title = "Precipitação Média por Mês - John F Kennedy Airport")


nyc_weather4 %>% filter(origin == 'LGA') -> nyc_weather7
View(nyc_weather7)

ggplot(nyc_weather7, aes(month, average_precip_month, group = 1)) +  geom_line(color = 'dodgerblue', lwd = 1.1) +
  labs(x="Mês",
       y = "Precipitação Média ",
       title = "Precipitação Média por Mês - La Guardia Airport")


#4.2 Análise da Visibilitade Média por mês 


ggplot(nyc_weather5, aes(month, average_visib_month, group = 1)) +  geom_line(color = 'dodgerblue', lwd = 1.1) +
  labs(x="Mês",
       y = "Visibilidade Média ",
       title = "Visibilidade Média por Mês - Newark Liberty Airport")

ggplot(nyc_weather6, aes(month, average_visib_month, group = 1)) +  geom_line(color = 'dodgerblue', lwd = 1.1) +
  labs(x="Mês",
       y = "Visibilidade Médi ",
       title = "Visibilidade Média por Mês - John F Kennedy Airport")


#ggplot(nyc_weather7, aes(month, average_precip_month, group = 1)) +  geom_line(color = 'dodgerblue', lwd = 1.1) +
ggplot(nyc_weather7, aes(month, average_visib_month, group = 1)) +  geom_line(color = 'dodgerblue', lwd = 1.1) +
  labs(x="Mês",
       y = "Visibilidade Média ",
       title = "Visibilidade Média por Mês - La Guardia Airport")

#____________________________________________________________FIM___________________________________________________________________
