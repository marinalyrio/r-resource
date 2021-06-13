# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
#
# Amauri Silva      - Matrícula: A58330068
# Caio Ramos        - Matrícula: A58338177
# Isabela Massaro   - Matrícula: A58337886
# Márcia Magalhães  - Matrícula: A58331043
#
# Turma: T10
# Curso: MBA EM BUSINESS ANALYTICS E BIG DATA
# Matéria: INFERÊNCIA ESTATÍSTICA
# --------------------------------------------------------------------------------------------------
# ==================================================================================================
# Leitura e inspeção do dataframe
airlines <- tibble(read.csv2("./data/inf_esta/nyc_airlines.csv", sep = ",", stringsAsFactors = TRUE))
airports <- tibble(read.csv2("./data/inf_esta/nyc_airports.csv", sep = ",", stringsAsFactors = TRUE))
flights  <- tibble(read.csv2("./data/inf_esta/nyc_flights.csv", sep = ",", stringsAsFactors = TRUE))
planes   <- tibble(read.csv2("./data/inf_esta/nyc_planes.csv", sep = ",", stringsAsFactors = TRUE))
weather  <- tibble(read.csv2("./data/inf_esta/nyc_weather.csv", sep = ",", stringsAsFactors = TRUE))

View(airlines)
View(airports)
View(flights)
View(planes)
View(weather)


# ::::INÍCIO - ISABELA:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# • Análises I: Apresente um panorama geral sobre a dinâmica dos voos de cada aeroporto no que se refere
  # à quantidade voos. Identifique os momentos de maior demanda (horários, dias da semana e meses do ano).
  # Como são os voos por empresa área? Duração e distâncias típicas de voos?










# ====FIM - ISABELA=================================================================================

# :::CAIO:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# • Análises II: Defina o conceito de atraso de partida e analise a quantidade e % de voos com atraso por
  # tempo e por cia área. Quais são os períodos que apresentam um % maior de voos com atraso? Quais são as top
  # 3 cia em termos de atraso? Qual é o aeroporto melhor e pior em termos de pontualidade?

# 1.Tratamentos pré-análise 

#join flights & weather e tratamentos
flights_weather <- left_join(nyc_flights, nyc_weather)

#criação variável atrasos e transformação para factor
flights_weather$atrasos = ifelse(flights_weather$dep_delay >= 60, "atrasado", 'pontual')
flights_weather <- flights_weather %>% transform(atrasos = as.factor(atrasos))

#eliminação de NAs
flights_weather2 <- flights_weather %>% filter(atrasos == 'atrasado' | atrasos == 'pontual')

#2. analises atrasos

#2.1 Por tempo

#2.1.1 quantidade de voos
ggplot(data = flights_weather4, aes(x = dep_delay)) + 
  geom_histogram(binwidth = 30, fill = 'dodgerblue1') +
  xlab("Tempo de atraso (minutos)") +
  ylab("Vôos") +
  ggtitle("Frequência de vôos atrasados por tempo")+
  scale_x_continuous(limits = c(30,500), breaks = seq(30,500, by = 30))

#2.1.2 Percentual de vôos

ggplot(data = flights_weather4, aes(x = dep_delay)) + 
  geom_boxplot(fill = 'dodgerblue1') +
  xlab("Tempo de atraso") +
  ylab("Vôos") +
  ggtitle("Distribuição vôos atrasados por tempo de atraso")+
  scale_x_continuous(limits = c(30,500), breaks = seq(30,500, by = 30))+
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())



#1.2 Por cia aérea

flights_weather3 <- left_join(flights_weather3, airlines, by = 'carrier')
#join com a base de airines para obter os nomes das cias aéreas

#1.2.1 quantidade


flights_weather4 <- flights_weather3 %>% filter(atrasos == 'atrasado')
#filtro no dataset mantendo apenas os vôos atrasados


ggplot(data = flights_weather4) + 
  geom_bar(mapping = aes(x = name),color = 'dodgerblue4', fill = 'dodgerblue1', position = "dodge") +
  xlab("Cia Aérea") +
  ylab("Atrasos") +
  ggtitle("Quantidade de atrasos por Cia Aérea") +
  theme(axis.text.x = element_text(angle = 90, size = 12))


#1.1.2 percentual

ggplot(data = flights_weather3) + 
  geom_bar(mapping = aes(x = name, fill = atrasos), color = 'dodgerblue4', position = "fill") +
  xlab("Cia Aérea") +
  ylab("Atrasos") +
  ggtitle("Percentual de vôos atrasados por Cia Aérea") +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c('atrasado' = 'orange', 'pontual' = 'dodgerblue1'))+
  theme(axis.text.x = element_text(angle = 90, size = 12))



#3 periodos

#3.1 Mes

flights_weather3 <- flights_weather2 %>% transform(month = as.factor(month))
# Modificando a variável mês para fator
summary(flights_weather3)


levels(flights_weather3$month)
levels(flights_weather3$month) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
levels(flights_weather3$month)
#renomeando os fatores mês

#3.1.1 Por quantidade

ggplot(data = flights_weather4) + 
  geom_bar(mapping = aes(x = month),color = 'dodgerblue4', fill = 'dodgerblue1', position = "dodge") +
  xlab("Mês") +
  ylab("Atrasos") +
  ggtitle("Quantidade de atrasos por Mês") +
  theme(axis.text.x = element_text(angle = 90, size = 12))

#3.1.2 Percentual

ggplot(data = flights_weather3) + 
  geom_bar(mapping = aes(x = month, fill = atrasos), color = 'dodgerblue4', position = "fill") +
  xlab("Mês") +
  ylab("Atrasos") +
  ggtitle("Percentual de vôos atrasados por Mês") +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c('atrasado' = 'orange', 'pontual' = 'dodgerblue1'))+
  theme(axis.text.x = element_text(angle = 90, size = 12))








# ====FIM - CAIO====================================================================================

# ::: AMAURI   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# • Análises III: Para as top 5 cia aéreas em termos de número de voos faça uma análise do panorama de voos
  # no que se refere distâncias e durações típicas, caracterização, tipo e idade da frota.


# Para as top 5 CIA aéreas em termos de ***número de voos*** faça uma
# Análise do panorama de voos
# Distâncias e durações típicas,
# Caracterização
# Tipo e idade da frota.


View(airlines)
View(airports)
View(flights)
View(planes)
View(weather)

# Obtendo Top 5 CIA AÉREA
flight_per_airline <- table(flights$carrier)
flight_per_airline <- sort(flight_per_airline, decreasing = TRUE)

View(flight_per_airline)

top_5_airlines <- head(flight_per_airline, 5)
top_5_airlines <- data.frame(top_5_airlines)
top_5_airlines <- rename(top_5_airlines, carrier=Var1, total_flight=Freq)

View(top_5_airlines)


# Análise do panorama de voos:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# United Air Lines Inc.-----------------------------------------------------------------------------
UA <- filter(flights, carrier == 'UA')
View(UA)

# O aeroporto EWR é massivamente o aeroporto de origem mais utilizado pela cia UA (United Air Lines Inc.)
ggplot(UA, aes(x = origin, y = dest))+
  geom_point()


# distâncias----------------------------------------------------------------------------------------
ggplot(UA, aes(x = origin, dest, color = air_time))+
  geom_point()

ggplot(UA, aes(x = distance, dest, color = origin))+
  geom_point(aes(size = distance))+
  xlab('Distance in miles')+
  ylab('Final destinantion')



# durações típicas ---------------------------------------------------------------------------------
UA$time_hour <- ymd_hms(UA$time_hour)
typeof(UA$time_hour)
View(UA)

ggplot(UA, aes(x = dest, y = hour, color = origin == 'EWR'))+
  geom_point()

ggplot(UA, aes(x = dep_delay, fill = origin))+
  geom_bar(position = position_dodge(preserve = 'single'))

data(mpg, package="ggplot2")
View(mpg)


# voos que não partiram (cancelados por algum motivo) ----------------------------------------------
canceled_flight <- filter(UA,  is.na(dep_time))
View(canceled_flight)
str(canceled_flight)

nrow(canceled_flight)
# 686
nrow(filter(canceled_flight, origin == 'EWR'))
# 435
nrow(filter(canceled_flight, origin == 'JFK'))
# 44
nrow(filter(canceled_flight, origin == 'LGA'))
# 207

# Voos com atraso maior ou igual a 1h----------------------------------------------------------------
delay_flight <- filter(UA, dep_delay >= 60)
View(delay_flight)

nrow(delay_flight)
# 3899

# media de atraso por mês---------------------------------------------------------------------------

nrow(filter(delay_flight, month == 1))
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


ggplot(delay_flight, aes(x = month, y = dep_delay, color = month))+
  geom_boxplot()



ggplot(delay_flight, aes(x = month, y = dep_delay, color = month))+
  geom_point()

ggplot(delay_flight, aes(x = month, color = month))+
  geom_bar()


delay_flight_V2 <- tibble(delay_flight)
delay_flight_V2$month <- gsub(1, 'JAN', delay_flight$month)
delay_flight_V2$month <- gsub(2, 'FEB', delay_flight$month)
delay_flight_V2$month <- gsub(3, 'MAR', delay_flight$month)
delay_flight_V2$month <- gsub(4, 'APR', delay_flight$month)
delay_flight_V2$month <- gsub(5, 'MAY', delay_flight$month)
delay_flight_V2$month <- gsub(6, 'JUN', delay_flight$month)
delay_flight_V2$month <- gsub(7, 'JUL', delay_flight$month)
delay_flight_V2$month <- gsub(8, 'AUG', delay_flight$month)
delay_flight_V2$month <- gsub(9, 'SEP', delay_flight$month)
delay_flight_V2$month <- gsub(10, 'OCT', delay_flight$month)
delay_flight_V2$month <- gsub(11, 'NOV', delay_flight$month)
delay_flight_V2$month <- gsub(12, 'DEZ', delay_flight$month)


name_months <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')

A <- month.abb(delay_flight$month)

delay_flight_V2$month_name <- name_months[delay_flight_V2$month]

View(delay_flight_V2)


# Tipo e idade da frota. ---------------------------------------------------------------------------

UA_AIRPLANE <- inner_join(UA, planes, by = 'tailnum')
View(UA_AIRPLANE)

UA_AIRPLANE <- select(UA_AIRPLANE, tailnum, year.y, type, manufacturer, model, engines, seats, speed, engine)
UA_AIRPLANE <- rename(UA_AIRPLANE, year = year.y)
View(UA_AIRPLANE)

ggplot(UA_AIRPLANE, aes(x = manufacturer, y =year))+
  geom_boxplot()

# Airbus Industrie (1970–2001)
# fonte: https://en.wikipedia.org/wiki/Airbus

# A frota utilizada pala United Air Lines  é predominantemente da Boeing
ggplot(UA_AIRPLANE, aes(x = model, y =year, color = manufacturer))+
  geom_point()


ggplot(UA_AIRPLANE, aes(x = model, y =year, color = manufacturer, label = seats))+
  geom_text()+
  xlab('Air plane model')+
  ylab(' Year facture')

# ====FIM - AMAURI====================================================================================

# :::MARCIA:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# • Análise IV: Análises gerais que vcs identificarem serem interessantes. Exercitem a criatividade!
# Descrição dos dados/ tratamentos aplicados, análise 4 (aqui cada um também pode contribuir se tiver ideias),
# esboço do documento











# ====FIM - MARCIA==================================================================================

# :::RAFAEL:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# • Análise IV: Análises gerais que vcs identificarem serem interessantes. Exercitem a criatividade!
# Descrição dos dados/ tratamentos aplicados, junção script R

#analisar variáveis

str(nyc_airlines); summary(nyc_airlines)
str(nyc_airports); summary(nyc_airports)
str(nyc_flights); summary(nyc_flights)
str(nyc_planes); summary(nyc_planes)
str(nyc_weather); summary(nyc_weather)


#transformar variáveis do dt flights para factor

nyc_flights$flight <- as.factor(nyc_flights$flight)

#converter variável time_hour de factor para data.

nyc_flights$time_hour <- ymd_hms(nyc_flights$time_hour)

#criar variáveis factor mês e dia da semana e estações do ano para ampliar possibilidade de análises.

nyc_flights$month_label <- month(nyc_flights$time_hour, label = TRUE)
nyc_flights$wday <- wday(nyc_flights$time_hour, label = TRUE, locale = 'English')
nyc_flights$seasons = ifelse(nyc_flights$month_label %in% c('jun', 'jul', 'aug'),'Summer',
                              ifelse(nyc_flights$month_label %in% c('sep','oct','nov'),'Autumn',
                                     ifelse(nyc_flights$month_label %in% c('dec','jan','feb'),'Winter','Spring')))


#criar variável date
nyc_flights$date <- str_c(nyc_flights$year, nyc_flights$month,
                                   nyc_flights$day, sep = '/')

# Criar variável com a categorização dos principais feriados/datas importantes americanos. Critério. Dia do feriado +/- dois dias.
# Ano novo por limitação da base, será considerado 30/12 e 31/12.

#Super Bowl: 01/03/2013, 02/03/2013, 03/03/2013, 04/03/2013, 05/03/2013
#Independence Day: 02/07/2013, 03/07/2013, 04/07/2013, 05/07/2013, 06/07/2013
#Labor Day: 31/08/2013, 01/09/2013, 02/09/2013, 03/09/2013, 04/09/2013
#Thanksgiving: 26/11/2013, 27/11/2013, 28/11/2013, 29/11/2013, 30/09/2013
#Christmas: 23/12/2013, 24/12/2013, 25/12/2013, 26/12/2013, 27/12/2013
#New Year's Day: 30/12/2013, 31/12/2013

nyc_flights$holiday = ifelse(nyc_flights$date %in% c('2013/3/01', '2013/3/1', '2013/3/3', '2013/3/4', '2013/3/5'),'Superbowl',
                              ifelse(nyc_flights$date %in% c('2013/7/2', '2013/7/3', '2013/7/4', '2013/7/5', '2013/7/6'),'Independence Day',
                                     ifelse(nyc_flights$date %in% c('2013/8/31', '2013/9/1', '2013/9/2', '2013/9/3', '2013/9/4'),'Labor Day',
                                            ifelse(nyc_flights$date %in% c('2013/11/26', '2013/11/27', '2013/11/28', '2013/11/29', '2013/11/30'),'Thanksgivings',
                                                   ifelse(nyc_flights$date %in% c('2013/12/23', '2013/12/24', '2013/12/25', '2013/12/26', '2013/12/27'),'Christmas',
                                                          ifelse(nyc_flights$date %in% c('2013/12/30', '2013/12/31'),'New Years Day','Regular'))))))

#transformar variável date em data.
nyc_flights$date <- ymd(nyc_flights$date)

#criar variável relativa ao atraso. Conceito adotado para categorizar o atraso: dep_delay >= 60 minutos

nyc_flights$delay = ifelse(nyc_flights$dep_delay >= 60,'Delayed', 'Not Delayed')

#criar variável chave ano + mês + dia + hora + origem nos dts flights e weather. Incluir separador para evitar duplicidade de observações quando realizar o join.

nyc_flights$time_hour_key <- str_c(nyc_flights$year, nyc_flights$month,
                                   nyc_flights$day, nyc_flights$hour,
                                   nyc_flights$origin, sep = '-')

nyc_weather$time_hour_key <- str_c(nyc_weather$year, nyc_weather$month,
                                   nyc_weather$day, nyc_weather$hour,
                                   nyc_weather$origin, sep = '-')

#excluir colunas do dt weather para evitar duplicidade de colunas quando realizar o join

nyc_weather <- nyc_weather %>% select(-year, -month, -day, -hour, -origin, -time_hour)

#Para realizar o join entre os dts flight e airports, substituir label da variável faa do dt nyc_airports para origin. 
colnames(nyc_airports)[which(names(nyc_airports) == 'faa')] <- 'origin'

#alterar variável year do dt planes, relativo ao ano de fabricação da aeronave, para não confundir com o year, ano da observação da amostra.
colnames(nyc_planes)[which(names(nyc_planes) == 'year')] <- 'year_manu'

#Iniciar leftjoins das bases. Left join escolhido para trazer as informações coincidentes com o dt principal, nyc_flights

nyc_fli_airl <- left_join(nyc_flights, nyc_airlines, by = 'carrier')

nyc_fli_airl_airp <- left_join(nyc_fli_airl, nyc_airports, by = 'origin')

nyc_fli_airl_aip_pl <- left_join(nyc_fli_airl_airp, nyc_planes, by = 'tailnum')

nyc_complete <- left_join(nyc_fli_airl_aip_pl, nyc_weather, by = 'time_hour_key')



# ====FIM - RAFAEL==================================================================================

# ::: NOSSAS CONCLUSOES:::::::::::::::::::::
# • Conclusões: que insights podem ser obtidos através dos dados






