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













# ====FIM - RAFAEL==================================================================================

# ::: NOSSAS CONCLUSOES:::::::::::::::::::::
# • Conclusões: que insights podem ser obtidos através dos dados






