# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
#
# Amauri Silva      - Matrícula: A58330068
# Caio Ramos        - Matrícula: A58338177
# Isabela Massaro   - Matrícula: A58337886
# Márcia Magalhães  - Matrícula: A58331043
# Marina Lyrio      - Matrícula: A55310554
#
# Turma: T10
# Curso: MBA EM BUSINESS ANALYTICS E BIG DATA
# Matéria: ANALISE DE MIDIAS SOCIAIS E TEXTO
# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------

# :::Importação das libs
install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
install.packages("tidytext")
library(tidytext)

# :::Limpa os dados da memória
rm(list = ls())

# :::Para evitar notação cientifica (3,4E+28)
options(scipen = 999)


# :::Importação do dataset
dataFrame = read.csv("./data/trump_insult_tweets_2014_to_2021.csv", sep = ',')

# Tratamento do dataset
dataFrame = select(dataFrame, -X)
dataFrame$year = year(dataFrame$date)
col_order = c("target", "insult", "tweet", "date", "year")

# Dataset final
twitters = dataFrame[, col_order]
View(twitters)



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# (Amauri)
# 1)Tratar a data (criar variável ano)

# (CAIO)
# 2) (Descritiva) - Tokenização, Contagem de Palavras
# Analise-1: geral
# Analise-2: por ano (2014 - 2021)


# 3) (Aprendiz.N.Superv.) - Análise de Sentimento.
# (Isabela)Analise-1: geral
# (Marina) Analise-2: por ano (2014 - 2021)

twitters_2 <- twitters
nrow(twitters_2)

#atribuindo uma identificação para cada tweet
for (i in 1:nrow(twitters_2)) {
  twitters_2$row[i] <- i
}

#abrindo cada tweet por palavra e definindo um sentimento (dicionário bing)
twitters_word <- twitters_2 
twitters_word <- unnest_tokens(twitters_word, input = tweet, output = word) 
twitters_word <-twitters_word %>% inner_join(get_sentiments("bing"), by = c("word" = "word"))

#removendo as colunas que não vão ser usadas
twitters_word <- twitters_word %>% select (-target, -insult)

#abrindo os tweets por sentimento, e agrupando por ano
twitters_ano <- twitters_word %>% count(row, sentiment,year) %>% spread(key = sentiment, value = n)
twitters_word <- twitters_word %>% count(row, sentiment) %>% spread(key = sentiment, value = n)

#trocando NA por 0, para cálculo do sentimento líquido
twitters_word$negative[which(is.na(twitters_word$negative))] <- 0
twitters_word$positive[which(is.na(twitters_word$positive))] <- 0

twitters_ano$negative[which(is.na(twitters_ano$negative))] <- 0
twitters_ano$positive[which(is.na(twitters_ano$positive))] <- 0

#calculando o sentimento líquido
twitters_word$sentimento <- twitters_word$positive - twitters_word$negative
twitters_word <- twitters_word %>% select (-positive, -negative)

twitters_ano$sentimento <- twitters_ano$positive - twitters_ano$negative
twitters_ano <- twitters_ano %>% select (-positive, -negative)

#plotando por anos
twitters_ano %>% ggplot(aes(x = nrow(twitters_ano), y = sentimento, color = row)) +geom_col() +
  facet_wrap(~year)

twitters_ano %>% ggplot(aes(x = year, y = sentimento, color = row)) +geom_col()


# (Marcia)
# 4) (Aprendiz.N.Superv.) - Lei de Zipf. TF-IDF
# Analise-1: geral


# (Amauri)
# 5) (Aprendiz.N.Superv.) - Topic Modeling (Clusterização de Documentos e Palavras)
# Analise-1: geral (Com diferentes números de grupo (valor de K))


















