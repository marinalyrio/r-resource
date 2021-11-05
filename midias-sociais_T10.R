# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
#
# Amauri Silva      - Matrícula: A58330068
# Caio Ramos        - Matrícula: A58338177
# Isabela Massaro   - Matrícula: A58337886
# Márcia Magalhães  - Matrícula: A58331043
# Marina 			      - Matrícula:
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


# (Marcia)
# 4) (Aprendiz.N.Superv.) - Lei de Zipf. TF-IDF
# Analise-1: geral


# (Amauri)
# 5) (Aprendiz.N.Superv.) - Topic Modeling (Clusterização de Documentos e Palavras)
# Analise-1: geral (Com diferentes números de grupo (valor de K))


















