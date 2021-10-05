# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
#
# Amauri Silva      - Matrícula: A58330068
# Caio Ramos        - Matrícula: A58338177
# Isabela Massaro   - Matrícula: A58337886
# Márcia Magalhães  - Matrícula: A58331043
# Marina            - Matrícula:
#
# Turma: T10
# Curso: MBA EM BUSINESS ANALYTICS E BIG DATA
# Matéria: Análise de Mídias Sociais e Mineração de Texto - Aulas 1, 2 e 3
# Prof. Eduardo de Rezende Francisco
# FGV-Management - 2S2021
#
# --------------------------------------------------------------------------------------------------
# ==================================================================================================
##########################################################################
# Organização da Base de dados
##########################################################################
# --------------------------------------------------------------------------------------------------
# ==================================================================================================

# :::Limpa os dados da memória
rm(list = ls())

# Para evitar notação cientifica (3,4E+28)
options(scipen = 999)

# :::Importação das libs
library(network)
library(sna)
library(igraph)

# 1 # Explore as rotinas Exemplo Rede One Mode_Paulista T10.R e Exemplo Rede Two Mode_Paulista T10.R .
    # Rode os códigos na plataforma R utilizando como base as tabelas Rede One Mode_Tarefa Aulas 1, 2 e 3_Paulista T10.xlsx
    # e Rede Two Mode_Tarefa Aulas 1, 2 e 3_ Paulista T10.xlsx. (atenção: não são as mesmas bases trabalhadas em sala).
    # Compile as saídas dos códigos (conteúdo das variáveis, gráficos, tabelas) em um documento Word (usando o modelo deste documento)
    # e comente seus resultados, análises, potenciais implicações gerenciais, etc, conforme discutido em sala nas Aulas 1, 2 e 3.

# 1 - A) :::: AMAURI   - REDE ONE MODE :::::::
# 1 - B) :::: ISABELLA - REDE TWO MODE :::::::
    
# Le o arquivo com as informações de compras
compras <- read.csv2("C:/Users/Isabella/Documents/MBA FGV/10.Análise de Mídias Sociais e Text Mining/Tarefas Aulas 123/Rede Two Mode_Tarefa Aulas 123_Paulista T10.csv",header=TRUE,sep = ",")

# caso compras seja um objeto tibble, convertê-lo de volta para dataframe
compras <- as.data.frame(compras)

# Adaptando o data.frame compras para que possa servir para a montagem da rede
compras1 <- compras[,2:20]
rownames(compras1) <- compras[,1]

# Construindo a rede a partir da matriz de relações (0 e 1)
teste <- function(compras1) {
  gplot(compras1)
  gplot(compras1,gmode="twomode",displaylabels = TRUE)
  gplot(compras1,gmode="twomode",displaylabels = TRUE,
        edge.col="gray",label.cex = 0.7,usearrows=FALSE)
}

# Aprimorando a representação da rede
gplot(compras1,gmode="twomode",displaylabels = TRUE,
      edge.col="gray",label.cex = 0.7,usearrows=FALSE,
      vertex.cex = closeness(compras1,gmode="twomode")*3)


# 2) :::: CAIO :::::::
    # Utilizando a Rede One Mode do arquivo Rede One Mode_Tarefa Aulas 1, 2 e 3_Paulista T10.xlsx descreva sua estrutura de
    # componentes, nós, arestas, centralidades, cliques, diâmetro, distância média e densidade. Faça pequenas modificações na tabela e veja seus resultados.



# 3) :::: MARINA :::::::
    # Inclua outras análises em seu código (usando as extensões sna, network ou igraph) e comente os resultados (seja criativo!).



# 4) :::: MARCIA :::::::
    # Utilizando a Rede Two Mode do arquivo Rede Two Mode_Tarefa Aulas 1, 2 e 3_Paulista T10.xlsx descreva sua estrutura
    # de componentes, nós, arestas, centralidades. Faça pequenas modificações na tabela e veja seus resultados.




