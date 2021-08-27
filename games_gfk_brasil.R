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
# Matéria: ANALISE DE SERIES TEMPORAIS
# --------------------------------------------------------------------------------------------------
# ==================================================================================================


##########################################################################
# Organização da Base de dados - Márcia
##########################################################################

#limpa variaveis da memoria
rm(list = ls())

#carrega os pacotes
install.packages("zoo")
library(zoo)
install.packages("forecast")
library(forecast)

#importa a base de dados
Games_GFK_Brasil <- read.csv2("C:/Users/marcia.silva/Desktop/Aulas MBA/Analise de series temporais/Trabalho em Grupo/Games_GFK_Brasil.csv")
View(Games_GFK_Brasil)

#evita número em format e
options(scipen=999)

#coverte a serie no formato de serie temporal
Games_GFK_Brasil_ts <- ts(Games_GFK_Brasil$Faturamento, start=c(2011,1), end=c(2020,5), frequency = 12)


#Analise estatistica
summary(Games_GFK_Brasil_ts)


#plotgráfico da série temporal
plot(Games_GFK_Brasil_ts, xlab="Data", ylab="Faturamento", ylim=c(3850000, 98500000), type="l")

------------------------
  
#separa as amostras em treinamento e teste

#define o tamanho da amostra de teste
tam_amostra_teste <- 23

#define o tamanho da amostra de treinamento
tam_amostra_treinamento <- length(Games_GFK_Brasil_ts) - tam_amostra_teste

#cria a serie temporal de treinamento
treinamento_ts <- window(Games_GFK_Brasil_ts, start=c(2011, 1), end=c(2011,tam_amostra_treinamento))

#cria a serie temporal de teste
validacao_ts <- window(Games_GFK_Brasil_ts, start=c(2011, tam_amostra_treinamento + 1), end=c(2011,tam_amostra_treinamento+tam_amostra_teste))

treinamento_ts
validacao_ts


#plota o grafico da serie temporal de treinamento e teste
plot(treinamento_ts, xlab="Data", ylab="Faturamento", xaxt="n" , ylim=c(3850000, 98500000), xlim=c(2011,2021), bty="l")

axis(1, at=seq(2011, 2021, 1), labels=format(seq(2011, 2021,1)))

lines(validacao_ts, bty="l", col="red")


##########################################################################
#Modelo de Tendência Exponencial - Márcia
##########################################################################
  
#Estima o modelo de tendÃªncia exp
modelo_tendencia_exp <- tslm(treinamento_ts ~ trend, lambda=0)

#resumo do modelo
summary(modelo_tendencia_exp)

#variáveis do modelo
modelo_tendencia_exp$coefficients
modelo_tendencia_exp$residuals
modelo_tendencia_exp$fitted.values


----------------------------
  
#Verificando resÃ­duos

#Plotando os resÃ­duos
plot(modelo_tendencia_exp$residuals, xlab="Data", ylab="ResÃ­duos", ylim=c(-1.2, 1.08), bty="l")

#calcula a autocorrelaÃ§Ã£o dos resÃ­duos
Acf(modelo_tendencia_exp$residuals)

#verifica os resÃ­duos com teste de Ljung-Box
checkresiduals(modelo_tendencia_exp, test="LB")

------------------------------
  
#plot modelo com tendencia
plot(treinamento_ts, xlab="Data", ylab="Faturamento", ylim=c(3850000, 98500000), bty="l")
lines(modelo_tendencia_exp$fitted.values, lwd=2)

#projeta o modelo durante o perÃ­odo de validaÃ§Ã£o
modelo_tendencia_exp_proj <- forecast(modelo_tendencia_exp, h = tam_amostra_teste, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_tendencia_exp_proj, xlab="Data", ylab="Faturamento", xaxt="n" , ylim=c(3850000, 98500000), xlim=c(2011,2020), bty="l", flty=2 ,main="Forecast from Exp regression model")

axis(1, at=seq(2011, 2020, 1), labels=format(seq(2011, 2020,1)))

lines(validacao_ts)
lines(treinamento_ts)
lines(modelo_tendencia_exp_proj$fitted, lwd=2, col="blue")


#Verifica a acuracia do modelo
accuracy(modelo_tendencia_exp$fitted.values, treinamento_ts)

#Calula o modelo naive
modelo_naive <- naive(treinamento_ts, level=0, h=tam_amostra_teste)

treinamento_ts[90]
modelo_naive

#Verifica a acuracia do modelo naive
accuracy(modelo_naive, validacao_ts)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_naive, xlab="Data", ylab="Faturamento", xaxt="n" , ylim=c(3850000, 98500000), xlim=c(2011, 2020), bty="l", flty=2, main="Forecast from Exp regression model")

axis(1, at=seq(2011, 2020, 1), labels=format(seq(2011, 2020,1)))

lines(validacao_ts)

---------------------------
#projetar para os prÃ³ximos 23 meses no futuro

#primeiramente reestimamos o modelo com todos os dados de treinamento e validacao
modelo_tendencia_exp_final <- tslm(Games_GFK_Brasil_ts ~ trend, lambda = 0)

#sumario do modelo
summary(modelo_tendencia_exp_final)

#projeta os prÃ³ximos 23 meses do futuro
modelo_tendencia_exp_final_proj <- forecast(modelo_tendencia_exp_final, h=23, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_tendencia_exp_final_proj, xlab="Data", ylab="Faturamento", xaxt="n" ,ylim=c(3850000, 98500000), xlim=c(2011, 2022), bty="l", flty=2, main="Forecast from Exp regression model")
axis(1, at=seq(2011, 2022, 1), labels=format(seq(2011, 2022,1)))
lines(validacao_ts)
lines(treinamento_ts)
lines(modelo_tendencia_exp_final_proj$fitted, lwd=2, col="blue")


################################################################################
#Modelo de Suavização sem tendência e sazonalidade - Márcia
################################################################################

#estima o modelo de suavizacao na base de treinamento
modelo_ses <- ets(treinamento_ts, model = "ANN")

summary(modelo_ses)

#projeta os proximos 23 meses
modelo_ses_proj <- forecast(modelo_ses, h=tam_amostra_teste, level=0.95)

#plota o grafico da projecao
plot(modelo_ses_proj, ylim=c(3850000, 98500000),ylab="Faturamento", xlab="Data", bty="l", xaxt="n", xlim=c(2011,2020.25), flty=2)

axis(1, at=seq(2011, 2020.25, 1), labels=format(seq(2011, 2020.25,1)))

lines(modelo_ses$fitted, lwd=2, col="blue")

lines(validacao_ts)

#valida precisao
accuracy(modelo_ses_proj,Games_GFK_Brasil_ts)


#Verificando resÃ­duos

#Plotando os resÃ­duos
plot(modelo_ses$residuals, xlab="Data", ylab="ResÃ­duos", ylim=c(-45000000,37000000), bty="l")

summary(modelo_ses_proj$residuals)

#calcula a autocorrelaÃ§Ã£o dos resÃ­duos
Acf(modelo_ses$residuals)

#verifica os resÃ­duos com teste de Ljung-Box
checkresiduals(modelo_ses, test="LB")

-------------------------------------------------------
  
#Preparar projecao

#primeiramente reestimamos o modelo com todos os dados de treinamento e validacao
modelo_ses_final <- ets(Games_GFK_Brasil_ts, model = "ANN")

#sumario do modelo
summary(modelo_ses_final)

#projeta os prÃ³ximos 23 meses do futuro
modelo_ses_final_proj <- forecast(modelo_ses_final, h=23, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_ses_final_proj, xlab="Data", ylab="Faturamento", ylim=c(3850000, 98500000), xlim=c(2011, 2022.5), bty="l", flty=2, main="Forecast from ETS (A,N,N)")

axis(1, at=seq(2011, 2022.5, 1), labels=format(seq(2011, 2022.5,1)))

lines(modelo_ses_final_proj$fitted, lwd=2, col="blue")


############################################################################################
#projeção série com algoritmo de busca de melhor modelo - Márcia
############################################################################################

#estima o modelo de suavizacao na base de treinamento
modelo_ses <- ets(treinamento_ts, model = "ZZZ", restrict = FALSE, allow.multiplicative.trend = TRUE)

#resumo modelo
summary(modelo_ses)

#projeta os proximos 23 meses
modelo_ses_proj <- forecast(modelo_ses, h=tam_amostra_teste, level=0.95)

#plota o grafica da projecao
plot(modelo_ses_proj, ylim=c(3850000, 98500000), ylab="Faturamento", xlab="Data", bty="l", xaxt="n", xlim=c(2011,2020.5), flty=2)

axis(1, at=seq(2011, 2020.5, 1), labels=format(seq(2011, 2020.5,1)))

lines(modelo_ses$fitted, lwd=2, col="blue")

lines(validacao_ts)

#verifica precisao
accuracy(modelo_ses_proj, validacao_ts)


#calcula a autocorrelaÃ§Ã£o dos resÃ­duos
Acf(modelo_ses$residuals)

#verifica os resÃ­duos com teste de Ljung-Box
checkresiduals(modelo_ses, test="LB")

