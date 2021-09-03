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
# Organização da Base de dados
##########################################################################
# --------------------------------------------------------------------------------------------------
# ==================================================================================================

# Limpa os dados da memória
rm(list = ls())

# Para evitar notação cientifica (3,4E+28)
options(scipen = 999)

# :::Importação das libs
library(forecast)
library(lubridate)
library(zoo)
library(urca)

# Importação do dataset
games_df = read.csv("./data/games_gfk_brasil.csv", sep = ";")

# Analise e visualização dos dados
str(games_df)
summary(games_df)
View(games_df)

# Gera o objeto de serie temporal
games_ts = ts(games_df$Faturamento, start = c(2011,1), end = c(2020,5), frequency = 12)
summary(games_ts)

#plotgráfico da série temporal
plot(games_ts, xlab="Data", ylab="Faturamento", ylim=c(3850000, 98500000), type="l")

# Amostra de teste (equivalente a 20% total do dataset 113)
amostra_teste = 23

# Amostra de treinamento
amostra_treinamento = length(games_ts) - amostra_teste

# Criando a serie temporal de treinamento
treinamento_ts =  window(games_ts, start = c(2011, 1), end = c(2011, amostra_treinamento))

# criando a serie temporal de teste
validacao_ts = window(games_ts, start = c(2011, amostra_treinamento + 1), end=c(2011, amostra_treinamento + amostra_teste))
# ESTRUTURA BASE - FIM :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

treinamento_ts
validacao_ts

#plota o grafico da serie temporal de treinamento e teste
plot(treinamento_ts, xlab="Data", ylab="Faturamento", xaxt="n" , ylim=c(3850000, 98500000), xlim=c(2011,2021), bty="l")
axis(1, at=seq(2011, 2021, 1), labels=format(seq(2011, 2021,1)))
lines(validacao_ts, bty="l", col="red")

# MODELO NAIVE - INICIO (AMAURI)::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Caculo do modelo naive
modelo_naive <- naive(treinamento_ts, level=0, h=amostra_teste)
modelo_naive
# Valida a acuracia do modelo naive
accuracy(modelo_naive, validacao_ts)

# Plot da serie temporal - Treinamento e teste
plot(modelo_naive, xlab="Tempo", ylab="Faturamento", xaxt="n" , ylim=c(3850331, 98420518), xlim=c(2011, 2020), bty="l", flty=2)

axis(1, at=seq(2011, 2020, 1), labels=format(seq(2011, 2020, 1)))
lines(validacao_ts)
# MODELO NAIVE - FIM :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

##########################################################################
#3.3 Modelo de Tendência Linear
##########################################################################

#Estima o modelo de tendência linear
modelo_tendencia_linear <- tslm(treinamento_ts ~ trend)

#resumo do modelo
summary(modelo_tendencia_linear)

#Verificando resíduos

#Plotando os resíduos
plot(modelo_tendencia_linear$residuals, xlab="Tempo", ylab="Resíduos", ylim=c(-500, 500), bty="l")

#calcula a autocorrelação dos resíduos
Acf(modelo_tendencia_linear$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_tendencia_linear, test="LB")

#plot modelo com tendencia

plot(treinamento_ts, xlab="Tempo", ylab="Passageiros", bty="l")
lines(modelo_tendencia_linear$fitted.values, lwd=2)

#projeta o modelo durante o período de validação
modelo_tendencia_linear_proj <- forecast(modelo_tendencia_linear, h = amostra_teste, level=0.95)

#plota o grafico da serie temporal de treinamento e teste

plot(modelo_tendencia_linear_proj, xlab="Tempo", ylab="Faturamento", xaxt="n" , bty="l", flty=2)
lines(validacao_ts)
lines(modelo_tendencia_linear_proj$fitted, lwd=2, col="blue")

#Verifica a acuracia do modelo
accuracy(modelo_tendencia_linear_proj, validacao_ts)

##########################################################################
#3.4 Modelo de Tendência Quadrático 
##########################################################################

#Séries Temporais de Treinamento e Validação já criadas
treinamento_ts
validacao_ts
tam_amostra_teste
tam_amostra_treinamento

#plota o grafico da serie temporal de treinamento e teste
plot(treinamento_ts, xlab="Data", ylab="Faturamento", xaxt="n" , ylim=c(3850000, 98500000), xlim=c(2011, 2022), bty="l")

axis(1, at=seq(2011, 2022, 1), labels=format(seq(2011, 2022,1)))

lines(validacao_ts, bty="l", col="red")

#Estima o modelo de tendÃªncia poli
modelo_tendencia_poli <- tslm(treinamento_ts ~ trend + I(trend^2))

#resumo do modelo
summary(modelo_tendencia_poli)

#Verificando resíduos

#Plotando os resíduos
plot(modelo_tendencia_poli$residuals, xlab="Data", ylab="Resíduos", ylim=c(-500, 500), bty="l")

#calcula a autocorrelaÃ§Ã£o dos resÃ­duos
Acf(modelo_tendencia_poli$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_tendencia_poli, test="LB")

#plot modelo com tendencia
plot(treinamento_ts, xlab="Tempo", ylab="Faturamento", ylim=c(3850000, 98500000), bty="l")
lines(modelo_tendencia_poli$fitted.values, lwd=2)

#projeta o modelo durante o perÃ­odo de validaÃ§Ã£o
modelo_tendencia_poli_proj <- forecast(modelo_tendencia_poli, h = tam_amostra_teste, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_tendencia_poli_proj, xlab="Tempo", ylab="Faturamento", xaxt="n" , ylim=c(3850000, 98500000), xlim=c(2011, 2022), bty="l", flty=2,main="Forecast from Polynomial regression model")

axis(1, at=seq(2011, 2022, 1), labels=format(seq(2011, 2022,1)))

lines(validacao_ts)
lines(modelo_tendencia_poli_proj$fitted, lwd=2, col="blue")

#Verifica a acuracia do modelo
accuracy(modelo_tendencia_poli_proj, validacao_ts)


##########################################################################
#3.5 Modelo de Tendência Exponencial
##########################################################################
  
#Estima o modelo de tendência exp
modelo_tendencia_exp <- tslm(treinamento_ts ~ trend, lambda=0)

#resumo do modelo
summary(modelo_tendencia_exp)

#variáveis do modelo
modelo_tendencia_exp$coefficients
modelo_tendencia_exp$residuals
modelo_tendencia_exp$fitted.values

----------------------------
  
#Verificando resíduos

#Plotando os resíduos
plot(modelo_tendencia_exp$residuals, xlab="Data", ylab="Resíduos", ylim=c(-1.2, 1.08), bty="l")

#calcula a autocorrelação dos resíduos
Acf(modelo_tendencia_exp$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_tendencia_exp, test="LB")

------------------------------
  
#plot modelo com tendencia
plot(treinamento_ts, xlab="Data", ylab="Faturamento", ylim=c(3850000, 98500000), bty="l")
lines(modelo_tendencia_exp$fitted.values, lwd=2)

#projeta o modelo durante o período de validação
modelo_tendencia_exp_proj <- forecast(modelo_tendencia_exp, h = amostra_teste, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_tendencia_exp_proj, xlab="Data", ylab="Faturamento", xaxt="n" , ylim=c(3850000, 98500000), xlim=c(2011,2020), bty="l", flty=2 ,main="Forecast from Exp regression model")

axis(1, at=seq(2011, 2020, 1), labels=format(seq(2011, 2020,1)))

lines(validacao_ts)
lines(treinamento_ts)
lines(modelo_tendencia_exp_proj$fitted, lwd=2, col="blue")


#Verifica a acuracia do modelo
accuracy(modelo_tendencia_exp$fitted.values, treinamento_ts)

#Calula o modelo naive
modelo_naive <- naive(treinamento_ts, level=0, h=amostra_teste)

treinamento_ts[90]
modelo_naive

#Verifica a acuracia do modelo naive
accuracy(modelo_naive, validacao_ts)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_naive, xlab="Data", ylab="Faturamento", xaxt="n" , ylim=c(3850000, 98500000), xlim=c(2011, 2020), bty="l", flty=2, main="Forecast from Exp regression model")

axis(1, at=seq(2011, 2020, 1), labels=format(seq(2011, 2020,1)))

lines(validacao_ts)

---------------------------
#projetar para os próximos 23 meses no futuro

#primeiramente reestimamos o modelo com todos os dados de treinamento e validacao
modelo_tendencia_exp_final <- tslm(games_ts ~ trend, lambda = 0)

#sumario do modelo
summary(modelo_tendencia_exp_final)

#projeta os próximos 23 meses do futuro
modelo_tendencia_exp_final_proj <- forecast(modelo_tendencia_exp_final, h=23, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_tendencia_exp_final_proj, xlab="Data", ylab="Faturamento", xaxt="n" ,ylim=c(3850000, 98500000), xlim=c(2011, 2022), bty="l", flty=2, main="Forecast from Exp regression model")
axis(1, at=seq(2011, 2022, 1), labels=format(seq(2011, 2022,1)))
lines(validacao_ts)
lines(treinamento_ts)
lines(modelo_tendencia_exp_final_proj$fitted, lwd=2, col="blue")

##########################################################################
#3.6 Modelo Sazonal
##########################################################################
# MODELO SAZONAL - INICIO (AMAURI)::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Plota a série temporal de cada ano de acordo com o mês
ggseasonplot(games_ts)

# Cria dummies mensais
dummies_mensais <- seasonaldummy(games_ts)
dummies_mensais
str(dummies_mensais)

# Estima o modelo de tendência linear
md_sazonal <- tslm(treinamento_ts ~ season)

# Analise do modelo
summary(md_sazonal)
typeof(md_sazonal)

# Verificanção dos resíduos
View(md_sazonal$residuals)
plot(md_sazonal$residuals, xlab="Tempo", ylab="Resíduos", ylim=c(-500, 500), bty="l")

# Calcula a autocorrelação dos resíduos
Acf(md_sazonal$residuals)

# Verificação dos resíduos - Teste de Ljung-Box
checkresiduals(md_sazonal, test="LB", plot = TRUE)
plot(md_sazonal$residuals, xlab="Tempo", ylab="Resíduos", ylim=c(-500, 500), bty="l")

# Calcula a autocorrelação dos resíduos
Acf(md_sazonal$residuals)

# Verificação dos resíduos - Teste de Ljung-Box
checkresiduals(md_sazonal, test="LB", plot = TRUE)

# Plot do modelo com sazonalidade
plot(treinamento_ts, xlab="Tempo", ylab="Faturamento", ylim=c(3850331, 98420518), bty="l")
lines(md_sazonal$fitted.values, lwd=2, col="blue")

# Projeta o modelo durante o período de validação
md_sazonal_proj <- forecast(md_sazonal, h = amostra_teste, level=0.95)

# Plot da serie temporal - Treinamento e teste
plot(md_sazonal_proj, xlab="Tempo", ylab="Faturamento", xaxt="n" , ylim=c(3850331, 98420518), xlim=c(2011, 2020), bty="l", flty=2, main="Forecast from Seasonal regression model")
# Adiciona valores (referente aos anos) no eixo X
axis(1, at=seq(2011, 2020, 1), labels=format(seq(2011, 2020, 1)))

lines(validacao_ts)
lines(md_sazonal_proj$fitted, lwd=2, col="blue")

# Valida a acuracia do modelo
accuracy(md_sazonal_proj, validacao_ts)
# MODELO SAZONAL - FIM::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


##########################################################################
#3.7 Modelo com Sazonalidade e Tendência Quadrática
##########################################################################

#Estima o modelo de tendência linear
modelo_sazonal_tend_linear <- tslm(treinamento_ts ~ season + trend + I(trend^2))

#resumo do modelo
summary(modelo_sazonal_tend_linear)

#Verificando resíduos

#Plotando os resíduos
plot(modelo_sazonal_tend_linear$residuals, xlab="Tempo", ylab="Resíduos", ylim=c(-500, 500), bty="l")

#calcula a autocorrelação dos resíduos
Acf(modelo_sazonal_tend_linear$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_sazonal_tend_linear, test="LB")

#plot modelo com sazonal_tend
plot(treinamento_ts, xlab="Tempo", ylab="Passageiros", bty="l")
lines(modelo_sazonal_tend_linear$fitted.values, lwd=2, col="blue")

#projeta o modelo durante o período de validação
modelo_sazonal_tend_linear_proj <- forecast(modelo_sazonal_tend_linear, h = tam_amostra_teste, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_sazonal_tend_linear_proj, xlab="Tempo", ylab="Faturamento", xaxt="n" , bty="l", flty=2, main="Forecast from Seasonal & Tendencia regression model")
lines(validacao_ts)
lines(modelo_sazonal_tend_linear_proj$fitted, lwd=2, col="blue")


#Verifica a acuracia do modelo
accuracy(modelo_sazonal_tend_linear_proj, validacao_ts)

################################################################################
#3.8 Modelo de Média Móvel
################################################################################

#calcula a media movel simples
ma_simples <- rollmean(Games_GFK_Brasil_ts, k=12, align="right")

#calcula a media centrada
ma_centrada <- ma(Games_GFK_Brasil_ts, order=12)

#plota as medias 
plot(Games_GFK_Brasil_ts, ylim=c(3850000, 98500000), ylab="Faturamento", xlab="Data", bty="l", xaxt="n", xlim=c(2011,2021.5))

axis(1, at=seq(2011,2021.5, 1), labels=format(seq(2011,2021.5, 1)))

lines(ma_centrada, lwd=2)

lines(ma_simples, lwd=2, lty=2)

legend(2018,98500000, c("Faturamento", "MA Centrada", "MA Simples"), lty=c(1,1,2), lwd=c(1,2,2), bty="n")

#Séries Temporais de Treinamento e Validação já criadas
treinamento_ts
validacao_ts
tam_amostra_teste
tam_amostra_treinamento

#estima o modelo de MA na base de treinamento
ma_simples <- rollmean(treinamento_ts, k=12, align="right")

#obtem a média da última janela movel de 12 meses para projeção
ultima_ma <- tail(ma_simples, 1)

#cria uma projeção que é a repeticao da ultima media da janela para o periodo de validacao
ma_simples_proj <- ts(rep(ultima_ma, tam_amostra_teste), start=c(2011, tam_amostra_treinamento+1), end = c(2011, tam_amostra_treinamento + tam_amostra_teste), freq=12)

#plota o grafico da projecao
plot(treinamento_ts, ylim=c(3850000, 98500000), ylab="Faturamento", xlab="Data", bty="l", xaxt="n", xlim=c(2011,2021.5))

axis(1, at=seq(2011, 2022, 1), labels=format(seq(2011, 2022, 1)))

lines(ma_simples, lwd=2, col="blue")

lines(ma_simples_proj, lwd=2, lty=2, col="blue")

lines(validacao_ts)

#valida a precisao da estimacao no periodo de treinamento
accuracy(ma_simples, treinamento_ts)

#valida a precisao da estimacao no periodo de validacao
accuracy(ma_simples_proj, validacao_ts)

#Plotando os resíduos
plot(treinamento_ts-ma_simples, xlab="Tempo", ylab="ResÃ­duos", ylim=c(-500, 500), bty="l")

#calcula a autocorrelação dos resíduos
Acf(treinamento_ts-ma_simples)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(treinamento_ts-ma_simples, test="LB")



################################################################################
#3.9 Modelo de Suavização sem tendência e sazonalidade
################################################################################

#estima o modelo de suavizacao na base de treinamento
modelo_ses <- ets(treinamento_ts, model = "ANN")

summary(modelo_ses)

#projeta os proximos 23 meses
modelo_ses_proj <- forecast(modelo_ses, h=amostra_teste, level=0.95)

#plota o grafico da projecao
plot(modelo_ses_proj, ylim=c(3850000, 98500000),ylab="Faturamento", xlab="Data", bty="l", xaxt="n", xlim=c(2011,2020.25), flty=2)

axis(1, at=seq(2011, 2020.25, 1), labels=format(seq(2011, 2020.25,1)))

lines(modelo_ses$fitted, lwd=2, col="blue")

lines(validacao_ts)

#valida precisao
accuracy(modelo_ses_proj,games_ts)


#Verificando resíduos

#Plotando os resíduos
plot(modelo_ses$residuals, xlab="Data", ylab="Resíduos", ylim=c(-45000000,37000000), bty="l")

summary(modelo_ses_proj$residuals)

#calcula a autocorrelação dos resíduos
Acf(modelo_ses$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_ses, test="LB")

-------------------------------------------------------
  
#Preparar projecao

#primeiramente reestimamos o modelo com todos os dados de treinamento e validacao
modelo_ses_final <- ets(games_ts, model = "ANN")

#sumario do modelo
summary(modelo_ses_final)

#projeta os próximos 23 meses do futuro
modelo_ses_final_proj <- forecast(modelo_ses_final, h=23, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_ses_final_proj, xlab="Data", ylab="Faturamento", ylim=c(3850000, 98500000), xlim=c(2011, 2022.5), bty="l", flty=2, main="Forecast from ETS (A,N,N)")

axis(1, at=seq(2011, 2022.5, 1), labels=format(seq(2011, 2022.5,1)))

lines(modelo_ses_final_proj$fitted, lwd=2, col="blue")

############################################################################################
#3.10 Modelo de Suavização Exponencial com tendência Multiplicativa(MNM)
############################################################################################
# MODELO DE SUAVIZAÇÃO EXPONENCIAL COM TENDÊNCIA MULTIPLICATIVA(MNM) - INÍCIO (AMAURI)::::::::::::::
# Estima o modelo de suavizacao  - Treinamento
modelo_ses <- ets(treinamento_ts, model = "MNM")
summary(modelo_ses)

# Projeta os próximos 12 meses
modelo_ses_proj <- forecast(modelo_ses, h=amostra_teste, level=0.95)

# Plot do grafica da projecao
plot(modelo_ses_proj, ylim=c(3850331, 98420518), ylab="Faturamento Pre-processado", xlab="Tempo", bty="l", xaxt="n", xlim=c(2011,2020), flty=2)
axis(1, at=seq(2011, 2020, 1), labels=format(seq(2011, 2020, 1)))
lines(modelo_ses$fitted, lwd=2, col="blue")
lines(validacao_ts)

# Valida a acuracia do modelo
accuracy(modelo_ses_proj, games_ts)

# Verificando resíduos
plot(modelo_ses$residuals, xlab="Tempo", ylab="Resíduos", ylim=c(-500, 500), bty="l")

# Calculo da autocorrelação dos resíduos
Acf(modelo_ses$residuals)

# Verificação dos resíduos - Teste de Ljung-Box
checkresiduals(modelo_ses, test="LB")
# MODELO DE SUAVIZAÇÃO EXPONENCIAL COM TENDÊNCIA MULTIPLICATIVA(MNM) - FIM::::::::::::::::::::::::::

############################################################################################
#3.11 Modelo de Suavização com Tendência e Sazonalidade Aditiva
############################################################################################

#estima o modelo de suavizacao na base de treinamento
modelo_ses <- ets(treinamento_ts, model = "AAA")

#resumo modelo
summary(modelo_ses)

#projeta os proximos 12 meses
modelo_ses_proj <- forecast(modelo_ses, h=tam_amostra_teste, level=0.95)

#plota o grafica da projecao
plot(modelo_ses_proj, ylim=c(3850331, 98420518), ylab="Faturamento Pre-processado", xlab="Tempo", bty="l", xaxt="n", xlim=c(2011,2020), flty=2)
axis(1, at=seq(2011, 2020, 1), labels=format(seq(2011, 2020, 1)))
lines(modelo_ses$fitted, lwd=2, col="blue")
lines(validacao_ts)

#verifica precisao
accuracy(modelo_ses_proj, validacao_ts)

#calcula a autocorrelaÃ§Ã£o dos resÃ­duos
Acf(modelo_ses$residuals)

#verifica os resÃ­duos com teste de Ljung-Box
checkresiduals(modelo_ses, test="LB")


############################################################################################
#3.12 Modelo Exponencial com Tendência Aditiva e Sazonalidade Multiplicativa
############################################################################################

#estima o modelo de suavizacao na base de treinamento
modelo_ses <- ets(treinamento_ts, model = "MAM")

#resumo modelo
summary(modelo_ses)

#projeta os proximos 12 meses
modelo_ses_proj <- forecast(modelo_ses, h=tam_amostra_teste, level=0.95)

#plota o grafica da projecao
plot(modelo_ses_proj, ylim=c(3850000, 98500000), ylab="Faturamento", xlab="Data", bty="l", xaxt="n", xlim=c(2011, 2022), flty=2)

axis(1, at=seq(2011, 2022, 1), labels=format(seq(2011, 2022, 1)))

lines(modelo_ses$fitted, lwd=2, col="blue")

lines(validacao_ts)

#verifica precisao
accuracy(modelo_ses_proj, validacao_ts)

#calcula a autocorrelaÃ§Ã£o dos resÃ­duos
Acf(modelo_ses$residuals)

#verifica os resÃ­duos com teste de Ljung-Box
checkresiduals(modelo_ses, test="LB")

#Preparar projecao

#primeiramente reestimamos o modelo com todos os dados de treinamento e validacao
modelo_ses_final <- ets(Games_GFK_Brasil_ts, model = "MAM")

#sumario do modelo
summary(modelo_ses_final)

#projeta os prÃ³ximos 36 meses do futuro
modelo_ses_final_proj <- forecast(modelo_ses_final, h=36, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_ses_final_proj, xlab="Data", ylab="Faturamento", ylim=c(-30000000, 98500000), xlim=c(2011, 2023), bty="l", flty=2, main="Série com Tendência Aditiva e Sazonalidade Multiplicativa")
axis(1, at=seq(2011, 2023, 1), labels=format(seq(2011, 2023,1)))
lines(modelo_ses_final_proj$fitted, lwd=2, col="blue")


############################################################################################
#3.13 Modelo de Suavização com Algoritmo de Otimização
############################################################################################

#estima o modelo de suavizacao na base de treinamento
modelo_ses <- ets(treinamento_ts, model = "ZZZ", restrict = FALSE, allow.multiplicative.trend = TRUE)

#resumo modelo
summary(modelo_ses)

#projeta os proximos 23 meses
modelo_ses_proj <- forecast(modelo_ses, h=amostra_teste, level=0.95)

#plota o grafica da projecao
plot(modelo_ses_proj, ylim=c(3850000, 98500000), ylab="Faturamento", xlab="Data", bty="l", xaxt="n", xlim=c(2011,2020.5), flty=2)

axis(1, at=seq(2011, 2020.5, 1), labels=format(seq(2011, 2020.5,1)))

lines(modelo_ses$fitted, lwd=2, col="blue")

lines(validacao_ts)

#verifica precisao
accuracy(modelo_ses_proj, validacao_ts)


#calcula a autocorrelação dos resíduos
Acf(modelo_ses$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_ses, test="LB")

############################################################################################
#3.14 Modelo SARIMA
############################################################################################

#Modelo Arima
Modelo_ARIMA <- auto.arima(treinamento_ts, stepwise=FALSE, approximation = FALSE)


#resumo modelo
summary(Modelo_ARIMA)

#projeta os proximos 12 meses
modelo_ARIMA_proj <- forecast(Modelo_ARIMA, h=tam_amostra_teste, level=0.95)

#plota o grafica da projecao

plot(modelo_ARIMA_proj, ylim=c(3850331, 98420518), ylab="Faturamento Pre-processado", xlab="Tempo", bty="l", xaxt="n", xlim=c(2011,2020), flty=2)
axis(1, at=seq(2011, 2020, 1), labels=format(seq(2011, 2020, 1)))
lines(modelo_ses$fitted, lwd=2, col="blue")
lines(validacao_ts)


lines(validacao_ts)

#verifica precisao
accuracy(modelo_ARIMA_proj, validacao_ts)

checkresiduals(modelo_ARIMA_proj, test="LB")


############################################################################################
#4. Projeção Modelo Final
############################################################################################

#estima o modelo de suavizacao na base completa
modelo_sazonal_tend_linear_final <- tslm(Games_GFK_Brasil_ts ~ season + trend + I(trend^2))


#resumo modelo
summary(modelo_sazonal_tend_linear_final)

#projeta os proximos 24 meses
modelo_sazonal_tend_linear_final_proj <- forecast(modelo_sazonal_tend_linear_final, h=24, level=0.95)

forecast(modelo_sazonal_tend_linear_final, h=24, level=0.95)

#plota o grafico da projecao
plot(modelo_sazonal_tend_linear_final_proj, ylab="Faturamento Pre-processado", xlab="Tempo", bty="l", xaxt="n", xlim=c(2011,2023), flty=2)
axis(1, at=seq(2011, 2023, 1), labels=format(seq(2011, 2023, 1)))
lines(modelo_sazonal_tend_linear_final$fitted, lwd=2, col="blue")

