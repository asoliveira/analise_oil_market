#================================================
# Preparando o Ambiente
#================================================

#Descobrindo os parâmetros do modelo ARIMA.
# 


#================================================
# Preparando o Ambiente
#================================================

library("ggplot2")

# Colocando mais de um plot em uma grid
library("gridExtra")

# Somente vou usar esta biblioteca para fazer previzões
library("forecast")
# 
library("tseries")

#Importando o arquivo de dados
opec_basket <- read.csv2("data_05_11_16.csv", header = T, sep=",", dec=".",
                         colClasses = c("Date", "numeric"))

# Talvez eu retire esta linha
opec_ts <- read.irts("data_05_11_16.csv", header = T,
                         sep=",", dec=".", format = "%Y-%m-%d")

# Talvez eu retire esta linha
lastopec_ts <- read.csv2("data_09_11_16.csv", header = T, sep=",", dec=".",
                         colClasses = c("Date", "numeric"))

# Estou recebendo um erro pelo R não reconhecer este dado como timeseries
#tsdiag(opec_ts)
# Verificando a análise da necessidade de realizar uma diferença nos dados
plot(opec_ts, main="Preço da Cesta OPEC", xlab="Data", ylab="Valor em USD/Barril")

hist(opec_ts$value, main="Frequência do Preço da Cesta OPEC", 
     xlab="Valor em USD/Barril", ylab="Frequência", c="blue", breaks=40)


#================================================
# Primeira etapa analisando os resíduos
#================================================
## Quando analisando os resídos.
opec_d0 <-arima(opec_basket$Value, c(0,0,0))

### Neste momento olha o ACF (Auto correlation)
### Verifica se parece possuir uma distribuição normal?
tsdiag(opec_d0)
opec_d0
### Verifica se parece possuir uma distribuição normal?
hist(opec_d0$residuals, breaks=40, main="Frequência do resíduo", 
     sub="Resíduo ARIMA(0,0,0)", xlab="", ylab="", c="gray50")

## Tentando 
opec_d1 <-arima(opec_basket$Value, c(0,1,0))
tsdiag(opec_d1)
### Parece possuir uma distribuição normal?
hist(opec_d1$residuals, breaks=40, main="Frequência do resíduo", 
     sub="Resíduo ARIMA(0,1,0)", xlab="", ylab="", c="gray50")

## Algumas avaliações
## Quando utilizando o modelo AR(1) a raiz NÃO É UNITÁRIA
## indicando uma série explosiva. O que reforça a necessidade da diferença. 
opec_r1 <-arima(opec_basket$Value, c(1,0,0))
opec_r1


#================================================
# Passando para segunda etapa AR e MA coeficiente
#================================================

# Plotando os gráficos ACF e PCF 
par(mfrow=c(1,2))
# acf(diff(opec_basket$Value), main="ACF")
# pacf(diff(opec_basket$Value),main="PAF")
gacf <- ggAcf(diff(opec_ts$value), main="ACF")
gpacf <- ggPacf(diff(opec_ts$value), main="PACF")
grid.arrange(gacf, gpacf, ncol=2)

# Modelo Indicado
opec_arima <- Arima(lastopec_ts$Value[1:3558], c(1,1,1), include.drift = T)
opec_arima

#================================================
# Análise do Modelo
#================================================
par(mfrow=c(1,1))
opec_arima
plot.Arima(opec_arima)
tsdiag(opec_arima)
hist(opec_arima$residuals, breaks=40, main="Frequência do resíduo", 
     sub="Resíduo ARIMA(1,1,1)", xlab="", ylab="", c="gray50")

adf.test(opec_basket$Value)
adf.test(diff(opec_basket$Value))
kpss.test(diff(opec_basket$Value))

#================================================
# Realizando previsões
#================================================
opec_f <- forecast(opec_arima, h=15)

vat <- seq(from=1, to=length(lastopec_ts$Value), by=10)
vlabel <- opec_basket$Date[vat]
plot(opec_f, include=40,type="b", col="red", xaxt="n")
axis(side=1, at=vat,labels=vlabel)
lines(lastopec_ts$Value[1:3597], col="green", type="p")
#grid (14,7, lty = 6, col = "cornsilk2") 
# abline(h=vat[(length(vat)-39):length(vat)], v=c(40,45,50,55), col="gray", lty=3)



