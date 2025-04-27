packages <- c("shiny","esquisse", "stats", "ggplot2", "rmarkdown", "tidyverse", 
              "tseries", "dplyr", "forecast", "tsibble")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))

library(tidyverse) 
library(dplyr)     
library(tsibble)  
library(fpp3)       
library(stats)
library(esquisse)
library(rmarkdown)
library(ggplot2)
library(knitr)
library(tinytex)
library(shiny)
library(forecast)

bitcoin <- read.csv("C:/Users/dasil/OneDrive/Ambiente de Trabalho/Quarto_Book_ou_Report/coin_Bitcoin.csv")
print(head(bitcoin))
summary(bitcoin)

#Transformação da da variável "Date" em numérica em vez de caracter 
bitcoinData <- bitcoin[,c("Date","Close")]
bitcoinData$Date <- as.Date(bitcoinData$Date)
str(bitcoinData)

#Transformação da dataframe bitcoin numa tsibble, numa série temporal. 
bitcoinTimeserie <- tsibble(bitcoinData, index = Date)
str(bitcoinTimeserie)

sum(is.na(bitcoinTimeserie))

#Representação da tsibble
plot(bitcoinTimeserie, col = "red", main = "Série temporal da Bitcoin", 
     type = "l", xlab = "Ano", ylab = "Dólares") 

bitcoinTimeserieUpTo2020<- bitcoinTimeserie |>
  tsibble::filter_index("2013-04-29" ~ "2019-12-31") |>
  print()

bitcoinTimeserieFrom2020<- bitcoinTimeserie |>
  tsibble::filter_index("2020-01-01" ~ "2021-07-06") |>
  print()

library(ggplot2)

ggplot(bitcoinTimeserieUpTo2020) +
  aes(x = Date, y = Close) +
  geom_line(colour = "#FF8C00") +
  labs(x = "Data (Ano)", 
       y = "Dólares", title = "Valor da Bitcoin até final de 2019") +
  theme_minimal()

ggplot(bitcoinTimeserieFrom2020) +
  aes(x = Date, y = Close) +
  geom_line(colour = "#56026A") +
  labs(x = "Data (Mês, Ano)", 
       y = "Dólares", title = "Valor da Bitcoin após início de 2020") +
  theme_minimal()

library(forecast)
trendBitcoin<-ma(bitcoinTimeserieFromMid2017, order = 30)
summary(trendBitcoin)


#Decomposição STL da série temporal
trendBitcoin<-ma(bitcoinTimeserieFrom2020, order = 30)
summary(trendBitcoin)
plot.ts(trendBitcoin)

library(tseries)
#Decomposição STL
bitcoinTimeserieFrom2020 |> 
  model(STL(Close ~ trend(window = 12) + season(), robust = TRUE)) |> 
  components() |> 
  autoplot() +
  labs(title = "Decomposição STL do Valor de Fecho da Bitcoin desde 2020")


bitcoinTimeserie2021<- bitcoinTimeserie |>
  tsibble::filter_index("2021-01-01" ~ "2021-07-06") |>
  print()

library(tseries)
#Decomposição STL
bitcoinTimeserie2021 |> 
  model(STL(Close ~ trend(window = 12) + season(), robust = TRUE)) |> 
  components() |> 
  autoplot() +
  labs(title = "Decomposição STL do Valor de Fecho da Bitcoin 2021")

#Modelo ETS e consequente previsão
modeloETSBitcoin <- bitcoinTimeserie2021 |> 
  model(ETS(Close ~ error("M") + trend("N") + season("N")))  

report(modeloETSBitcoin)
#alpha com valor de ~90%

previsaoETSBitcoin <- modeloETSBitcoin |> forecast(h = 10)
previsaoETSBitcoin

previsaoETSBitcoin |> autoplot(bitcoinTimeserie2021) +             
  geom_line(aes(y = .fitted), col="red",      
            data = augment(modeloETSBitcoin)) +            
  labs(x = "Data (Mês)", y="Dólares", title="Previsão ETS do Valor de Fecho da Bitcoin")



#Modelo ARIMA 
adf.test(na.omit(bitcoinTimeserieFromMid2017$Close))

bitcoinTimeserie2021<-na.omit(bitcoinTimeserie2021)
sum(is.na(bitcoinTimeserie2021))


bitcoinDiff <- diff(bitcoinTimeserie2021$Close)

plot(bitcoinDiff, type='l', ylab='Valor de Fecho da Bitcoin (em Dólares)', main = "Gráfico da primeira Diferenciação da Série Temporal Bitcoin")

adf.test(bitcoinDiff)

bitcoinDiff2 <- diff(bitcoinDiff)
plot(bitcoinDiff2, type='l', ylab='Valor de Fecho da Bitcoin (em Dólares)', 
     main = "Gráfico da segunda Diferenciação da Série Temporal Bitcoin")

par(mfrow=c(1,2))
acf(bitcoinDiff2, lag.max = 12, main="ACF da Série temporal da Bitcoin")
pacf(bitcoinDiff2, lag.max = 12, main="PACF da Série temporal da Bitcoin")

modeloArima <- bitcoinTimeserie2021 |>
  model(ARIMA(Close ~ pdq(0,0,4)))


report(modeloArima)

bitcoinArima <- bitcoinTimeserie2021 |> 
  model(ARIMA(Close)) 
report(bitcoinArima)                


bitcoinArima |> forecast(h=10) |> 
  autoplot(bitcoinTimeserie2021, show_gap = FALSE) + 
  geom_line(aes(y = .fitted), col="red", data = augment(bitcoinArima)) +
  labs(x = "Data (Mês)", y = "Dólares", title = "Previsão ARIMA do Valor de Fecho da Bitcoin")
####################################################














