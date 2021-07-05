##### Code for running the model for Macroeconometrics paper
#Joao Macosso

#
# install.packages("AER")
# install.packages("forecast")
# install.packages("vars")
# install.packages("mFilter")
# install.packages("TSstudio")

#import librabries

library(quantmod)
library(tidyverse)
library(zoo)
library(xts)
library(AER)
library(dynlm)
library(forecast)
library(stargazer)
library(scales)
library(urca)

library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library("urca")
library(stargazer)
library(lubridate)

options(digits=7)
options(scipen = 999)

setwd("C:\\Users\\User\\Google Drive\\UW WNE\\Semester 4\\Macroeconometrics")

rm(list = ls())
## Reading the data
#start date and end date
from <- "2006-04-01"
to = "2021-04-30" 

getSymbols(Symbols = c("BRL=X","^BVSP","CL=F"),
           from = from,
           to = to,
           src = "yahoo",
           adjust = TRUE,
           periodicity = "daily")

USD_BRL <- Cl(`BRL=X`)
USD_BRL <- USD_BRL[!is.na(USD_BRL)]

IBOVESPA <- Cl(`BVSP`)
IBOVESPA <- IBOVESPA[!is.na(IBOVESPA)]

CP <- Cl(`CL=F`)
CP <- CP[!is.na(CP)]


rate = read.csv("STI-20210608181625181.csv", sep = ";")
colnames(rate) <- c("date", "rate")


rate <- rate %>%
        filter(rate >=0.001)### Clearing data with values close to zero as this is due to lack of data

rate$date <- as.Date(rate$date, format = "%d/%m/%Y")
rate <- rate[!is.na(rate$date),]
colSums(is.na(rate))

rate = xts(rate, order.by = rate$date)
rate = rate$rate



model_data <- merge(merge(merge(USD_BRL, IBOVESPA, join = "inner"), CP, join = "inner"),
                    rate, join = "left")
model_data$rate <- model_data$rate*100

head(model_data)
colnames(model_data) <- c("USD_BRL", "IBOVESPA", "CP", "rate")
model_data_level <- model_data

length(model_data[is.na(model_data$USD_BRL), "USD_BRL"])
colSums(is.na(model_data))
model_data <- model_data[!is.na(model_data$rate),]

head(model_data)
model_data$dif_CP <- diff(model_data$CP, differences = 1)
model_data$dif_IBOVESPA <- diff(model_data$IBOVESPA, differences = 1)
model_data$dif_USD_BRL <- diff(model_data$USD_BRL, differences = 1)
model_data$dif_rate <- diff(model_data$rate, differences = 1)


model_data <- model_data[,c("dif_CP","dif_IBOVESPA", "dif_USD_BRL", "dif_rate")]
model_data <- model_data[!is.na(model_data$dif_CP),]
model_data <- model_data[!is.na(model_data$dif_rate),]
dif_rate <- model_data[,"dif_rate"]
model_data <- model_data[,c("dif_CP","dif_IBOVESPA", "dif_USD_BRL", "dif_rate")]



plot(model_data_level$USD_BRL,
     main = "USD_BRL", grid.col = NA,
     col = "blue")

ts_plot(model_data_level$USD_BRL,)
plot(model_data_level$IBOVESPA,
     main = "IBOVESPA", grid.col = NA,
     col = "blue")

plot(model_data_level$CP,
     main = "Crude Oil Price in USD", grid.col = NA,
     col = "blue")

ts.plot(model_data_level$rate)

plot(model_data_level$rate,
     main = paste("Average Interest rate financial institutions"), grid.col = NA,
     col = "blue")


model_data_level$day <- wday(model_data_level)

###Test for stationarity
pp.test(CP)
pp.test(model_data$USD_BRL)
pp.test(model_data$IBOVESPA)
pp.test(model_data$CP)
pp.test(model_data$rate)

adf.test(model_data$USD_BRL)
adf.test(model_data$IBOVESPA)
adf.test(model_data$CP)
adf.test(model_data$rate)






colSums(is.na(model_data))

plot(model_data$dif_CP,
     main = "First difference Crude Oil Price", grid.col = NA,
     col = "blue")
plot(model_data$dif_IBOVESPA,
     main = "First difference IBOVESPA", grid.col = NA,
     col = "blue")

plot(model_data$dif_USD_BRL,
     main = "First difference dif_USD_BRL", grid.col = NA,
     col = "blue")
plot(model_data$dif_rate,
     main = "First difference Average Interest rate", grid.col = NA,
     col = "blue")

pp.test(model_data$dif_CP)
pp.test(model_data$dif_IBOVESPA)
pp.test(model_data$dif_USD_BRL)
pp.test(model_data$dif_rate)

adf.test(model_data$dif_CP)
adf.test(model_data$dif_IBOVESPA)
adf.test(model_data$dif_USD_BRL)
adf.test(model_data$dif_rate)

adf.test(model_data$dif_rate)

lagselect <- VARselect(model_data, lag.max = 5, type = "const")
lagselect$selection

apply(model_data, 2, length)
p =10

unrest_VAR <- VAR(model_data, p = p, type = "const", season = NULL) 
summary(unrest_VAR)


Serial1 <- serial.test(unrest_VAR, type = "PT.asymptotic")
Serial1

Arch1 <- arch.test(unrest_VAR, lags.multi = p, multivariate.only = TRUE)
Arch1

Norm1 <- normality.test(unrest_VAR, multivariate.only = TRUE)
Norm1


Stability1 <- stability(unrest_VAR, type = "OLS-CUSUM")
plot(Stability1)


#testing for granger causaity
GrangerUSD_BRL<- causality(unrest_VAR, cause = "dif_USD_BRL")
GrangerUSD_BRL
GrangerIBOVESPA<- causality(unrest_VAR, cause = "dif_IBOVESPA")
GrangerIBOVESPA
GrangerCP<- causality(unrest_VAR, cause = "dif_CP")
GrangerCP
GrangerRate<- causality(unrest_VAR, cause = "dif_rate")
GrangerRate



joahnsen_coint <- ca.jo(model_data, type = "trace", K=p, ecdet="none", spec="longrun")
summary(joahnsen_coint)
#########
#Inpulse reponse functions

n_head <- 20
heads <- c(1:9)
USD_BRLirf <- irf(unrest_VAR, impulse = "dif_USD_BRL",
                  response = "dif_USD_BRL", n.ahead = n_head ,
                  boot = TRUE)
plot(USD_BRLirf, ylab = "dif_USD_BRL",
     main = "USD_BRL's shock to USD_BRL")


USD_BRL_IBOVESPA_irf <- irf(unrest_VAR, impulse = "dif_USD_BRL",
                  response = "dif_IBOVESPA", n.ahead = n_head ,
                  boot = TRUE)
plot(USD_BRL_IBOVESPA_irf, ylab = "dif_USD_BRL",
     main = "USD_BRL's shock to IBOVESPA")


USD_BRL_CP_irf <- irf(unrest_VAR, impulse = "dif_USD_BRL",
                            response = "dif_CP", n.ahead = n_head ,
                            boot = TRUE)
plot(USD_BRL_CP_irf, ylab = "dif_USD_BRL",
     main = "USD_BRL's shock to CP")

CP_CP_irf <- irf(unrest_VAR, impulse = "dif_CP",
                      response = "dif_CP", n.ahead = n_head ,
                      boot = TRUE)
plot(CP_CP_irf, ylab = "CP",
     main = "CP's shock to CP")


CP_USD_BRL_irf <- irf(unrest_VAR, impulse = "dif_CP",
                 response = "dif_USD_BRL", n.ahead = n_head ,
                 boot = TRUE)
plot(CP_USD_BRL_irf, ylab = "CP",
     main = "CP's shock to USD_BRL")

CP_IBOVESPA_irf <- irf(unrest_VAR, impulse = "dif_CP",
                      response = "dif_IBOVESPA", n.ahead = n_head ,
                      boot = TRUE)
plot(CP_IBOVESPA_irf, ylab = "CP",
     main = "CP's shock to IBOVESPA")

Rate_IBOVESPA_irf <- irf(unrest_VAR, impulse = "dif_rate",
                       response = "dif_IBOVESPA", n.ahead = n_head ,
                       boot = TRUE)
plot(Rate_IBOVESPA_irf, ylab = "Interest rate",
     main = "Interest rate shock to IBOVESPA")

CP_Rate_irf <- irf(unrest_VAR, impulse = "dif_CP",
                         response = "dif_rate", n.ahead = n_head ,
                         boot = TRUE)
plot(CP_Rate_irf, ylab = "CP",
     main = "Interest CP shock to rate")

stationarity <- function(df, x){
        # Define empty object
        df <- NULL
        # The way to write "i in ..."
        for (i in 1 : length(x)){
                # We change column names as x[1] = "e" and so on
                out1 <- ur.df(model_data[,x[i]], type = "drift", selectlags = "BIC")
                out2 <- ur.df(model_data[,x[i]], type = "trend", selectlags = "BIC")
                # rbind will collect rows after they are combined for each x[i]
                df <- rbind(df,
                            # cbind will work on inner part, when you combine 
                            # the 8 resulting numbers for current x[i]
                            cbind(out1@teststat[1],
                                  out1@cval[1,1],
                                  out1@cval[1,2],
                                  out1@cval[1,3],
                                  out2@teststat[1],
                                  out2@cval[1,1],
                                  out2@cval[1,2],
                                  out2@cval[1,3]))
        }
        # assign column names
        colnames(df) <- c("T-test(drift)", "1%", "5%", "10%",
                          "T-test(trend)", "1%", "5%", "10%")
        # assign row names
        rownames(df) <- x
        # result
        print(df)
        
}

stationarity(model_data, c("dif_CP","dif_IBOVESPA", "dif_USD_BRL", "dif_rate"))



lagselect <- VARselect(model_data, lag.max = 50, type = "const")

lagselect$selection
var_lags <- as.matrix(lagselect$criteria)
var_lags <- as.data.frame(t(var_lags))
var_lags$Lag <- index(var_lags)

var_lags %>%
        summarise(AIC = Lag[which.min(`AIC(n)`)],
                  HQ = Lag[which.min(`HQ(n)`)],
                  SC = Lag[which.min(`SC(n)`)],
                  FPE =  Lag[which.min(`FPE(n)`)])


var_lags$Lag[min(var_lags$`AIC(n)`)]

min_AIC <- min(var_lags$`AIC(n)`)
min_HQ <- min(var_lags$`HQ(n)`)
min_SC <- min(var_lags$`SC(n)`)
min_FPE <- min(var_lags$`FPE(n)`)

var_lags <- var_lags %>%
       mutate(AIC = ifelse(`AIC(n)`== min(min_AIC), paste(round(`AIC(n)`,6),"***"), round(`AIC(n)`,6)),
              HQ = ifelse(`HQ(n)`== min(min_HQ), paste(round(`HQ(n)`,6),"***"), round(`HQ(n)`,6)),
              SC = ifelse(`SC(n)`== min(min_SC), paste(round(`SC(n)`,6),"***"), round(`SC(n)`,6)),
              FPE = ifelse(`FPE(n)`== min(min_FPE), paste(round(`FPE(n)`,3),"***"), round(`FPE(n)`,3))) %>%
        dplyr::select("AIC", "HQ", "SC", "FPE", "Lag")

write.csv(var_lags, "var_lags_selection.csv", row.names = F)



