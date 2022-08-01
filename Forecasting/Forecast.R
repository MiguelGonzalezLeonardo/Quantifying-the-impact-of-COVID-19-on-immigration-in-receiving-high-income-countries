
setwd("D:\\IIASA\\QUANTMIG\\Immigration_COVID\\Repository\\Forecasting")


#Libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(dplyr)
library(tseries)
library(astsa)
library(forecast)
library(Rmisc)


########################
########  Data  ########
########################

#Data
data <- read.csv(file = 'Immigration.csv', sep = ',')

#Melt
data <- melt(data, id=c("Country"))
colnames(data) <- c("Country","Year", "Value")

data$Year <- recode(data$Year,"X2012"= 2012,
                    "X2013"= 2013,
                    "X2014"= 2014,
                    "X2015"= 2015,
                    "X2016"= 2016,
                    "X2017"= 2017,
                    "X2018"= 2018,
                    "X2019"= 2019,
                    "X2020"= 2020)

#Remove 2020
data2 <- subset(data, Year < 2020)

#Observed 2020
data_2020 <- subset(data, Year == 2020)

#Data countries
Austria <- subset(data2, Country == "Austria")
Denmark <- subset(data2, Country == "Denmark")
Germany <- subset(data2, Country == "Germany")
Finland <- subset(data2, Country == "Finland")
France <- subset(data2, Country == "France")
Ireland <- subset(data2, Country == "Ireland")
Italy <- subset(data2, Country == "Italy")
Netherlands <- subset(data2, Country == "Netherlands")
Norway <- subset(data2, Country == "Norway")
Spain <- subset(data2, Country == "Spain")
Sweden <- subset(data2, Country == "Sweden")
Switzerland <- subset(data2, Country == "Switzerland")
Australia <- subset(data2, Country == "Australia")
Canada <- subset(data2, Country == "Canada")
USA <- subset(data2, Country == "USA")

#Data countries 2020
Austria_2020 <- subset(data_2020, Country == "Austria")
Denmark_2020 <- subset(data_2020, Country == "Denmark")
Germany_2020 <- subset(data_2020, Country == "Germany")
France_2020 <- subset(data_2020, Country == "France")
Finland_2020 <- subset(data_2020, Country == "Finland")
Ireland_2020 <- subset(data_2020, Country == "Ireland")
Italy_2020 <- subset(data_2020, Country == "Italy")
Netherlands_2020 <- subset(data_2020, Country == "Netherlands")
Norway_2020 <- subset(data_2020, Country == "Norway")
Finlad_2020 <- subset(data_2020, Country == "Spain")
Spain_2020 <- subset(data_2020, Country == "Spain")
Sweden_2020 <- subset(data_2020, Country == "Sweden")
Switzerland_2020 <- subset(data_2020, Country == "Switzerland")
Australia_2020 <- subset(data_2020, Country == "Australia")
Canada_2020 <- subset(data_2020, Country == "Canada")
USA_2020 <- subset(data_2020, Country == "USA")



############################
########  Forecast  ########
############################


#### Austria ####

auto.arima(Austria$Value)
AR_Austria <- arima(Austria$Value, order=c(0,0,0))
print(AR_Austria)
residuals(AR_Austria)

#Use predict to make 1-step through x-step forecasts
predict(AR_Austria, n.ahead = 2)

# Run to plot series plus the forecast and 95% prediction intervals
ts.plot(Austria$Value, xlim = c(1, 10), ylim = c(50000, 250000))
AR_forecast <- predict(AR_Austria, n.ahead = 2)$pred
AR_forecast_se <- predict(AR_Austria, n.ahead = 2)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(x=9, y=Austria_2020$Value, type = "p", col = 3)

#Fields results
F_Austria <- data.frame(AR_forecast)
F_Austria$Name <- "Austria"
Year <- data.frame(c(2020, 2021))
F_Austria <- cbind(F_Austria, Year)

CI_Austria <- data.frame(AR_forecast_se)
CI_Austria$Name <- "Austria"
CI_Austria <- cbind(CI_Austria, Year)


#### Denmark ####

auto.arima(Denmark$Value)
AR_Denmark <- arima(Denmark$Value, order=c(0,0,0))
print(AR_Denmark)
residuals(AR_Denmark)

#Use predict to make 1-step through x-step forecasts
predict(AR_Denmark, n.ahead = 2)

# Run to plot series plus the forecast and 95% prediction intervals
ts.plot(Denmark$Value, xlim = c(1, 10), ylim = c(0, 150000))
AR_forecast <- predict(AR_Denmark, n.ahead = 2)$pred
AR_forecast_se <- predict(AR_Denmark, n.ahead = 2)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(x=9, y=Denmark_2020$Value, type = "p", col = 3)

#Fields results
F_Denmark <- data.frame(AR_forecast)
F_Denmark$Name <- "Denmark"
Year <- data.frame(c(2020, 2021))
F_Denmark <- cbind(F_Denmark, Year)

CI_Denmark <- data.frame(AR_forecast_se)
CI_Denmark$Name <- "Denmark"
CI_Denmark <- cbind(CI_Denmark, Year)


#### Germany ####

auto.arima(Germany$Value)
AR_Germany <- arima(Germany$Value, order=c(0,0,0))
print(AR_Germany)
residuals(AR_Germany)

#Use predict to make 1-step through x-step forecasts
predict(AR_Germany, n.ahead = 2)

# Run to plot series plus the forecast and 95% prediction intervals
ts.plot(Germany$Value, xlim = c(1, 10), ylim = c(200000, 1600000))
AR_forecast <- predict(AR_Germany, n.ahead = 2)$pred
AR_forecast_se <- predict(AR_Germany, n.ahead = 2)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(x=9, y=Germany_2020$Value, type = "p", col = 3)

#Fields results
F_Germany <- data.frame(AR_forecast)
F_Germany$Name <- "Germany"
Year <- data.frame(c(2020, 2021))
F_Germany <- cbind(F_Germany, Year)

CI_Germany <- data.frame(AR_forecast_se)
CI_Germany$Name <- "Germany"
CI_Germany <- cbind(CI_Germany, Year)


#### Finland ####

auto.arima(Finland$Value)
AR_Finland <- arima(Finland$Value, order=c(0,0,0))
print(AR_Finland)
residuals(AR_Finland)

#Use predict to make 1-step through x-step forecasts
predict(AR_Finland, n.ahead = 2)

# Run to plot series plus the forecast and 95% prediction intervals
ts.plot(Finland$Value, xlim = c(1, 10), ylim = c(0, 50000))
AR_forecast <- predict(AR_Finland, n.ahead = 2)$pred
AR_forecast_se <- predict(AR_Finland, n.ahead = 2)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(x=9, y=Finland_2020$Value, type = "p", col = 3)

#Fields results
F_Finland <- data.frame(AR_forecast)
F_Finland$Name <- "Finland"
Year <- data.frame(c(2020, 2021))
F_Finland <- cbind(F_Finland, Year)

CI_Finland <- data.frame(AR_forecast_se)
CI_Finland$Name <- "Finland"
CI_Finland <- cbind(CI_Finland, Year)


#### France ####

auto.arima(France$Value)
AR_France <- arima(France$Value, order=c(0,1,0))
print(AR_France)
residuals(AR_France)

#Use predict to make 1-step through x-step forecasts
predict(AR_France, n.ahead = 2)

# Run to plot series plus the forecast and 95% prediction intervals
ts.plot(France$Value, xlim = c(1, 10), ylim = c(200000, 500000))
AR_forecast <- predict(AR_France, n.ahead = 2)$pred
AR_forecast_se <- predict(AR_France, n.ahead = 2)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(x=9, y=France_2020$Value, type = "p", col = 3)

#Fields results
F_France <- data.frame(AR_forecast)
F_France$Name <- "France"
Year <- data.frame(c(2020, 2021))
F_France <- cbind(F_France, Year)

CI_France <- data.frame(AR_forecast_se)
CI_France$Name <- "France"
CI_France <- cbind(CI_France, Year)


#### Ireland ####

auto.arima(Ireland$Value)
AR_Ireland <- arima(Ireland$Value, order=c(0,1,0))
print(AR_Ireland)
residuals(AR_Ireland)

#Use predict to make 1-step through x-step forecasts
predict(AR_Ireland, n.ahead = 2)

# Run to plot series plus the forecast and 95% prediction intervals
ts.plot(Ireland$Value, xlim = c(1, 10), ylim = c(0, 150000))
AR_forecast <- predict(AR_Ireland, n.ahead = 2)$pred
AR_forecast_se <- predict(AR_Ireland, n.ahead = 2)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(x=9, y=Ireland_2020$Value, type = "p", col = 3)

#Fields results
F_Ireland <- data.frame(AR_forecast)
F_Ireland$Name <- "Ireland"
Year <- data.frame(c(2020, 2021))
F_Ireland <- cbind(F_Ireland, Year)

CI_Ireland <- data.frame(AR_forecast_se)
CI_Ireland$Name <- "Ireland"
CI_Ireland <- cbind(CI_Ireland, Year)


#### Italy ####

auto.arima(Italy$Value)
AR_Italy <- arima(Italy$Value, order=c(0,0,0))
print(AR_Italy)
residuals(AR_Italy)

#Use predict to make 1-step through x-step forecasts
predict(AR_Italy, n.ahead = 2)

# Run to plot series plus the forecast and 95% prediction intervals
ts.plot(Italy$Value, xlim = c(1, 10), ylim = c(200000, 500000))
AR_forecast <- predict(AR_Italy, n.ahead = 2)$pred
AR_forecast_se <- predict(AR_Italy, n.ahead = 2)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(x=9, y=Italy_2020$Value, type = "p", col = 3)

#Fields results
F_Italy <- data.frame(AR_forecast)
F_Italy$Name <- "Italy"
Year <- data.frame(c(2020, 2021))
F_Italy <- cbind(F_Italy, Year)

CI_Italy <- data.frame(AR_forecast_se)
CI_Italy$Name <- "Italy"
CI_Italy <- cbind(CI_Italy, Year)


#### Netherlands ####

auto.arima(Netherlands$Value)
AR_Netherlands <- arima(Netherlands$Value, order=c(0,1,0))
print(AR_Netherlands)
residuals(AR_Netherlands)

#Use predict to make 1-step through x-step forecasts
predict(AR_Netherlands, n.ahead = 2)

# Run to plot series plus the forecast and 95% prediction intervals
ts.plot(Netherlands$Value, xlim = c(1, 10), ylim = c(0, 300000))
AR_forecast <- predict(AR_Netherlands, n.ahead = 2)$pred
AR_forecast_se <- predict(AR_Netherlands, n.ahead = 2)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(x=9, y=Netherlands_2020$Value, type = "p", col = 3)

#Fields results
F_Netherlands <- data.frame(AR_forecast)
F_Netherlands$Name <- "Netherlands"
Year <- data.frame(c(2020, 2021))
F_Netherlands <- cbind(F_Netherlands, Year)

CI_Netherlands <- data.frame(AR_forecast_se)
CI_Netherlands$Name <- "Netherlands"
CI_Netherlands <- cbind(CI_Netherlands, Year)


#### Norway ####

auto.arima(Norway$Value)
AR_Norway <- arima(Norway$Value, order=c(0,1,0))
print(AR_Norway)
residuals(AR_Norway)

#Use predict to make 1-step through x-step forecasts
predict(AR_Norway, n.ahead = 2)

# Run to plot series plus the forecast and 95% prediction intervals
ts.plot(Norway$Value, xlim = c(1, 10), ylim = c(0, 150000))
AR_forecast <- predict(AR_Norway, n.ahead = 2)$pred
AR_forecast_se <- predict(AR_Norway, n.ahead = 2)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(x=9, y=Norway_2020$Value, type = "p", col = 3)

#Fields results
F_Norway <- data.frame(AR_forecast)
F_Norway$Name <- "Norway"
Year <- data.frame(c(2020, 2021))
F_Norway <- cbind(F_Norway, Year)

CI_Norway <- data.frame(AR_forecast_se)
CI_Norway$Name <- "Norway"
CI_Norway <- cbind(CI_Norway, Year)


#### Spain ####

auto.arima(Spain$Value)
AR_Spain <- arima(Spain$Value, order=c(0,2,0))
print(AR_Spain)
auto.arima(Spain$Value)
residuals(AR_Spain)

#Use predict to make 1-step through x-step forecasts
predict(AR_Spain, n.ahead = 2)

# Run to plot series plus the forecast and 95% prediction intervals
ts.plot(Spain$Value, xlim = c(1, 10), ylim = c(250000, 1100000))
AR_forecast <- predict(AR_Spain, n.ahead = 2)$pred
AR_forecast_se <- predict(AR_Spain, n.ahead = 2)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(x=9, y=Spain_2020$Value, type = "p", col = 3)

#Fields results
F_Spain <- data.frame(AR_forecast)
F_Spain$Name <- "Spain"
Year <- data.frame(c(2020, 2021))
F_Spain <- cbind(F_Spain, Year)

CI_Spain <- data.frame(AR_forecast_se)
CI_Spain$Name <- "Spain"
CI_Spain <- cbind(CI_Spain, Year)


#### Sweden ####

auto.arima(Sweden$Value)
AR_Sweden <- arima(Sweden$Value, order=c(0,0,0))
print(AR_Sweden)
residuals(AR_Sweden)

#Use predict to make 1-step through x-step forecasts
predict(AR_Sweden, n.ahead = 2)

# Run to plot series plus the forecast and 95% prediction intervals
ts.plot(Sweden$Value, xlim = c(1, 10), ylim = c(0, 250000))
AR_forecast <- predict(AR_Sweden, n.ahead = 2)$pred
AR_forecast_se <- predict(AR_Sweden, n.ahead = 2)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(x=9, y=Sweden_2020$Value, type = "p", col = 3)

#Fields results
F_Sweden <- data.frame(AR_forecast)
F_Sweden$Name <- "Sweden"
Year <- data.frame(c(2020, 2021))
F_Sweden <- cbind(F_Sweden, Year)

CI_Sweden <- data.frame(AR_forecast_se)
CI_Sweden$Name <- "Sweden"
CI_Sweden <- cbind(CI_Sweden, Year)


#### Switzerland ####

auto.arima(Switzerland$Value)
AR_Switzerland <- arima(Switzerland$Value, order=c(0,1,0))
print(AR_Switzerland)
residuals(AR_Switzerland)

#Use predict to make 1-step through x-step forecasts
predict(AR_Switzerland, n.ahead = 2)

# Run to plot series plus the forecast and 95% prediction intervals
ts.plot(Switzerland$Value, xlim = c(1, 10), ylim = c(0, 250000))
AR_forecast <- predict(AR_Switzerland, n.ahead = 2)$pred
AR_forecast_se <- predict(AR_Switzerland, n.ahead = 2)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(x=9, y=Switzerland_2020$Value, type = "p", col = 3)

#Fields results
F_Switzerland <- data.frame(AR_forecast)
F_Switzerland$Name <- "Switzerland"
Year <- data.frame(c(2020, 2021))
F_Switzerland <- cbind(F_Switzerland, Year)

CI_Switzerland <- data.frame(AR_forecast_se)
CI_Switzerland$Name <- "Switzerland"
CI_Switzerland <- cbind(CI_Switzerland, Year)


#### Australia ####

auto.arima(Australia$Value)
AR_Australia <- arima(Australia$Value, order=c(0,1,0))
print(AR_Australia)
residuals(AR_Australia)

#Use predict to make 1-step through x-step forecasts
predict(AR_Australia, n.ahead = 2)

# Run to plot series plus the forecast and 95% prediction intervals
ts.plot(Australia$Value, xlim = c(1, 10), ylim = c(200000, 900000))
AR_forecast <- predict(AR_Australia, n.ahead = 2)$pred
AR_forecast_se <- predict(AR_Australia, n.ahead = 2)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(x=9, y=Australia_2020$Value, type = "p", col = 3)

#Fields results
F_Australia <- data.frame(AR_forecast)
F_Australia$Name <- "Australia"
Year <- data.frame(c(2020, 2021))
F_Australia <- cbind(F_Australia, Year)

CI_Australia <- data.frame(AR_forecast_se)
CI_Australia$Name <- "Australia"
CI_Australia <- cbind(CI_Australia, Year)


#### Canada ####

auto.arima(Canada$Value)
AR_Canada <- arima(Canada$Value, order=c(0,0,0))
print(AR_Canada)
residuals(AR_Canada)

#Use predict to make 1-step through x-step forecasts
predict(AR_Canada, n.ahead = 2)

# Run to plot series plus the forecast and 95% prediction intervals
ts.plot(Canada$Value, xlim = c(1, 10), ylim = c(100000, 400000))
AR_forecast <- predict(AR_Canada, n.ahead = 2)$pred
AR_forecast_se <- predict(AR_Canada, n.ahead = 2)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(x=9, y=Canada_2020$Value, type = "p", col = 3)

#Fields results
F_Canada <- data.frame(AR_forecast)
F_Canada$Name <- "Canada"
Year <- data.frame(c(2020, 2021))
F_Canada <- cbind(F_Canada, Year)

CI_Canada <- data.frame(AR_forecast_se)
CI_Canada$Name <- "Canada"
CI_Canada <- cbind(CI_Canada, Year)


#### USA ####

auto.arima(USA$Value)
AR_USA <- arima(USA$Value, order=c(0,0,0))
print(AR_USA)
residuals(AR_USA)

#Use predict to make 1-step through x-step forecasts
predict(AR_USA, n.ahead = 2)

# Run to plot series plus the forecast and 95% prediction intervals
ts.plot(USA$Value, xlim = c(1, 10), ylim = c(300000, 2000000))
AR_forecast <- predict(AR_USA, n.ahead = 2)$pred
AR_forecast_se <- predict(AR_USA, n.ahead = 2)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(x=9, y=USA_2020$Value, type = "p", col = 3)

#Fields results
F_USA <- data.frame(AR_forecast)
F_USA$Name <- "USA"
Year <- data.frame(c(2020, 2021))
F_USA <- cbind(F_USA, Year)

CI_USA <- data.frame(AR_forecast_se)
CI_USA$Name <- "USA"
CI_USA <- cbind(CI_USA, Year)



##########################
#### Forecast data set ###
##########################


#Forescasting values as numeric
F_Austria$AR_forecast <- as.numeric(F_Austria$AR_forecast)
F_Australia$AR_forecast <- as.numeric(F_Australia$AR_forecast)
#F_Belgium$AR_forecast <- as.numeric(F_Belgium$AR_forecast)
F_Germany$AR_forecast <- as.numeric(F_Germany$AR_forecast)
F_Denmark$AR_forecast <- as.numeric(F_Denmark$AR_forecast)
F_Finland$AR_forecast <- as.numeric(F_Finland$AR_forecast)
F_Ireland$AR_forecast <- as.numeric(F_Ireland$AR_forecast)
F_Italy$AR_forecast <- as.numeric(F_Italy$AR_forecast)
F_Netherlands$AR_forecast <- as.numeric(F_Netherlands$AR_forecast)
F_Norway$AR_forecast <- as.numeric(F_Norway$AR_forecast)
F_Spain$AR_forecast <- as.numeric(F_Spain$AR_forecast)
F_Sweden$AR_forecast <- as.numeric(F_Sweden$AR_forecast)
F_Switzerland$AR_forecast <- as.numeric(F_Switzerland$AR_forecast)
F_USA$AR_forecast <- as.numeric(F_USA$AR_forecast)

#Join countries
F_data <- rbind(F_Austria, F_Australia, F_Germany, F_Canada, F_Denmark, F_Finland, F_France, F_Ireland,
                F_Italy, F_Netherlands, F_Norway, F_Spain, F_Sweden, F_Switzerland, F_USA)

colnames(F_data) <- c("Value","Country", "Year")
F_data <- F_data[, c(2, 3, 1)]
F_data <- subset(F_data, Year == 2020)
F_data$Type <- "Forecasting 2020"


#CI values as numeric
CI_Austria$AR_forecast_se <- as.numeric(CI_Austria$AR_forecast_se)
CI_Australia$AR_forecast_se <- as.numeric(CI_Australia$AR_forecast_se)
#CI_Belgium$AR_forecast_se <- as.numeric(CI_Belgium$AR_forecast_se)
CI_Germany$AR_forecast_se <- as.numeric(CI_Germany$AR_forecast_se)
CI_Denmark$AR_forecast_se <- as.numeric(CI_Denmark$AR_forecast_se)
CI_Finland$AR_forecast_se <- as.numeric(CI_Finland$AR_forecast_se)
CI_Ireland$AR_forecast_se <- as.numeric(CI_Ireland$AR_forecast_se)
CI_Italy$AR_forecast_se <- as.numeric(CI_Italy$AR_forecast_se)
CI_Netherlands$AR_forecast_se <- as.numeric(CI_Netherlands$AR_forecast_se)
CI_Norway$AR_forecast_se <- as.numeric(CI_Norway$AR_forecast_se)
CI_Spain$AR_forecast_se <- as.numeric(CI_Spain$AR_forecast_se)
CI_Sweden$AR_forecast_se <- as.numeric(CI_Sweden$AR_forecast_se)
CI_Switzerland$AR_forecast_se <- as.numeric(CI_Switzerland$AR_forecast_se)
CI_USA$AR_forecast_se <- as.numeric(CI_USA$AR_forecast_se)

#Join countries
CI_data <- rbind(CI_Austria, CI_Australia, CI_Germany, CI_Canada, CI_Denmark, CI_Finland, CI_France, CI_Ireland,
                CI_Italy, CI_Netherlands, CI_Norway, CI_Spain, CI_Sweden, CI_Switzerland, CI_USA)

colnames(CI_data) <- c("CI","Country", "Year")
CI_data <- CI_data[, c(2, 3, 1)]
CI_data <- subset(CI_data, Year == 2020)



#### Final data set ###

#Data set first plot

#Join data 2012-19 and forecasting 2020
data2$Type <- "Observed data 2012-2019"
data2 <- subset(data2, Country != "Belgium")
data_final <- rbind(data2, F_data)

#Join CI
data_final <- left_join(data_final, CI_data, by =c("Country", "Year"))

#Join observed data 2020
colnames(data_2020) <- c("Country", "Year", "C_2020")
data_final <- left_join(data_final, data_2020, by =c("Country", "Year"))

#Create 2019 ARIMA for visualisation
data_final_2019 <- subset(data_final, Year == 2019)
data_final_2019$Type <- "Forecasting 2020"
data_final <- rbind(data_final, data_final_2019)

#Create 2019 observed data for visualisation
data_final_2019 <- subset(data_final, Year == 2019)
data_final_2019$Type <- "Observed data 2020"
data_final <- rbind(data_final, data_final_2019)

#Create 2020 data in long format for visualisation
data_final_2020 <- subset(data_final, Year == 2020)
data_final_2020$Type <- "Observed data 2020"
data_final_2020$Value <- data_final_2020$C_2020
data_final <- rbind(data_final, data_final_2020)

#Create low and high CI
data_final$CI[is.na(data_final$CI)] <- 0
data_final$CI_L <- data_final$Value - data_final$CI*2 
data_final$CI_H <- data_final$Value + data_final$CI*2
data_final$CI_L[data_final$Type!="Forecasting 2020"]<- NA
data_final$CI_H[data_final$Type!="Forecasting 2020"]<- NA
data_final$Value <- data_final$Value / 1000
data_final$CI <- data_final$CI / 1000
data_final$CI_L <- data_final$CI_L / 1000
data_final$CI_H <- data_final$CI_H / 1000
data_final$C_2020 <- data_final$C_2020 / 1000

#Recode labels
data_final <- data_final %>% 
  mutate(Type = case_when(
    Type == "Observed data 2012-2019" ~ "Observed 2012-2020",
    Type == "Observed data 2020" ~ "Observed 2012-2020",
    Type == "Forecasting 2020"~ "Forecasted 2020"))

#Order
data_final$Country <- factor(data_final$Country, levels = c("Australia", "Spain", "Sweden", "USA",
                                                            "France","Norway","Germany", "Italy", "Canada",
                                                            "Netherlands","Denmark", "Ireland", 
                                                            "Austria", "Switzerland", "Finland"))

data_final$Type <- factor(data_final$Type, levels = c("Observed 2012-2020", "Forecasted 2020"))


#Data set second plot

data_final_2020 <- subset(data_final, Year == 2020 & Type=="Forecasted 2020")

data_final_2020$Dif <- (data_final_2020$Value - data_final_2020$C_2020) / data_final_2020$Value *100 *-1

data_final_2020$Sig <- ifelse(data_final_2020$C_2020 + data_final_2020$CI*2 > data_final_2020$Value, 0, 1)

data_final_2020$Sig <- recode(data_final_2020$Sig, "0"= "Not statistically significant",
                              "1"= "Statistically significant")

data_final_2020$Sig <- factor(data_final_2020$Sig, levels = c("Statistically significant",
                                                              "Not statistically significant"))



#####################################################
################### Visualization ###################
#####################################################


#Plot 1#

theme_set(theme_pubr())

bmp(file="Forecasting.bmp", width=2.9, height=4.6, units="in", res=300)

#Australia

OZ <- ggplot(data_final, aes(x=Year, y=Value, group=Type)) +
  
  #facet_wrap(.~Country, ncol=5, scales = "free_y")+

  geom_line(aes(color=Type, size=Type, linetype=Type),
            filter(data_final, Country == "Australia"))+
  
  scale_color_manual(values=c("darkblue","#E69F00"))+
  
  scale_size_manual(values=c(0.4, 0.4))+
  
  scale_linetype_manual(values=c("solid", "solid"))+

  geom_ribbon(aes(ymin=CI_L, ymax=CI_H), fill="#E69F00", alpha=0.2,
              filter(data_final, Country == "Australia"))+
  
  scale_x_continuous(limits=c(2012,2020), breaks=seq(2012,2020,4))+
  scale_y_continuous(limits=c(180,720), breaks=seq(200,650,150))+
  
  ggtitle("Australia")+
  
  labs(x=NULL,y = NULL) +
  
  theme_bw()+
  theme(plot.title = element_text(size=6, face="bold", hjust = 0.5),
        strip.background = element_rect(color="grey15",size = 0.35),
        
        #panel.border = element_rect(color="grey15",size = 0.35),
        panel.border = element_blank (),
        panel.grid.major = element_blank (),
        panel.grid.minor=element_blank (),
        
        axis.text.x = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.ticks = element_line(color="grey15", size = 0.55),
        
        legend.title = element_blank (),
        legend.text = element_text(colour="black", size =7, face="plain"),
        legend.position="right",
        legend.direction = "vertical",
        legend.justification=c(0.5,0.5),
        legend.key.width=unit(0.8,"cm"),
        legend.key.height=unit(.5,"cm"))


#Spain

ES <- ggplot(data_final, aes(x=Year, y=Value, group=Type)) +
  
  geom_line(aes(color=Type, size=Type, linetype=Type),
            filter(data_final, Country == "Spain"))+
  
  scale_color_manual(values=c("darkblue","#E69F00"))+
  
  scale_size_manual(values=c(0.4, 0.4))+
  
  scale_linetype_manual(values=c("solid", "solid"))+
  
  geom_ribbon(aes(ymin=CI_L, ymax=CI_H), fill="#E69F00", alpha=0.2,
              filter(data_final, Country == "Spain"))+
  
  scale_x_continuous(limits=c(2012,2020), breaks=seq(2012,2020,4))+
  scale_y_continuous(limits=c(250,920), breaks=seq(300,900,200))+
  
  ggtitle("Spain")+
  
  labs(x=NULL,y = NULL) +
  
  theme_bw()+
  theme(plot.title = element_text(size=6, face="bold", hjust = 0.5),
        
        strip.background = element_rect(color="grey15",size = 0.35),
        
        #panel.border = element_rect(color="grey15",size = 0.35),
        panel.border = element_blank (),
        panel.grid.major = element_blank (),
        panel.grid.minor=element_blank (),
        
        axis.text.x = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.ticks = element_line(color="grey15", size = 0.55))


#Sweden

sw <- ggplot(data_final, aes(x=Year, y=Value, group=Type)) +
  
  geom_line(aes(color=Type, size=Type, linetype=Type),
            filter(data_final, Country == "Sweden"))+
  
  scale_color_manual(values=c("darkblue","#E69F00"))+
  
  scale_size_manual(values=c(0.4, 0.4))+
  
  scale_linetype_manual(values=c("solid", "solid"))+
  
  geom_ribbon(aes(ymin=CI_L, ymax=CI_H), fill="#E69F00", alpha=0.2,
              filter(data_final, Country == "Sweden"))+
  
  scale_x_continuous(limits=c(2012,2020), breaks=seq(2012,2020,4))+
  scale_y_continuous(limits=c(65,175), breaks=seq(70,160,30))+
  
  ggtitle("Sweden")+
  
  labs(x=NULL,y = NULL) +
  
  theme_bw()+
  theme(plot.title = element_text(size=6, face="bold", hjust = 0.5),
        
        strip.background = element_rect(color="grey15",size = 0.35),
        
        #panel.border = element_rect(color="grey15",size = 0.35),
        panel.border = element_blank (),
        panel.grid.major = element_blank (),
        panel.grid.minor=element_blank (),
        
        axis.text.x = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.ticks = element_line(color="grey15", size = 0.55))


#USA

USA <- ggplot(data_final, aes(x=Year, y=Value, group=Type)) +
  
  geom_line(aes(color=Type, size=Type, linetype=Type),
            filter(data_final, Country == "USA"))+
  
  scale_color_manual(values=c("darkblue","#E69F00"))+
  
  scale_size_manual(values=c(0.4, 0.4))+
  
  scale_linetype_manual(values=c("solid", "solid"))+
  
  geom_ribbon(aes(ymin=CI_L, ymax=CI_H), fill="#E69F00", alpha=0.2,
              filter(data_final, Country == "USA"))+
  
  scale_x_continuous(limits=c(2012,2020), breaks=seq(2012,2020,4))+
  scale_y_continuous(limits=c(800,1700), breaks=seq(800,1700,300))+
  
  ggtitle("USA")+
  
  labs(x=NULL,y = NULL) +
  
  theme_bw()+
  theme(plot.title = element_text(size=6, face="bold", hjust = 0.5),
        
        strip.background = element_rect(color="grey15",size = 0.35),
        
        #panel.border = element_rect(color="grey15",size = 0.35),
        panel.border = element_blank (),
        panel.grid.major = element_blank (),
        panel.grid.minor=element_blank (),
        
        axis.text.x = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.ticks = element_line(color="grey15", size = 0.55))


#Norway

NO <- ggplot(data_final, aes(x=Year, y=Value, group=Type)) +
  
  geom_line(aes(color=Type, size=Type, linetype=Type),
            filter(data_final, Country == "Norway"))+
  
  scale_color_manual(values=c("darkblue","#E69F00"))+
  
  scale_size_manual(values=c(0.4, 0.4))+
  
  scale_linetype_manual(values=c("solid", "solid"))+
  
  geom_ribbon(aes(ymin=CI_L, ymax=CI_H), fill="#E69F00", alpha=0.2,
              filter(data_final, Country == "Norway"))+
  
  scale_x_continuous(limits=c(2012,2020), breaks=seq(2012,2020,4))+
  scale_y_continuous(limits=c(30,71), breaks=seq(35,65,15))+
  
  ggtitle("Norway")+
  
  labs(x=NULL,y = NULL) +
  
  theme_bw()+
  theme(plot.title = element_text(size=6, face="bold", hjust = 0.5),
        
        strip.background = element_rect(color="grey15",size = 0.35),
        
        #panel.border = element_rect(color="grey15",size = 0.35),
        panel.border = element_blank (),
        panel.grid.major = element_blank (),
        panel.grid.minor=element_blank (),
        
        axis.text.x = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.ticks = element_line(color="grey15", size = 0.55))


#France

FR <- ggplot(data_final, aes(x=Year, y=Value, group=Type)) +
  
  geom_line(aes(color=Type, size=Type, linetype=Type),
            filter(data_final, Country == "France"))+
  
  scale_color_manual(values=c("darkblue","#E69F00"))+
  
  scale_size_manual(values=c(0.4, 0.4))+
  
  scale_linetype_manual(values=c("solid", "solid"))+
  
  geom_ribbon(aes(ymin=CI_L, ymax=CI_H), fill="#E69F00", alpha=0.2,
              filter(data_final, Country == "France"))+
  
  scale_x_continuous(limits=c(2012,2020), breaks=seq(2012,2020,4))+
  scale_y_continuous(limits=c(200,500), breaks=seq(200,500,100))+
  
  ggtitle("France")+
  
  labs(x=NULL,y = NULL) +
  
  theme_bw()+
  theme(plot.title = element_text(size=6, face="bold", hjust = 0.5),
        
        strip.background = element_rect(color="grey15",size = 0.35),
        
        #panel.border = element_rect(color="grey15",size = 0.35),
        panel.border = element_blank (),
        panel.grid.major = element_blank (),
        panel.grid.minor=element_blank (),
        
        axis.text.x = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.ticks = element_line(color="grey15", size = 0.55))


#Germany

GR <- ggplot(data_final, aes(x=Year, y=Value, group=Type)) +
  
  geom_line(aes(color=Type, size=Type, linetype=Type),
            filter(data_final, Country == "Germany"))+
  
  scale_color_manual(values=c("darkblue","#E69F00"))+
  
  scale_size_manual(values=c(0.4, 0.4))+
  
  scale_linetype_manual(values=c("solid", "solid"))+
  
  geom_ribbon(aes(ymin=CI_L, ymax=CI_H), fill="#E69F00", alpha=0.2,
              filter(data_final, Country == "Germany"))+
  
  scale_x_continuous(limits=c(2012,2020), breaks=seq(2012,2020,4))+
  scale_y_continuous(limits=c(380,1575), breaks=seq(500,1500,300))+
  
  ggtitle("Germany")+
  
  labs(x=NULL,y = NULL) +
  
  theme_bw()+
  theme(plot.title = element_text(size=6, face="bold", hjust = 0.5),
        
        strip.background = element_rect(color="grey15",size = 0.35),
        
        #panel.border = element_rect(color="grey15",size = 0.35),
        panel.border = element_blank (),
        panel.grid.major = element_blank (),
        panel.grid.minor=element_blank (),
        
        axis.text.x = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.ticks = element_line(color="grey15", size = 0.55))


#Italy

IT <- ggplot(data_final, aes(x=Year, y=Value, group=Type)) +
  
  geom_line(aes(color=Type, size=Type, linetype=Type),
            filter(data_final, Country == "Italy"))+
  
  scale_color_manual(values=c("darkblue","#E69F00"))+
  
  scale_size_manual(values=c(0.4, 0.4))+
  
  scale_linetype_manual(values=c("solid", "solid"))+
  
  geom_ribbon(aes(ymin=CI_L, ymax=CI_H), fill="#E69F00", alpha=0.2,
              filter(data_final, Country == "Italy"))+
  
  scale_x_continuous(limits=c(2012,2020), breaks=seq(2012,2020,4))+
  scale_y_continuous(limits=c(180,460), breaks=seq(200,450,75))+
  
  ggtitle("Italy")+
  
  labs(x=NULL,y = NULL) +
  
  theme_bw()+
  theme(plot.title = element_text(size=6, face="bold", hjust = 0.5),
        
        strip.background = element_rect(color="grey15",size = 0.35),
        
        #panel.border = element_rect(color="grey15",size = 0.35),
        panel.border = element_blank (),
        panel.grid.major = element_blank (),
        panel.grid.minor=element_blank (),
        
        axis.text.x = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.ticks = element_line(color="grey15", size = 0.55))


#Netherlands

NL <- ggplot(data_final, aes(x=Year, y=Value, group=Type)) +
  
  geom_line(aes(color=Type, size=Type, linetype=Type),
            filter(data_final, Country == "Netherlands"))+
  
  scale_color_manual(values=c("darkblue","#E69F00"))+
  
  scale_size_manual(values=c(0.4, 0.4))+
  
  scale_linetype_manual(values=c("solid", "solid"))+
  
  geom_ribbon(aes(ymin=CI_L, ymax=CI_H), fill="#E69F00", alpha=0.2,
              filter(data_final, Country == "Netherlands"))+
  
  scale_x_continuous(limits=c(2012,2020), breaks=seq(2012,2020,4))+
  scale_y_continuous(limits=c(100,250), breaks=seq(100,250,50))+
  
  ggtitle("Netherlands")+
  
  labs(x=NULL,y = NULL) +
  
  theme_bw()+
  theme(plot.title = element_text(size=6, face="bold", hjust = 0.5),
        
        strip.background = element_rect(color="grey15",size = 0.35),
        
        #panel.border = element_rect(color="grey15",size = 0.35),
        panel.border = element_blank (),
        panel.grid.major = element_blank (),
        panel.grid.minor=element_blank (),
        
        axis.text.x = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.ticks = element_line(color="grey15", size = 0.55))


#Canada

CA <- ggplot(data_final, aes(x=Year, y=Value, group=Type)) +
  
  geom_line(aes(color=Type, size=Type, linetype=Type),
            filter(data_final, Country == "Canada"))+
  
  scale_color_manual(values=c("darkblue","#E69F00"))+
  
  scale_size_manual(values=c(0.4, 0.4))+
  
  scale_linetype_manual(values=c("solid", "solid"))+
  
  geom_ribbon(aes(ymin=CI_L, ymax=CI_H), fill="#E69F00", alpha=0.2,
              filter(data_final, Country == "Canada"))+
  
  scale_x_continuous(limits=c(2012,2020), breaks=seq(2012,2020,4))+
  scale_y_continuous(limits=c(150,410), breaks=seq(150,375,75))+
  
  ggtitle("Canada")+
  
  labs(x=NULL,y = NULL) +
  
  theme_bw()+
  theme(plot.title = element_text(size=6, face="bold", hjust = 0.5),
        
        strip.background = element_rect(color="grey15",size = 0.35),
        
        #panel.border = element_rect(color="grey15",size = 0.35),
        panel.border = element_blank (),
        panel.grid.major = element_blank (),
        panel.grid.minor=element_blank (),
        
        axis.text.x = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.ticks = element_line(color="grey15", size = 0.55))


#Ireland

IR <- ggplot(data_final, aes(x=Year, y=Value, group=Type)) +
  
  geom_line(aes(color=Type, size=Type, linetype=Type),
            filter(data_final, Country == "Ireland"))+
  
  scale_color_manual(values=c("darkblue","#E69F00"))+
  
  scale_size_manual(values=c(0.4, 0.4))+
  
  scale_linetype_manual(values=c("solid", "solid"))+
  
  geom_ribbon(aes(ymin=CI_L, ymax=CI_H), fill="#E69F00", alpha=0.2,
              filter(data_final, Country == "Ireland"))+
  
  scale_x_continuous(limits=c(2012,2020), breaks=seq(2012,2020,4))+
  scale_y_continuous(limits=c(25,125), breaks=seq(25,125,50))+
  
  ggtitle("Ireland")+
  
  labs(x=NULL,y = NULL) +
  
  theme_bw()+
  theme(plot.title = element_text(size=6, face="bold", hjust = 0.5),
        
        strip.background = element_rect(color="grey15",size = 0.35),
        
        #panel.border = element_rect(color="grey15",size = 0.35),
        panel.border = element_blank (),
        panel.grid.major = element_blank (),
        panel.grid.minor=element_blank (),
        
        axis.text.x = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.ticks = element_line(color="grey15", size = 0.55))


#Austria

AU <- ggplot(data_final, aes(x=Year, y=Value, group=Type)) +
  
  geom_line(aes(color=Type, size=Type, linetype=Type),
            filter(data_final, Country == "Austria"))+
  
  scale_color_manual(values=c("darkblue","#E69F00"))+
  
  scale_size_manual(values=c(0.4, 0.4))+
  
  scale_linetype_manual(values=c("solid", "solid"))+
  
  geom_ribbon(aes(ymin=CI_L, ymax=CI_H), fill="#E69F00", alpha=0.2,
              filter(data_final, Country == "Austria"))+
  
  scale_x_continuous(limits=c(2012,2020), breaks=seq(2012,2020,4))+
  scale_y_continuous(limits=c(50,200), breaks=seq(50,200,50))+
  
  ggtitle("Austria")+
  
  labs(x=NULL,y = NULL) +
  
  theme_bw()+
  theme(plot.title = element_text(size=6, face="bold", hjust = 0.5),
        
        strip.background = element_rect(color="grey15",size = 0.35),
        
        #panel.border = element_rect(color="grey15",size = 0.35),
        panel.border = element_blank (),
        panel.grid.major = element_blank (),
        panel.grid.minor=element_blank (),
        
        axis.text.x = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.ticks = element_line(color="grey15", size = 0.55))


#Denmark

DK <- ggplot(data_final, aes(x=Year, y=Value, group=Type)) +
  
  geom_line(aes(color=Type, size=Type, linetype=Type),
            filter(data_final, Country == "Denmark"))+
  
  scale_color_manual(values=c("darkblue","#E69F00"))+
  
  scale_size_manual(values=c(0.4, 0.4))+
  
  scale_linetype_manual(values=c("solid", "solid"))+
  
  geom_ribbon(aes(ymin=CI_L, ymax=CI_H), fill="#E69F00", alpha=0.2,
              filter(data_final, Country == "Denmark"))+
  
  scale_x_continuous(limits=c(2012,2020), breaks=seq(2012,2020,4))+
  scale_y_continuous(limits=c(20,100), breaks=seq(25,100,25))+
  
  ggtitle("Denmark")+
  
  labs(x=NULL,y = NULL) +
  
  theme_bw()+
  theme(plot.title = element_text(size=6, face="bold", hjust = 0.5),
        
        strip.background = element_rect(color="grey15",size = 0.35),
        
        #panel.border = element_rect(color="grey15",size = 0.35),
        panel.border = element_blank (),
        panel.grid.major = element_blank (),
        panel.grid.minor=element_blank (),
        
        axis.text.x = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.ticks = element_line(color="grey15", size = 0.55))


#Switzerland

SWI <- ggplot(data_final, aes(x=Year, y=Value, group=Type)) +
  
  geom_line(aes(color=Type, size=Type, linetype=Type),
            filter(data_final, Country == "Switzerland"))+
  
  scale_color_manual(values=c("darkblue","#E69F00"))+
  
  scale_size_manual(values=c(0.4, 0.4))+
  
  scale_linetype_manual(values=c("solid", "solid"))+
  
  geom_ribbon(aes(ymin=CI_L, ymax=CI_H), fill="#E69F00", alpha=0.2,
              filter(data_final, Country == "Switzerland"))+
  
  scale_x_continuous(limits=c(2012,2020), breaks=seq(2012,2020,4))+
  scale_y_continuous(limits=c(100,185), breaks=seq(100,175,25))+
  
  ggtitle("Switzerland")+
  
  labs(x=NULL,y = NULL) +
  
  theme_bw()+
  theme(plot.title = element_text(size=6, face="bold", hjust = 0.5),
        
        strip.background = element_rect(color="grey15",size = 0.35),
        
        #panel.border = element_rect(color="grey15",size = 0.35),
        panel.border = element_blank (),
        panel.grid.major = element_blank (),
        panel.grid.minor=element_blank (),
        
        axis.text.x = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.ticks = element_line(color="grey15", size = 0.55))


#Finland

FI <- ggplot(data_final, aes(x=Year, y=Value, group=Type)) +
  
  geom_line(aes(color=Type, size=Type, linetype=Type),
            filter(data_final, Country == "Finland"))+
  
  scale_color_manual(values=c("darkblue","#E69F00"))+
  
  scale_size_manual(values=c(0.4, 0.4))+
  
  scale_linetype_manual(values=c("solid", "solid"))+
  
  geom_ribbon(aes(ymin=CI_L, ymax=CI_H), fill="#E69F00", alpha=0.2,
              filter(data_final, Country == "Finland"))+
  
  scale_x_continuous(limits=c(2012,2020), breaks=seq(2012,2020,4))+
  scale_y_continuous(limits=c(20,42), breaks=seq(20,40,10))+
  
  ggtitle("Finland")+
  
  labs(x=NULL,y = NULL) +
  
  theme_bw()+
  theme(plot.title = element_text(size=6, face="bold", hjust = 0.5),
        
        strip.background = element_rect(color="grey15",size = 0.35),
        
        #panel.border = element_rect(color="grey15",size = 0.35),
        panel.border = element_blank (),
        panel.grid.major = element_blank (),
        panel.grid.minor=element_blank (),
        
        axis.text.x = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 5.5, face="plain"),
        axis.ticks = element_line(color="grey15", size = 0.55))


#Figuras
figure <- plot_grid( OZ + theme(legend.position="none"),
                     ES + theme(legend.position="none"),
                     sw + theme(legend.position="none"),
                     USA + theme(legend.position="none"),
                     FR + theme(legend.position="none"),
                     NO + theme(legend.position="none"),
                     GR + theme(legend.position="none"),
                     IT + theme(legend.position="none"),
                     CA + theme(legend.position="none"),
                     NL + theme(legend.position="none"),
                     DK + theme(legend.position="none"),
                     IR + theme(legend.position="none"),
                     AU + theme(legend.position="none"),
                     SWI + theme(legend.position="none"),
                     FI + theme(legend.position="none"),
                     labels = NULL,
                     ncol = 3, nrow = 5)
figure

#Crear leyenda
legend_b <- get_legend(OZ + guides(col=guide_legend(nrow=1))+
                         theme(legend.title = element_blank (),
                               legend.text = element_text(size = 6.2, face="plain"),
                               legend.position= "bottom",
                               legend.key.width=unit(.5,"cm"),
                               legend.key.height=unit(.5,"cm")))

#Figuras + leyenda
figure2 <- plot_grid(figure, legend_b,ncol = 1, rel_heights = c(3.7, .2))
figure2

dev.off()



#Plot 2#

bmp(file="Change.bmp", width=3, height=3.2, units="in", res=300)

ggplot(data_final_2020, aes(x=Dif, y=Country, group=Sig)) +
  
  geom_point(aes(color=Sig, alpha=Sig), fill="black", size=2)+
  
  scale_color_manual(values=c("blue","blue"))+
  
  scale_alpha_manual(values=c(1,0.2))+
  
  scale_x_continuous(limits=c(-61.5,6), breaks=seq(-60,0,10))+
  
  labs(x="Immigration change (%)", y = NULL) +
  
  geom_vline(xintercept = 0, size=0.65, linetype="dashed", col="grey40")+
  
  theme_bw()+
  theme(axis.title.x = element_text(size=7, face="plain"),
        axis.title.y = element_text(size=7, face="plain"),
        strip.background = element_rect(color="grey15",size = 0.35),
        
        #panel.border = element_rect(color="grey15",size = 0.35),
        panel.border = element_blank (),
        panel.grid.major = element_line(colour = "grey75", size=0.08),
        panel.grid.minor=element_blank (),
        
        axis.text.x = element_text(colour = "grey10", size = 7, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 7, face="plain"),
        axis.ticks = element_line(color="grey30", size = 0.6),
        
        legend.title = element_blank (),
        legend.text = element_text(colour="black", size =7.2, face="plain"),
        #legend.position="right",
        legend.position=c(0.36,0.91),
        legend.direction = "vertical",
        legend.justification=c(0.5,0.5),
        legend.key.width=unit(0.2,"cm"),
        legend.key.height=unit(.2,"cm"))

dev.off()


