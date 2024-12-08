# loading packages
library(vars)
library(tseries)
library(quantmod) # - this package is used to fetch marcoeconimic data from FED

# loading data
Nat_Gas_Futures_data <- read.csv("data/Natural_Gas_Futures_historical_prices.csv")
Oil_Futures_data <- read.csv("data/Crude_Oil_WTI_Futures_historical_prices.csv")
Coal_Futures_data <- read.csv("data/NewCastle_Coal_Futures_historical_prices.csv")
US_Elec_Monthly_data <- read.csv("???")

# Creating vectors of last traded prices for Futures data
Nat_Gas_Futures_price <- ts(Nat_Gas_Futures_data$Last, start = as.yearmon(Nat_Gas_Futures_data$Time[1]), frequency = 12)
Oil_Futures_price <- ts(Oil_Futures_data$Last)
Coal_Futures_price <- Coal_Futures_data$Last
?as.yearmon
# Ploting the data
plot(Nat_Gas_Futures_price)
plot(Oil_Futures_price)
plot(Coal_Futures_price)

# Testing for stationarity for both time series and creating stationary time series of same lengths
adf.test(Nat_Gas_Futures_price) # Test for stationarity of the Natural Gas Future price series
adf.test(Oil_Futures_price) # Test for stationarity of the Oil Futures price series
adf.test(Coal_Futures_price) # Test for stationarity of the Coal Futures price series
adf.test(US_Elec_Monthly$???) # Test for stationarity of the US Electricity Monthly generation series

# Apply diff-log-transformation if necessary


# Check for stationarity again 

# Estimating vector autoregession and testing for causality
# 1. Natural Gas Futures vs US Electricity Generation

VAR_est <- VAR(cbind(Oil_returns[2:length(Oil_returns)],Inflation_CH), ic="AIC", lag.max = 24)
coeftest(VAR_est)
causality(VAR_est, cause="Oil_returns")["Granger"] # Testing for Granger-causal effects of the inflation rate on oil returns
causality(VAR_est, cause="Inflation_CH")["Granger"] # Testing for Granger-causal effects of oil returns on inflation rate

# Interpretation


# Degression: Plotting impulse response functions
plot(irf(VAR_est, impulse="Inflation_CH", response="Oil_returns")) # We shock Oil_returns at time t
plot(irf(VAR_est, impulse="Oil_returns", response="Inflation_CH")) # We shock CH_Inflation at time t
# Interpretation:


# 2. Oil Futures vs US Electricity Generation

VAR_est <- VAR(cbind(Oil_returns[2:length(Oil_returns)],Inflation_CH), ic="AIC", lag.max = 24)
coeftest(VAR_est)
causality(VAR_est, cause="Oil_returns")["Granger"] # Testing for Granger-causal effects of the inflation rate on oil returns
causality(VAR_est, cause="Inflation_CH")["Granger"] # Testing for Granger-causal effects of oil returns on inflation rate

# Interpretation


# Degression: Plotting impulse response functions
plot(irf(VAR_est, impulse="Inflation_CH", response="Oil_returns")) # We shock Oil_returns at time t
plot(irf(VAR_est, impulse="Oil_returns", response="Inflation_CH")) # We shock CH_Inflation at time t
# Interpretation:

# 3. Coal Futures vs US Electricity Generation

VAR_est <- VAR(cbind(Oil_returns[2:length(Oil_returns)],Inflation_CH), ic="AIC", lag.max = 24)
coeftest(VAR_est)
causality(VAR_est, cause="Oil_returns")["Granger"] # Testing for Granger-causal effects of the inflation rate on oil returns
causality(VAR_est, cause="Inflation_CH")["Granger"] # Testing for Granger-causal effects of oil returns on inflation rate

# Interpretation


# Degression: Plotting impulse response functions
plot(irf(VAR_est, impulse="Inflation_CH", response="Oil_returns")) # We shock Oil_returns at time t
plot(irf(VAR_est, impulse="Oil_returns", response="Inflation_CH")) # We shock CH_Inflation at time t
# Interpretation:

