library(forecast)

##########
# PART 1 #
##########

# read data by using scan command
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
# converting/transforming data into time series
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries
plot(birthstimeseries)

# decomposing birth time series into its components
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriescomponents
# plot seasonal component
plot(birthstimeseriescomponents)
# seasonal component of the time series
birthstimeseriescomponents$seasonal
# trend of time series
birthstimeseriescomponents$trend

# We can get seasonally-adjusted values by subtracting seasonal component from the ts
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
birthstimeseriesseasonallyadjusted
plot(birthstimeseriesseasonallyadjusted)


# forecasting using HoltWinters model- model fitting
birthstimeseriesforecasts <- HoltWinters(birthstimeseries, beta=FALSE, gamma=FALSE)
birthstimeseriesforecasts # alpha = 0.45
# Check fitted values 
birthstimeseriesforecasts$fitted
# plot forecasted values
plot(birthstimeseriesforecasts)
# check for sum of square errors
birthstimeseriesforecasts$SSE
# predict birth for next 3 years
birthstimeseriesforecasts3 <- forecast:::forecast.HoltWinters(birthstimeseriesforecasts, h=36)
birthstimeseriesforecasts3 
# plot forecsated values for next 3 years with confidence interval
forecast:::plot.forecast(birthstimeseriesforecasts3)

################
# PART 2 ARIMA #
################



# reading data skip first line
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
# convert it into time series
volcanodusTS <- ts(volcanodust,start=c(1500))
volcanodusTS
# plot time series
plot.ts(volcanodusTS)

# check for auto corelation 
acf(volcanodusTS, lag.max=20) 
acf(volcanodusTS, lag.max=20, plot=FALSE)

# check for partial auto corelation
pacf(volcanodusTS, lag.max=20)
pacf(volcanodusTS, lag.max=20, plot=FALSE)

# choosing model using auto.arima function()
forecast::auto.arima(volcanodust) # gives ARIMA (1,0,2)
forecast::auto.arima(volcanodust,ic="bic") # gives ARIMA (2,0,0)
volcanodustTSarima <- arima(volcanodusTS, order=c(2,0,0))
volcanodustTSarima
# forecast for next 31 years
volcanodustTSforecasts <- forecast:::forecast(volcanodustTSarima, h=31)
volcanodustTSforecasts

plot(volcanodustTSforecasts)

acf(volcanodustTSforecasts$residuals, lag.max=20)
Box.test(volcanodustTSforecasts$residuals, lag=20, type="Ljung-Box")

plot.ts(volcanodustTSforecasts$residuals)   

