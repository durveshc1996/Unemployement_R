library(TSA)
library(fUnitRoots)
library(forecast)
library(CombMSC)
library(lmtest)
library(fGarch)
library(rugarch)
library(tseries)
library(FitAR)



sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}
residual.analysis <- function(model, std = TRUE,start = 2, class = c("ARIMA","GARCH","ARMA-GARCH", "garch", "fGARCH")[1]){
  library(TSA)
  library(FitAR)
  if (class == "ARIMA"){
    if (std == TRUE){
      res.model = rstandard(model)
    }else{
      res.model = residuals(model)
    }
  }else if (class == "GARCH"){
    res.model = model$residuals[start:model$n.used]
  }else if (class == "garch"){
    res.model = model$residuals[start:model$n.used]  
  }else if (class == "ARMA-GARCH"){
    res.model = model@fit$residuals
  }else if (class == "fGARCH"){
    res.model = model@residuals
  }else {
    stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
  }
  par(mfrow=c(3,2))
  plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot of standardised residuals")
  abline(h=0)
  hist(res.model,main="Histogram of standardised residuals")
  qqnorm(res.model,main="QQ plot of standardised residuals")
  qqline(res.model, col = 2)
  acf(res.model,main="ACF of standardised residuals")
  print(shapiro.test(res.model))
  k=0
  LBQPlot(res.model, lag.max = 30, StartLag = k + 1, k = 0, SquaredQ = FALSE)
  par(mfrow=c(1,1))
}

unemp <- read.csv('dataset.csv', header = TRUE)
unemp_ts <- ts(unemp$value, start = c(2010,1), end = c(2021,4), frequency = 12)
unemp_ts
plot(unemp_ts, xlab='Year', ylab='Unemployment Rate', type='o', main='Time Series plot of unemployment rate in Australia')

# Checking the correlation
y = unemp_ts
x = zlag(y)
index = 2:length(x)
plot(y=y[index], x=x[index], xlab = "Previous year's Unemployment Rate", ylab='Unemployment Rate',  main = 'Scatter plot of Unemployment Rate in consecutive years')
cor(y[index], x[index])

# plotting acf plot to check for stationarity
par(mfrow=c(1,2))
acf(unemp_ts, main = 'ACF for Unemployment Rate series', xlab = 'Lag', ylab = 'ACF')
pacf(unemp_ts, main = 'PACF for Unemployment Rate series', xlab = 'Lag', ylab = 'Partial ACF')

# ADF test for stationarity
library(tseries)
adf.test(unemp_ts)

# Log Transformation
par(mfrow=c(1,1))
unemp_log <- log(unemp_ts)
plot(unemp_log,  xlab='Year', ylab='Unemployment Rate', type='o', main='Log Transformed Series', ylim=c(1.2,2))

# Box-Cox Transformation
BC <- BoxCox.ar(y = unemp_ts, lambda = seq(-2,2,0.1))
BC$ci
mean(BC$ci)
lambda <- mean(BC$ci)
unemp_tsBC <- ((unemp_ts^lambda)-1)/lambda
plot(unemp_tsBC,  xlab='Year', ylab='Unemployment Rate', type='o', main='Box-Cox Transformed Series')


# First order differencing
unemp_diff <- diff(unemp_log, differences = 1)
plot(unemp_diff, xlab='Year', ylab='Unemployment Rate', type='o', main='First Difference Series')

#Test for volatility
McLeod.Li.test(y=unemp_diff, main = 'McLeod-Li test for monthly return series')

#Test for Normality
qqnorm(unemp_diff)
qqline(unemp_diff, col = 2, lwd = 1, lty = 2)

# Checking stationarity of first differencing using adf test
adf.test(unemp_diff)
pp.test(unemp_diff)

par(mfrow=c(1,2))
acf(unemp_diff, ci.type="ma", main = "ACF of unemployment series.",  xlab = 'Lag', ylab = 'ACF') # q = 0,3
pacf(unemp_diff, main = "PACF of unemployment series.",  xlab = 'Lag', ylab = 'Partial ACF') # p = 3
# ARIMA(3,1,0) and ARIMA(3,1,3)

par(mfrow=c(1,1))
eacf(unemp_diff)
# ARIMA(0,1,0), ARIMA(0,1,1), ARIMA(1,1,1)

bic_ms = armasubsets(y=unemp_diff,nar=5,nma=5,y.name='test',ar.method='ols')
plot(bic_ms)
# ARIMA(3,1,0)

# Overall, we have { ARIMA(0,1,0), ARIMA(0,1,1), ARIMA(1,1,1), ARIMA(3,1,0), ARIMA(3,1,1) }

pval <- c(0, 1, 3)
dval <- 1
qval <- 0:3

ordertable <- expand.grid(pval, dval, qval)
ordertable <- ordertable[-c(2, 7, 8, 10, 11), ]
ordertable

modelfit <- function(x, dat){
  m=Arima(dat, order=c(x[[1]], x[[2]], x[[3]]),method="ML")
  return(m)
  #return(coeftest(m))
  #return(residual.analysis(m))
} 

Fits <- plyr::alply(ordertable, 1, modelfit, dat = unemp_diff)
Fits

model_010_css = arima(unemp_log,order=c(0,1,0),method='CSS')

coeftest(model_010_css)

model_010_ml = arima(unemp_log,order=c(0,1,0),method='ML')
coeftest(model_010_ml)
residual.analysis(model = model_010_ml)


model_011_css = arima(unemp_log,order=c(0,1,1),method='CSS')
coeftest(model_011_css)

model_011_ml = arima(unemp_log,order=c(0,1,1),method='ML')
coeftest(model_011_ml)
residual.analysis(model = model_011_ml)


model_111_css = arima(unemp_log,order=c(1,1,1),method='CSS')
coeftest(model_111_css)

model_111_ml = arima(unemp_log,order=c(1,1,1),method='ML')
coeftest(model_111_ml)
residual.analysis(model = model_111_ml)


model_310_css = arima(unemp_log,order=c(3,1,0),method='CSS')
coeftest(model_310_css)

model_310_ml = arima(unemp_log,order=c(3,1,0),method='ML')
coeftest(model_310_ml)
residual.analysis(model = model_310_ml)


model_313_css = arima(unemp_log,order=c(3,1,3),method='CSS')
coeftest(model_313_css)

model_313_ml = arima(unemp_log,order=c(3,1,3),method='ML')
coeftest(model_313_ml)
residual.analysis(model = model_313_ml)


sc.AIC=AIC(model_010_ml, model_011_ml, model_111_ml,model_310_ml,model_313_ml, model_311_ml, model_312_ml)
sc.BIC=AIC(model_010_ml, model_011_ml, model_111_ml,model_310_ml,model_313_ml, model_311_ml, model_312_ml, k=log(length(unemp_ts)))

sort.score(sc.AIC, score = "aic")
sort.score(sc.BIC, score = "aic")

# best model ARIMA(3,1,0), plot acf, pacf of residuals from ARIMA(3,1,0).

res.model_310_ml <- model_310_ml$residuals
acf(res.model_310_ml)
pacf(res.model_310_ml)

model_311_css = arima(unemp_log,order=c(3,1,1),method='CSS')
coeftest(model_311_css)

model_311_ml = arima(unemp_log,order=c(3,1,1),method='ML')
coeftest(model_311_ml)
residual.analysis(model = model_311_ml)

res.model_311_ml <- model_311_ml$residuals
acf(res.model_311_ml)
pacf(res.model_311_ml)

model_312_css = arima(unemp_log,order=c(3,1,2),method='CSS')
coeftest(model_312_css)

model_312_ml = arima(unemp_log,order=c(3,1,2),method='ML')
coeftest(model_312_ml)
residual.analysis(model = model_312_ml)

res.model_312_ml <- model_312_ml$residuals
acf(res.model_312_ml)
pacf(res.model_312_ml)

# Best Model ARIMA(3,1,2)

# Overfitted models ARIMA(4,1,2) & ARIMA(3,1,3)
model_412_css = arima(unemp_log,order=c(4,1,2),method='CSS')
coeftest(model_412_css)

model_412_ml = arima(unemp_log,order=c(4,1,2),method='ML')
coeftest(model_412_ml)
residual.analysis(model = model_412_ml)


library(forecast)
fit = Arima(unemp_log,c(3,1,2), method = 'ML') 
summary(fit)
forecastlog = forecast(fit,h=10)

forecastlog_1 = as.data.frame(forecastlog)

forecastraw=exp(forecastlog$mean)
forecastraw

par(mfrow=c(1,1))
plot(unemp_ts,xlab='Year', xlim=c(2010,2022.5), ylab='Unemployment rate',  main='Forcast of unemployment rate for next 10 months')
lines(ts(as.vector(forecastraw), start=c(2021,5), frequency=12), col="red", type="l")

