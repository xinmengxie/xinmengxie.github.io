# load packages
library(forecast)
library(urca)
library(ggplot2)
library(zoo)
library(lmtest)

# read file
US <- read.csv("USSTHPI.csv")
CA <- read.csv("CASTHPI.csv")

# turn to ts(US
US.ts <- ts(US$USSTHPI, start = c(1975, 1), freq = 4)
CA.ts <- ts(CA$CASTHPI, start = c(1975, 1), freq = 4)


## seasonal adjusted
UsComponents <- decompose(US.ts)
US.ts <- US.ts - UsComponents$seasonal
plot(US.ts)
## df test
US.test <- ur.df(US.ts,type="trend",selectlags="BIC")
print(summary(US.test))  ### 2.1183 < 6.49

## seasonal adjusted
CaComponents <- decompose(CA.ts)
CA.ts <- CA.ts - CaComponents$seasonal
plot(CA.ts)
## df test
CA.test <- ur.df(CA.ts,type="trend",selectlags="BIC")
print(summary(CA.test))  ### 6.417 < 6.49

## ggplot
df.US <- data.frame(date=as.Date(as.yearmon(as.vector(as.matrix(US$DATE)))), US=as.matrix(US.ts))

df.CA <- data.frame(date=as.Date(as.yearmon(as.vector(as.matrix(CA$DATE)))), CA=as.matrix(CA.ts))

ggplot() + 
  geom_line(data=df.CA, mapping=aes(x=date, y=CA, col = "CA")) +
  geom_line(data=df.US, mapping=aes(x=date, y=US, col = "US")) +
  #geom_line(data=df.Int, mapping=aes(x=date, y=Int, col = "Int")) +
  ylab("HPI") 
#xlab("year")  +
#theme(axis.title.y = element_text(angle = 0),aspect.ratio=0.4)


# run cointegrating regression, and store residuals
creg.model <- lm(US.ts ~ CA.ts)
resid <- residuals(creg.model)
resid.ts <- ts(resid, start = c(1975,1), freq = 4)

# run Augmented Dickey/Fuller on residuals 
# Note:  I usually do this w/o trend
# Also, note:  Always plot these residuals too
plot(resid.ts)

df.test <- ur.df(resid.ts,selectlags="BIC")
print(summary(df.test)) ### -4.0614 < -1.95

### Create a single dataframe
US.ts = log(US.ts) ## log the to series to use linear regression forecasting model
CA.ts = log(CA.ts)
plot(US.ts)
plot(CA.ts)

d_US.ts <- diff(US.ts, differences = 2)
plot(d_US.ts)
d_US.test <- ur.df(d_US.ts,selectlags="BIC")
print(summary(d_US.test)) ### -19.0688  < -1.95

d_CA.ts     <- diff(CA.ts, differences = 2)  
plot(d_CA.ts)


# cross-correlation(covariance)  
ccf(d_US.ts, d_CA.ts, type = c("correlation","covariance"))
#0.5 * 4 = 2 lags = 2

d_US_lag.ts <- stats::lag(d_US.ts,-1)
d_US_lag2.ts <- stats::lag(d_US.ts,-2)


d_CA_lag.ts <- stats::lag(d_CA.ts, -1)
d_CA_lag2.ts <- stats::lag(d_CA.ts, -2)

residlag.ts <- stats::lag(resid.ts,-1)

DF.ts <- cbind(d_US.ts, d_US_lag.ts, d_US_lag2.ts, d_CA_lag.ts, d_CA_lag2.ts, residlag.ts)
DF.ts <- na.omit(DF.ts)


### Conduct a forecast exercise (8-steps ahead forecast)

#### window for train and test
train <- window(DF.ts,start = c(1975,3),end = c(2017,4)) ## since we did one order difference on hd and then take the lags for both hd and low, thus, the DF.ts starts from 1990.3
train_us <- window(US.ts, start = c(1975,3),end = c(2017,4))
train_us <- exp(train_us)
test <- window(DF.ts,start = c(2018,1))
test_us <- window(US.ts, start = c(2018,1))
test_us <- exp(test_us[1:8])

#### Build four models (VECM, Var, ARIMA, ARIMAX)
#### vector error correction model
ec.mod  <- lm( d_US.ts ~ d_US_lag.ts + d_US_lag2.ts + d_CA_lag.ts + d_CA_lag2.ts + residlag.ts, data = train)
pred_ec <- predict(ec.mod, test)
#### convert the twice differenced series into the original sereis
diff_2017_07_2017_10 <- log(df.US[df.US[,1] == "2017-10-01",][,2]) - log(df.US[df.US[,1] == "2017-07-01",][,2])
d_ec <- diff_2017_07_2017_10 + cumsum(pred_ec)
log_ec <- log(df.US[df.US[,1] == "2017-10-01",][,2]) + cumsum(d_ec)
ec <- exp(log_ec)
#### get the RMSE of vecm
print(accuracy(ec, test_us))

#### var(benchmark)
Var.mod <- lm(d_US.ts ~ d_US_lag.ts + d_US_lag2.ts, data = train)
pred_Var <- predict(Var.mod, test)
#### convert the twice differenced series into the original sereis
d_Var <- diff_2017_07_2017_10 + cumsum(pred_Var)
log_Var <- log(df.US[df.US[,1] == "2017-10-01",][,2]) + cumsum(d_Var)
Var <- exp(log_Var)
#### get the RMSE of Var
print(accuracy(Var, test_us))

#### ARIMAX
Arimax.mod <- lm( d_US.ts ~ d_US_lag.ts + d_US_lag2.ts + d_CA_lag.ts + d_CA_lag2.ts, data = train)
pred_Arimax <- predict(Arimax.mod, test)
#### convert the twice differenced series into the original sereis
d_Arimax <- diff_2017_07_2017_10 + cumsum(pred_Arimax)
log_Arimax <- log(df.US[df.US[,1] == "2017-10-01",][,2]) + cumsum(d_Arimax)
Arimax <- exp(log_Arimax)
#### get the RMSE of Arimax
print(accuracy(Arimax, test_us))

#### Arima
Arima.mod <- auto.arima(train_us) # train_us is the original sereis withour log 
Arima <- forecast(Arima.mod, h = 8)
#### get the RMSE of Arima
print(accuracy(Arima$mean, test_us))


#### plot original sereis and the four forecast sereis
df.ec <- data.frame(date = as.Date(as.yearmon(as.vector(time(test)))), ec =as.matrix(ec))
df.Var <- data.frame(date = as.Date(as.yearmon(as.vector(time(test)))), Var =as.matrix(Var))
df.Arimax <- data.frame(date = as.Date(as.yearmon(as.vector(time(test)))), Arimax =as.matrix(Arimax))
df.Arima <- data.frame(date = as.Date(as.yearmon(as.vector(time(test)))), Arima =as.matrix(Arima$mean))

ggplot() + 
  geom_line(data=df.US, mapping=aes(x=date, y=US, col = "US")) +
  geom_line(data=df.ec, mapping=aes(x=date, y=ec, col = "ec")) +
  geom_line(data=df.Var, mapping=aes(x=date, y=Var, col = "Var")) +
  geom_line(data=df.Arimax, mapping=aes(x=date, y=Arimax, col = "Arimax")) +
  geom_line(data=df.Arima, mapping=aes(x=date, y=Arima, col = "Arima")) +
  ylab("US HPI") 


## zoom in the plot
df.us.zoom <- df.US[-(1:170),]
ggplot() + 
  #geom_line(data=df.CA, mapping=aes(x=date, y=CA, col = "CA")) +
  geom_line(data=df.us.zoom, mapping=aes(x=date, y=US, col = "US")) +
  #geom_line(data=df.Int, mapping=aes(x=date, y=Int, col = "Int")) +
  geom_line(data=df.ec, mapping=aes(x=date, y=ec, col = "ec")) +
  geom_line(data=df.Var, mapping=aes(x=date, y=Var, col = "Var")) +
  geom_line(data=df.Arimax, mapping=aes(x=date, y=Arimax, col = "Arimax")) +
  geom_line(data=df.Arima, mapping=aes(x=date, y=Arima, col = "Arima")) +
  ylab("US HPI") 

# Granger test
print(grangertest(US.ts ~ CA.ts, order=2))