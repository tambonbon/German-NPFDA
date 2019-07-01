setwd("C:/Users/LENOVO/OD/Unimaas/Thesis")
library("xts")
library("anytime")

germanyFc1y <- ((read.csv("germanyFc1y.csv")))
row.names(germanyFc1y) = germanyFc1y[,1]
germanyFc1y[,1] = NULL
germanyFc1y = germanyFc1y[,-13]
germanyFc1y = as.matrix(germanyFc1y)

learning1y = 1:444 # end of 2009
testing1y = 445:492 # ebd of 2013


germany <- ((read.csv("germany.csv", header = T, stringsAsFactors = F)))
names(germany) <- c("Date","1Y", "2Y", "3Y", "4Y", "1y", "6Y", "7Y", "8Y","9Y","10Y")

#germany <- (read.csv("germany.csv", header = T, stringsAsFactors = F))
#for(i in c(1:nrow(germany))){
#  germany[i,2:ncol(germany)] <- as.numeric(germany[i,2:ncol(germany)])
#}

dates <- seq(anytime("1973-01"), anytime("2018-12"), by="months")
germany.ts = xts(x = germany, order.by = dates)
germany <- germany[,-12:-16]
germany.ts <- germany.ts[,-12:-16]
germany.ts = germany.ts[,-1]

###########
# for demeaned
germany <- germany[,c(-1,-12:-16)]
mu = 0
for(i in 1:nrow(germany.ts)){
  mu[i] = sum(as.numeric(germany.ts[i,]))
  mu[i] = mu[i]/10
}
mu = as.matrix(mu)
date <- seq(as.Date("1973-01-01"), as.Date("2013-12-01"), by="months")
mu = mu[1:492,] # for forecast
mu = xts(x = mu, order.by = date)
mu = cbind.data.frame(date,mu)
######
# plot(mu, lty = 1, type = "l")
# 
# names(mu) = c("Dates", "Mean")
# mu = as.matrix(mu)
# 
# mu.ts = xts(mu, order.by = date)
library("tseries")
library("forecast")
mu$clean = tsclean(mu.ts)
plot(mu$clean)
### nothing changes
library(ggplot2)
ggplot(mu, aes(date,mu)) + geom_line() + scale_x_date('month') + ylab("Mean yield (%)") + xlab("")

mu$ma = ma(mu$mu, order = 12)
mu$ma30  = ma(mu$mu, order = 4) 
ggplot() +
  geom_line(data = mu, aes(x = date, y = mu, colour = "Counts")) +
  geom_line(data = mu, aes(x = date, y = ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = mu, aes(x = date, y = ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')
count_ma = ts(na.omit(mu$ma), frequency = 12)
decomp = stl(count_ma, s.window = "periodic")
deseasonal_cnt = seasadj(decomp)
plot(decomp)

###### Stationary test by ADF ###
adf.test(count_ma, alternative = "stationary") #H0: nstationary
# 0.27 -> non stationary
count_mu = ts(na.omit(mu$mu, frequency = 30))
adf.test(count_mu, alternative = "stationary")
# 0.18 -> non stationary

##### ACF & PACF ####
acf(count_ma)
pacf(count_ma)
acf(count_mu)
pacf(count_mu)

####
count_ma_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_ma_d1)
adf.test(count_ma_d1)
acf(count_ma_d1)
pacf(count_ma_d1)

count_mu_d1 = diff(count_mu, differences = 1)
plot(count_mu_d1)
adf.test(count_mu_d1)
acf(count_mu_d1)
pacf(count_mu_d1)
#stationary

###### ARIMA
fit = auto.arima(deseasonal_cnt, seasonal = F)
auto.arima(count_mu,seasonal = F)
tsdisplay(residuals(fit), lag.max = 45, main = '(2,1,1) Model residuals')
fcast = forecast(fit, h=60)
plot(fcast)

fit1 = arima(count_ma_d1, order = c(0,1,0))
tsdisplay(residuals(fit1), lag.max = 45, main = 'Random walk model resids')
fcast1 = forecast(fit1, h=60)
plot(fcast1)

muts = ts(na.omit(mu$mu))
fit2 = auto.arima(count_mu, seasonal = T)
fcast2 = forecast(fit2, h = 60)
plot(fcast2)

fit3  = auto.arima(count_ma_d1, seasonal = F)
fit3
fcast3 = forecast(fit3, h = 60)
plot(fcast3)
tsdisplay(residuals(fit3))


######
forecastnp = as.numeric(fcast1$mean) + as.vector(t(mse1y1mDem))
forecastnp

save.image("gerfc1ymu.RData")
