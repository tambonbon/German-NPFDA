setwd("C:/Users/LENOVO/OD/Unimaas/Thesis")
library("xts")
library("anytime")

germanyFc5y <- ((read.csv("germanyFc5y.csv")))
row.names(germanyFc5y) = germanyFc5y[,1]
germanyFc5y[,1] = NULL
germanyFc5y = germanyFc5y[,-13]
germanyFc5y = as.matrix(germanyFc5y)

learning5y = 1:444 # end of 2009
testing5y = 445:492 # ebd of 2013


germany <- ((read.csv("germany.csv", header = T, stringsAsFactors = F)))
names(germany) <- c("Date","1Y", "2Y", "3Y", "4Y", "5Y", "6Y", "7Y", "8Y","9Y","10Y")

germany <- (read.csv("germany.csv", header = T, stringsAsFactors = F))
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

ger5ydemeaned = germany[,5] -mu
plot(1:552,ger5ydemeaned)
abline(h = 0)

testlm = lm(ger5ydemeaned ~  c(1:552))
summary(testlm)
lines(1:552, predict(testlm, data.frame(x = 1:552)))


##################
pred.reg5y =  matrix(numeric(), nrow = 2, ncol = 12)
rownames(pred.reg5y) = paste0(seq(37,46))
year = matrix(numeric(), nrow = 10, ncol = 12)

ger5yvec = as.vector(t(germanyFc5y))

s2 = 0
gerFc5y.past.learn = germanyFc5y[1:35,]
gerFc5y.past.testing = germanyFc5y[36:41,]
yearEndTest = germanyFc5y[41,]

tGer5y = t(germanyFc5y)

#for(h in 37:46){
  for(s in 1:12){
 gerFc5y.futur.s = #pred.reg5y   
   germanyFc5y[39:40,s]
 
     result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.s,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2[s] = result.pred5y.step.s$Predicted.values
  }
 # pred.reg5y[h,s] = matrix(c(h,s2))
  
#}
s2a=0
for(s in 1:12){
  gerFc5y.futur.s1 = germanyFc5y[40:41,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.s1,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2a[s] = cbind(result.pred5y.step.s$Predicted.values)
}

s2b=0
for(s in 1:12){
  gerFc5y.futur.s2 = germanyFc5y[41:42,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.s2,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2b[s] = cbind(result.pred5y.step.s$Predicted.values)
}

#}

s2c=0
for(s in 1:12){
  gerFc5y.futur.s3 = germanyFc5y[42:43,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.s3,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2c[s] = cbind(result.pred5y.step.s$Predicted.values)
}

s2d=0
for(s in 1:12){
  gerFc5y.futur.s4 = germanyFc5y[43:44,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.s4,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2d[s] = cbind(result.pred5y.step.s$Predicted.values)
}

mse5y1m = rbind(s2,s2a,s2b,s2c,s2d)
mean(mse5y1m)
msetest5y1m =0
dat = germanyFc5y[41:45,]
msetest5y1m = round(sum((mse5y1m - dat)^2)/60,4)
msetest5y1m = sqrt(msetest5y1m)
msetest5y1m
cummse5y1m = 0
for(i in 1:60){
  cummse5y1m = ((round(((as.vector(t(mse5y1m)) - as.vector(t(dat)))^2)/10,4)))
  cummse5y1m = cumsum(cummse5y1m)
}

# mseTest1 = round(sum((s2 - yearEndTest)^2)/12,4)
# mseTest1 = sqrt(mseTest1)
# mseTest1
# 
# pred5ytest = 0
# for(s in 1:12){
# 
#     gerFc5yTest = germanyFc5y[27:46,s]
#     result.pred5ytest = funopare.knn.lcv(gerFc5yTest,gerFc5y.past.learn,gerFc5y.past.testing,
#                                             4,kind.of.kernel="quadratic",semimetric="pca")
#     pred5ytest[h] = result.pred5ytest$Predicted.values
#   
# }
# pred5ytest = pred5ytest[c(37:46)]
# 
# pred.reg5y = pred.reg5y[c(37:46)]
# mse5y = 0
# mse5ycum = 0
# yearEnd5y = germanyFc5y[37:46,1] #germ[493:552,5] #1 month ahead
# 
# for(i in 493:552){
#   mse5y = round(((pred.reg5y - yearEnd5y)^2)/60,4)
#   mse5ycum = cumsum(sqrt(mse5y))
# }
# mse.reg5y = round(sum((pred.reg5y-yearEnd5y)^2)/10,4)
# mse.reg5y = sqrt(mse.reg5y)
# 
# mseTest = round(sum((pred5ytest - yearEnd5y)^2)/10,4)
# mseTest = sqrt(mseTest)
# 
# mseTest1 = round(sum((s2 - yearEndTest)^2)/12,4)
# mseTest1 = sqrt(mseTest1)
# mseTest1
# 
 seq = seq(as.Date("2014/01/01"), as.Date("2018/12/01"), by = "month")
plot(seq, as.vector(t(mse5y1m)), xlab=" Year", ylab=" Yield ",
     main=paste("1-month horizon: MSE=",msetest5y1m,sep=" " ),lty=1, type = "l")
#points(seq, as.vector(t(dat)))
par(new=T)
plot(seq, as.vector(t(dat)),lty = 6, pch =1, axes = F, xlab = "", ylab="")
#par(new=T)
#plot(seq, cummse5y1m, lty = 2,ylim=c(0,0.2), axes = F, xlab = "", ylab ="")
legend("topright",legend = c("Fitted NP-FDA", "Actual"), lty = c(1,6), pch = c(NA,1))
# 
# mean5y1m = mean(mse5y)
# mean5y1m 
#########################
#########################
# pred5y3m = 0
# for(h in 37:46){
#   for(s in seq(1,12,by = 3)){
#     gerFc5y.futur.3m = germanyFc5y[h,s]
#     result.pred.step.3m = funopare.knn.lcv(gerFc5y.futur.3m,gerFc5y.past.learn,gerFc5y.past.testing,
#                                            5,kind.of.kernel="quadratic",semimetric="pca")
#   }
#   pred5y3m[h] = result.pred.step.3m$Predicted.values
#   
# }

gerFc5y3m = as.matrix(NA,nrow = 2, ncol=12)
gerFc5y3m = germanyFc5y[40:42,]
gerFc5y3m = gerFc5y3m[1,c(-1:-2)]
gerFc5y3m = gerFc5y3m[,c(-4:-12)]


gerFc5y3m = as.vector(tGer5y[472:495])
gerFc5y3m = matrix(gerFc5y3m,nrow = 2, ncol=12)


learntest =(germanyFc5y[1:41,])
testtest = (germanyFc5y[42:46,])
h = 3
futtest = (germanyFc5y[40:41,h])
#for(i in (1:nrow(testtest))){

    resulttest = funopare.knn.lcv(futtest,learntest,testtest,
                                  4,kind.of.kernel="quadratic",semimetric="pca")
    resulttest$Predicted.values
  
#}



s2 = 0
for(s in 1:12){
  gerFc5y.futur.3m1 = gerFc5y3m[,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.3m1,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2[s] = result.pred5y.step.s$Predicted.values
}
# pred.reg5y[h,s] = matrix(c(h,s2))

#}
gerFc5y3m = as.vector(tGer5y[496:519])
gerFc5y3m = matrix(gerFc5y3m,nrow = 2, ncol=12)
s2a=0
for(s in (1:12)){
  gerFc5y.futur.3m2 = gerFc5y3m[,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.3m2,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2a[s] = cbind(result.pred5y.step.s$Predicted.values)
}

s2b=0
for(s in (1:12)){
  gerFc5y.futur.3m3 = gerFc5y3m[42:43,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.3m3,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2b[s] = cbind(result.pred5y.step.s$Predicted.values)
}

#}

s2c=0
for(s in (1:12)){
  gerFc5y.futur.3m4 = gerFc5y3m[43:44,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.3m4,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2c[s] = cbind(result.pred5y.step.s$Predicted.values)
}

s2d=0
for(s in (1:12)){
  gerFc5y.futur.s4 = gerFc5y3m[44:45,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.s4,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2d[s] = (result.pred5y.step.s$Predicted.values)
}


mse5y3m = rbind(s2,s2a,s2b,s2c,s2d)
mse5y3m = mse5y3m[!is.na(mse5y3m)]
mse5y3m = matrix(mse5y3m, 5,4)
mean(mse5y3m)
msetest5y3m =0
dat5y3m = germanyFc5y[41:45,seq(1,12,by =3)]
msetest5y3m = round(sum((as.vector(t(mse5y3m)) - as.vector(t(dat5y3m)))^2)/20,4)
msetest5y3m = sqrt(msetest5y3m)
msetest5y3m

# pred5y3m = pred5y3m[c(37:46)]
# mse5y3m = 0
# mse.reg5y3m = round(sum((pred5y3m-yearEnd5y)^2)/10,4)
# mse.reg5y3m = sqrt(mse.reg5y3m)
install.packages("anytime")
library("anytime")
seq3m = seq(anytime("2014/01"), anytime("2018/12"), by =  3)

plot(1:20, as.vector(t(mse5y3m)), xlab="Year", ylab="Yield ",
     main=paste("3-month horizon: MSE=",msetest5y3m,sep=" " ),lty=1, type = "l", xaxt="n")
par(new=T)
plot(1:60, as.vector(t(dat)), lty = 6, pch = 1, axes = F, xlab = "", ylab="")
axis(1, at = 1, labels = 2014)
axis(1, at =15,labels = 2015)
axis(1, at = 30, labels = 2016)
axis(1, at = 45, labels = 2017)
axis(1, at = 60, labels = 2018)
legend("topright",legend = c("Fitted NP-FDA", "Actual"), lty = c(1,6), pch = c(NA,1))

########################
########################

s2 = 0
for(s in seq(1,12, by = 6)){
  gerFc5y.futur.6m1 = germanyFc5y[40:41,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.6m1,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2[s] = result.pred5y.step.s$Predicted.values
}
# pred.reg5y[h,s] = matrix(c(h,s2))

#}
s2a=0
for(s in seq(1,12, by = 6)){
  gerFc5y.futur.6m2 = germanyFc5y[41:42,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.6m2,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2a[s] = cbind(result.pred5y.step.s$Predicted.values)
}

s2b=0
for(s in seq(1,12, by = 6)){
  gerFc5y.futur.6m3 = germanyFc5y[42:43,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.6m3,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2b[s] = cbind(result.pred5y.step.s$Predicted.values)
}

#}

s2c=0
for(s in seq(1,12, by = 6)){
  gerFc5y.futur.6m4 = germanyFc5y[43:44,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.6m4,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2c[s] = cbind(result.pred5y.step.s$Predicted.values)
}

s2d=0
for(s in seq(1,12, by = 6)){
  gerFc5y.futur.6s4 = germanyFc5y[44:45,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.6s4,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2d[s] = (result.pred5y.step.s$Predicted.values)
}

mse5y6m =  rbind(s2,s2a,s2b,s2c,s2d)
mse5y6m = mse5y6m[!is.na(mse5y6m)]
mse5y6m = matrix(mse5y6m, 5,2)
mean(mse5y3m)
msetest5y6m = 0
dat5y6m = germanyFc5y[41:45,seq(1,12,by =6)]
msetest5y6m = round(sum((mse5y6m - dat5y6m)^2)/20,4)
msetest5y6m = sqrt(msetest5y6m)
msetest5y6m

plot(1:10, as.vector(t(mse5y6m)), xlab="Year", ylab="Yield ",
     main=paste("6-month horizon: MSE=",msetest5y6m,sep=" " ),lty=1, type = "l", xaxt="n")
par(new=T)
plot(1:60, as.vector(t(dat)), lty = 6, pch = 1, axes = F, xlab = "", ylab="")
axis(1, at = 1, labels = 2014)
axis(1, at =15,labels = 2015)
axis(1, at = 30, labels = 2016)
axis(1, at = 45, labels = 2017)
axis(1, at = 60, labels = 2018)
legend("topright",legend = c("Fitted NP-FDA", "Actual"), lty = c(1,6), pch = c(NA,1))

#######################
######################
s2 = 0
for(s in seq(1,12, by = 12)){
  gerFc5y.futur.12m1 = germanyFc5y[40:41,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.12m1,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2[s] = result.pred5y.step.s$Predicted.values
}
# pred.reg5y[h,s] = matrix(c(h,s2))

#}
s2a=0
for(s in seq(1,12, by = 12)){
  gerFc5y.futur.12m2 = germanyFc5y[41:42,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.12m2,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2a[s] = cbind(result.pred5y.step.s$Predicted.values)
}

s2b=0
for(s in seq(1,12, by = 12)){
  gerFc5y.futur.12m3 = germanyFc5y[42:43,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.12m3,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2b[s] = cbind(result.pred5y.step.s$Predicted.values)
}

#}

s2c=0
for(s in seq(1,12, by = 12)){
  gerFc5y.futur.12m4 = germanyFc5y[43:44,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.12m4,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2c[s] = cbind(result.pred5y.step.s$Predicted.values)
}

s2d=0
for(s in seq(1,12, by = 12)){
  gerFc5y.futur.12s4 = germanyFc5y[44:45,s]
  result.pred5y.step.s = funopare.knn.lcv(gerFc5y.futur.12s4,gerFc5y.past.learn,gerFc5y.past.testing,
                                          4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2d[s] = (result.pred5y.step.s$Predicted.values)
}

mse5y12m =  rbind(s2,s2a,s2b,s2c,s2d)
mse5y12m = mse5y12m[!is.na(mse5y12m)]
mse5y12m = matrix(mse5y12m, 5,1)
mean(mse5y3m)
msetest5y12m = 0
dat5y12m = germanyFc5y[41:45,seq(1,12,by =12)]
msetest5y12m = round(sum((mse5y12m - dat5y12m)^2)/10,4)
msetest5y12m = sqrt(msetest5y12m)
msetest5y12m

plot(1:5, as.vector(t(mse5y12m)), xlab="Year", ylab="Yield ",
     main=paste("12-month horizon: MSE=",msetest5y12m,sep=" " ),lty=1, type = "l", xaxt="n")
par(new=T)
plot(1:60, as.vector(t(dat)), lty = 6, pch = 1, axes = F, xlab = "", ylab="")
axis(1, at = 1, labels = 2014)
axis(1, at =15,labels = 2015)
axis(1, at = 30, labels = 2016)
axis(1, at = 45, labels = 2017)
axis(1, at = 60, labels = 2018)
legend("topright",legend = c("Fitted NP-FDA", "Actual"), lty = c(1,6), pch = c(NA,1))

germanyFc = 0

##################
## RANDOM WALK ###
##################
library("xts")
library("forecast")
library("tseries")
ger5yts = xts(germany[1:492,6], order.by = date)
ggplot(ger5yts, aes(date,ger5yts[,1])) + geom_line() + scale_x_date('month') + ylab("Yield (%)") + xlab("")
names(ger5yts) = c("Yields")
ger5yts$ma = ma(ger5yts$Yields, order = 12)
ger5yts = xts(ger5yts, order.by = date)

ger5yts$ma30 = ma(ger5yts$Yields, order = 4)
ger5yts = xts(ger5yts, order.by = date)

ggplot() +
  geom_line(data = ger5yts, aes(x = date, y = Yields, colour = "Counts")) +
  geom_line(data = ger5yts, aes(x = date, y = ma,   colour = "Yearly Moving Average"))  +
  geom_line(data = ger5yts, aes(x = date, y = ma30, colour = "Quaterly Moving Average"))  


##### Decoompose data
ger5yts = cbind.data.frame(date,ger5yts)
count_ma5y = ts(na.omit((ger5yts$ma)), frequency = 12)
decomposed = decompose(count_ma5y, type = "mult")
plot(decomposed)
decomp5y = stl(count_ma5y, s.window = "periodic")
deseason5y = seasadj(decomp5y) #Returns seasonally adjusted data constructed by removing the seasonal component.
plot(decomp5y)

adf.test(count_ma5y, alternative = "stationary") # nonstationary
Acf(count_ma5y, main = "")
Pacf(count_ma5y, main= "")

adf.test(deseason5y, alternative = "stationary") # nonstationary
Acf(deseason5y, main = "")
Pacf(deseason5y, main= "")

count_d1_5y = diff(deseason5y, differences = 1)
plot(count_d1_5y)
adf.test(count_d1_5y, alternative = "stationary") # stationary
Acf(count_d1_5y, main = "")
Pacf(count_d1_5y, main= "")

fit1  = auto.arima(count_d1_5y, seasonal = F)
fit = arima(count_d1_5y, order = c(0,1,0))
tsdisplay(residuals(fit), lag.max = 15, main = "Seasonal Model Resids")
# fcast5y = forecast(fit, h = 60)
fcast5y = forecast(fit1, h = 60)
plot(fcast5y)
fcast5y
plot(forecast(fit1,h=60))

msetest5y1mRw =0
datfda = germanyFc5y[41:45,]
msetest5y1mRw = round(sum((as.numeric(fcast5y$mean) - as.numeric(t(datfda)))^2)/60,4)
msetest5y1mRw= sqrt(msetest5y1mRw)
msetest5y1mRw
# fsave.image("5y3m.RData")
  