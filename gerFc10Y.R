setwd("C:/Users/LENOVO/OD/Unimaas/Thesis")
library("xts")
library("anytime")

germanyFc10y <- ((read.csv("germanyFc10y.csv")))
row.names(germanyFc10y) = germanyFc10y[,1]
germanyFc10y[,1] = NULL
germanyFc10y = germanyFc10y[,-13]
germanyFc10y = as.matrix(germanyFc10y)

learning10y = 1:444 # end of 2009
testing10y = 445:492 # ebd of 2013


germany <- ((read.csv("germany.csv", header = T, stringsAsFactors = F)))
names(germany) <- c("Date","10y", "10y", "3Y", "4Y", "10y", "6Y", "7Y", "8Y","9Y","10Y")

# germany <- (read.csv("germany.csv", header = T, stringsAsFactors = F))
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

ger10ydemeaned = germany[,10] -mu
plot(1:552,ger10ydemeaned, type = "l")
abline(h = 0)

testlm = lm(ger10ydemeaned ~  c(1:552))
summary(testlm)
lines(1:552, predict(testlm, data.frame(x = 1:552)))


##################
pred.reg10y =  matrix(numeric(), nrow = 2, ncol = 12)
rownames(pred.reg10y) = paste0(seq(37,46))
year = matrix(numeric(), nrow = 10, ncol = 12)

ger10yvec = as.vector(t(germanyFc10y))

s2 = 0
gerFc10y.past.learn = germanyFc10y[1:35,]
gerFc10y.past.testing = germanyFc10y[36:41,]
yearEndTest = germanyFc10y[41,]

tGer10y = t(germanyFc10y)

#for(h in 37:46){
for(s in 1:12){
  gerFc10y.futur.s = #pred.reg10y   
    germanyFc10y[39:40,s]
  
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.s,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2[s] = result.pred10y.step.s$Predicted.values
}
# pred.reg10y[h,s] = matrix(c(h,s2))

#}
s2a=0
for(s in 1:12){
  gerFc10y.futur.s1 = germanyFc10y[40:41,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.s1,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2a[s] = cbind(result.pred10y.step.s$Predicted.values)
}

s2b=0
for(s in 1:12){
  gerFc10y.futur.s2 = germanyFc10y[41:42,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.s2,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2b[s] = cbind(result.pred10y.step.s$Predicted.values)
}

#}

s2c=0
for(s in 1:12){
  gerFc10y.futur.s3 = germanyFc10y[42:43,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.s3,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2c[s] = cbind(result.pred10y.step.s$Predicted.values)
}

s2d=0
for(s in 1:12){
  gerFc10y.futur.s4 = germanyFc10y[43:44,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.s4,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2d[s] = cbind(result.pred10y.step.s$Predicted.values)
}

############# TEST ###########
##############################
for(s in 1:12){
  gerFc10y.futur.s = #pred.reg10y   
    germanyFc10y[39:40,s]
  
  result.pred10y.step.s = fun.knn.lcv(gerFc10y.futur.s,gerFc10y.past.learn,gerFc10y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="deriv")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2[s] = result.pred10y.step.s$Predicted.values
}
# pred.reg10y[h,s] = matrix(c(h,s2))

#}
s2a=0
for(s in 1:12){
  gerFc10y.futur.s1 = germanyFc10y[40:41,s]
  result.pred10y.step.s = funopare.knn(gerFc10y.futur.s1,gerFc10y.past.learn,gerFc10y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="deriv",)
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2a[s] = cbind(result.pred10y.step.s$Predicted.values)
}

s2b=0
for(s in 1:12){
  gerFc10y.futur.s2 = germanyFc10y[41:42,s]
  result.pred10y.step.s = fun.knn.lcv(gerFc10y.futur.s2,gerFc10y.past.learn,gerFc10y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="deriv")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2b[s] = cbind(result.pred10y.step.s$Predicted.values)
}

#}

s2c=0
for(s in 1:12){
  gerFc10y.futur.s3 = germanyFc10y[42:43,s]
  result.pred10y.step.s = fun.knn.lcv(gerFc10y.futur.s3,gerFc10y.past.learn,gerFc10y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="deriv")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2c[s] = cbind(result.pred10y.step.s$Predicted.values)
}

s2d=0
for(s in 1:12){
  gerFc10y.futur.s4 = germanyFc10y[43:44,s]
  result.pred10y.step.s = fun.knn.lcv(gerFc10y.futur.s4,gerFc10y.past.learn,gerFc10y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="deriv")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2d[s] = cbind(result.pred10y.step.s$Predicted.values)
}



#############################
mse10y1m = rbind(s2,s2a,s2b,s2c,s2d)
mean(mse10y1m)
msetest10y1m =0
dat = germanyFc10y[41:45,]
msetest10y1m = round(sum((as.vector(t(mse10y1m)) - as.numeric(t(dat)))^2)/60,4)
msetest10y1m = sqrt(msetest10y1m)
msetest10y1m
cummse10y1m = 0
for(i in 1:60){
  cummse10y1m = ((round(((as.vector(t(mse10y1m)) - as.vector(t(dat)))^2)/10,4)))
  cummse10y1m = cumsum(cummse10y1m)
}

# mseTest1 = round(sum((s2 - yearEndTest)^2)/12,4)
# mseTest1 = sqrt(mseTest1)
# mseTest1
# 
# pred10ytest = 0
# for(s in 1:12){
# 
#     gerFc10yTest = germanyFc10y[27:46,s]
#     result.pred10ytest = funopare.knn.lcv(gerFc10yTest,gerFc10y.past.learn,gerFc10y.past.testing,
#                                             4,kind.of.kernel="quadratic",semimetric="pca")
#     pred10ytest[h] = result.pred10ytest$Predicted.values
#   
# }
# pred10ytest = pred10ytest[c(37:46)]
# 
# pred.reg10y = pred.reg10y[c(37:46)]
# mse10y = 0
# mse10ycum = 0
# yearEnd10y = germanyFc10y[37:46,1] #germ[493:552,5] #1 month ahead
# 
# for(i in 493:552){
#   mse10y = round(((pred.reg10y - yearEnd10y)^2)/60,4)
#   mse10ycum = cumsum(sqrt(mse10y))
# }
# mse.reg10y = round(sum((pred.reg10y-yearEnd10y)^2)/10,4)
# mse.reg10y = sqrt(mse.reg10y)
# 
# mseTest = round(sum((pred10ytest - yearEnd10y)^2)/10,4)
# mseTest = sqrt(mseTest)
# 
# mseTest1 = round(sum((s2 - yearEndTest)^2)/12,4)
# mseTest1 = sqrt(mseTest1)
# mseTest1
# 
seq = seq(as.Date("2014/01/01"), as.Date("2018/12/01"), by = "month")
plot(seq, as.vector(t(mse10y1m)), xlab=" Year", ylab=" Yield ",
     main=paste("1-month horizon: MSE=",msetest10y1m,sep=" " ),lty=1, type = "l")
#points(seq, as.vector(t(dat)))
par(new=T)
plot(seq, as.vector(t(dat)),lty = 6, pch =1, axes = F, xlab = "", ylab="")
#par(new=T)
#plot(seq, cummse10y1m, lty = 2,ylim=c(0,0.2), axes = F, xlab = "", ylab ="")
legend("topright",legend = c("Fitted NP-FDA", "Actual"), lty = c(1,6), pch = c(NA,1))
# 
# mean10y1m = mean(mse10y)
# mean10y1m 
#########################
#########################
# pred10y3m = 0
# for(h in 37:46){
#   for(s in seq(1,12,by = 3)){
#     gerFc10y.futur.3m = germanyFc10y[h,s]
#     result.pred.step.3m = funopare.knn.lcv(gerFc10y.futur.3m,gerFc10y.past.learn,gerFc10y.past.testing,
#                                            5,kind.of.kernel="quadratic",semimetric="pca")
#   }
#   pred10y3m[h] = result.pred.step.3m$Predicted.values
#   
# }

gerFc10y3m = as.matrix(NA,nrow = 2, ncol=12)
gerFc10y3m = germanyFc10y[40:42,]
gerFc10y3m = gerFc10y3m[1,c(-1:-2)]
gerFc10y3m = gerFc10y3m[,c(-4:-12)]


gerFc10y3m = as.vector(tGer10y[472:495])
gerFc10y3m = matrix(gerFc10y3m,nrow = 2, ncol=12)


learntest =(germanyFc10y[1:41,])
testtest = (germanyFc10y[42:46,])
h = 3
futtest = (germanyFc10y[40:41,h])
#for(i in (1:nrow(testtest))){

resulttest = funopare.knn.lcv(futtest,learntest,testtest,
                              4,kind.of.kernel="quadratic",semimetric="pca")
resulttest$Predicted.values

#}



s2 = 0
for(s in 1:12){
  gerFc10y.futur.3m1 = gerFc10y3m[,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.3m1,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2[s] = result.pred10y.step.s$Predicted.values
}
# pred.reg10y[h,s] = matrix(c(h,s2))

#}
gerFc10y3m = as.vector(tGer10y[496:519])
gerFc10y3m = matrix(gerFc10y3m,nrow = 2, ncol=12)
s2a=0
for(s in (1:12)){
  gerFc10y.futur.3m2 = gerFc10y3m[,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.3m2,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2a[s] = cbind(result.pred10y.step.s$Predicted.values)
}

s2b=0
for(s in (1:12)){
  gerFc10y.futur.3m3 = gerFc10y3m[42:43,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.3m3,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2b[s] = cbind(result.pred10y.step.s$Predicted.values)
}

#}

s2c=0
for(s in (1:12)){
  gerFc10y.futur.3m4 = gerFc10y3m[43:44,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.3m4,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2c[s] = cbind(result.pred10y.step.s$Predicted.values)
}

s2d=0
for(s in (1:12)){
  gerFc10y.futur.s4 = gerFc10y3m[44:45,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.s4,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2d[s] = (result.pred10y.step.s$Predicted.values)
}


mse10y3m = rbind(s2,s2a,s2b,s2c,s2d)
mse10y3m = mse10y3m[!is.na(mse10y3m)]
mse10y3m = matrix(mse10y3m, 5,4)
mean(mse10y3m)
msetest10y3m =0
dat10y3m = germanyFc10y[41:45,seq(1,12,by =3)]
msetest10y3m = round(sum((as.vector(t(mse10y3m)) - as.vector(t(dat10y3m)))^2)/20,4)
msetest10y3m = sqrt(msetest10y3m)
msetest10y3m

# pred10y3m = pred10y3m[c(37:46)]
# mse10y3m = 0
# mse.reg10y3m = round(sum((pred10y3m-yearEnd10y)^2)/10,4)
# mse.reg10y3m = sqrt(mse.reg10y3m)
install.packages("anytime")
library("anytime")
seq3m = seq(anytime("2014/01"), anytime("2018/12"), by =  3)

plot(1:20, as.vector(t(mse10y3m)), xlab="Year", ylab="Yield ",
     main=paste("3-month horizon: MSE=",msetest10y3m,sep=" " ),lty=1, type = "l", xaxt="n")
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
  gerFc10y.futur.6m1 = germanyFc10y[40:41,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.6m1,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2[s] = result.pred10y.step.s$Predicted.values
}
# pred.reg10y[h,s] = matrix(c(h,s2))

#}
s2a=0
for(s in seq(1,12, by = 6)){
  gerFc10y.futur.6m2 = germanyFc10y[41:42,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.6m2,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2a[s] = cbind(result.pred10y.step.s$Predicted.values)
}

s2b=0
for(s in seq(1,12, by = 6)){
  gerFc10y.futur.6m3 = germanyFc10y[42:43,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.6m3,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2b[s] = cbind(result.pred10y.step.s$Predicted.values)
}

#}

s2c=0
for(s in seq(1,12, by = 6)){
  gerFc10y.futur.6m4 = germanyFc10y[43:44,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.6m4,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2c[s] = cbind(result.pred10y.step.s$Predicted.values)
}

s2d=0
for(s in seq(1,12, by = 6)){
  gerFc10y.futur.6s4 = germanyFc10y[44:45,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.6s4,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2d[s] = (result.pred10y.step.s$Predicted.values)
}

mse10y6m =  rbind(s2,s2a,s2b,s2c,s2d)
mse10y6m = mse10y6m[!is.na(mse10y6m)]
mse10y6m = matrix(mse10y6m, 5,2)
mean(mse10y3m)
msetest10y6m = 0
dat10y6m = germanyFc10y[41:45,seq(1,12,by =6)]
msetest10y6m = round(sum((mse10y6m - dat10y6m)^2)/20,4)
msetest10y6m = sqrt(msetest10y6m)
msetest10y6m

plot(1:10, as.vector(t(mse10y6m)), xlab="Year", ylab="Yield ",
     main=paste("6-month horizon: MSE=",msetest10y6m,sep=" " ),lty=1, type = "l", xaxt="n")
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
  gerFc10y.futur.12m1 = germanyFc10y[40:41,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.12m1,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2[s] = result.pred10y.step.s$Predicted.values
}
# pred.reg10y[h,s] = matrix(c(h,s2))

#}
s2a=0
for(s in seq(1,12, by = 12)){
  gerFc10y.futur.12m2 = germanyFc10y[41:42,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.12m2,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2a[s] = cbind(result.pred10y.step.s$Predicted.values)
}

s2b=0
for(s in seq(1,12, by = 12)){
  gerFc10y.futur.12m3 = germanyFc10y[42:43,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.12m3,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2b[s] = cbind(result.pred10y.step.s$Predicted.values)
}

#}

s2c=0
for(s in seq(1,12, by = 12)){
  gerFc10y.futur.12m4 = germanyFc10y[43:44,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.12m4,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2c[s] = cbind(result.pred10y.step.s$Predicted.values)
}

s2d=0
for(s in seq(1,12, by = 12)){
  gerFc10y.futur.12s4 = germanyFc10y[44:45,s]
  result.pred10y.step.s = funopare.knn.lcv(gerFc10y.futur.12s4,gerFc10y.past.learn,gerFc10y.past.testing,
                                           4,kind.of.kernel="quadratic",semimetric="pca")
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2d[s] = (result.pred10y.step.s$Predicted.values)
}

mse10y12m =  rbind(s2,s2a,s2b,s2c,s2d)
mse10y12m = mse10y12m[!is.na(mse10y12m)]
mse10y12m = matrix(mse10y12m, 5,1)
mean(mse10y3m)
msetest10y12m = 0
dat10y12m = germanyFc10y[41:45,seq(1,12,by =12)]
msetest10y12m = round(sum((mse10y12m - dat10y12m)^2)/10,4)
msetest10y12m = sqrt(msetest10y12m)
msetest10y12m

plot(1:5, as.vector(t(mse10y12m)), xlab="Year", ylab="Yield ",
     main=paste("12-month horizon: MSE=",msetest10y12m,sep=" " ),lty=1, type = "l", xaxt="n")
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
library("ggplot2")
date <- seq(as.Date("1973-01-01"), as.Date("2013-12-01"), by="months")

ger10yts = xts(germany[1:492,10], order.by = date)
ggplot(ger10yts, aes(date,ger10yts[,1])) + geom_line() + scale_x_date('month') + ylab("Yield (%)") + xlab("")
names(ger10yts) = c("Yields")
ger10yts$ma = ma(ger10yts$Yields, order = 12)
ger10yts = xts(ger10yts, order.by = date)

ger10yts$ma30 = ma(ger10yts$Yields, order = 4)
ger10yts = xts(ger10yts, order.by = date)

ggplot() +
  geom_line(data = ger10yts, aes(x = date, y = Yields, colour = "Counts")) +
  geom_line(data = ger10yts, aes(x = date, y = ma,   colour = "Yearly Moving Average"))  +
  geom_line(data = ger10yts, aes(x = date, y = ma30, colour = "Quaterly Moving Average"))  


##### Decoompose data
ger10yts = cbind.data.frame(date,ger10yts)
count_ma10y = ts(na.omit((ger10yts$ma)), frequency = 12)
decomposed = decompose(count_ma10y, type = "mult")
plot(decomposed)
decomp10y = stl(count_ma10y, s.window = "periodic")
deseason10y = seasadj(decomp10y) #Returns seasonally adjusted data constructed by removing the seasonal component.
plot(decomp10y)

adf.test(count_ma10y, alternative = "stationary") # stationary
Acf(count_ma10y, main = "")
Pacf(count_ma10y, main= "")
# 
adf.test(deseason10y, alternative = "stationary")
Acf(deseason10y)
Pacf(deseason10y)


count_d1_10y = diff(deseason10y, differences = 1)
plot(count_d1_10y)
adf.test(count_d1_10y, alternative = "stationary") # stationary
Acf(count_d1_10y, main = "")
Pacf(count_d1_10y, main= "")

fit1  = auto.arima(deseason10y, seasonal = F)
fit = arima(count_d1_10y, order = c(0,1,0))
tsdisplay(residuals(fit), lag.max = 15, main = "Seasonal Model Resids")
fcast10y = forecast(fit, h = 60)
plot(fcast10y)
fcast10y
plot(forecast(fit1,h=60))

msetest10y1mRw =0
datfda = germanyFc10y[41:45,]
msetest10y1mRw = round(sum((as.numeric(fcast10y$mean) - as.numeric(t(datfda)))^2)/60,4)
msetest10y1mRw= sqrt(msetest10y1mRw)
msetest10y1mRw
#fsave.image("10y3m.RData")
