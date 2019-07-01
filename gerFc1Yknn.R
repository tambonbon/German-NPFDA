s2 = 0
gerFc1y.past.learn = germanyFc1y[1:35,]
gerFc1y.past.testing = germanyFc1y[36:41,]
yearEndTest = germanyFc1y[41,]

tGer1y = t(germanyFc1y)

#for(h in 37:46){
for(s in 1:12){
  gerFc1y.futur.s = #pred.reg1y   
    germanyFc1y[39:40,s]
  
  result.pred1y.step.s = funopare.knn(gerFc1y.futur.s,gerFc1y.past.learn,gerFc1y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg1y[h,s] = matrix()[h,result.pred1y.step.s$Predicted.values[s]]
  s2[s] = result.pred1y.step.s$Predicted.values
}
# pred.reg1y[h,s] = matrix(c(h,s2))

#}
s2a=0
for(s in 1:12){
  gerFc1y.futur.s1 = germanyFc1y[40:41,s]
  result.pred1y.step.s = funopare.knn(gerFc1y.futur.s1,gerFc1y.past.learn,gerFc1y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg1y[h,s] = matrix()[h,result.pred1y.step.s$Predicted.values[s]]
  s2a[s] = cbind(result.pred1y.step.s$Predicted.values)
}

s2b=0
for(s in 1:12){
  gerFc1y.futur.s2 = germanyFc1y[41:42,s]
  result.pred1y.step.s = funopare.knn(gerFc1y.futur.s2,gerFc1y.past.learn,gerFc1y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg1y[h,s] = matrix()[h,result.pred1y.step.s$Predicted.values[s]]
  s2b[s] = cbind(result.pred1y.step.s$Predicted.values)
}

#}

s2c=0
for(s in 1:12){
  gerFc1y.futur.s3 = germanyFc1y[42:43,s]
  result.pred1y.step.s = funopare.knn(gerFc1y.futur.s3,gerFc1y.past.learn,gerFc1y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg1y[h,s] = matrix()[h,result.pred1y.step.s$Predicted.values[s]]
  s2c[s] = cbind(result.pred1y.step.s$Predicted.values)
}

s2d=0
for(s in 1:12){
  gerFc1y.futur.s4 = germanyFc1y[43:44,s]
  result.pred1y.step.s = funopare.knn(gerFc1y.futur.s4,gerFc1y.past.learn,gerFc1y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg1y[h,s] = matrix()[h,result.pred1y.step.s$Predicted.values[s]]
  s2d[s] = cbind(result.pred1y.step.s$Predicted.values)
}

mse1y1mknn = rbind(s2,s2a,s2b,s2c,s2d)
mean(mse1y1mknn)
msetest1y1mknn =0
dat = germanyFc1y[41:45,]
msetest1y1mknn = round(sum((mse1y1mknn - dat)^2)/60,4)
msetest1y1mknn = sqrt(msetest1y1mknn)
msetest1y1mknn
# cummse1y1m = 0
# for(i in 1:60){
#   cummse1y1m = ((round(((as.vector(t(mse1y1m)) - as.vector(t(dat)))^2)/10,4)))
#   cummse1y1m = cumsum(cummse1y1m)
# }