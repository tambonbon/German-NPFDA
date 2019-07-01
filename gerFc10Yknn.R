s2 = 0
gerFc10y.past.learn = germanyFc10y[1:35,]
gerFc10y.past.testing = germanyFc10y[36:41,]
yearEndTest = germanyFc10y[41,]

tGer10y = t(germanyFc10y)

#for(h in 37:46){
for(s in 1:12){
  gerFc10y.futur.s = #pred.reg10y   
    germanyFc10y[39:40,s]
  
  result.pred10y.step.s = funopare.knn(gerFc10y.futur.s,gerFc10y.past.learn,gerFc10y.past.testing,
                                       4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2[s] = result.pred10y.step.s$Predicted.values
}
# pred.reg10y[h,s] = matrix(c(h,s2))

#}
s2a=0
for(s in 1:12){
  gerFc10y.futur.s1 = germanyFc10y[40:41,s]
  result.pred10y.step.s = funopare.knn(gerFc10y.futur.s1,gerFc10y.past.learn,gerFc10y.past.testing,
                                       4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2a[s] = cbind(result.pred10y.step.s$Predicted.values)
}

s2b=0
for(s in 1:12){
  gerFc10y.futur.s2 = germanyFc10y[41:42,s]
  result.pred10y.step.s = funopare.knn(gerFc10y.futur.s2,gerFc10y.past.learn,gerFc10y.past.testing,
                                       4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2b[s] = cbind(result.pred10y.step.s$Predicted.values)
}

#}

s2c=0
for(s in 1:12){
  gerFc10y.futur.s3 = germanyFc10y[42:43,s]
  result.pred10y.step.s = funopare.knn(gerFc10y.futur.s3,gerFc10y.past.learn,gerFc10y.past.testing,
                                       4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2c[s] = cbind(result.pred10y.step.s$Predicted.values)
}

s2d=0
for(s in 1:12){
  gerFc10y.futur.s4 = germanyFc10y[43:44,s]
  result.pred10y.step.s = funopare.knn(gerFc10y.futur.s4,gerFc10y.past.learn,gerFc10y.past.testing,
                                       4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg10y[h,s] = matrix()[h,result.pred10y.step.s$Predicted.values[s]]
  s2d[s] = cbind(result.pred10y.step.s$Predicted.values)
}

mse10y1mknn = rbind(s2,s2a,s2b,s2c,s2d)
mean(mse10y1mknn)
msetest10y1mknn =0
dat = germanyFc10y[41:45,]
msetest10y1mknn = round(sum((mse10y1mknn - dat)^2)/60,4)
msetest10y1mknn = sqrt(msetest10y1mknn)
msetest10y1mknn
# cummse10y1m = 0
# for(i in 1:60){
#   cummse10y1m = ((round(((as.vector(t(mse10y1m)) - as.vector(t(dat)))^2)/10,4)))
#   cummse10y1m = cumsum(cummse10y1m)
# }