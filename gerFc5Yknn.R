s2 = 0
gerFc5y.past.learn = germanyFc5y[1:35,]
gerFc5y.past.testing = germanyFc5y[36:41,]
yearEndTest = germanyFc5y[41,]

tGer5y = t(germanyFc5y)

#for(h in 37:46){
for(s in 1:12){
  gerFc5y.futur.s = #pred.reg5y   
    germanyFc5y[39:40,s]
  
  result.pred5y.step.s = funopare.knn(gerFc5y.futur.s,gerFc5y.past.learn,gerFc5y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2[s] = result.pred5y.step.s$Predicted.values
}
# pred.reg5y[h,s] = matrix(c(h,s2))

#}
s2a=0
for(s in 1:12){
  gerFc5y.futur.s1 = germanyFc5y[40:41,s]
  result.pred5y.step.s = funopare.knn(gerFc5y.futur.s1,gerFc5y.past.learn,gerFc5y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2a[s] = cbind(result.pred5y.step.s$Predicted.values)
}

s2b=0
for(s in 1:12){
  gerFc5y.futur.s2 = germanyFc5y[41:42,s]
  result.pred5y.step.s = funopare.knn(gerFc5y.futur.s2,gerFc5y.past.learn,gerFc5y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2b[s] = cbind(result.pred5y.step.s$Predicted.values)
}

#}

s2c=0
for(s in 1:12){
  gerFc5y.futur.s3 = germanyFc5y[42:43,s]
  result.pred5y.step.s = funopare.knn(gerFc5y.futur.s3,gerFc5y.past.learn,gerFc5y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2c[s] = cbind(result.pred5y.step.s$Predicted.values)
}

s2d=0
for(s in 1:12){
  gerFc5y.futur.s4 = germanyFc5y[43:44,s]
  result.pred5y.step.s = funopare.knn(gerFc5y.futur.s4,gerFc5y.past.learn,gerFc5y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg5y[h,s] = matrix()[h,result.pred5y.step.s$Predicted.values[s]]
  s2d[s] = cbind(result.pred5y.step.s$Predicted.values)
}

mse5y1mknn = rbind(s2,s2a,s2b,s2c,s2d)
mean(mse5y1mknn)
msetest5y1mknn =0
dat = germanyFc5y[41:45,]
msetest5y1mknn = round(sum((mse5y1mknn - dat)^2)/60,4)
msetest5y1mknn = sqrt(msetest5y1mknn)
msetest5y1mknn
# cummse5y1m = 0
# for(i in 1:60){
#   cummse5y1m = ((round(((as.vector(t(mse5y1m)) - as.vector(t(dat)))^2)/10,4)))
#   cummse5y1m = cumsum(cummse5y1m)
# }