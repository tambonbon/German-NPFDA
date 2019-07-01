s2 = 0
gerFc2y.past.learn = germanyFc2y[1:35,]
gerFc2y.past.testing = germanyFc2y[36:41,]
yearEndTest = germanyFc2y[41,]

tGer2y = t(germanyFc2y)

#for(h in 37:46){
for(s in 1:12){
  gerFc2y.futur.s = #pred.reg2y   
    germanyFc2y[39:40,s]
  
  result.pred2y.step.s = funopare.knn(gerFc2y.futur.s,gerFc2y.past.learn,gerFc2y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg2y[h,s] = matrix()[h,result.pred2y.step.s$Predicted.values[s]]
  s2[s] = result.pred2y.step.s$Predicted.values
}
# pred.reg2y[h,s] = matrix(c(h,s2))

#}
s2a=0
for(s in 1:12){
  gerFc2y.futur.s1 = germanyFc2y[40:41,s]
  result.pred2y.step.s = funopare.knn(gerFc2y.futur.s1,gerFc2y.past.learn,gerFc2y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg2y[h,s] = matrix()[h,result.pred2y.step.s$Predicted.values[s]]
  s2a[s] = cbind(result.pred2y.step.s$Predicted.values)
}

s2b=0
for(s in 1:12){
  gerFc2y.futur.s2 = germanyFc2y[41:42,s]
  result.pred2y.step.s = funopare.knn(gerFc2y.futur.s2,gerFc2y.past.learn,gerFc2y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg2y[h,s] = matrix()[h,result.pred2y.step.s$Predicted.values[s]]
  s2b[s] = cbind(result.pred2y.step.s$Predicted.values)
}

#}

s2c=0
for(s in 1:12){
  gerFc2y.futur.s3 = germanyFc2y[42:43,s]
  result.pred2y.step.s = funopare.knn(gerFc2y.futur.s3,gerFc2y.past.learn,gerFc2y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg2y[h,s] = matrix()[h,result.pred2y.step.s$Predicted.values[s]]
  s2c[s] = cbind(result.pred2y.step.s$Predicted.values)
}

s2d=0
for(s in 1:12){
  gerFc2y.futur.s4 = germanyFc2y[43:44,s]
  result.pred2y.step.s = funopare.knn(gerFc2y.futur.s4,gerFc2y.past.learn,gerFc2y.past.testing,
                                      4,kind.of.kernel="quadratic",semimetric="pca", q = mu)
  #pred.reg2y[h,s] = matrix()[h,result.pred2y.step.s$Predicted.values[s]]
  s2d[s] = cbind(result.pred2y.step.s$Predicted.values)
}

mse2y1mknn = rbind(s2,s2a,s2b,s2c,s2d)
mean(mse2y1mknn)
msetest2y1mknn =0
dat = germanyFc2y[41:45,]
msetest2y1mknn = round(sum((mse2y1mknn - dat)^2)/60,4)
msetest2y1mknn = sqrt(msetest2y1mknn)
msetest2y1mknn
# cummse2y1m = 0
# for(i in 1:60){
#   cummse2y1m = ((round(((as.vector(t(mse2y1m)) - as.vector(t(dat)))^2)/10,4)))
#   cummse2y1m = cumsum(cummse2y1m)
# }