mse2y1mDem = as.vector(t(mse2y1mDem))

fcnpfda2y = fcast3$mean + mse2y1mDem

mean(fcnpfda2y)

msetest2y1mfda =0
datfda2y = germanyFc2y[41:45,]
msetest2y1mfda = round(sum((as.numeric(t(fcnpfda2y)) - as.vector(t(datfda2y)))^2)/60,4)
msetest2y1mfda= sqrt(msetest2y1mfda)
msetest2y1mfda

# cummse2y1mfda = 0
# for(i in 1:60){
#   cummse2y1mfda = ((round(((as.numeric(fcnpfda2y) - datfda2y)^2)/60,4)))
#   cummse2y1mfda = cumsum((cummse2y1mfda))
# }
# plot(1:60, cummse2y1mfda, type = "l")
for (i in 1:60){
  ger2yCsfe = round(((as.numeric(fcast2y$mean) - as.vector(t(datfda2y)))^2 - (as.vector(t(fcnpfda2y)) - as.vector(t(datfda2y)))^2),4)
  ger2yCsfe = cumsum(ger2yCsfe)
}
plot(1:60, ger2yCsfe, type = "l")

for (i in 1:60){
  ger2yCsfe1 = round(((as.numeric(fcast2y$mean) - as.vector(t(datfda2y)))^2 - (as.vector(t(mse2y1m)) - as.vector(t(datfda2y)))^2),4)
  ger2yCsfe1 = cumsum(ger2yCsfe1)
}
plot(1:60, ger2yCsfe1, type = "l")

for (i in 1:60){
  ger2yCsfe2 = round(((as.numeric(fcast2y$mean) - as.vector(t(datfda2y)))^2 - (as.vector(t(mse2y1mknn)) - as.vector(t(datfda2y)))^2),4)
  ger2yCsfe2 = cumsum(ger2yCsfe2)
}
plot(seq, ger2yCsfe2, type = "l")
