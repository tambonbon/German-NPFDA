mse1y1mDem = as.vector(t(mse1y1mDem))

fcnpfda1y = fcast3$mean + mse1y1mDem

msetest1y1mfda =0
datfda1y = germanyFc1y[41:45,]
msetest1y1mfda = round(sum((as.numeric(t(fcnpfda1y)) - as.vector(t(datfda1y)))^2)/60,4)
msetest1y1mfda= sqrt(msetest1y1mfda)
msetest1y1mfda

# cummse1y1mfda = 0
# for(i in 1:60){
#   cummse1y1mfda = ((round(((as.numeric(fcnpfda1y) - datfda1y)^2)/60,4)))
#   cummse1y1mfda = cumsum((cummse1y1mfda))
# }
# plot(1:60, cummse1y1mfda, type = "l")

for (i in 1:60){
  ger1yCsfe = round(((as.numeric(fcast1y$mean) - as.vector(t(datfda1y)))^2 - (as.vector(t(fcnpfda1y)) - as.vector(t(datfda1y)))^2),4)
  ger1yCsfe = cumsum(ger1yCsfe)
}
plot(seq, ger1yCsfe, type = "l")

for (i in 1:60){
  ger1yCsfe1 = round(((as.numeric(fcast1y$mean) - as.vector(t(datfda1y)))^2 - (as.vector(t(mse1y1m)) - as.vector(t(datfda1y)))^2),4)
  ger1yCsfe1 = cumsum(ger1yCsfe1)
}
plot(seq, ger1yCsfe1, type = "l")

for (i in 1:60){
  ger1yCsfe2 = round(((as.numeric(fcast1y$mean) - as.vector(t(datfda1y)))^2 - (as.vector(t(mse1y1mknn)) - as.vector(t(datfda1y)))^2),4)
  ger1yCsfe2 = cumsum(ger1yCsfe2)
}
plot(seq, ger1yCsfe2, type = "l")
