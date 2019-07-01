mse10y1mDem = as.vector(t(mse10y1mDem))

fcnpfda10y = fcast3$mean + mse10y1mDem

msetest10y1mfda =0
datfda10y = germanyFc10y[41:45,]
msetest10y1mfda = round(sum((as.numeric(t(fcnpfda10y)) - as.vector(t(datfda10y)))^2)/60,4)
msetest10y1mfda= sqrt(msetest10y1mfda)
msetest10y1mfda

# cummse10y1mfda = 0
# for(i in 1:60){
#   cummse10y1mfda = ((round(((as.numeric(fcnpfda10y) - datfda10y)^2)/60,4)))
#   cummse10y1mfda = cumsum((cummse10y1mfda))
# }
# plot(1:60, cummse10y1mfda, type = "l")
for (i in 1:60){
  ger10yCsfe = round(((as.numeric(fcast10y$mean) - as.vector(t(datfda10y)))^2 - (as.vector(t(fcnpfda10y)) - as.vector(t(datfda10y)))^2),4)
  ger10yCsfe = cumsum(ger10yCsfe)
}
plot(1:60, ger10yCsfe, type = "l")

for (i in 1:60){
  ger10yCsfe1 = round(((as.numeric(fcast10y$mean) - as.vector(t(datfda10y)))^2 - (as.vector(t(mse10y1m)) - as.vector(t(datfda10y)))^2),4)
  ger10yCsfe1 = cumsum(ger10yCsfe1)
}
plot(1:60, ger10yCsfe1, type = "l")

for (i in 1:60){
  ger10yCsfe2 = round(((as.numeric(fcast10y$mean) - as.vector(t(datfda10y)))^2 - (as.vector(t(mse10y1mknn)) - as.vector(t(datfda10y)))^2),4)
  ger10yCsfe2 = cumsum(ger10yCsfe2)
}
plot(seq, ger10yCsfe2, type = "l")
