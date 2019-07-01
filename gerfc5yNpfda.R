mse5y1mDem = as.vector(t(mse5y1mDem))

fcnpfda5y = fcast3$mean + mse5y1mDem

msetest5y1mfda =0
datfda5y = germanyFc5y[41:45,]
msetest5y1mfda = round(sum((as.numeric(t(fcnpfda5y)) - as.vector(t(datfda5y)))^2)/60,4)
msetest5y1mfda= sqrt(msetest5y1mfda)
msetest5y1mfda

# cummse5y1mfda = 0
# for(i in 1:60){
#   cummse5y1mfda = ((round(((as.numeric(t(fcnpfda5y)) - as.vector(t(datfda5y)))^2)/60,4)))
#   cummse5y1mfda = cumsum((cummse5y1mfda))
# }
# plot(1:60, cummse5y1mfda, type = "l")
for (i in 1:60){
  ger5yCsfe = round(((as.numeric(fcast5y$mean) - as.vector(t(datfda5y)))^2 - (as.numeric(t(fcnpfda5y)) - as.vector(t(datfda5y)))^2),4)
  ger5yCsfe = cumsum(ger5yCsfe)
}
plot(1:60, ger5yCsfe, type = "l")

for (i in 1:60){
  ger5yCsfe1 = round(((as.numeric(fcast5y$mean) - as.vector(t(datfda5y)))^2 - (as.vector(t(mse5y1m)) - as.vector(t(datfda5y)))^2),4)
  ger5yCsfe1 = cumsum(ger5yCsfe1)
}
plot(1:60, ger5yCsfe1, type = "l")

for (i in 1:60){
  ger5yCsfe2 = round(((as.numeric(fcast5y$mean) - as.vector(t(datfda5y)))^2 - (as.vector(t(mse5y1mknn)) - as.vector(t(datfda5y)))^2),4)
  ger5yCsfe2 = cumsum(ger5yCsfe2)
}
plot(seq, ger5yCsfe2, type = "l")