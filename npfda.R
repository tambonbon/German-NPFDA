fcnpfda1 = fcast1$mean + mse1y1mDem
fcnpfda2 = fcast1$mean + mse2y1mDem
fcnpfda5 = fcast1$mean + mse5y1mDem
fcnpfda10 = fcast1$mean + mse10y1mDem

# plot(seq, ger1yCsfe, main = "Maturity 12-months", type = "l", lwd =2)
# par(new = T)
# plot(seq, ger1yCsfe1, lty = 3, axes = F, xlab ="", ylab="", type = "l", lwd =2)
# legend(y = -5,legend = c("Fitted NP-FDA", "Actual"), lty = c(1,6), pch = c(NA,1))
library("xts")

fc1y = cbind.data.frame(ger1yCsfe1, ger1yCsfe, ger1yCsfe2)
dat1yFc = xts(fc1y, order.by = seq)
names(dat1yFc) = c("KER", "DEM", "MU")

par(mfrow = c(1,1))
plot.xts(dat1yFc, screens = 1,bg = "white",  legend.loc = "bottomright", auto.legend=TRUE, main = "12-month", cex = 0.5)
events = xts(c("Brexit"), as.Date(c("2016-06-23")))
par(mfrow = c(2,2))
addEventLines(events, srt = 90,on = 0, pos =2 , lwd = 2, col = "blue")

fc2y = cbind.data.frame(ger2yCsfe1, ger2yCsfe, ger2yCsfe2)
dat2yFc = xts(fc2y, order.by = seq)
names(dat2yFc) = c("KER", "DEM", "MU")
pdf(file = NULL)
plot.xts(dat2yFc, screens = 1,bg = "white",  legend.loc = "bottomright", auto.legend=TRUE, main = "24-month", plot = F, cex = 0.5)
dev.off()
events = xts(c("Brexit"), as.Date(c("2016-06-23")))
addEventLines(events, srt = 90, pos =2 , lwd = 2, col = "blue")

fc5y = cbind.data.frame(ger5yCsfe1, ger5yCsfe, ger5yCsfe2)
dat5yFc = xts(fc5y, order.by = seq)
names(dat5yFc) = c("KER", "DEM", "MU")
pdf(file = NULL)
plot.xts(dat5yFc, screens = 1,bg = "white",  legend.loc = "bottomright", auto.legend=TRUE, main = "60-month", cex = 0.5)
dev.off()
events = xts(c("Brexit"), as.Date(c("2016-06-23")))
addEventLines(events, srt = 90, pos =2 , lwd = 2, col = "blue")


fc10y = cbind.data.frame(ger10yCsfe1, ger10yCsfe,ger10yCsfe2)
dat10yFc = xts(fc10y, order.by = seq)
names(dat10yFc) = c("KER", "DEM", "MU")
pdf(file = NULL)
plot.xts(dat10yFc, screens = 1,bg = "white",  legend.loc = "bottomright", auto.legend=TRUE, main = "120-month", cex = 0.5)
dev.off() 
events = xts(c("Brexit"), as.Date(c("2016-06-23")))
addEventLines(events, srt = 90, pos =2 , lwd = 2, col = "blue")

par(mfrow = c(2,2))
plot(1:10, germany[50,], xlab = "Year of maturities", ylab = "Yields (%)", main = "Yield curve in 1977-01-31", type = "l", lwd =2)
plot(1:10, germany[150,], xlab = "Year of maturities", ylab = "Yields (%)", main = "Yield curve in 1984-05-31",type ="l")
plot(1:10, germany[300,], xlab = "Year of maturities", ylab = "Yields (%)", main = "Yield curve in 1997-01-31", type = "l", lwd =2)
plot(1:10, germany[310,], xlab = "Year of maturities", ylab = "Yields (%)", main = "Yield curve in 1997-10-31", type = "l", lwd =2)

germany.xts = as.data.frame(germany)
germany.xts = xts(germany.xts, order.by = dates)
par(mfrow = c(1,1))
plot.xts(germany.xts, legend.loc = "topright", cex = 0.5, main = "German term structure", ylab = "Yields")

par(mfrow = c(2,2))
plot(seq, as.vector(t(mse1y1m)), type = "l", lwd =2,xlab=" Year", ylab=" Yield (%) ",
     main=("Forecasting yield for 12-month bond"))
par(new = T)
plot(seq, as.vector(t(mse1y1mknn)), type = "l", lwd =2, lty = 3, axes = F, xlab = "", ylab="")
par(new = T)
plot(seq, rev(as.vector(t(fcnpfda1y))), type = "l", lwd =2, lty = 4, axes = F, xlab = "", ylab="")
par(new=T)
plot(seq,as.vector(t(datfda1y)),axes = F, xlab = "", ylab="")
par(new = F)

plot(seq, as.vector(t(mse2y1m)), type = "l", lwd =2,xlab=" Year", ylab=" Yield (%) ",
     main=("Forecasting yield for 24-month bond"))
par(new = T)
plot(seq, as.vector(t(mse2y1mknn)), type = "l", lwd =2, lty = 3,  axes = F, xlab = "", ylab="")
par(new = T)
plot(seq, rev(as.vector(t(fcnpfda2y))), type = "l", lwd =2, lty = 4, axes = F, xlab = "", ylab="")
par(new=T)
plot(seq,as.vector(t(datfda2y)),axes = F, xlab = "", ylab="")


plot(seq, as.vector(t(mse5y1m)), type = "l", lwd =2,xlab=" Year", ylab=" Yield (%) ",
     main=("Forecasting yield for 60-month bond"))
par(new = T)
plot(seq, as.vector(t(mse5y1mknn)), type = "l", lwd =2, lty = 3,axes = F, xlab = "", ylab="")
par(new = T)
plot(seq, (as.vector(t(fcnpfda5y))), type = "l", lwd =2, lty = 4,axes = F, xlab = "", ylab="")
par(new=T)
plot(seq,as.vector(t(datfda5y)),axes = F, xlab = "", ylab="")


plot(seq, as.vector(t(mse10y1m)), type = "l", lwd =2,xlab=" Year", ylab=" Yield (%) ",
     main=("Forecasting yield for 120-month bond"))
par(new = T)
plot(seq, as.vector(t(mse10y1mknn)), type = "l", lwd =2, lty = 3,axes = F, xlab = "", ylab="")
par(new = T)
plot(seq, rev(as.vector(t(fcnpfda10y))), type = "l", lwd =2, lty = 4, axes = F, xlab = "", ylab="")
par(new=T)
plot(seq,as.vector(t(datfda10y)),axes = F, xlab = "", ylab="", lwd =2)
legend(x = )
#save.image("npfda.RData")

