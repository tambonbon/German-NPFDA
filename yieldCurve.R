rm(list = ls(all = TRUE))                        # Remove all previous objects
setwd("C:/Users/LENOVO/OD/Unimaas/Thesis")

library("xts")
install.packages("ggplot2")
library("ggplot2")
install.packages("ggthemes")
library("ggthemes")
install.packages("plotly")
library("plotly")
install.packages("reshape2")
library("reshape2")  # To reshape data sets
install.packages("anytime")
library("anytime")

germany <- (read.csv("germany.csv", header = T, stringsAsFactors = F))
# for(i in c(1:nrow(germany))){
#  germany[i,2:ncol(germany)] <- as.numeric(germany[i,2:ncol(germany)])
# }

dates <- seq(anytime("1973-01"), anytime("2018-12"), by="months")
germany.ts1 = xts(x = germany, order.by = dates)
germany <- germany[,-12:-16]
germany.ts1 <- germany.ts1[,-12:-16]
germany.ts1 = germany.ts1[,-1]

names(germany) <- c("Date","1Y", "2Y", "3Y", "4Y", "5Y", "6Y", "7Y", "8Y","9Y","10Y")
names(germany.ts1) <- c("1Y", "2Y", "3Y", "4Y", "5Y", "6Y", "7Y", "8Y","9Y","10Y")
# row.names(germany) = germany[,1]
# germany[,1] = NULL
# germ = t(germany)
# colnames(germ) = germ[1,]
# germ[1,] = NULL

v <- ggplot(germany.ts1,
            aes(x = date, y = value, color = variable)) +
  geom_line(size = 1) +
  scale_color_gdocs("", breaks = names(germany)) +
  labs(title = "Germany Zero-coupon yields",
       y = "Percent", x = "Date", caption = "Source: CEIC")
v
plot.xts(germany.ts1)
ggsave("gbond-yields.png", v, width = 8, height = 4.5, dpi = 300)
germany.ts1 = as.data.frame(germany.ts1)
p <- plot_ly(z = ~germany.ts1, y = index(germany.ts1), x = names(germany.ts1), 
             colors = gdocs_pal()(3)) %>%
  add_surface() %>%
  layout(title = "Germany Zero-coupon yields<br />Source: CEIC",
         scene = list(
           xaxis = list(title = "Maturity", gridcolor = "rgb(255, 255, 255)",
                        zerolinecolor = "rgb(255, 255, 255)",
                        showbackground = TRUE,
                        backgroundcolor = "rgb(240, 240,240)"), showgrid = F, 
           yaxis = list(title = "Date", gridcolor = "rgb(255, 255, 255)",
                        zerolinecolor = "rgb(255, 255, 255)",
                        showbackground = TRUE,
                        backgroundcolor = "rgb(230, 230,230)"), showgrid = F, 
           zaxis = list(title = "Percent", gridcolor = "rgb(255, 255, 255)",
                        zerolinecolor = "rgb(255, 255, 255)",
                        showbackground = TRUE,
                        backgroundcolor = "rgb(220, 220,220)"), showgrid = F
         ))
p
htmlwidgets::saveWidget(as_widget(p), "bond-yields.html")
