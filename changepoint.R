install.packages("bcp")
install.packages("tidyverse")

setwd("C:\\Users\\User\\Desktop\\ebg408\\shiny")
filepath = "data"
filenames = list.files(path = filepath, pattern="*.csv")

x13 = read.csv(paste0("data/", filenames[13]), header = TRUE)

library(bcp)
bcp.7 <- bcp(x13$p1Stock)
bcp.plot <- plot(bcp.7, separated = FALSE, main = "Change point of StockPrice",
     xlab = "trials")
ggplotly(bcp.plot)
bcp.7$posterior.prob


point.list <- list()
for (i in 1:(length(bcp.7$posterior.prob)-1)){
  if(bcp.7$posterior.prob[i] > 0.5){
    trial = bcp.7[["data"]][,1][i]
    prob = bcp.7[["posterior.prob"]][i]
    table = data.frame(trial, prob)
    point.list[[i]] <- table
  }
}
point.df <- do.call(rbind.data.frame,point.list)

point.DF <- data.frame()
for (i in 1:(length(bcp.7$posterior.prob)-1)){
  if(bcp.7$posterior.prob[i] > 0.5){
    point.DF <- append(point.DF,i)
    point.DF <- append(point.DF,bcp.7$posterior.prob[i])
    
  }
}
