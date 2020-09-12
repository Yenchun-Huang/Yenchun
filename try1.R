library(shiny)
library(tidyverse)
install.packages("zoo")

setwd("C:\\Users\\User\\Desktop\\ebg408\\shiny")
filepath = "data"
filenames = list.files(path = filepath, pattern="*.csv")
fileset <- lapply(paste0(filepath,"/",filenames), read.csv)
fileset[[13]]
library(zoo)
n = 5
data <- fileset[[13]] %>% 
  mutate(., movavg = rollmean(fileset[[13]]$StockPrice, n,
                              fill = NA, align = "right")) %>%
  mutate(., buy = ifelse(fileset[[13]]$p1Decision=="buy",1,0)
         , sell = ifelse(fileset[[13]]$p1Decision=="sell",1,0)
         , notrade = ifelse(fileset[[13]]$p1Decision=="no trade",1,0))%>% 
  select(., Trials, buy, sell, notrade, StockPrice, movavg)
data.long <- gather(data, price_type, price, StockPrice, movavg)

plot <- ggplot(data.long, aes(x = Trials, y = price, color = price_type))+
  geom_line()+
  geom_point()+
  theme_classic()
plot
for (i in n:101) {
  if(data$movavg[i] == max(data$movavg[n:101])){
    print(i)
    max.trial = i
  }
}
for(i in 1:as.numeric(max.trial)){
  if(sum(data$buy[i:i+9]) >= 9){
    print("第"+i+"回合")
    print("long-holder")
  }
}
for(i in as.numeric(max.trial):92){
  if(sum(data$sell[i:i+9]) >= 9){
    print("第"+i+"回合")
    print("most-profit")
  }
}


