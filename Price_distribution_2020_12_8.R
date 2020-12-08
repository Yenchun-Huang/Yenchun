library(tidyverse)
library(zoo)
library(ggpubr)
library(cowplot)
library(DataCombine)
setwd("C:\\Users\\User\\Desktop\\ebg408\\shiny")
filepath = "data"
filenames = list.files(path = filepath, pattern="*.csv")
fileset <- lapply(paste0(filepath,"/",filenames), read.csv)

SelectMaxPrice <- function(player.no){
  if(player.no%%2 == 1){
    data.cp <- data.frame(trials = fileset[[player.no]]$Trials,
                          Stock = fileset[[player.no]]$p1Stock,
                          Decision = fileset[[player.no]]$p1Decision,
                          StockPrice = fileset[[player.no]]$StockPrice)
  }else{
    data.cp <- data.frame(trials = fileset[[player.no]]$Trials,
                          Stock = fileset[[player.no]]$p2Stock,
                          Decision = fileset[[player.no]]$p2Decision,
                          StockPrice = fileset[[player.no]]$StockPrice)
  }
  data <- data.cp %>% 
    select(., trials, Stock, StockPrice) %>% 
    filter(., trials %in% c(1:100))
  Max_price = max(data$StockPrice)
  Max_trials = c(data$trials[data$StockPrice == Max_price])
  need_data <- data.frame("Player.no" = player.no,
                          "Max_price" = Max_price,
                          "Max_trials" = Max_trials)
  
  return(need_data)
}
SelectMinPrice <- function(player.no){
  if(player.no%%2 == 1){
    data.cp <- data.frame(trials = fileset[[player.no]]$Trials,
                          Stock = fileset[[player.no]]$p1Stock,
                          Decision = fileset[[player.no]]$p1Decision,
                          StockPrice = fileset[[player.no]]$StockPrice)
  }else{
    data.cp <- data.frame(trials = fileset[[player.no]]$Trials,
                          Stock = fileset[[player.no]]$p2Stock,
                          Decision = fileset[[player.no]]$p2Decision,
                          StockPrice = fileset[[player.no]]$StockPrice)
  }
  data <- data.cp %>% 
    select(., trials, Stock, StockPrice) %>% 
    filter(., trials %in% c(1:100))
  Min_price = min(data$StockPrice)
  Min_trials = c(data$trials[data$StockPrice == Min_price])
  need_data <- data.frame("Player.no" = player.no,
                          "Min_price" = Min_price,
                          "Min_trials" = Min_trials)
  
  return(need_data)
}

Max_Price_list <- list()
Min_Price_list <- list()
for (i in 11:160) {
  Max_Price_list[[i]] <- SelectMaxPrice(i)
  Min_Price_list[[i]] <- SelectMinPrice(i)
}
Max_Price_data <- do.call(rbind, Max_Price_list)
Min_Price_data <- do.call(rbind, Min_Price_list)

ggplot(Max_Price_data)+
  geom_histogram(aes(Max_price),binwidth = 5)

ggplot(Max_Price_data)+
  geom_histogram(aes(Max_trials),binwidth = 1)
