library(tidyverse)
library(zoo)
install.packages("zoo")

setwd("C:\\Users\\User\\Desktop\\ebg408\\shiny")
filepath = "data"
filenames = list.files(path = filepath, pattern="*.csv")
fileset <- lapply(paste0(filepath,"/",filenames), read.csv)

data <- fileset[[27]] %>% 
  mutate(., buy = ifelse(fileset[[27]]$p1Decision=="buy",1,0)
         , sell = ifelse(fileset[[27]]$p1Decision=="sell",1,0)
         , notrade = ifelse(fileset[[27]]$p1Decision=="no trade",1,0))%>%
  mutate(.,Decision = p1Decision, lag_Decision = lag(p1Decision)) %>% 
  select(., Trials, Decision, lag_Decision,buy,sell,notrade)

change_point <- function(data, nmoving, ntrials){
  selected.data1 <- data %>% 
    mutate(.,mov_buy = rollsum(buy, as.integer(nmoving), fill = NA, align = "right"),
           mov_sell = rollsum(sell, as.integer(nmoving), fill = NA, align = "right"),
           mov_notrade = rollsum(notrade, as.integer(nmoving), fill = NA, align = "right")) %>% 
    select(., Trials, Decision, lag_Decision, mov_buy, mov_sell, mov_notrade)
  type_list1 <- list()
  trials_s_list1 <- list()
  trials_e_list1 <- list()
  for (i in as.integer(ntrials):101) {
    n <- round(as.integer(ntrials*0.8))
    if(selected.data1$mov_buy[i] >= n){
      trials_s_list1 <- append(trials_s_list1, i-n)
      trials_e_list1 <- append(trials_e_list1, i)
      type_list1 <- append(type_list1,"80% buy")
    }else if(selected.data1$mov_sell[i] >= n){
      trials_s_list1 <- append(trials_s_list1, i-n)
      trials_e_list1 <- append(trials_e_list1, i)
      type_list1 <- append(type_list1,"80% sell")
    }else if(selected.data1$mov_notrade[i] >= n){
      trials_s_list1 <- append(trials_s_list1, i-n)
      trials_e_list1 <- append(trials_e_list1, i)
      type_list1 <- append(type_list1,"80% no trade")
    }else{
      j = i-n
      table <- table(selected.data1$Decision[j], selected.data1$lag_Decision[j])
      if(table[4,2]==1){
        j = j+1
        table1 = table(selected.data1$Decision[j],selected.data1$lag_Decision[j])
        if(table1[2,4]==1){
          j = j+1
          table2 = table(selected.data1$Decision[j],selected.data1$lag_Decision[j])
          if(table2[4,2]==1){
            j = j+1
            table3 = table(selected.data1$Decision[j],selected.data1$lag_Decision[j])
            if(table3[2,4]==1){
              j = j+1
              table4 = table(selected.data1$Decision[j],selected.data1$lag_Decision[j])
              if(table4[4,2]==1){
                trials_s_list1 <- append(trials_s_list1, selected.data1$Trials[j-n])
                trials_e_list1 <- append(trials_e_list1, selected.data1$Trials[j])
                type_list1 <- append(type_list1,"cross")
              }
            }
          }
        }
      }
      if(table[2,4]==1){
        j = j+1
        table1 = table(selected.data1$Decision[j],selected.data1$lag_Decision[j])
        if(table1[4,2]==1){
          j = j+1
          table2 = table(selected.data1$Decision[j],selected.data1$lag_Decision[j])
          if(table2[2,4]==1){
            j = j+1
            table3 = table(selected.data1$Decision[j],selected.data1$lag_Decision[j])
            if(table3[4,2]==1){
              j = j+1
              table4 = table(selected.data1$Decision[j],selected.data1$lag_Decision[j])
              if(table4[2,4]==1){
                trials_s_list1 <- append(trials_s_list1, selected.data1$Trials[j-n])
                trials_e_list1 <- append(trials_e_list1, selected.data1$Trials[j])
                type_list1 <- append(type_list1,"cross")
              }
            }
          }
        }
      }
    }
  }
  trials_s_df1 <- do.call(rbind, trials_s_list1)
  trials_e_df1 <- do.call(rbind, trials_e_list1)
  type_df1 <- do.call(rbind, type_list1)
  change_point.data1 <- data.frame(start = trials_s_df1, end = trials_e_df1, type = type_df1)
  change_point_list1 <- list()
  for(n in 1:length(change_point.data1$type)){
    for (m in 1:length(change_point.data1$type)){
      if(change_point.data1$end[n]+2 == change_point.data1$start[m]){
        if(change_point.data1$type[n] != change_point.data1$type[m]){
          change_point_list1 <- append(change_point_list1, change_point.data1$end[n]+1)
        }
      }
    }
  }
  change_point1 <- do.call(rbind, change_point_list1)
  p1change_point <- data.frame(p1 = change_point1)
  return(p1change_point)
}
a <- change_point(data, 5,5)





















#test long|short-position
# n = 5
# data <- fileset[[23]] %>% 
#   mutate(., movavg = rollmean(fileset[[23]]$StockPrice, n,
#                               fill = NA, align = "right")) %>%
#   mutate(., buy = ifelse(fileset[[23]]$p1Decision=="buy",1,0)
#          , sell = ifelse(fileset[[23]]$p1Decision=="sell",1,0)
#          , notrade = ifelse(fileset[[23]]$p1Decision=="no trade",1,0))
#   # select(., Trials, buy, sell, notrade, StockPrice, movavg)
# data.long <- gather(data, price_type, price, StockPrice, movavg)
# 
# plot <- ggplot(data.long, aes(x = Trials, y = price, color = price_type))+
#   geom_line()+
#   geom_point()+
#   theme_classic()
# plot
# for (i in n:101) {
#   if(data$movavg[i] == max(data$movavg[n:101])){
#     print(i)
#     max.trial = i
#   }
# }
# data.roll <- data %>% 
#   mutate(., roll_buy = rollsum(data$buy, 10, fill = NA, align = "right")
#          , roll_sell = rollsum(data$sell, 10, fill = NA, align = "right")
#          ,roll_nt = rollsum(data$notrade, 10, fill = NA, align = "right")
#   ) 
# 
# actionlist <- list()
# for(j in 1:as.numeric(max.trial)){
#   if(data.roll$roll_buy[j+9] >= 9){actionlist <- append(actionlist,1)}
#   else{actionlist <- append(actionlist,0)}
# }
# for(k in as.numeric(max.trial):92){
#   if(data.roll$roll_sell[j+9] >= 9){actionlist <- append(actionlist,1)}
#   else{actionlist <- append(actionlist,0)}
# }
# actiondf <- do.call(rbind, actionlist)
# typelist <- list()
# if(sum(actiondf[1:max.trial])==0){
#   typelist <- append(typelist, "False")
#   }else{
#     typelist <- append(typelist, "True")}
# if(sum(actiondf[max.trial:92])==0){
#   typelist <- append(typelist, "False")
#   }else{
#     typelist <- append(typelist, "True")}
# p1 <- do.call(rbind, typelist)
# typedf <- data.frame(p1, row.names = c("long-position", "short-psosition"))
# 
# 
# 
