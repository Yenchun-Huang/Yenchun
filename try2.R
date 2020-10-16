library(shiny)
library(tidyverse)
library(zoo)
setwd("C:\\Users\\User\\Desktop\\ebg408\\shiny")
filepath = "data"
filenames = list.files(path = filepath, pattern="*.csv")
fileset <- lapply(paste0(filepath,"/",filenames), read.csv)
data.cp <- data.frame(trials = fileset[[11]]$Trials, Stock = fileset[[11]]$p1Stock, Decision = fileset[[11]]$p1Decision)
data.cp <- data.cp %>% 
  mutate(., buy = ifelse(Decision=="buy",1,0)
         , sell = ifelse(Decision=="sell",1,0)
         , notrade = ifelse(Decision=="no trade",1,0))%>%
  mutate(., lag_Decision = lag(Decision)) %>% 
  select(., trials, Stock, Decision, lag_Decision,buy,sell,notrade) %>% 
  mutate(., player.no = 11)

#mark notrade ####
no_trade <- function(data){
  nt_start <- list()
  nt_end <- list()
  nt_trials <- list()
  for (i in 1:length(data$trials)){
    for (j in (i+1):length(data$trials)) {
      notrade_avg <- mean(data$notrade[i:j])
      if(j-i >=10 & notrade_avg>= 0.8){
        nt_trials <- append(nt_trials, j-i)
        nt_start <- append(nt_start, i)
        nt_end <- append(nt_end, j)
      }
    }
  }
  if(length(nt_trials) != 0){
    for (a in 1:length(nt_trials)) {
      if(nt_trials[[a]] == max(unlist(nt_trials))){
        start = data[nt_start[[a]],]$trials
        end = data[nt_end[[a]],]$trials
        section_type <- data.frame(type = "NA", start = 1, end = start)
        section_type <- add_row(section_type, type = "no trade", start = start, end = end)
        section_type <- add_row(section_type, type = "NA", start = end, end = length(data$trials))
        data.n1 <- filter(data, data$trials < nt_start[[a]])
        data.n2 <- filter(data, nt_end[[a]] < data$trials)
        data.n <- rbind(data.n1, data.n2)
        result <- list(data = data.n, type = section_type)
        return(result)
      }
    }
  }else{
    data.n <- data
    section_type <- data.frame()
    result <- list(data = data.n, type = section_type)
    return(result)
  }
}
#mark short-term trade####
short_term <- function(data){
  sh_start <- list()
  sh_end <- list()
  sh_trials <- list()
  for (i in 1:length(data$trials)){
    for (j in (i+1):length(data$trials)) {
      stock <- data$Stock
      delta_stock <- abs(max(stock[i:j])-min(stock[i:j]))
      if((j-i >=10) & delta_stock<=2){
        sh_trials <- append(sh_trials, j-i)
        sh_start <- append(sh_start, i)
        sh_end <- append(sh_end, j)
      }
    }
  }
  if(length(sh_trials) != 0){
    for (a in 1:length(sh_trials)) {
      if(sh_trials[[a]] == max(unlist(sh_trials))){
        start = data[sh_start[[a]],]$trials
        end = data[sh_end[[a]],]$trials
        section_type <- data.frame(type = "NA", start = 1, end = start)
        section_type <- add_row(section_type, type = "short-term", start = start, end = end)
        section_type <- add_row(section_type, type = "NA", start = end, end = length(data$trials))
        data.sh1 <- data %>% filter(., trials < start)
        data.sh2 <- data %>% filter(., trials > end)
        data.sh <- rbind(data.sh1, data.sh2)
        result <- list(data = data.sh, type = section_type)
        return(result)
      }
    }
  }else{
    data.sh <- data
    section_type <- data.frame()
    result <- list(data = data.sh, type = section_type)
    return(result)
  }
}
#mark long-position####
long_position <- function(data){
  lp_start <- list()
  lp_trials <- list()
  lp_slope <- list()
  for (max in 1:length(data$trials)) {
    if(data$Stock[max] == max(data$Stock)){
      max_point <- data$trials[max]
    }
  }
  # for (i in 1:max_point){
  #   for (j in (i+1):max_point) {
  #     delta_stock <- max(data$Stock[i:j])-min(data$Stock[i:j])
  #     if(delta_stock>=10){
  #       lp_trials <- append(lp_trials, j-i)
  #       lp_start <- append(lp_start, i)
  #       lp_end <- append(lp_end, j)
  #       lp_slope <- append(lp_slope, (delta_stock/(j-i)))
  #     }else if(j-i >=10 & (delta_stock/(j-i))>=0.8){
  #       lp_trials <- append(lp_trials, j-i)
  #       lp_start <- append(lp_start, i)
  #       lp_end <- append(lp_end, j)
  #       lp_slope <- append(lp_slope, (delta_stock/(j-i)))
  #     }
  #   }
  # }
  for(i in 1:max_point){
    delta_stock <- max(data$Stock[i:max_point]) - min(data$Stock[i:max_point])
    
    if(delta_stock>=10){
      lp_trials <- append(lp_trials, max_point-i)
      lp_start <- append(lp_start, i)
      lp_slope <- append(lp_slope, (delta_stock/(max_point-i)))
      }else if(max_point-i >=10 & (delta_stock/(max_point-i))>=0.8){
        lp_trials <- append(lp_trials, max_point-i)
        lp_start <- append(lp_start, i)
        lp_slope <- append(lp_slope, (delta_stock/(max_point-i)))
      }
  }
  if(length(lp_trials)!=0){
    for (a in 1:length(lp_trials)) {
      if(lp_slope[[a]] == max(unlist(lp_slope))){
        start = data[lp_start[[a]],]$trials
        section_type <- data.frame(type = "NA", start = 1, end = start)
        section_type <- add_row(section_type, type = "long-position", start = start, end = max_point)
        section_type <- add_row(section_type, type = "NA", start = max_point, end = length(data$trials))
        data.lp1 <- data %>% filter(., trials < start)
        data.lp2 <- data %>% filter(., trials > max_point)
        result <- list(data1 = data.lp1, data2 = data.lp2, type = section_type)
        return(result)
      }
    }
  }else{
    data.lp <- data
    section_type <- data.frame()
    result <- list(data = data.lp, type = section_type)
    return(result)
  }
}
long_position(data.cp)
#mark short-position####
short_position <- function(data){
  sp_start <- list()
  sp_end <- list()
  sp_trials <- list()
  sp_slope <- list()
  for (max in 1:length(data$trials)) {
    if(data$Stock[max] == max(data$Stock)){
      max_point <- data$trials[max]
    }
  }
  # for (i in max_point:length(data$trials)){
  #   for (j in (i+1):length(data$trials)) {
  #     delta_stock <- min(data$Stock[i:j])-max(data$Stock[i:j])
  #     if(!is.na(delta_stock)&delta_stock <= -10){
  #       sp_trials <- append(sp_trials, j-i)
  #       sp_start <- append(sp_start, i)
  #       sp_end <- append(sp_end, j)
  #       sp_slope <- append(sp_slope, (delta_stock/(j-i)))
  #     }else if(j-i >=10 & (delta_stock/(j-i))<= -0.8){
  #       sp_trials <- append(sp_trials, j-i)
  #       sp_start <- append(sp_start, i)
  #       sp_end <- append(sp_end, j)
  #       sp_slope <- append(sp_slope, (delta_stock/(j-i)))
  #     }
  #   }
  # }
  for(i in max_point:length(data$trials)){
    delta_stock <- min(max_point:i) - max(max_point:i)
    if(delta_stock <= -10){
      sp_trials <- append(sp_trials, i-max_point)
      sp_end <- append(sp_end, i)
      sp_slope <- append(sp_slope, delta_stock/(i-max_point))
    }else if (i-max_point >=10 & delta_stock/(i-max_point)<= -0.8){
      sp_trials <- append(sp_trials, i-max_point)
      sp_end <- append(sp_end, i)
      sp_slope <- append(sp_slope, delta_stock/(i-max_point))
    }
  }
  if(length(sp_trials)!=0){
    for (a in 1:length(sp_trials)) {
      if(sp_slope[[a]] == min(unlist(sp_slope))){
        end = data[sp_end[[a]],]$trials
        section_type <- data.frame(type = "NA", start = 1, end = max_point)
        section_type <- add_row(section_type, type = "short-position", start = max_point, end = end)
        section_type <- add_row(section_type, type = "NA", start = end, end = length(data$trials))
        data.sp1 <- data %>% filter(., trials < max_point)
        data.sp2 <- data %>% filter(., trials > end)
        data.sp <- rbind(data.sp1, data.sp2)
        result <- list(data = data.sp, type = section_type)
        return(result)
      }
    }
  }else{
    data.sp <- data
    section_type <- data.frame()
    result <- list(data = data.sp, type = section_type)
    return(result)
  }
}
short_position(data.cp)

if(length(no_trade(data.cp)$type) != 0){
  result.nt <- no_trade(data.cp)$type %>% 
    mutate(., trials = end - start, stage = c(1:length(.)), player.no = paste0("no.", data.cp$player.no[1]))
  result <- result.nt
}else{
  result.nt <- data.frame()
}
if(length(short_term(data.cp)$type) != 0){
  result.st <- short_term(data.cp)$type %>% 
    mutate(., trials = end - start, stage = c(1:length(.)), player.no = paste0("no.", data.cp$player.no[1]))
}else{
  result.st <- data.frame()
}
if(length(short_position(data.cp)$type) != 0){
  result.sp <- short_position(data.cp)$type %>% 
    mutate(., trials = end - start, stage = c(1:length(.)), player.no = paste0("no.", data.cp$player.no[1]))
}else{
  result.sp <- data.frame()
}
if(length(long_position(data.cp)$type != 0)){
  result.lp <- long_position(data.cp)$type %>% 
    mutate(., trials = end - start, stage = c(1:length(.)), player.no = paste0("no.", data.cp$player.no[1]))
}else{
  result.lp <- data.frame()
}
result <- list(result.nt, result.st, result.lp, result.sp)
# result <- rbind(result.nt, result.st, result.lp, result.sp)
g <- ggplot(width = 0.5)
for(i in 1:length(result)){
  if(length(result[i][[1]]!=0)){
    g <- g+
      geom_col(data = result[i][[1]], 
               aes(x = player.no, y = trials, group = stage, fill = type, label = start, label1 = end, alpha = 0.5),
               width = 0.1,
               position = position_stack(reverse = TRUE))
  }
}
g <- g +
  scale_fill_manual(breaks = c("NA", "no trade", "short-term", "short-position", "long-position"),
                    values = c("NA","#161a1d", "#fed766", "#fe4a49", "#009fb7")
  )+
  theme_classic()+
  coord_flip()
ggplotly(g)

PlotFunc <- function(player.no){
  player.data <- fileset[[player.no]][-101,]
  if (player.no%%2 ==1){
    needed.data <- data.frame(trial = player.data$Trials,
                              action = player.data$p1Decision,
                              stock = player.data$p1Stock)
  }else{
    needed.data <- data.frame(trial = player.data$Trials,
                              action = player.data$p2Decision,
                              stock = player.data$p2Stock)
  }
  plt <- ggplot(needed.data, aes(x=trial, y=stock)) +
    ggtitle(paste0("Subject No. ",player.no))+
    geom_point(aes(color=action))
  ggplotly(plt)
}
PlotFunc(37)


# Maxpoint <- function(data){
#   for (max in 1:length(data$trials)) {
#     if(data.cp$Stock[max] == max(data$Stock)){
#       max_point1 <- data.cp$trials[max]
#     }
#   }
#   data.cp.max <- filter(data, data$Stock != max(data$Stock))
#   for (max in 1:length(data.cp.max$trials)) {
#     if(data.cp.max$Stock[max] == max(data.cp.max$Stock)){
#       max_point2 <- data.cp.max$trials[max]
#     }
#   }
#   if(max_point1 > max_point2){
#     m = max_point1
#     max_point1 = max_point2 
#     max_point2 = m
#   }
#   return(c(max_point1, max_point2))
# }










