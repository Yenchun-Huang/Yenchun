library(shiny)
library(tidyverse)
library(zoo)
setwd("C:\\Users\\User\\Desktop\\ebg408\\shiny")
filepath = "data"
filenames = list.files(path = filepath, pattern="*.csv")
fileset <- lapply(paste0(filepath,"/",filenames), read.csv)
data.cp <- data.frame(trials = fileset[[1]]$Trials, Stock = fileset[[21]]$p1Stock, Decision = fileset[[21]]$p1Decision)
data.cp <- data.cp %>% 
  mutate(., buy = ifelse(Decision=="buy",1,0)
         , sell = ifelse(Decision=="sell",1,0)
         , notrade = ifelse(Decision=="no trade",1,0))%>%
  mutate(., lag_Decision = lag(Decision)) %>% 
  select(., trials, Stock, Decision, lag_Decision,buy,sell,notrade) %>% 
  mutate(., player.no = 13)

FindInterval <- function(list_start, list_end, string_type){
  result.list <- list()  
  interval_start = list_start[[1]] 
  interval_end = list_start[[1]] 
  i_interval = 1  
  i_result = 1
  
  while (i_interval <= length(list_start)){
    interval_start = list_start[[i_interval]]
    
    if (interval_start >= interval_end){
      while (list_start[[i_interval]] == interval_start){
        interval_end = list_end[[i_interval]]
        if (i_interval == length(list_start)){
          break
        }else{
          i_interval = i_interval + 1
        }
      }
      if (i_interval == length(list_start) & interval_end < 100){
        result.row <- data.frame(start = result.list[[i_result-1]]$end + 1, end = interval_start - 1, type = "NA")
        result.list[[i_result]] <- result.row
        i_result = i_result + 1
        result.row <- data.frame(start = interval_start, end = interval_end, type = string_type)
        result.list[[i_result]] <- result.row
        i_result = i_result + 1
        result.row <- data.frame(start = interval_end + 1, end = 100, type = "NA")
        result.list[[i_result]] <- result.row
        i_result = i_result + 1
      }else if (i_result == 1 & interval_start > 1){
        result.row <- data.frame(start = 1, end = interval_start-1, type = "NA")
        result.list[[i_result]] <- result.row
        i_result = i_result + 1
        result.row <- data.frame(start = interval_start, end = interval_end, type = string_type)
        result.list[[i_result]] <- result.row
        i_result = i_result + 1
      }else {
        result.row <- data.frame(start = result.list[[i_result-1]]$end + 1, end = interval_start - 1, type = "NA")
        result.list[[i_result]] <- result.row
        i_result = i_result + 1
        result.row <- data.frame(start = interval_start, end = interval_end, type = string_type)
        result.list[[i_result]] <- result.row
        i_result = i_result + 1
      }
    }else{
      i_interval = i_interval + 1
    }
    
  }
  result.table = do.call(rbind, result.list)
  return(result.table)
}
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
    result <- FindInterval(nt_start, nt_end, string_type = "unchange")
    return(result)
    # for (a in 1:length(nt_trials)) {
    #   if(nt_trials[[a]] == max(unlist(nt_trials))){
    #     start = data[nt_start[[a]],]$trials
    #     end = data[nt_end[[a]],]$trials
    #     section_type <- data.frame(type = "NA", start = 1, end = start)
    #     section_type <- add_row(section_type, type = "no trade", start = start, end = end)
    #     section_type <- add_row(section_type, type = "NA", start = end, end = length(data$trials))
    #     data.n1 <- filter(data, data$trials < nt_start[[a]])
    #     data.n2 <- filter(data, nt_end[[a]] < data$trials)
    #     data.n <- rbind(data.n1, data.n2)
    #     result <- list(data = data.n, type = section_type)
    #     return(result)
    #   }
    # }
  }else{
    section_type <- data.frame()
    result <- list(type = section_type)
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
    result <- FindInterval(sh_start, sh_end, string_type = "flat")
    return(result)
    # for (a in 1:length(sh_trials)) {
    #   if(sh_trials[[a]] == max(unlist(sh_trials))){
    #     start = data[sh_start[[a]],]$trials
    #     end = data[sh_end[[a]],]$trials
    #     section_type <- data.frame(type = "NA", start = 1, end = start)
    #     section_type <- add_row(section_type, type = "short-term", start = start, end = end)
    #     section_type <- add_row(section_type, type = "NA", start = end, end = length(data$trials))
    #     data.sh1 <- data %>% filter(., trials < start)
    #     data.sh2 <- data %>% filter(., trials > end)
    #     data.sh <- rbind(data.sh1, data.sh2)
    #     result <- list(data = data.sh, type = section_type)
    #     return(result)
    #   }
    # }
  }else{
    section_type <- data.frame()
    result <- list(type = section_type)
    return(result)
  }
}
#mark long-position####
long_position <- function(data){
  lp_start <- list()
  lp_trials <- list()
  lp_slope <- list()
  lp_ds <- list()
  lp_start_s <- list()
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
      lp_ds <- append(lp_ds, delta_stock)
    }else if(max_point-i >=10 & (delta_stock/(max_point-i))>=0.8){
      lp_trials <- append(lp_trials, max_point-i)
      lp_start <- append(lp_start, i)
      lp_ds <- append(lp_ds, delta_stock)
    }
  }
  if(length(lp_trials)!=0){
    for (a in 1:length(lp_trials)) {
      if(lp_ds[[a]] == max(unlist(lp_ds))){
        lp_slope <- append(lp_slope, lp_ds[[a]]/lp_trials[[a]])
        lp_start_s <- append(lp_start_s, lp_start[[a]])
      }
    }
    for (b in 1:length(lp_slope)) {
      if(lp_slope[[b]] == max(unlist(lp_slope))){
        start = data[lp_start_s[[b]],]$trials
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
#mark short-position####
short_position <- function(data){
  sp_end <- list()
  sp_trials <- list()
  sp_slope <- list()
  sp_ds <- list()
  sp_end_s <- list()
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
    delta_stock <- min(data$Stock[max_point:i])-max(data$Stock[max_point:i])
    if(delta_stock <= -10){
      sp_trials <- append(sp_trials, i-max_point)
      sp_end <- append(sp_end, i)
      sp_ds <- append(sp_ds, delta_stock)
    }else if (i-max_point >=10 & delta_stock/(i-max_point)<= -0.8){
      sp_trials <- append(sp_trials, i-max_point)
      sp_end <- append(sp_end, i)
      sp_ds <- append(sp_ds, delta_stock)
    }
  }
  if(length(sp_trials)!=0){
    for (a in 1:length(sp_trials)) {
      if(sp_ds[[a]] == max(unlist(sp_ds))){
        sp_slope <- append(sp_slope, sp_ds[[a]]/sp_trials[[a]])
        sp_end_s <- append(sp_end_s, sp_end[[a]])
      }
    }
    for (b in 1:length(sp_slope)) {
      if(sp_slope[[b]] == min(unlist(sp_slope))){
        end = data[sp_end_s[[b]],]$trials
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
if(length(no_trade(data.cp)) != 1){
  result.nt <- no_trade(data.cp) %>% 
    mutate(., trials = end - start, stage = c(1:length(no_trade(data.cp)[1])), player.no = paste0("no.", data.cp$player.no[1]))
}else{
  result.nt <- data.frame()
}
if(length(short_term(data.cp)) != 1){
  result.st <- short_term(data.cp) %>% 
    mutate(., trials = end - start, stage = c(1:length(short_term(data.cp)[1])), player.no = paste0("no.", data.cp$player.no[1]))
}else{
  result.st <- data.frame()
}
if(length(short_position(data.cp)$type) != 0){
  result.sp <- short_position(data.cp)$type %>% 
    mutate(., trials = end - start, stage = c(1:length(.[1])), player.no = paste0("no.", data.cp$player.no[1]))
}else{
  result.sp <- data.frame()
}
if(length(long_position(data.cp)$type) != 0){
  result.lp <- long_position(data.cp)$type %>% 
    mutate(., trials = end - start, stage = c(1:length(.[1])), player.no = paste0("no.", data.cp$player.no[1]))
}else{
  result.lp <- data.frame()
}
result <- list(result.nt, result.st, result.lp, result.sp)
g <- ggplot()
for(i in 1:length(result)){
  if(length(result[i][[1]])!=0){
    g <- g+
      geom_col(data = result[i][[1]], 
               aes(x = player.no, y = trials,group = stage, fill = type, label = start, label1 = end, alpha = 0.5),
               width = 0.5,
               position = position_stack(reverse = TRUE))
  }
}
g <- g +
  scale_fill_manual(breaks = c("NA", "unchange", "flat", "short-position", "long-position"),
                    values = c("NA","#161a1d", "#fed766", "#fe4a49", "#009fb7")
  )+
  theme_classic()+
  coord_flip()
ggplotly(g, height = 250)

#mark short_time_position
short_time_position <- function(data){
  for (max in 1:length(data$trials)) {
    if(data$Stock[max] == max(data$Stock)){
      max_point <- data$trials[max]
    }
  }
  sl_start <- list()
  sl_end <- list()
  for (i in 1:max_point){
    for (j in (i+10):max_point) {
      stock <- data$Stock
      delta_stock <- max(stock[i:j])-min(stock[i:j])
      if(delta_stock>=8){
        sl_start <- append(sl_start, i)
        sl_end <- append(sl_end, j)
      }
    }
  }
  ss_start <- list()
  ss_end <- list()
  for (i in max_point:length(data$trials)){
    for (j in (i+10):length(data$trials)) {
      stock <- data$Stock
      delta_stock <- min(stock[i:j])-max(stock[i:j])
      print(delta_stock)
      if(delta_stock <= -8 & !is.na(delta_stock)){
        ss_start <- append(ss_start, i)
        ss_end <- append(ss_end, j)
      }
    }
  }
  if(length(sl_start) != 0 & length(ss_start) != 0 ){
    result.sl <- FindInterval(sl_start, sl_end, string_type = "long-position")
    result.ss <- FindInterval(ss_start, ss_end, string_type = "short-position")
    result <- rbind(result.sl, result.ss)
    return(result)
  }else if(length(sl_start) != 0 & length(ss_start) == 0){
    result <- FindInterval(sl_start, sl_end, string_type = "long-position")
    return(result)
  }else if(length(sl_start) == 0 & length(ss_start) != 0){
    result <- FindInterval(ss_start, ss_end, string_type = "short-position")
    return(result)
  }else{
    section_type <- data.frame()
    result <- list(type = section_type)
    return(result)
  }
}
short_time_position(data.cp)
