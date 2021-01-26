library(tidyverse)
library(zoo)
library(ggpubr)
library(cowplot)
library(DataCombine)
setwd("C:\\Users\\User\\Desktop\\ebg408\\shiny")
filepath = "data"
filenames = list.files(path = filepath, pattern="*.csv")
fileset <- lapply(paste0(filepath,"/",filenames), read.csv)

#Support function####
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
      if (i_result == 1){
        result.row <- data.frame(start = 1,
                                 end = interval_start,
                                 type = "NA")
      }else {
        result.row <- data.frame(start = result.list[[i_result-1]]$end,
                                 end = interval_start,
                                 type = "NA")
      }
      result.list[[i_result]] <- result.row
      i_result = i_result + 1
      result.row <- data.frame(start = interval_start,
                               end = interval_end,
                               type = string_type)
      result.list[[i_result]] <- result.row
      i_result = i_result + 1
    }else{
      i_interval = i_interval + 1
    }
  }
  if (result.list[[i_result-1]]$end < 100){
    result.row <- data.frame(start = result.list[[i_result-1]]$end,
                             end = 100,
                             type = "NA")
    result.list[[i_result]] <- result.row
  }
  result.table = do.call(rbind, result.list)
  return(result.table)
}
#mark no trade 
no_trade <- function(data){
  nt_start <- list()
  nt_end <- list()
  nt_trials <- list()
  for (i in 1:length(data$trials)){
    for (j in (i+1):length(data$trials)) {
      stock <- data$Stock
      delta_stock <- abs(max(stock[i:j])-min(stock[i:j]))
      notrade_avg <- mean(data$notrade[i:j])
      if(j-i >=10 & notrade_avg>= 0.8 & delta_stock<=2){
        nt_trials <- append(nt_trials, j-i)
        nt_start <- append(nt_start, i)
        nt_end <- append(nt_end, j)
      }
    }
  }
  if(length(nt_trials) != 0){
    result <- FindInterval(nt_start, nt_end, string_type = "unchange")
    return(result)
  }else{
    section_type <- data.frame()
    result <- list(type = section_type)
    return(result)
  }
}
#mark short-term trade
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
  }else{
    section_type <- data.frame()
    result <- list(type = section_type)
    return(result)
  }
}
#algorithm
NearestMaxDistance <- function(row.data, start, end){
  # define. Distance is the absolute value of
  # 'the difference value' among the given row data
  tmp.distance = row.data[[start]]
  tmp.NMDneighbor = start
  for (j in start:end){
    if (abs(row.data[[j]]) >= abs(tmp.distance)){
      tmp.distance = row.data[[j]]
      tmp.NMDneighbor = j
    }
  }
  
  # NearestMaxDistance returns the corresponding value (with sign) of
  # maximum Distance among the given row data, and its corresponding index
  return(c(distance = tmp.distance, NMDneighbor = tmp.NMDneighbor))
}

# ChangePoint Algorithm
ChangePointAlgorithm_1127 <- function(player.no){
  ##########################################################
  # 1. load data and preprocessing
  # parameter: player.no
  
  # load data by player no.
  # discard 101th row of data
  player.data <- fileset[[player.no]][-101,]
  
  # interested in only 'Trial', 'Action', and 'Stock'
  # include only 1, 100, 'inconsistent point'
  # definition: the trial at which action is inconsistent with previous trial
  if (player.no%%2 ==1){
    TAS.data <- data.frame(trial = player.data$Trials,
                           action = player.data$p1Decision,
                           stock = player.data$p1Stock) %>%
      mutate(., lag_action = lag(action)) %>%
      filter(., action != lag_action | trial == c(1,100))
  }else{
    TAS.data <- data.frame(trial = player.data$Trials,
                           action = player.data$p2Decision,
                           stock = player.data$p2Stock) %>%
      mutate(., lag_action = lag(action)) %>%
      filter(., action != lag_action | trial == c(1,100))
  }
  
  ##########################################################
  # 2. matrix representation
  # calculate matrix of subtraction
  mat.data <- rep(TAS.data$stock, length(TAS.data$stock))
  mat.n <- length(TAS.data$stock)
  mat.bycol <- matrix(mat.data, ncol = mat.n, byrow = FALSE, dimnames = list(TAS.data$trial, TAS.data$trial))
  mat.byrow <- matrix(mat.data, ncol = mat.n, byrow = TRUE, dimnames = list(TAS.data$trial, TAS.data$trial))
  
  # Stock[row_name] - Stock[col_name]
  mat.subtract <- mat.bycol - mat.byrow
  
  # discard the repeated upper right half of the matrix
  mat.subtract[upper.tri(mat.subtract)] <- NA
  
  # corresponding exact trial no.
  # will be used to reference trial no. in the algorithm
  exact.trial.no <- as.integer(rownames(mat.subtract))
  
  ##########################################################
  # 3. start algorithm
  # initial condition
  section.result <- list()
  i.result <- 1
  i.point <- 1
  i.previous.point <- 1
  
  # start algorithm
  while (i.point <= mat.n){
    # calculate NearestMaxDistance
    initial.NMD <- NearestMaxDistance(mat.subtract[i.point,], i.previous.point, i.point)
    
    # initialization
    # if pair with NearestMaxDistance >= 5 is found
    # keep searching to see if pattern continue
    if (abs(initial.NMD[['distance']]) >= 5){
      # temporarily save initial result
      start.point <- initial.NMD[['NMDneighbor']]
      end.point <- i.point
      
      # examine whether the pattern continues among the next 10 trials (after current end.point)
      # if continue, update end.point to the new one
      # there are chances that start.point may update too
      previous.NMD <- initial.NMD  # for comparison
      while (i.point <= mat.n & exact.trial.no[i.point] - exact.trial.no[end.point] <= 10){
        # directly save result if it is the last point
        if (i.point == mat.n){
          section.result[[i.result]] <- data.frame(start = exact.trial.no[start.point],
                                                   end = exact.trial.no[end.point],
                                                   type = case_when(
                                                     previous.NMD[['distance']] < 0 ~ "short-position",
                                                     previous.NMD[['distance']] > 0 ~ "long-position"
                                                   ))
          break
        }
        
        # start examine next point
        i.point <- i.point + 1
        
        next.NMD <- NearestMaxDistance(mat.subtract[i.point,], i.previous.point, i.point)
        original.evaluation <- previous.NMD[['distance']]*previous.NMD[['distance']]
        updated.evaluation <- next.NMD[['distance']]*previous.NMD[['distance']]
        
        # if pattern continue, update start point and end point of that range
        if (updated.evaluation > original.evaluation){
          start.point <- next.NMD[['NMDneighbor']]
          end.point <- i.point
          previous.NMD <- next.NMD
        }
      }
      
      # save final result
      section.result[[i.result]] <- data.frame(start = exact.trial.no[start.point],
                                               end = exact.trial.no[end.point],
                                               type = case_when(
                                                 previous.NMD[['distance']] < 0 ~ "short-position",
                                                 previous.NMD[['distance']] > 0 ~ "long-position"
                                               ))
      i.result <- i.result + 1
      
      # reset index
      i.point <- end.point
      i.previous.point <- end.point
      
    }else {
      i.point <- i.point + 1
    }
  }
  
  # output as dataframe
  result.table <- do.call(rbind, section.result)
  if(result.table[1,]$start != 1){
    na_first <- data.frame(start = 1, end = result.table[1,]$start, type = "NA")
    result.table <- InsertRow(result.table, na_first, RowNum = 1)
  }
  j = 2
  while (j <= length(result.table[,1])) {
    if (result.table[j,]$start != result.table[j-1,]$end){
      na_row <- data.frame(start = result.table[j-1,]$end, end = result.table[j,]$start, type = "NA")
      result.table <- InsertRow(result.table, na_row,RowNum = j)
    }
    j = j+1
  }
  return(result.table)
}

#Data manipulate function####
data_cp_sn <- function(player.no){
  player.no = player.no
  group.no = ifelse(player.no%%2==1, ((player.no+1)/2), (player.no/2))
  if(player.no%%2 == 1){
    data.cp <- data.frame(trials = fileset[[player.no]]$Trials,
                          Stock = fileset[[player.no]]$p1Stock,
                          Decision = fileset[[player.no]]$p1Decision)
  }else{
    data.cp <- data.frame(trials = fileset[[player.no]]$Trials,
                          Stock = fileset[[player.no]]$p2Stock,
                          Decision = fileset[[player.no]]$p2Decision)
  }
  data.cp <- data.cp %>%
    mutate(., buy = ifelse(Decision=="buy",1,0)
           , sell = ifelse(Decision=="sell",1,0)
           , notrade = ifelse(Decision=="no trade",1,0))%>%
    mutate(., lag_Decision = lag(Decision)) %>%
    select(., trials, Stock, Decision, lag_Decision,buy,sell,notrade) %>%
    mutate(., player.no = player.no, group.no = group.no)
  if(length(no_trade(data.cp)) != 1){
    result.nt <- no_trade(data.cp) %>% 
      mutate(., trials = end - start + 1, player.no = data.cp$player.no[1], group.no = data.cp$group.no[1])
    na_list = list()
    for(na in 1:length(result.nt$trials)){
      if(result.nt$trials[na]<=0){
        na_list <- append(na_list, na)
      }
    }
    if(length(na_list > 0)){
      result.nt <- result.nt[-c(unlist(na_list)),] 
    }else{
      result.nt <- result.nt
    }
  }else{
    result.nt <- data.frame()
  }
  if(length(short_term(data.cp)) != 1){
    result.st <- short_term(data.cp) %>% 
      mutate(., trials = end - start + 1, player.no = data.cp$player.no[1], group.no = data.cp$group.no[1])
    na_list = list()
    for(na in 1:length(result.st$trials)){
      if(result.st$trials[na]<=0){
        na_list <- append(na_list, na)
      }
    }
    if(length(na_list > 0)){
      result.st <- result.st[-c(unlist(na_list)),]
    }else{
      result.st <- result.st
    }
  }else{
    result.st <- data.frame()
  }
  result <- list(result.nt, result.st)
  result.table <- do.call(rbind, result)
  result.table <- filter(result.table, type != "NA")
  return(result.table)
}
data_cp_ls <- function(player.no){
  player.no = player.no
  group.no = ifelse(player.no%%2==1, ((player.no+1)/2), (player.no/2))
  cp.result <- cbind(ChangePointAlgorithm_1127(player.no),
                     group.no = group.no,
                     player.no = player.no) %>%
    mutate(.,trials = end - start + 1) %>% 
    filter(., type != "NA")
  return(cp.result)
}
#all_data_table####
long_table <- data.frame()
short_table <- data.frame()
st_table <- data.frame()
nt_table <- data.frame()
for (i in 11:160) {
  lg <- data_cp_ls(i) %>%
    filter(., type == "long-position")
  long_table <- rbind(long_table, lg)
  sh <- data_cp_ls(i) %>%
    filter(., type == "short-position")
  short_table <- rbind(short_table, sh)
  st <- data_cp_sn(i) %>%
    filter(., type == "flat")
  st_table <- rbind(st_table, st)
  nt <- data_cp_sn(i) %>%
    filter(., type == "unchange")
  nt_table <- rbind(nt_table, nt)
}
long_table <- long_table %>%
  mutate(., no. = c(1:length(.[,1])))
long_table <- long_table[c("type", "no.", "group.no","player.no","start","end")]
short_table <- short_table%>%
  mutate(., no. = c(1:length(.[,1])))
short_table <- short_table[c("type", "no.", "group.no","player.no","start","end")]
st_table <- st_table%>%
  mutate(., no. = c(1:length(.[,1])))
st_table <- st_table[c("type", "no.", "group.no","player.no","start","end")]
nt_table <- nt_table%>%
  mutate(., no. = c(1:length(.[,1])))
nt_table <- nt_table[c("type", "no.", "group.no","player.no","start","end")]
write.table(long_table, file = "C:\\Users\\User\\Desktop\\ebg408\\shiny\\data_table\\long_position_all_table.csv", sep = ",", row.names = FALSE)
write.table(short_table, file = "C:\\Users\\User\\Desktop\\ebg408\\shiny\\data_table\\short_position_all_table.csv", sep = ",", row.names = FALSE)
write.table(st_table, file = "C:\\Users\\User\\Desktop\\ebg408\\shiny\\data_table\\flat_all_table.csv", sep = ",", row.names = FALSE)
write.table(nt_table, file = "C:\\Users\\User\\Desktop\\ebg408\\shiny\\data_table\\unchange_table.csv", sep = ",", row.names = FALSE)

#data_table####
#data preprocessing
data <- list()
#remove first ten individual's data
for (i in 11:160) {
  if(i%%2 == 1){
    p2 = i+1
    data[[i-10]] = fileset[[i]] %>%
      rename(., Cash = p1Cash, Stock = p1Stock, TotalAsset = p1TotalAsset, Decision = p1Decision) %>%
      mutate(., opp_Cash = fileset[[p2]]$p2Cash, opp_Stock = fileset[[p2]]$p2Stock,
             opp_TotalAsset = fileset[[p2]]$p2TotalAsset, opp_Decision = fileset[[p2]]$p2Decision) %>%
      mutate(., group.no = (i+1)/2, player.no = i, Dprice = StockPrice - lag(StockPrice), lag_Decision = lag(Decision)) %>%
      select(.,group.no, player.no, Trials, StockPrice, Dprice, Cash, Stock, TotalAsset, Decision, lag_Decision, opp_Cash, opp_Stock, opp_TotalAsset, opp_Decision)
  }else{
    p1 = i-1
    data[[i-10]] = fileset[[i]] %>%
      rename(., Cash = p2Cash, Stock = p2Stock, TotalAsset = p2TotalAsset, Decision = p2Decision) %>%
      mutate(., opp_Cash = fileset[[p1]]$p1Cash, opp_Stock = fileset[[p1]]$p1Stock,
             opp_TotalAsset = fileset[[p1]]$p1TotalAsset, opp_Decision = fileset[[p1]]$p1Decision) %>%
      mutate(., group.no = (i)/2, player.no = i, Dprice = StockPrice - lag(StockPrice), lag_Decision = lag(Decision)) %>%
      select(.,group.no, player.no, Trials, StockPrice, Dprice, Cash, Stock, TotalAsset, Decision, lag_Decision, opp_Cash, opp_Stock, opp_TotalAsset, opp_Decision)
  }
}
#select data
#LP----
for(i in 1:length(long_table[,1])){
  for(j in 1:length(data)){
    long_data <- data.frame()
    if(long_table[i,]$player.no == data[[j]]$player.no[1]){
      start = long_table[i,]$start
      end = long_table[i,]$end
      long_data <- data[[j]][start:end,] %>%
        mutate(type = "long-position", no. = long_table[i,]$no.)
      long_data <- long_data[c("type", "no.", "group.no","player.no","Trials", "StockPrice",
                               "Cash", "Stock", "TotalAsset", "Decision", "lag_Decision", "opp_Cash", "opp_Stock", "opp_TotalAsset", "opp_Decision")]
      write.table(long_data, file = paste0("C:\\Users\\User\\Desktop\\ebg408\\shiny\\data_table\\LP_data\\LP_",
                                           long_table[i,]$no.,"_", long_table[i,]$player.no,"_", long_table[i,]$start,"_", long_table[i,]$end,".csv"),
                  sep = ",", row.names = FALSE)
    }
  }
}
#SP----
for(i in 1:length(short_table[,1])){
  for(j in 1:length(data)){
    short_data <- data.frame()
    if(short_table[i,]$player.no == data[[j]]$player.no[1]){
      start = short_table[i,]$start
      end = short_table[i,]$end
      short_data <- data[[j]][start:end,] %>%
        mutate(type = "short-position", no. = short_table[i,]$no.)
      short_data <- short_data[c("type", "no.", "group.no","player.no","Trials", "StockPrice",
                               "Cash", "Stock", "TotalAsset", "Decision", "lag_Decision", "opp_Cash", "opp_Stock", "opp_TotalAsset", "opp_Decision")]
      write.table(short_data, file = paste0("C:\\Users\\User\\Desktop\\ebg408\\shiny\\data_table\\SP_data\\SP_",
                                           short_table[i,]$no.,"_", short_table[i,]$player.no,"_", short_table[i,]$start,"_", short_table[i,]$end,".csv"),
                  sep = ",", row.names = FALSE)
    }
  }
}
#F----
for(i in 1:length(st_table[,1])){
  for(j in 1:length(data)){
    st_data <- data.frame()
    if(st_table[i,]$player.no == data[[j]]$player.no[1]){
      start = st_table[i,]$start
      end = st_table[i,]$end
      st_data <- data[[j]][start:end,] %>%
        mutate(type = "flat", no. = st_table[i,]$no.)
      st_data <- st_data[c("type", "no.", "group.no","player.no","Trials", "StockPrice",
                               "Cash", "Stock", "TotalAsset", "Decision", "lag_Decision", "opp_Cash", "opp_Stock", "opp_TotalAsset", "opp_Decision")]
      write.table(st_data, file = paste0("C:\\Users\\User\\Desktop\\ebg408\\shiny\\data_table\\F_data\\F_",
                                           st_table[i,]$no.,"_", st_table[i,]$player.no,"_", st_table[i,]$start,"_", st_table[i,]$end,".csv"),
                  sep = ",", row.names = FALSE)
    }
  }
}
#U----
for(i in 1:length(nt_table[,1])){
  for(j in 1:length(data)){
    nt_data <- data.frame()
    if(nt_table[i,]$player.no == data[[j]]$player.no[1]){
      start = nt_table[i,]$start
      end = nt_table[i,]$end
      nt_data <- data[[j]][start:end,] %>%
        mutate(type = "unchanged", no. = long_table[i,]$no.)
      nt_data <- nt_data[c("type", "no.", "group.no","player.no","Trials", "StockPrice",
                               "Cash", "Stock", "TotalAsset", "Decision", "lag_Decision", "opp_Cash", "opp_Stock", "opp_TotalAsset", "opp_Decision")]
      write.table(nt_data, file = paste0("C:\\Users\\User\\Desktop\\ebg408\\shiny\\data_table\\U_data\\U_",
                                           nt_table[i,]$no.,"_", nt_table[i,]$player.no,"_", nt_table[i,]$start,"_", nt_table[i,]$end,".csv"),
                  sep = ",", row.names = FALSE)
    }
  }
}



#export pictures----
#preparing data----
data_cp <- function(player.no){
  player.no = player.no
  group.no = ifelse(player.no%%2==1, ((player.no+1)/2), (player.no/2))
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
  data.cp <- data.cp %>% 
    mutate(., buy = ifelse(Decision=="buy",1,0)
           , sell = ifelse(Decision=="sell",1,0)
           , notrade = ifelse(Decision=="no trade",1,0))%>%
    mutate(., lag_Decision = lag(Decision)) %>% 
    select(., trials, Stock,StockPrice, Decision, lag_Decision,buy,sell,notrade) %>% 
    mutate(., player.no = player.no, group.no = group.no) %>% 
    filter(., trials == c(1:100))
  return(data.cp)
}
#Supporting Function----
PlotFunc <- function(player.no){
  player.data <- fileset[[player.no]][-101,]
  if (player.no%%2 ==1){
    needed.data <- data.frame(trials = player.data$Trials,
                              action = player.data$p1Decision,
                              stock = player.data$p1Stock,
                              checkhistory = player.data$p1ChechHistory)
  }else{
    needed.data <- data.frame(trials = player.data$Trials,
                              action = player.data$p2Decision,
                              stock = player.data$p2Stock,
                              checkhistory = player.data$p2ChechHistory)
  }
  checkhistory <- list()
  for (i in 1:length(needed.data[,1])) {
    if(needed.data$checkhistory[i] =="yes"){
      checkhistory <- c(checkhistory, needed.data$trials[i])
    }
  }
  plt <- ggplot(needed.data, aes(x=trials, y=stock)) +
    ggtitle(paste0("Subject No. ",player.no))+
    geom_point(aes(color=action))+
    geom_line()+
    geom_vline(xintercept = unlist(checkhistory))+
    xlim(0,100)+
    theme(legend.title = element_text(size = 6),
          legend.text = element_text(size = 6))+
    theme_classic()
  return(plt)
}

#plot preparing----
#stock_plot----
point_plot <- list()
for(i in c(11:160)){
  point_plot[[i]] <- PlotFunc(i)
}
#stockPrice_plot----
StockPriceFunc <- function(group.no){
  player.no1 = group.no*2-1
  player.no2 = group.no*2
  player.data1 <- fileset[[player.no1]]
  player.data2 <- fileset[[player.no2]]
  if(player.data1$p1Cash[101] > player.data2$p2Cash[101]){
    win <- "p1 win"
  }else if(player.data1$p1Cash[101] < player.data2$p2Cash[101]){
    win <- "p2 win"
  }else{
    win <- "error"
  }
  player.data <- fileset[[player.no2]][-101,]
  need.data <- data.frame(trials = player.data$Trials,
                          StockPrice = player.data$StockPrice)
  plt <- ggplot(need.data, aes(x = trials, y = StockPrice)) +
    ggtitle(paste0("Group No. ", group.no,"& ", win))+
    geom_point()+
    geom_line()+
    coord_cartesian(xlim = c(0, 100))+
    theme_classic()
  return(plt)
}
stockprice_plot <- list()
for (i in c(6:80)) {
  stockprice_plot[[i]] <- StockPriceFunc(i)
}
#behavioral pattern plot with unchanged----
bp_plot_un <- list()
for (j in c(11:160)) {
  data.cp <- data_cp(j)
  cp.data <- ChangePointAlgorithm_1127(j) %>% 
    mutate(., trials = end - start, stage = c(1:length(ChangePointAlgorithm_1127(j)[,1])), player.no = paste0("no.", data.cp$player.no[1]))
  cp.data$trials[1] <- cp.data$trials[1]+1
  if(length(no_trade(data.cp)) != 1){
    result.nt <- no_trade(data.cp) %>% 
      mutate(., trials = end - start, player.no = paste0("no.", data.cp$player.no[1]))
    result.nt$trials[1] <- result.nt$trials[1]+1
    na_list = list()
    for(na in 1:length(result.nt$trials)){
      if(result.nt$trials[na]<=0){
        na_list <- append(na_list, na)
      }
    }
    if(length(na_list > 0)){
      result.nt <- result.nt[-c(unlist(na_list)),] 
      result.nt <- result.nt%>% 
        mutate(., stage = c(1:length(result.nt[,1])))
    }else{
      result.nt <- result.nt %>% 
        mutate(., stage = c(1:length(result.nt[,1])))
    }
  }else{
    result.nt <- data.frame()
  }
  if(length(short_term(data.cp)) != 1){
    result.st <- short_term(data.cp) %>% 
      mutate(., trials = end - start, player.no = paste0("no.", data.cp$player.no[1]))
    result.st$trials[1] <- result.st$trials[1]+1
    na_list = list()
    for(na in 1:length(result.st$trials)){
      if(result.st$trials[na]<=0){
        na_list <- append(na_list, na)
      }
    }
    if(length(na_list > 0)){
      result.st <- result.st[-c(unlist(na_list)),]
      result.st <- result.st %>% 
        mutate(., stage = c(1:length(result.st[,1])))
    }else{
      result.st <- result.st %>% 
        mutate(., stage = c(1:length(result.st[,1])))
    }
  }else{
    result.st <- data.frame()
  }
  result <- list(result.nt, result.st, cp.data)
  g <- ggplot()
  for(i in 1:length(result)){
    if(length(result[i][[1]])!=0){
      g <- g+
        geom_col(data = result[i][[1]], 
                 aes(x = player.no, y = trials, group = stage, fill = type, label = start, label1 = end),
                 width = 0.5,
                 position = position_stack(reverse = TRUE))
    }
  }
  g <- g +
    scale_fill_manual(breaks = c("unchange","flat", "short-position", "long-position", "NA"),
                      values = c("#161a1d80","#fed76680", "#009fb780", "#fe4a4980", "#FFFFFF00")
    )+
    coord_cartesian(ylim = c(0, 100))+
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 10))+
    theme_classic()+
    coord_flip()
  
  bp_plot_un[[j]] <- g
}

#behavioral pattern plot without unchanged----
bp_plot <- list()
for (j in c(11:160)) {
  data.cp <- data_cp(j)
  cp.data <- ChangePointAlgorithm_1127(j) %>% 
    mutate(., trials = end - start, stage = c(1:length(ChangePointAlgorithm_1127(j)[,1])), player.no = paste0("no.", data.cp$player.no[1]))
  cp.data$trials[1] <- cp.data$trials[1]+1
  if(length(short_term(data.cp)) != 1){
    result.st <- short_term(data.cp) %>% 
      mutate(., trials = end - start, player.no = paste0("no.", data.cp$player.no[1]))
    result.st$trials[1] <- result.st$trials[1]+1
    na_list = list()
    for(na in 1:length(result.st$trials)){
      if(result.st$trials[na]<=0){
        na_list <- append(na_list, na)
      }
    }
    if(length(na_list > 0)){
      result.st <- result.st[-c(unlist(na_list)),]
      result.st <- result.st %>% 
        mutate(., stage = c(1:length(result.st[,1])))
    }else{
      result.st <- result.st %>% 
        mutate(., stage = c(1:length(result.st[,1])))
    }
  }else{
    result.st <- data.frame()
  }
  result <- list(result.st, cp.data)
  g <- ggplot()
  for(i in 1:length(result)){
    if(length(result[i][[1]])!=0){
      g <- g+
        geom_col(data = result[i][[1]], 
                 aes(x = player.no, y = trials,group = stage, fill = type, label = start, label1 = end),
                 width = 0.5,
                 position = position_stack(reverse = TRUE))
    }
  }
  g <- g +
    scale_fill_manual(breaks = c("unchange","flat", "short-position", "long-position","NA"),
                      values = c("#161a1d80","#fed76680", "#009fb780", "#fe4a4980","#FFFFFF00")
    )+
    coord_cartesian(ylim = c(0, 100))+
    theme_classic()+
    theme(legend.position = "none")+
    coord_flip()
  bp_plot[[j]] <- g
}

#merge----
paintplot <- function(n){
  m1 = 2*n-1
  m2 = 2*n
  
  p1 <- bp_plot[[m1]]
  p2 <-  bp_plot_un[[m1]]
  p3 <- point_plot[[m1]]
  p4 <- stockprice_plot[[n]]
  p5 <- point_plot[[m2]]
  p6 <- bp_plot_un[[m2]]
  p7 <- bp_plot[[m2]]
  
  plots <- list(p1,p2,p3,p4,p5,p6,p7)
  grobs <- lapply(plots, as_grob)
  plot_widths <- lapply(grobs, function(x) {x$widths})
  # Aligning the left margins of all plots
  aligned_widths <- align_margin(plot_widths, "first")
  # Aligning the right margins of all plots as well
  aligned_widths <- align_margin(aligned_widths, "last")
  # Setting the dimensions of plots to the aligned dimensions
  for (i in seq_along(plots)) {
    grobs[[i]]$widths <- aligned_widths[[i]]
  }
  # Draw aligned plots
  plot <- plot_grid(plotlist = grobs,rel_heights = c(1,1,2,2,2,1,1), ncol = 1)
  return(plot)
}
ChangePointAlgorithm_1127(26)
paintplot(80)
pdf("Rplot.pdf", width = 8, height = 11)
paintplot(13)
dev.off()
pdf("plot.pdf",width = 8, height = 11)
par(mfrow=c(1,1))
for(i in 6:80){
  plot(paintplot(i))
}
dev.off()



# #test----
# # for (no in 1:160) {
# #   data <- data_cp(no)
# #   point_plot <- PlotFunc(no)
# #   group = no/2
# #   
# # }
# no = 1
# no1 = 2*no-1
# no2 = 2*no
# data <- data_cp(no)
# paint_plot_p <- function(no){
#   data <- data_cp(no)
#   point_plt <- PlotFunc(no)
#   data.cp <- data
#   cp.data <- ChangePointAlgorithm(no, 5, 1, 15) %>% 
#     mutate(., trials = end - start+1, stage = c(1:length(no_trade(data.cp)[1])), player.no = paste0("no.", data.cp$player.no[1]))
#   if(length(no_trade(data.cp)) != 1){
#     result.nt <- no_trade(data.cp) %>% 
#       mutate(., trials = end - start+1, stage = c(1:length(no_trade(data.cp)[1])), player.no = paste0("no.", data.cp$player.no[1]))
#   }else{
#     result.nt <- data.frame()
#   }
#   if(length(short_term(data.cp)) != 1){
#     result.st <- short_term(data.cp) %>% 
#       mutate(., trials = end - start+1, stage = c(1:length(short_term(data.cp)[1])), player.no = paste0("no.", data.cp$player.no[1]))
#   }else{
#     result.st <- data.frame()
#   }
#   result_u <- list(result.nt, result.st, cp.data)
#   gu <- ggplot()
#   for(i in 1:length(result_u)){
#     if(length(result_u[i][[1]])!=0){
#       gu <- gu+
#         geom_col(data = result_u[i][[1]], 
#                  aes(x = player.no, y = trials,group = stage, fill = type, label = start, label1 = end),
#                  width = 0.5,
#                  position = position_stack(reverse = TRUE))
#     }
#   }
#   gu <- gu +
#     scale_fill_manual(breaks = c("unchange", "flat", "short-position", "long-position"),
#                       values = c("#161a1d80", "#fed76680", "#fe4a4980", "#009fb780")
#     )+
#     ylim(0,100)+
#     theme_classic()+
#     coord_flip()
#   
#   result <- list(result.st, cp.data)
#   g <- ggplot()
#   for(i in 1:length(result)){
#     if(length(result[i][[1]])!=0){
#       g <- g+
#         geom_col(data = result[i][[1]], 
#                  aes(x = player.no, y = trials,group = stage, fill = type, label = start, label1 = end),
#                  width = 0.5,
#                  position = position_stack(reverse = TRUE))
#     }
#   }
#   g <- g +
#     scale_fill_manual(breaks = c("unchange", "flat", "short-position", "long-position"),
#                       values = c("#161a1d80", "#fed76680", "#fe4a4980", "#009fb780")
#     )+
#     ylim(0,100)+
#     theme_classic()+
#     coord_flip()
#   pg <- ggarrange(point_plt, gu, g,ncol = 1,align = "v")
#   return(pg)
# }
# 
# pp1 <- paint_plot_p(no1)
# pp1
# pp2 <- paint_plot_p(no2)
# pp2
# stockprice_plt <- StockPriceFunc(no)
# stockprice_plt
# ggarrange(pp1, stockprice_plt, pp2,  ncol = 1, align = "v")
# library(cowplot)
# plot_grid(pp1, stockprice_plt, pp2,align = "v",ncol = 1,axis = "b", labels = "AUTO")

