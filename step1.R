library(tidyverse)
library(zoo)
library(ggpubr)
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
                                 end = interval_start - 1,
                                 type = "NA")
      }else {
        result.row <- data.frame(start = result.list[[i_result-1]]$end + 1,
                                 end = interval_start - 1,
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
    result.row <- data.frame(start = result.list[[i_result-1]]$end + 1,
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
MaxIndex <- function(vctr, cp){
  tmp.value = vctr[[cp]]
  tmp.j = cp
  for (j in cp:length(vctr)){
    if (!is.na(vctr[[j]]) & abs(vctr[[j]]) > abs(tmp.value)){
      tmp.value = vctr[[j]]
      tmp.j = j
    }else{
      tmp.value = tmp.value
      tmp.j = tmp.j
    }
  }
  return(c(tmp.value, tmp.j))
  
}
ChangePointAlgorithm <- function(player.no, threshold, threshold.small, threshold.small.trials){
  player.data <- fileset[[player.no]][-101,]
  if (player.no%%2 ==1){
    needed.data <- data.frame(trial = player.data$Trials,
                              action = player.data$p1Decision,
                              stock = player.data$p1Stock) %>%
      mutate(., lag_action = lag(action)) %>%
      filter(., action != lag_action | trial == c(1,100))
  }else{
    needed.data <- data.frame(trial = player.data$Trials,
                              action = player.data$p2Decision,
                              stock = player.data$p2Stock) %>%
      mutate(., lag_action = lag(action)) %>%
      filter(., action != lag_action | trial == c(1,100))
  }
  mat.data <- rep(needed.data$stock, length(needed.data$stock))
  mat.n <- length(needed.data$stock)
  mat.bycol <- matrix(mat.data, ncol = mat.n, byrow = FALSE, dimnames = list(needed.data$trial, needed.data$trial))
  mat.byrow <- matrix(mat.data, ncol = mat.n, byrow = TRUE, dimnames = list(needed.data$trial, needed.data$trial))
  mat.substract <- mat.bycol - mat.byrow
  for (i in 1:mat.n){
    for (j in 1:mat.n){
      if(i <= j){
        mat.substract[i,j] = NA
      }
    }
  }
  i <- 2 
  j <- 1 
  cp.former = 1
  cp.latter = 1  
  result = list()
  result.index = 1
  trial.tag = as.integer(rownames(mat.substract))
  next.pattern.shno = TRUE
  while(i < mat.n) {
    ith.rowdata = mat.substract[i,]
    tmp.substr.value = MaxIndex(ith.rowdata, cp.former)[1]
    tmp.substr.index = MaxIndex(ith.rowdata, cp.former)[2]
    tmp.first.value.of.row = mat.substract[i, cp.former]
    if(!is.na(tmp.substr.value) & abs(tmp.substr.value) >= threshold){
      cp.former = tmp.substr.index
      record.data = TRUE
      substr.value = tmp.substr.value
      end.loop = FALSE
      while(end.loop == FALSE){
        if (record.data){
          cp.latter = i
        }
        if(i==mat.n) {
          result[[result.index]] <- data.frame(start= trial.tag[cp.former] + ifelse(cp.former==1, 0, 1),
                                               end = trial.tag[cp.latter],
                                               type = ifelse(substr.value > 0, "long-position", "short-position"))
          result.index = result.index + 1
          break
        }
        i <- i + 1
        next.ith.rowdata = mat.substract[i,]
        check.break.loop.list <- list()
        check.contin.loop.list <- list()
        for (j in cp.former:length(next.ith.rowdata)){
          check.break.loop.list[j] <- abs(next.ith.rowdata[j])>=threshold & substr.value*next.ith.rowdata[j]<0
          check.contin.loop.list[j] <- abs(next.ith.rowdata[j]) > abs(substr.value)
        }
        check.break.loop <- ifelse(TRUE %in% check.break.loop.list, TRUE, FALSE)
        check.contin.loop <- ifelse(TRUE %in% check.contin.loop.list, TRUE, FALSE)
        if (check.break.loop | trial.tag[i] - trial.tag[cp.latter] >= threshold.small.trials){
          result[[result.index]] <- data.frame(start = trial.tag[cp.former] + ifelse(cp.former==1, 0, 1),
                                               end = trial.tag[cp.latter],
                                               type = ifelse(substr.value > 0, "long-position", "short-position"))
          result.index = result.index + 1
          
          cp.former = cp.latter
          i = cp.latter + 1
          end.loop = TRUE
          
        }else if (check.contin.loop){
          substr.value = MaxIndex(next.ith.rowdata, cp.former)[1]
          record.data = TRUE
        }else {
          record.data = FALSE
        }
      }
      next.pattern.shno = TRUE
    }else if(!is.na(tmp.first.value.of.row) & abs(tmp.first.value.of.row) <= threshold.small & next.pattern.shno){
      end.loop = FALSE
      tmp.cp = cp.former
      sum.shno = 0
      while(end.loop == FALSE){
        if(i==mat.n){
          result[[result.index]] <- data.frame(start = trial.tag[cp.former] + ifelse(cp.former==1, 0, 1),
                                               end = trial.tag[cp.latter],
                                               type = "NA")
          result.index = result.index + 1
          break
        }
        i <- i + 1
        next.ith.pointdata = mat.substract[i, cp.former]
        if (abs(next.ith.pointdata) <= threshold.small & sum.shno == 0){
          tmp.cp = i
          sum.shno = sum.shno + abs(next.ith.pointdata)
        }else{
          end.loop = TRUE
          lasting.trials = as.integer(trial.tag[tmp.cp]) - as.integer(trial.tag[cp.former])
          if (lasting.trials >= threshold.small.trials){
            cp.latter = tmp.cp
            result[[result.index]] <- data.frame(start= trial.tag[cp.former] + ifelse(cp.former==1, 0, 1),
                                                 end = trial.tag[cp.latter],
                                                 type = "NA")
            result.index = result.index + 1
            cp.former = cp.latter
            i = cp.latter + 1
          }else{
            next.pattern.shno = FALSE
          }
        }
      }
      
    }else{
      i <- i+1
    }
  }
  if (trial.tag[cp.latter]+1 >= (100-threshold.small.trials)){
    # result[[result.index - 1]]['end'] <- 100
    
  }else if (cp.latter == 1){
    result[[result.index]] <- data.frame(start = 1,
                                         end = 100,
                                         type = "NA")
    
  }else{
    result[[result.index]] <- data.frame(start = trial.tag[cp.latter]+1,
                                         end = 100,
                                         type = "NA")
    
  }
  result.table <- do.call(rbind, result)
  i = 2
  while(i <= length(result.table[,1])){
    if (result.table[i,]$type == result.table[i-1,]$type){
      result.table[i-1,]$end = result.table[i,]$end
      result.table <- result.table[-i,]
    }
    i = i + 1
  }
  
  return(result.table)
}

# #Data manipulate function####
# data_cp_sn <- function(player.no){
#   player.no = player.no
#   group.no = ifelse(player.no%%2==1, ((player.no+1)/2), (player.no/2))
#   if(player.no%%2 == 1){
#     data.cp <- data.frame(trials = fileset[[player.no]]$Trials,
#                           Stock = fileset[[player.no]]$p1Stock, 
#                           Decision = fileset[[player.no]]$p1Decision)
#   }else{
#     data.cp <- data.frame(trials = fileset[[player.no]]$Trials,
#                           Stock = fileset[[player.no]]$p2Stock, 
#                           Decision = fileset[[player.no]]$p2Decision)
#   }
#   data.cp <- data.cp %>% 
#     mutate(., buy = ifelse(Decision=="buy",1,0)
#            , sell = ifelse(Decision=="sell",1,0)
#            , notrade = ifelse(Decision=="no trade",1,0))%>%
#     mutate(., lag_Decision = lag(Decision)) %>% 
#     select(., trials, Stock, Decision, lag_Decision,buy,sell,notrade) %>% 
#     mutate(., player.no = player.no, group.no = group.no)
#   if(length(no_trade(data.cp)$start) != 0){
#     result.nt <- no_trade(data.cp) %>% 
#       mutate(., trials = end - start+1,
#              group.no = group.no,
#              player.no = player.no)
#   }else{
#     result.nt <- data.frame()
#   }
#   if(length(short_term(data.cp)$start) != 0){
#     result.st <- short_term(data.cp) %>% 
#       mutate(., trials = end - start+1,
#              group.no = group.no,
#              player.no = player.no)
#   }else{
#     result.st <- data.frame()
#   }
#   result <- list(result.nt, result.st)
#   result.table <- do.call(rbind, result)
#   result.table <- filter(result.table, type != "NA")
#   return(result.table)
# }
# data_cp_ls <- function(player.no){
#   player.no = player.no
#   group.no = ifelse(player.no%%2==1, ((player.no+1)/2), (player.no/2))
#   threshold.cp = 5
#   threshold.small.cp = 1
#   threshold.small.trials.cp = 15
#   cp.result <- cbind(ChangePointAlgorithm(player.no, threshold.cp, threshold.small.cp, threshold.small.trials.cp),
#                      group.no = group.no,
#                      player.no = player.no) %>%
#     mutate(.,trials = end - start + 1)
#   return(cp.result)
# }
# #all_data_table####
# long_table <- data.frame()
# short_table <- data.frame()
# st_table <- data.frame()
# nt_table <- data.frame()
# for (i in 1:160) {
#   lg <- data_cp_ls(i) %>% 
#     filter(., type == "long-position")
#   long_table <- rbind(long_table, lg)
#   sh <- data_cp_ls(i) %>% 
#     filter(., type == "short-position")
#   short_table <- rbind(short_table, sh)
#   st <- data_cp_sn(i) %>% 
#     filter(., type == "flat")
#   st_table <- rbind(st_table, st)
#   nt <- data_cp_sn(i) %>% 
#     filter(., type == "unchange")
#   nt_table <- rbind(nt_table, nt)
# }
# long_table <- long_table %>% 
#   mutate(., no. = c(1:length(.[,1])))
# long_table <- long_table[c("type", "no.", "group.no","player.no","start","end")]
# short_table <- short_table%>% 
#   mutate(., no. = c(1:length(.[,1]))) 
# short_table <- short_table[c("type", "no.", "group.no","player.no","start","end")]
# st_table <- st_table%>% 
#   mutate(., no. = c(1:length(.[,1])))
# st_table <- st_table[c("type", "no.", "group.no","player.no","start","end")]
# nt_table <- nt_table%>% 
#   mutate(., no. = c(1:length(.[,1])))
# nt_table <- nt_table[c("type", "no.", "group.no","player.no","start","end")]
# write.table(long_table, file = "C:\\Users\\User\\Desktop\\ebg408\\shiny\\table\\long_position_all_table.csv", sep = ",", row.names = FALSE)
# write.table(short_table, file = "C:\\Users\\User\\Desktop\\ebg408\\shiny\\table\\short_position_all_table.csv", sep = ",", row.names = FALSE)
# write.table(st_table, file = "C:\\Users\\User\\Desktop\\ebg408\\shiny\\table\\flat_all_table.csv", sep = ",", row.names = FALSE)
# write.table(nt_table, file = "C:\\Users\\User\\Desktop\\ebg408\\shiny\\table\\unchange_table.csv", sep = ",", row.names = FALSE)
# 
# #data_table####
# #data preprocessing
# data <- list()
# for (i in 1:160) {
#   if(i%%2 == 1){
#     p2 = i+1
#     data[[i]] = fileset[[i]] %>% 
#       rename(., Cash = p1Cash, Stock = p1Stock, TotalAsset = p1TotalAsset, Decision = p1Decision) %>% 
#       mutate(., opp_Cash = fileset[[p2]]$p2Cash, opp_Stock = fileset[[p2]]$p2Stock, 
#              opp_TotalAsset = fileset[[p2]]$p2TotalAsset, opp_Decision = fileset[[p2]]$p2Decision) %>%
#       mutate(., group.no = (i+1)/2, player.no = i) %>% 
#       select(.,group.no, player.no, Trials, StockPrice, Cash, Stock, TotalAsset, Decision, opp_Cash, opp_Stock, opp_TotalAsset, opp_Decision)
#   }else{
#     p1 = i-1
#     data[[i]] = fileset[[i]] %>% 
#       rename(., Cash = p2Cash, Stock = p2Stock, TotalAsset = p2TotalAsset, Decision = p2Decision) %>% 
#       mutate(., opp_Cash = fileset[[p1]]$p1Cash, opp_Stock = fileset[[p1]]$p1Stock, 
#              opp_TotalAsset = fileset[[p1]]$p1TotalAsset, opp_Decision = fileset[[p1]]$p1Decision) %>%
#       mutate(., group.no = (i)/2, player.no = i) %>% 
#       select(.,group.no, player.no, Trials, StockPrice, Cash, Stock, TotalAsset, Decision, opp_Cash, opp_Stock, opp_TotalAsset, opp_Decision)
#   }
# }
# #select data
# #LP----
# for(i in 1:length(long_table[,1])){
#   for(j in 1:length(data)){
#     long_data <- data.frame()
#     if(long_table[i,]$player.no == data[[j]]$player.no[1]){
#       start = long_table[i,]$start
#       end = long_table[i,]$end
#       long_data <- data[[j]][start:end,] %>% 
#         mutate(type = "long-position", no. = long_table[i,]$no.)
#       long_data <- long_data[c("type", "no.", "group.no","player.no","Trials", "StockPrice",
#                                "Cash", "Stock", "TotalAsset", "Decision", "opp_Cash", "opp_Stock", "opp_TotalAsset", "opp_Decision")]
#       write.table(long_data, file = paste0("C:\\Users\\User\\Desktop\\ebg408\\shiny\\table\\LP_data\\LP_",
#                                            long_table[i,]$no.,"_", long_table[i,]$player.no,"_", long_table[i,]$start,"_", long_table[i,]$end,".csv"),
#                   sep = ",", row.names = FALSE)
#     }
#   }
# }
# #SP----
# for(i in 1:length(short_table[,1])){
#   for(j in 1:length(data)){
#     short_data <- data.frame()
#     if(short_table[i,]$player.no == data[[j]]$player.no[1]){
#       start = short_table[i,]$start
#       end = short_table[i,]$end
#       short_data <- data[[j]][start:end,] %>% 
#         mutate(type = "short-position", no. = short_table[i,]$no.)
#       short_data <- short_data[c("type", "no.", "group.no","player.no","Trials", "StockPrice",
#                                "Cash", "Stock", "TotalAsset", "Decision", "opp_Cash", "opp_Stock", "opp_TotalAsset", "opp_Decision")]
#       write.table(short_data, file = paste0("C:\\Users\\User\\Desktop\\ebg408\\shiny\\table\\SP_data\\SP_",
#                                            short_table[i,]$no.,"_", short_table[i,]$player.no,"_", short_table[i,]$start,"_", short_table[i,]$end,".csv"),
#                   sep = ",", row.names = FALSE)
#     }
#   }
# }
# #F----
# for(i in 1:length(st_table[,1])){
#   for(j in 1:length(data)){
#     st_data <- data.frame()
#     if(st_table[i,]$player.no == data[[j]]$player.no[1]){
#       start = st_table[i,]$start
#       end = st_table[i,]$end
#       st_data <- data[[j]][start:end,] %>% 
#         mutate(type = "flat", no. = st_table[i,]$no.)
#       st_data <- st_data[c("type", "no.", "group.no","player.no","Trials", "StockPrice",
#                                "Cash", "Stock", "TotalAsset", "Decision", "opp_Cash", "opp_Stock", "opp_TotalAsset", "opp_Decision")]
#       write.table(st_data, file = paste0("C:\\Users\\User\\Desktop\\ebg408\\shiny\\table\\F_data\\F_",
#                                            st_table[i,]$no.,"_", st_table[i,]$player.no,"_", st_table[i,]$start,"_", st_table[i,]$end,".csv"),
#                   sep = ",", row.names = FALSE)
#     }
#   }
# }
# #U----
# for(i in 1:length(nt_table[,1])){
#   for(j in 1:length(data)){
#     nt_data <- data.frame()
#     if(nt_table[i,]$player.no == data[[j]]$player.no[1]){
#       start = nt_table[i,]$start
#       end = nt_table[i,]$end
#       nt_data <- data[[j]][start:end,] %>% 
#         mutate(type = "unchanged", no. = long_table[i,]$no.)
#       nt_data <- nt_data[c("type", "no.", "group.no","player.no","Trials", "StockPrice",
#                                "Cash", "Stock", "TotalAsset", "Decision", "opp_Cash", "opp_Stock", "opp_TotalAsset", "opp_Decision")]
#       write.table(nt_data, file = paste0("C:\\Users\\User\\Desktop\\ebg408\\shiny\\table\\U_data\\U_",
#                                            nt_table[i,]$no.,"_", nt_table[i,]$player.no,"_", nt_table[i,]$start,"_", nt_table[i,]$end,".csv"),
#                   sep = ",", row.names = FALSE)
#     }
#   }
# }
# 
# 


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
                              stock = player.data$p1Stock)
  }else{
    needed.data <- data.frame(trials = player.data$Trials,
                              action = player.data$p2Decision,
                              stock = player.data$p2Stock)
  }
  plt <- ggplot(needed.data, aes(x=trials, y=stock)) +
    ggtitle(paste0("Subject No. ",player.no))+
    geom_point(aes(color=action))+
    geom_line()+
    xlim(0,100)+
    theme_classic()
  return(plt)
}
#plot preparing----
data = list()
#stock_plot----
point_plot <- list()
for(i in c(1:160)){
  data[[i]] <- data_cp(i)
  point_plot[[i]] <- PlotFunc(i)
}
#stockPrice_plot----
stockprice_plot <- list()
for (i in c(1:80)) {
  group.no = 2*i
  data_sp <- data[[group.no]]
  plt <- ggplot(data_sp, aes(x = trials, y = StockPrice)) +
    ggtitle(paste0("Group No. ",i))+
    geom_point()+
    geom_line()+
    xlim(0,100)+
    theme_classic()
  stockprice_plot[[i]] <- plt
}
#behavioral pattern plot with unchanged----
bp_plot_un <- list()
for (j in c(1:160)) {
  data.cp <- data[[j]]
  cp.data <- ChangePointAlgorithm(j, 5, 1, 15) %>% 
    mutate(., trials = end - start, stage = c(1:length(no_trade(data.cp)[1])), player.no = paste0("no.", data.cp$player.no[1]))
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
  result <- list(result.nt, result.st, cp.data)
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
    scale_fill_manual(breaks = c("unchange", "flat", "short-position", "long-position"),
                      values = c("#161a1d", "#fed766", "#fe4a49", "#009fb7")
    )+
    ylim(0,100)+
    theme_classic()+
    coord_flip()
  bp_plot_un[[j]] <- g
}
#behavioral pattern plot without unchanged----
bp_plot <- list()
for (j in c(1:160)) {
  data.cp <- data[[j]]
  cp.data <- ChangePointAlgorithm(j, 5, 1, 15) %>% 
    mutate(., trials = end - start, stage = c(1:length(no_trade(data.cp)[1])), player.no = paste0("no.", data.cp$player.no[1]))
  if(length(short_term(data.cp)) != 1){
    result.st <- short_term(data.cp) %>% 
      mutate(., trials = end - start, stage = c(1:length(short_term(data.cp)[1])), player.no = paste0("no.", data.cp$player.no[1]))
  }else{
    result.st <- data.frame()
  }
  result <- list(result.st, cp.data)
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
    scale_fill_manual(breaks = c("unchange","flat", "short-position", "long-position"),
                      values = c("#161a1d","#fed766", "#fe4a49", "#009fb7")
    )+
    ylim(0,100)+
    theme_classic()+
    coord_flip()
  bp_plot[[j]] <- g
}
n = 1
m1 = 2*n-1
m2 = 2*n
ex_plot <- ggarrange(pointplot1, bp_plot_un[[m1]], bp_plot[[m1]], stockpriceplot,
          bp_plot_un[[m2]], bp_plot[[m2]], pointplot2, ncol = 1, align = "v")
ex_plot
#test----
# for (no in 1:160) {
#   data <- data_cp(no)
#   point_plot <- PlotFunc(no)
#   group = no/2
#   
# }
no = 1
no1 = 2*no-1
no2 = 2*no
data <- data_cp(no)
paint_plot_p <- function(no){
  data <- data_cp(no)
  point_plt <- PlotFunc(no)
  
  data.cp <- data
  cp.data <- ChangePointAlgorithm(no, 5, 1, 15) %>% 
    mutate(., trials = end - start+1, stage = c(1:length(no_trade(data.cp)[1])), player.no = paste0("no.", data.cp$player.no[1]))
  if(length(no_trade(data.cp)) != 1){
    result.nt <- no_trade(data.cp) %>% 
      mutate(., trials = end - start+1, stage = c(1:length(no_trade(data.cp)[1])), player.no = paste0("no.", data.cp$player.no[1]))
  }else{
    result.nt <- data.frame()
  }
  if(length(short_term(data.cp)) != 1){
    result.st <- short_term(data.cp) %>% 
      mutate(., trials = end - start+1, stage = c(1:length(short_term(data.cp)[1])), player.no = paste0("no.", data.cp$player.no[1]))
  }else{
    result.st <- data.frame()
  }
  result_u <- list(result.nt, result.st, cp.data)
  gu <- ggplot()
  for(i in 1:length(result_u)){
    if(length(result_u[i][[1]])!=0){
      gu <- gu+
        geom_col(data = result_u[i][[1]], 
                 aes(x = player.no, y = trials,group = stage, fill = type, label = start, label1 = end),
                 width = 0.5,
                 position = position_stack(reverse = TRUE))
    }
  }
  gu <- gu +
    scale_fill_manual(breaks = c("unchange", "flat", "short-position", "long-position"),
                      values = c("#161a1d80", "#fed76680", "#fe4a4980", "#009fb780")
    )+
    ylim(0,100)+
    theme_classic()+
    coord_flip()
  
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
    scale_fill_manual(breaks = c("unchange", "flat", "short-position", "long-position"),
                      values = c("#161a1d80", "#fed76680", "#fe4a4980", "#009fb780")
    )+
    ylim(0,100)+
    theme_classic()+
    coord_flip()
  pg <- ggarrange(point_plt, gu, g,ncol = 1,align = "v")
  return(pg)
}
pp1 <- paint_plot_p(no1)
pp2 <- paint_plot_p(no2)
stockprice_plt <- ggplot(data, aes(x = trials, y = StockPrice)) +
  ggtitle(paste0("Group No. ",ceiling(no/2)))+
  geom_point()+
  geom_line()+
  xlim(0,100)+
  theme_classic()
ggarrange(pp1, stockprice_plt, pp2,  ncol = 1, align = "v")




