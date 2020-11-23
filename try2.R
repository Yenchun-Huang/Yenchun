library(tidyverse)
library(zoo)
library(ggpubr)
setwd("C:\\Users\\User\\Desktop\\ebg408\\shiny")
filepath = "data"
filenames = list.files(path = filepath, pattern="*.csv")
fileset <- lapply(paste0(filepath,"/",filenames), read.csv)
data.cp <- data.frame(trials = fileset[[2]]$Trials, Stock = fileset[[2]]$p2Stock, Decision = fileset[[2]]$p2Decision)
data.cp <- data.cp %>% 
  mutate(., buy = ifelse(Decision=="buy",1,0)
         , sell = ifelse(Decision=="sell",1,0)
         , notrade = ifelse(Decision=="no trade",1,0))%>%
  mutate(., lag_Decision = lag(Decision)) %>% 
  select(., trials, Stock, Decision, lag_Decision,buy,sell,notrade) %>% 
  mutate(., player.no = 2)

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
  print(nt_end)
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
# #mark long-position####
# long_position <- function(data){
#   lp_start <- list()
#   lp_trials <- list()
#   lp_slope <- list()
#   lp_ds <- list()
#   lp_start_s <- list()
#   for (max in 1:length(data$trials)) {
#     if(data$Stock[max] == max(data$Stock)){
#       max_point <- data$trials[max]
#     }
#   }
#   
#   for(i in 1:max_point){
#     delta_stock <- max(data$Stock[i:max_point]) - min(data$Stock[i:max_point])
#     if(delta_stock>=10){
#       lp_trials <- append(lp_trials, max_point-i)
#       lp_start <- append(lp_start, i)
#       lp_ds <- append(lp_ds, delta_stock)
#     }else if(max_point-i >=10 & (delta_stock/(max_point-i))>=0.8){
#       lp_trials <- append(lp_trials, max_point-i)
#       lp_start <- append(lp_start, i)
#       lp_ds <- append(lp_ds, delta_stock)
#     }
#   }
#   if(length(lp_trials)!=0){
#     for (a in 1:length(lp_trials)) {
#       if(lp_ds[[a]] == max(unlist(lp_ds))){
#         lp_slope <- append(lp_slope, lp_ds[[a]]/lp_trials[[a]])
#         lp_start_s <- append(lp_start_s, lp_start[[a]])
#       }
#     }
#     for (b in 1:length(lp_slope)) {
#       if(lp_slope[[b]] == max(unlist(lp_slope))){
#         start = data[lp_start_s[[b]],]$trials
#         section_type <- data.frame(type = "NA", start = 1, end = start)
#         section_type <- add_row(section_type, type = "long-position", start = start, end = max_point)
#         section_type <- add_row(section_type, type = "NA", start = max_point, end = length(data$trials))
#         data.lp1 <- data %>% filter(., trials < start)
#         data.lp2 <- data %>% filter(., trials > max_point)
#         result <- list(data1 = data.lp1, data2 = data.lp2, type = section_type)
#         return(result)
#       }
#     }
#   }else{
#     section_type <- data.frame()
#     result <- list(type = section_type)
#     return(result)
#   }
# }
# #mark short-position####
# short_position <- function(data){
#   sp_end <- list()
#   sp_trials <- list()
#   sp_slope <- list()
#   sp_ds <- list()
#   sp_end_s <- list()
#   for (max in 1:length(data$trials)) {
#     if(data$Stock[max] == max(data$Stock)){
#       max_point <- data$trials[max]
#     }
#   }
#   for(i in max_point:length(data$trials)){
#     delta_stock <- min(data$Stock[max_point:i])-max(data$Stock[max_point:i])
#     if(delta_stock <= -10){
#       sp_trials <- append(sp_trials, i-max_point)
#       sp_end <- append(sp_end, i)
#       sp_ds <- append(sp_ds, delta_stock)
#     }else if (i-max_point >=10 & delta_stock/(i-max_point)<= -0.8){
#       sp_trials <- append(sp_trials, i-max_point)
#       sp_end <- append(sp_end, i)
#       sp_ds <- append(sp_ds, delta_stock)
#     }
#   }
#   if(length(sp_trials)!=0){
#     for (a in 1:length(sp_trials)) {
#       if(sp_ds[[a]] == min(unlist(sp_ds))){
#         sp_slope <- append(sp_slope, sp_ds[[a]]/sp_trials[[a]])
#         sp_end_s <- append(sp_end_s, sp_end[[a]])
#       }
#     }
#     for (b in 1:length(sp_slope)) {
#       if(sp_slope[[b]] == min(unlist(sp_slope))){
#         end = data[sp_end_s[[b]],]$trials
#         section_type <- data.frame(type = "NA", start = 1, end = max_point)
#         section_type <- add_row(section_type, type = "short-position", start = max_point, end = end)
#         section_type <- add_row(section_type, type = "NA", start = end, end = length(data$trials))
#         data.sp1 <- data %>% filter(., trials < max_point)
#         data.sp2 <- data %>% filter(., trials > end)
#         data.sp <- rbind(data.sp1, data.sp2)
#         result <- list(data = data.sp, type = section_type)
#         return(result)
#       }
#     }
#   }else{
#     section_type <- data.frame()
#     result <- list(type = section_type)
#     return(result)
#   }
# }
#algorithm####
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

#data processing####
cp.data <- ChangePointAlgorithm(146, 5, 1, 15) %>% 
  mutate(., trials = end - start, stage = c(1:length(no_trade(data.cp)[1])), player.no = paste0("no.", data.cp$player.no[1]))
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
result <- list(result.nt, result.st, cp.data)

g <- ggplot()
for(i in 1:length(result)){
  if(length(result[i][[1]])!=0){
    g <- g+
      geom_col(data = result[i][[1]], 
               aes(x = player.no, y = trials, group = stage, fill = type, label = start, label1 = end, alpha = 0.5),
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
g
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
    geom_line(aes(x=trials, y=stock))+
    xlim(0,100)+
    theme_classic()
  return(plt)
}
plt <- PlotFunc(146)
p <- ggarrange(g, plt,
          ncol = 1, nrow = 2, align = "v")
p


