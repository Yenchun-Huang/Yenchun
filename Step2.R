#library or install packages
library(tidyverse)
library(zoo)
library(ggplot2)
library(lattice)
library(RColorBrewer)
library(plotly)
library(ggpubr)
#Import data
setwd("C:\\Users\\User\\Desktop\\ebg408\\shiny\\data_table\\")
#unchange----
U_files <- list.files(path = "U_data\\")
U_fileset <- list()
for (i in 1:length(U_files)) {
  U_fileset[[i]] <- read.csv(paste0("U_data\\",U_files[i]), header = T, sep = ",")
}
#flat----
F_files <- list.files(path = "F_data\\")
F_fileset <- list()
for (i in 1:length(F_files)) {
  F_fileset[[i]] <- read.csv(paste0("F_data\\",F_files[i]), header = T, sep = ",")
}
#long-position----
LP_files <- list.files(path = "LP_data\\")
LP_fileset <- list()
for (i in 1:length(LP_files)) {
  LP_fileset[[i]] <- read.csv(paste0("LP_data\\",LP_files[i]), header = T, sep = ",")
}
#short-position----
SP_files <- list.files(path = "SP_data\\")
SP_fileset <- list()
for (i in 1:length(SP_files)) {
  SP_fileset[[i]] <- read.csv(paste0("SP_data\\",SP_files[i]), header = T, sep = ",")
}
#Supporting function----
#table with Dprice < = > 0
contingency_table_ratio <- function(data){
  data <- data %>% 
    select(., Decision,StockPrice, Dprice)
  data <- drop_na(data)
  for (i in 1:length(data$Dprice)) {
    if(data$Dprice[i] < 0){
      data$Dprice[i] <- "<0"
    }else if(data$Dprice[i] > 0){
      data$Dprice[i] <- ">0"
    }else{
      data$Dprice[i] <- "=0"
    }
  }
  data$Decision <- factor(data$Decision, levels = c("buy","no trade","sell"))
  data$Dprice <- factor(data$Dprice, levels = c("<0","=0",">0"))
  c_table <- table(data$Decision, data$Dprice, dnn = c("Decision", "Dprice"))
  n <- length(data$Decision)
  table <- data.frame("buy_less" = c_table[1,1]/n, "buy_equal" = c_table[1,2]/n, "buy_more" = c_table[1,3]/n,
                      "no trade_less" = c_table[2,1]/n, "no trade_equal" = c_table[2,2]/n, "no trade_more" = c_table[2,3]/n,
                      "sell_less" = c_table[3,1]/n, "sell_equal" = c_table[3,2]/n, "sell_more" = c_table[3,3]/n)
  return(table)
}
#table with Dprice, Dcash, DAsset(i-j)
contingency_table_3 <- function(data){
  data <- data %>% 
    mutate(., Dcash = Cash - lag(Cash), Asset = TotalAsset - opp_TotalAsset) %>%
    mutate(., DAsset = Asset - lag(Asset)) %>% 
    select(., Decision, Dprice, Dcash, DAsset)
  data <- drop_na(data)
  for (i in 1:length(data$Dprice)) {
    if(data$Dprice[i] < 0){
      data$Dprice[i] <- "Dp<0"
    }else if(data$Dprice[i] > 0){
      data$Dprice[i] <- "Dp>0"
    }else{
      data$Dprice[i] <- "Dp=0"
    }
  }
  for (i in 1:length(data$Dcash)) {
    if(data$Dcash[i] < 0){
      data$Dcash[i] <- "Dc<0"
    }else if(data$Dcash[i] > 0){
      data$Dcash[i] <- "Dc>0"
    }else{
      data$Dcash[i] <- "Dc=0"
    }
  }
  for (i in 1:length(data$DAsset)) {
    if(data$DAsset[i] < 0){
      data$DAsset[i] <- "DA<0"
    }else if(data$DAsset[i] > 0){
      data$DAsset[i] <- "DA>0"
    }else{
      data$DAsset[i] <- "DA=0"
    }
  }
  data$Decision <- factor(data$Decision, levels = c("buy","no trade","sell"))
  data$Dprice <- factor(data$Dprice, levels = c("Dp<0","Dp=0","Dp>0"))
  data$Dcash <- factor(data$Dcash, levels = c("Dc<0","Dc=0","Dc>0"))
  data$DAsset <- factor(data$DAsset, levels = c("DA<0","DA=0","DA>0"))
  c3_table <- table(data$Decision, data$Dprice,data$Dcash, data$DAsset, dnn = c("Decision", "Dprice", "Dcash", "DAsset"))
  a <- as.data.frame(c3_table)
  a <- a %>% 
    mutate(ratio = Freq/sum(Freq)) %>% 
    select(Decision, Dprice, Dcash, DAsset, ratio)
  b <- pivot_wider(a, names_from = c(Decision, Dprice, Dcash, DAsset), values_from = ratio)
  table <- as.data.frame(b)
  return(table)
}

#data manipulation -> prepare for k-means----
#U
{U_table_than <- list()
U_table_ratio <- list()
for (U_no in 1:length(U_fileset)) {
  con_table <- contingency_table_3(U_fileset[[U_no]])
  con_table_ratio <- contingency_table_ratio(U_fileset[[U_no]])
  U_table_than[[U_no]] <- con_table
  U_table_ratio[[U_no]] <- con_table_ratio
}
U_table <- do.call(rbind,U_table_than)
U_table_ratio <- do.call(rbind, U_table_ratio)
U_table_no <- U_table %>% 
  mutate(., type = "unchange", no = 1:length(U_table[,1]))
U_table_ratio_no <- U_table_ratio %>% 
  mutate(., type = "unchange", no = 1:length(U_table_ratio[,1]))}
#F
{F_table_than <- list()
F_table_ratio <- list()
for (F_no in 1:length(F_fileset)) {
  con_table <- contingency_table_3(F_fileset[[F_no]])
  con_table_ratio <- contingency_table_ratio(F_fileset[[F_no]])
  F_table_than[[F_no]] <- con_table
  F_table_ratio[[F_no]] <- con_table_ratio
}
F_table <- do.call(rbind, F_table_than)
F_table_ratio <- do.call(rbind, F_table_ratio)
F_table_no <- F_table %>% 
  mutate(., type = "flat", no = 1:length(F_table[,1]))
F_table_ratio_no <- F_table_ratio %>% 
  mutate(., type = "flat", no = 1:length(F_table_ratio[,1]))}
#LP
{LP_table_than <- list()
LP_table_ratio <- list()
for (LP_no in 1:length(LP_fileset)) {
  con_table <- contingency_table_3(LP_fileset[[LP_no]])
  con_table_ratio <- contingency_table_ratio(LP_fileset[[LP_no]])
  LP_table_than[[LP_no]] <- con_table
  LP_table_ratio[[LP_no]] <- con_table_ratio
}
LP_table <- do.call(rbind, LP_table_than)
LP_table_ratio <- do.call(rbind, LP_table_ratio)
LP_table_no <- LP_table %>% 
  mutate(., type = "long-position", no = 1:length(LP_table[,1]))
LP_table_ratio_no <- LP_table_ratio %>% 
  mutate(., type = "long-position", no = 1:length(LP_table_ratio[,1]))}
#SP
{SP_table_than <- list()
SP_table_ratio <- list()
for (SP_no in 1:length(SP_fileset)) {
  con_table <- contingency_table_3(SP_fileset[[SP_no]])
  con_table_ratio <- contingency_table_ratio(SP_fileset[[SP_no]])
  SP_table_than[[SP_no]] <- con_table
  SP_table_ratio[[SP_no]] <- con_table_ratio
}
SP_table <- do.call(rbind, SP_table_than)
SP_table_ratio <- do.call(rbind, SP_table_ratio)
SP_table_no <- SP_table %>% 
  mutate(., type = "short-position", no = 1:length(SP_table[,1]))
SP_table_ratio_no <- SP_table_ratio %>% 
  mutate(., type = "short-position", no = 1:length(SP_table_ratio[,1]))}
#conmerge to one data frame
{all_table <- rbind(U_table, F_table, LP_table, SP_table)
all_table_ratio <- rbind(U_table_ratio, F_table_ratio, LP_table_ratio, SP_table_ratio)
all_table_no <- rbind(U_table_no, F_table_no, LP_table_no, SP_table_no)
all_table_ratio_no <- rbind(U_table_ratio_no, F_table_ratio_no, LP_table_ratio_no, SP_table_ratio_no)}
#k-means----
#尋找最佳的k值
findK <- function(data){
  klist <- seq(2:20)
  knnFunction <- function(x) {
    kms <- kmeans(data, centers = x, nstart = 1)
    ratio <- kms$tot.withinss / (kms$tot.withinss + kms$betweenss)
  }
  ratios <- sapply(klist, knnFunction)
  # k value與準確度視覺化
  df <- data.frame(
    kv = klist, KMratio = ratios)
  g <- ggplot(df, aes(x = kv, y = KMratio, label = kv, color = KMratio)) +
    geom_point(size = 5) + geom_text(vjust = 2)
  return(g)
}
#K最佳解----
# #U
# findK(U_table)
# #U最佳解:K_centers = 9
# U_km <- kmeans(U_table, centers = 9)
# U_km
# U_km$cluster
# #F
# findK(F_table)
# #F最佳解:K_centers = 10
# F_km <- kmeans(F_table, centers = 10)
# F_km
# F_km$cluster
# #LP
# findK(LP_table)
# #F最佳解:K_centers = 11
# LP_km <- kmeans(LP_table, centers = 11)
# LP_km
# LP_km$cluster
# #SP
# findK(SP_table)
# #F最佳解:K_centers = 10
# SP_km <- kmeans(SP_table, centers = 10)
# SP_km
# SP_km$cluster

#K means for all section----
findK(all_table)
findK(all_table_ratio)
#mutiple variables----
#Dprice,Decision,Dcash,DAsset
#use ratio
#select k=10 as centers
all_table
all_table_drop <- all_table %>% 
  select_if(colSums(.) != 0)
all_table_drop
km <- kmeans(all_table_drop, centers = 10)
centers <- km$centers
centers
r_data <- matrix(centers, 45, 10)
levelplot(r_data, ylab = "condition", xlab = "group", 
          main = "K-means clustering(number)", col.regions = gray(100:0/100))
#ggplot
{group = paste(rep("group.",10), c(1:10), sep = "")
condition = names(all_table_drop)
r_data <- expand.grid(group = group, condition = condition)
r_data$ratio = matrix(centers)
r_data <- r_data %>% 
  mutate(., text = paste0("X:", group, "\n", "Y:", condition, "\n", "ratio:", ratio))
g_fourtype_ratio <- ggplot(r_data, aes(x=group, y=condition, fill=ratio, text=text))+
  geom_tile()+
  ggtitle("K-means clustering(ratio)")+
  scale_fill_gradientn(colors = c("white", "blue"))
ggplotly(g_fourtype_ratio)}

#only Dprice----
#use ratio
#select k=9 as centers
km_ratio <- kmeans(all_table_ratio, centers = 9)
km_ratio
centers_ratio <- km_ratio$centers
data_ratio <- matrix(centers_ratio, 9, 9)
#use ggplotly
group = paste(rep("group.",9), c(1:9), sep = "")
condition = c("buy_<0", "buy_=0", "buy_>0",
              "no.trade_<0", "no.trade_=0", "no.trade_>0",
              "sell_<0", "sell_=0", "sell_>0")
ratio_data = expand.grid(group = group, condition = condition)
ratio_data$ratio = matrix(centers_ratio)
ratio_data <- ratio_data %>% 
  mutate(., text = paste0("X:", group, "\n", "Y:", condition, "\n", "ratio:", ratio))
ratio_data
g_ratio <- ggplot(ratio_data, aes(x=group, y=condition, fill=ratio, text=text))+
  geom_tile()+
  ggtitle("K-means clustering(ratio)")+
  scale_fill_gradientn(colors = c("blue", "white", "red"))
g_ratio
ggplotly(g_ratio, tooltip = "text")
#use barplot to present 9 groups
ratio_data_sep <- ratio_data %>% 
  separate(condition, c("decision", "condition"), "_") %>% 
  select(., group, decision, condition, ratio)
gplot <- list()
for (no in 1:9) {
  group.no <- paste0("group.",no)
  g <- ggplot(ratio_data_sep[ratio_data_sep[,"group"] == group.no,])+
    geom_bar(aes(x=condition, y=ratio, fill = decision),
             position = "dodge",
             stat="identity")+
    ggtitle(group.no)
  gplot[[no]] <- g
}
{g1 <- gplot[[1]]
g2 <- gplot[[2]]
g3 <- gplot[[3]]
g4 <- gplot[[4]]
g5 <- gplot[[5]]
g6 <- gplot[[6]]
g7 <- gplot[[7]]
g8 <- gplot[[8]]
g9 <- gplot[[9]]}
ggarrange(g1,g2,g3,g4,g5,g6,g7,g8,g9, ncol = 3, nrow = 3)



#preparing data
all_table_ratio_no <- all_table_ratio_no %>% 
  mutate(., group = km_ratio$cluster)
#append clustering to original data





#

#all section price----
#upper bound
find_upper <- function(data_list,no){
  data <- data_list[[no]]
  contingency_table_1(data)
  #find max price
  #filter decision == buy
  data1 <- data %>% 
    filter(., Decision != "buy")
  maxprice <- max(data1$StockPrice)
  maxprice <- ifelse(maxprice == -Inf, NA, maxprice)
  
  #find max trial
  #contain decision == buy
  max_price <- max(data$StockPrice)
  max_trials = c(data$Trials[data$StockPrice == max_price])
  if(length(max_trials) == 1){
    max_trial = list(max_trials)
  }else if(length(max_trials) >= 1){
    decision_list <- list()
    for(i in 1:length(max_trials)){
      each_trial <- max_trials[i]
      for (n in 1:length(data$Trials)) {
        if(data$Trials[n] == each_trial){
          each_decision <- data$Decision[n]
        }
      }
      if(each_decision != "buy"){
        decision_list[i] <- 1
      }else{
        decision_list[i] <- 0
      }
    }
    b_per <- sum(as.integer(decision_list))/length(decision_list)
    if(b_per > 0.5){
      max_trial <- list(c(max_trials))
    }else{
      max_trial <- list(NA)
    }
  }
  #export max price(decision != buy) & trials(average) when reach max price
  return(c(maxprice, max_trial))
}
find_upper(F_fileset, 44)
#save upper bound for each section
#F
F_maxprice <- list()
for (F_num in 1:length(F_fileset)) {
  F_maxprice[[F_num]] <- find_upper(F_fileset, F_num)[[1]]
}
F_maxprice_DF <- data.frame(max_price = unlist(F_maxprice))
#U
U_maxprice <- list()
for (U_num in 1:length(U_fileset)) {
  U_maxprice[[U_num]] <- find_upper(U_fileset, U_num)[[1]]
}
U_maxprice_DF <- data.frame(max_price = unlist(U_maxprice))
#LP
LP_maxprice <- list()
for (LP_num in 1:length(LP_fileset)) {
  LP_maxprice[[LP_num]] <- find_upper(LP_fileset, LP_num)[[1]]
}
LP_maxprice_DF <- data.frame(max_price = unlist(LP_maxprice))
#SP
SP_maxprice <- list()
for (SP_num in 1:length(SP_fileset)) {
  SP_maxprice[[SP_num]] <- find_upper(SP_fileset, SP_num)[[1]]
}
SP_maxprice_DF <- data.frame(max_price = unlist(SP_maxprice))
#lower bound
find_lower <- function(data_list,no){
  data <- data_list[[no]]
  contingency_table_1(data)
  #find min price
  #filter decision == sell
  data1 <- data %>% 
    filter(., Decision != "sell", Stock != 0)
  minprice <- min(data1$StockPrice)
  minprice <- ifelse(minprice == Inf, NA, minprice)
  #find min trial
  #contain decision == sell
  min_price <- min(data$StockPrice)
  min_trials = c(data$Trials[data$StockPrice == min_price])
  if(length(min_trials) == 1){
    min_trial = list(min_trials)
  }else if(length(min_trials) >= 1){
    decision_list <- list()
    for(i in 1:length(min_trials)){
      each_trial <- min_trials[i]
      for (n in 1:length(data$Trials)) {
        if(data$Trials[n] == each_trial){
          each_decision <- data$Decision[n]
        }
      }
      if(each_decision != "sell"){
        decision_list[i] <- 1
      }else{
        decision_list[i] <- 0
      }
    }
    s_per <- sum(as.integer(decision_list))/length(decision_list)
    if(s_per > 0.5){
      min_trial <- list(c(min_trials))
    }else{
      min_trial <- list(NA)
    }
  }
  #export min price(decision != sell) & trials(average) when reach min price
  return(c(minprice, min_trial))
}
find_lower(SP_fileset, 186)
#save lower bound for each section
#F
F_minprice <- list()
for (F_num in 1:length(F_fileset)) {
  F_minprice[[F_num]] <- find_lower(F_fileset, F_num)[[1]]
}
F_minprice_DF <- data.frame(min_price = unlist(F_minprice))
#U
U_minprice <- list()
for (U_num in 1:length(U_fileset)) {
  U_minprice[[U_num]] <- find_lower(U_fileset, U_num)[[1]]
}
U_minprice_DF <- data.frame(min_price = unlist(U_minprice))
#LP
LP_minprice <- list()
for (LP_num in 1:length(LP_fileset)) {
  LP_minprice[[LP_num]] <- find_lower(LP_fileset, LP_num)[[1]]
}
LP_minprice_DF <- data.frame(min_price = unlist(LP_minprice))
#SP
SP_minprice <- list()
for (SP_num in 1:length(SP_fileset)) {
  SP_minprice[[SP_num]] <- find_lower(SP_fileset, SP_num)[[1]]
}
SP_minprice_DF <- data.frame(min_price = unlist(SP_minprice))
#plot
pdf("hist.pdf")
par(mfrow = c(1,1))
ggplot(F_maxprice_DF)+
  geom_histogram(aes(max_price), binwidth = 5)+
  coord_cartesian(xlim = c(0, 200))+
  ggtitle("Upper bound(flat)")
ggplot(U_maxprice_DF)+
  geom_histogram(aes(max_price),binwidth = 5)+
  coord_cartesian(xlim = c(0, 200))+
  ggtitle("Upper bound(unchange)")
ggplot(LP_maxprice_DF)+
  geom_histogram(aes(max_price),binwidth = 5)+
  coord_cartesian(xlim = c(0, 200))+
  ggtitle("Upper bound(long position)")
ggplot(SP_maxprice_DF)+
  geom_histogram(aes(max_price),binwidth = 5)+
  coord_cartesian(xlim = c(0, 200))+
  ggtitle("Upper bound(short position)")
ggplot(F_minprice_DF)+
  geom_histogram(aes(min_price), binwidth = 5)+
  coord_cartesian(xlim = c(0, 200))+
  ggtitle("lower bound(flat)")
ggplot(U_minprice_DF)+
  geom_histogram(aes(min_price),binwidth = 5)+
  coord_cartesian(xlim = c(0, 200))+
  ggtitle("lower bound(unchange)")
ggplot(LP_minprice_DF)+
  geom_histogram(aes(min_price),binwidth = 5)+
  coord_cartesian(xlim = c(0, 200))+
  ggtitle("lower bound(long position)")
ggplot(SP_minprice_DF)+
  geom_histogram(aes(min_price),binwidth = 5)+
  coord_cartesian(xlim = c(0, 200))+
  ggtitle("lower bound(short position)")
dev.off()

