#library or install packages
library(tidyverse)
library(zoo)
library(ggpubr)
library(cowplot)
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
#select data----
U_fileset[[]]
data <- U_fileset[[2]] %>% 
  select(., Decision, Dprice) %>% 
  transform(., Decision = factor(Decision, levels = c("sell","no trade","buy"),
                                 labels = c(-1, 0, 1)))
kmeans <- function(data, centers){
  km <- kmeans(data, centers = centers)
  #組內距離平方和WSS
  print(km$tot.withinss)
  #組間距離平方和BSS
  print(km$betweenss)
  #總離均差平方和TSS
  TSS <- km$tot.withinss + km$betweenss
  print(TSS)
  #ratio
  ratio <- km$tot.withinss/(km$tot.withinss + km$betweenss)
  print(ratio)
}
kmeans(data, 3)
