#import package data
library(shiny)
library(tidyverse)
library(psych)
library(plyr)
library(plotly)
library(zoo)
options(scipen = 999)
#setwd("D:/ebg408/corshiny/data")
filepath = "data"
filenames = list.files(path = filepath, pattern="*.csv")
fileset <- lapply(paste0(filepath,"/",filenames), read.csv)

server <- function(input, output) {
  
  #1.output-correlation ####
  #1.2 StockPrice----
  #reactive data
  data.select.pa1 <- reactive({
    selected.group.pa = as.integer(input$groups)
    p1.number.pa = selected.group.pa*2-1
    data1.exp.price <- data.frame(
      group = selected.group.pa,
      player = p1.number.pa,
      trials = fileset[[p1.number.pa]]$Trials,
      action = fileset[[p1.number.pa]]$p1Decision,
      stockprice = fileset[[p1.number.pa]]$StockPrice
    )
    data1.exp.price <- data1.exp.price[-101,] %>% 
      filter(trials %in% (seq(input$trials[1],input$trials[2])))
    
  })
  data.select.pa2 <- reactive({
    selected.group.pa = as.integer(input$groups)
    p2.number.pa = selected.group.pa*2
    data2.exp.price <- data.frame(
      group = selected.group.pa,
      player = p2.number.pa,
      trials = fileset[[p2.number.pa]]$Trials,
      action = fileset[[p2.number.pa]]$p2Decision,
      stockprice = fileset[[p2.number.pa]]$StockPrice
    )
    data2.exp.price <- data2.exp.price[-101,] %>% 
      filter(trials %in% (seq(input$trials[1],input$trials[2])))
  })
  
  #price-action boxplot
  output$paplot1 <- renderPlotly({
    data = data.select.pa1()
    plot1.exp.price <- ggplot(data,aes(x=action, y=stockprice, color=action))+
      ggtitle(paste0("Subject No.",as.integer(input$groups)*2-1))+
      geom_boxplot()
    if(input$showhline==TRUE){
      plot1.exp.price <- plot1.exp.price+geom_hline(yintercept = as.integer(input$price.pa))
    } else{
    }
    ggplotly(plot1.exp.price)
  })
  
  output$paplot2 <- renderPlotly({
    data = data.select.pa2()
    plot2.exp.price <- ggplot(data,aes(x=action, y=stockprice, color=action))+
      ggtitle(paste0("Subject No.",as.integer(input$groups)*2))+
      geom_boxplot()
    if(input$showhline==TRUE){
      plot2.exp.price <- plot2.exp.price+geom_hline(yintercept = as.integer(input$price.pa))
    } else{
      105   }
    ggplotly(plot2.exp.price)
  })
  
  #price-action contingency table
  data.contingency.pa1 <- reactive({
    data <- data.select.pa1()
    contingency.pa1 <- data %>%
      mutate(expected = ifelse(data$stockprice >= as.integer(input$price.pa), paste0(">=",input$price.pa), paste0("<",input$price.pa))) %>%
      select(expected,action)
  })
  
  data.contingency.pa2 <- reactive({
    data <- data.select.pa2()
    contingency.pa1 <- data %>%
      mutate(expected = ifelse(data$stockprice >= as.integer(input$price.pa), paste0(">=",input$price.pa), paste0("<",input$price.pa))) %>%
      select(expected,action)
  })
  
  output$padata1 <- renderTable({
    tbl1 = table(data.contingency.pa1())
    as.data.frame.matrix(tbl1)
  }, include.rownames=TRUE)
  
  output$padata2 <- renderTable({
    tbl2 = table(data.contingency.pa2())
    as.data.frame.matrix(tbl2)
  }, include.rownames=TRUE)
  #1.3 dprice----
  data.select.dp1 <- reactive({
    selected.group.dp = as.integer(input$groups)
    p1.number.dp = selected.group.dp*2-1
    data1.dprice <- data.frame(
      group = selected.group.dp,
      player = p1.number.dp,
      trials = fileset[[p1.number.dp]]$Trials,
      action = fileset[[p1.number.dp]]$p1Decision,
      dprice = fileset[[p1.number.dp]]$StockPrice - lag(fileset[[p1.number.dp]]$StockPrice)
    )
    data1.dprice <- data1.dprice[-101,] %>% 
      filter(trials %in% (seq(input$trials[1],input$trials[2])),
             !is.na(dprice))
    
    return(data1.dprice)
  })
  
  data.select.dp2 <- reactive({
    selected.group.dp = as.integer(input$groups)
    p2.number.dp = selected.group.dp*2
    data2.dprice <- data.frame(
      group = selected.group.dp,
      player = p2.number.dp,
      trials = fileset[[p2.number.dp]]$Trials,
      action = fileset[[p2.number.dp]]$p2Decision,
      dprice = fileset[[p2.number.dp]]$StockPrice - lag(fileset[[p2.number.dp]]$StockPrice)
    )
    data2.dprice <- data2.dprice[-101,] %>% 
      filter(trials %in% (seq(input$trials[1],input$trials[2])),
             !is.na(dprice))
    
    return(data2.dprice)
  })
  
  output$dprice.plot1 <- renderPlotly({
    data = data.select.dp1()
    plot1.dprice <- ggplot(data,aes(x=action, y=dprice, color=action))+
      ggtitle(paste0("Subject No.",as.integer(input$groups)*2-1))+
      geom_boxplot()
    
    ggplotly(plot1.dprice)
  })
  
  output$dprice.plot2 <- renderPlotly({
    data = data.select.dp2()
    plot2.dprice <- ggplot(data,aes(x=action, y=dprice, color=action))+
      ggtitle(paste0("Subject No.",as.integer(input$groups)*2))+
      geom_boxplot()
    
    ggplotly(plot2.dprice)
  })
  #1.4 dstock----
  data.select.dstck1 <- reactive({
    #create action dataframe
    selected.group.dstck = as.integer(input$groups)
    p1.number.dstck = selected.group.dstck*2-1
    action.data <- data.frame(trials = fileset[[p1.number.dstck]]$Trials,
                              last_turn = lag(fileset[[p1.number.dstck]]$p1Decision),
                              this_turn = fileset[[p1.number.dstck]]$p1Decision) %>%
      filter(., trials %in% seq(input$trials[1],input$trials[2]),
             !is.na(last_turn)) %>%
      group_by(., last_turn, this_turn) %>%
      dplyr::summarise(.,n=n()) 
    #add conditional probability
    action.data.list <- list()
    for (i in unique(action.data$last_turn)){
      tmp <- action.data %>% 
        filter(.,last_turn==i) %>%
        mutate(.,cond_prob=round(n/sum(n), digit=2))
      action.data.list[[i]] <- tmp
    }
    action.data <- do.call(rbind, action.data.list)
    
    return(action.data)
  })
  
  data.select.dstck2 <- reactive({
    #create action dataframe
    selected.group.dstck = as.integer(input$groups)
    p2.number.dstck = selected.group.dstck*2
    action.data <- data.frame(trials = fileset[[p2.number.dstck]]$Trials,
                              last_turn = lag(fileset[[p2.number.dstck]]$p2Decision),
                              this_turn = fileset[[p2.number.dstck]]$p2Decision) %>%
      filter(., trials %in% seq(input$trials[1],input$trials[2]),
             !is.na(last_turn)) %>%
      group_by(., last_turn, this_turn) %>%
      dplyr::summarise(.,n=n()) 
    #add conditional probability
    action.data.list <- list()
    for (i in unique(action.data$last_turn)){
      tmp <- action.data %>% 
        filter(.,last_turn==i) %>%
        mutate(.,cond_prob=round(n/sum(n), digit=2))
      action.data.list[[i]] <- tmp
    }
    action.data <- do.call(rbind, action.data.list)
    
    return(action.data)
  })
  
  output$dstck.plot1 <- renderPlotly({
    dstck.plot1 <- ggplot(data.select.dstck1())+
      ggtitle(label = paste0("Subject No.",as.integer(input$groups)*2-1))+
      geom_col(aes(x=last_turn, y=cond_prob, fill=this_turn), position = position_dodge2(preserve = "single"))+
      coord_cartesian(ylim = c(0, 1))+
      theme_minimal()
    ggplotly(dstck.plot1)
  })
  
  output$dstck.plot2 <- renderPlotly({
    dstck.plot2 <- ggplot(data.select.dstck2())+
      ggtitle(label = paste0("Subject No.",as.integer(input$groups)*2))+
      geom_col(aes(x=last_turn, y=cond_prob, fill=this_turn), position = position_dodge2(preserve = "single"))+
      coord_cartesian(ylim = c(0, 1))+
      theme_minimal()
    ggplotly(dstck.plot2)
  })
  
  #little table
  output$dstck.tbl1 <- renderTable({
    tbl1.action <- data.select.dstck1()[,c(1,2,4)] %>%
      spread(., last_turn, cond_prob) %>%
      column_to_rownames(., var = "this_turn")
    return(tbl1.action)
  },  include.rownames=TRUE)
  
  output$dstck.tbl2 <- renderTable({
    tbl2.action <- data.select.dstck2()[,c(1,2,4)] %>%
      spread(., last_turn, cond_prob) %>%
      column_to_rownames(., var = "this_turn")
    return(tbl2.action)
  },  include.rownames=TRUE)
  
  #1.5 dstock.j----
  data.select.dstckj1 <- reactive({
    #create action dataframe
    selected.group.dstckj = as.integer(input$groups)
    p1.number.dstckj = selected.group.dstckj*2-1
    action.data <- data.frame(trials = fileset[[p1.number.dstckj]]$Trials,
                              last_turn_counter = lag(fileset[[p1.number.dstckj+1]]$p2Decision),
                              this_turn = fileset[[p1.number.dstckj]]$p1Decision) %>%
      filter(., trials %in% seq(input$trials[1],input$trials[2]),
             !is.na(last_turn_counter)) %>%
      group_by(., last_turn_counter, this_turn) %>%
      dplyr::summarise(.,n=n()) 
    #add conditional probability
    action.data.list <- list()
    for (i in unique(action.data$last_turn_counter)){
      tmp <- action.data %>% 
        filter(.,last_turn_counter==i) %>%
        mutate(.,cond_prob=round(n/sum(n), digit=2))
      action.data.list[[i]] <- tmp
    }
    action.data <- do.call(rbind, action.data.list)
    
    return(action.data)
  })
  
  data.select.dstckj2 <- reactive({
    #create action dataframe
    selected.group.dstckj = as.integer(input$groups)
    p2.number.dstckj = selected.group.dstckj*2
    action.data <- data.frame(trials = fileset[[p2.number.dstckj]]$Trials,
                              last_turn_counter = lag(fileset[[p2.number.dstckj-1]]$p1Decision),
                              this_turn = fileset[[p2.number.dstckj]]$p2Decision) %>%
      filter(., trials %in% seq(input$trials[1],input$trials[2]),
             !is.na(last_turn_counter)) %>%
      group_by(., last_turn_counter, this_turn) %>%
      dplyr::summarise(.,n=n()) 
    #add conditional probability
    action.data.list <- list()
    for (i in unique(action.data$last_turn_counter)){
      tmp <- action.data %>% 
        filter(.,last_turn_counter==i) %>%
        mutate(.,cond_prob=round(n/sum(n), digit=2))
      action.data.list[[i]] <- tmp
    }
    action.data <- do.call(rbind, action.data.list)
    
    return(action.data)
  })
  
  output$dstckj.plot1 <- renderPlotly({
    dstckj.plot1 <- ggplot(data.select.dstckj1())+
      ggtitle(label = paste0("Subject No.",as.integer(input$groups)*2-1))+
      geom_col(aes(x=last_turn_counter, y=cond_prob, fill=this_turn), position = position_dodge2(preserve = "single"))+
      coord_cartesian(ylim = c(0, 1))+
      theme_minimal()
    ggplotly(dstckj.plot1)
  })
  
  output$dstckj.plot2 <- renderPlotly({
    dstckj.plot2 <- ggplot(data.select.dstckj2())+
      ggtitle(label = paste0("Subject No.",as.integer(input$groups)*2))+
      geom_col(aes(x=last_turn_counter, y=cond_prob, fill=this_turn), position = position_dodge2(preserve = "single"))+
      coord_cartesian(ylim = c(0, 1))+
      theme_minimal()
    ggplotly(dstckj.plot2)
  })
  
  #little table
  output$dstckj.tbl1 <- renderTable({
    tbl1.action <- data.select.dstckj1()[,c(1,2,4)] %>%
      spread(., last_turn_counter, cond_prob) %>%
      column_to_rownames(., var = "this_turn")
    return(tbl1.action)
  },  include.rownames=TRUE)
  
  output$dstckj.tbl2 <- renderTable({
    tbl2.action <- data.select.dstckj2()[,c(1,2,4)] %>%
      spread(., last_turn_counter, cond_prob) %>%
      column_to_rownames(., var = "this_turn")
    return(tbl2.action)
  },  include.rownames=TRUE)
  
  
  #1.6 totalasset.ij----
  data.select.asstij1 <- reactive({
    selected.group.asstij = as.integer(input$groups)
    p1.number.asstij = selected.group.asstij*2-1
    p2.number.asstij = selected.group.asstij*2
    data1.asstij <- data.frame(
      group = selected.group.asstij,
      player = p1.number.asstij,
      trials = fileset[[p1.number.asstij]]$Trials,
      action = fileset[[p1.number.asstij]]$p1Decision,
      totalasset.ij = fileset[[p1.number.asstij]]$p1TotalAsset - fileset[[p2.number.asstij]]$p2TotalAsset
    )
    data1.asstij <- data1.asstij[-101,] %>% 
      filter(trials %in% (seq(input$trials[1],input$trials[2])))
    
    return(data1.asstij)
  })
  
  data.select.asstij2 <- reactive({
    selected.group.asstij = as.integer(input$groups)
    p1.number.asstij = selected.group.asstij*2-1
    p2.number.asstij = selected.group.asstij*2
    data2.asstij <- data.frame(
      group = selected.group.asstij,
      player = p2.number.asstij,
      trials = fileset[[p2.number.asstij]]$Trials,
      action = fileset[[p2.number.asstij]]$p2Decision,
      totalasset.ij = fileset[[p2.number.asstij]]$p2TotalAsset - fileset[[p1.number.asstij]]$p1TotalAsset
    )
    data2.asstij <- data2.asstij[-101,] %>% 
      filter(trials %in% (seq(input$trials[1],input$trials[2])))
    
    return(data2.asstij)
  })
  
  output$asstij.plot1 <- renderPlotly({
    data = data.select.asstij1()
    plot1.asstij <- ggplot(data,aes(x=action, y=totalasset.ij, color=action))+
      ggtitle(paste0("Subject No.",as.integer(input$groups)*2-1))+
      geom_boxplot()
    
    ggplotly(plot1.asstij)
  })
  
  output$asstij.plot2 <- renderPlotly({
    data = data.select.asstij2()
    plot2.asstij <- ggplot(data,aes(x=action, y=totalasset.ij, color=action))+
      ggtitle(paste0("Subject No.",as.integer(input$groups)*2))+
      geom_boxplot()
    
    ggplotly(plot2.asstij)
  })
  #2.output-price ####
  #prepare data: all StockPrice in one table
  for (i in 1:160) {
    fileset[[i]]$Group <- ifelse(i%%2==1, rep((i+1)/2, times = 101), rep(i/2, times=101))
    fileset[[i]]$player <- ifelse(i%%2==1, rep("p1", times = 101), rep("p2", times=101))
  }
  big.data.table <- do.call(rbind.fill, fileset) %>%
    filter(., player == "p1") %>%
    select(., Group, Trials, StockPrice)
  #prepare data: MaxPrice, MinPrice, FinalPrice for each group
  price.data.list <- list()
  for (i in seq(1,160,2)){
    group <- (i+1)/2
    MaxPrice <- max(fileset[[i]]$StockPrice)
    MinPrice <- min(fileset[[i]]$StockPrice)
    FinalPrice <- fileset[[i]]$StockPrice[101]
    price.data.list[[i]] <- data.frame(group, MaxPrice, MinPrice, FinalPrice)
  }
  
  price.data.table = do.call(rbind, price.data.list)
  price.data.table.long <- gather(price.data.table, type, value, MaxPrice, MinPrice, FinalPrice)
  
  #price-boxplot
  output$priceboxplot <- renderPlotly({
    price.boxplot <- ggplot()+
      geom_boxplot(data=big.data.table, aes(x=factor(Group), y=StockPrice), position = "identity")+
      geom_point(data=price.data.table.long, aes(x=factor(group), y=value, color=type))+
      geom_hline(yintercept = 100, color="red")+
      theme_classic()
    ggplotly(price.boxplot, width=1300)
  })
  #price-histogram
  output$pricehistogram <- renderPlotly({
    price.histogram <- ggplot(price.data.table.long, aes(x = value, fill = type ))+
      geom_histogram(position="identity", alpha=0.7)
    ggplotly(price.histogram)
  })
  #price-summary table
  output$pricesumtable <- renderTable({
    price.summary <- data.frame(
      MinPrice = c(min(price.data.table$MinPrice),mean(price.data.table$MinPrice),
                   max(price.data.table$MinPrice),median(price.data.table$MinPrice),sd(price.data.table$MinPrice)),
      MaxPrice = c(min(price.data.table$MaxPrice),mean(price.data.table$MaxPrice),
                   max(price.data.table$MaxPrice),median(price.data.table$MaxPrice),sd(price.data.table$MaxPrice)),
      FinalPrice = c(min(price.data.table$FinalPrice),mean(price.data.table$FinalPrice),
                     max(price.data.table$FinalPrice),median(price.data.table$FinalPrice),sd(price.data.table$FinalPrice)),
      row.names = c("min","mean","max","median","sd")
    )
  },include.rownames = TRUE)
  
  #3.output-final cash ####
  #prepare data
  data.list = list()
  for (i in (1:160)) {
    group <- ifelse(i%%2==1, ((i+1)/2), (i/2))
    player <- i
    finalcash <- ifelse(i%%2==1, fileset[[i]]$p1Cash[101], fileset[[i]]$p2Cash[101])
    finalstock <- ifelse(i%%2==1, fileset[[i]]$p1Stock[101], fileset[[i]]$p2Stock[101])
    data.list[[i]] <- data.frame(group, player, finalcash, finalstock)
  }
  data.table = do.call(rbind, data.list)
  #reactive data
  data.select.cash <- reactive({
    selected.group = as.integer(input$group.cash)
    p1.number = selected.group*2-1
    p2.number = selected.group*2
    
    data.table <- data.table %>%
      mutate(color = case_when(player==p1.number ~ toString(p1.number),
                               player==p2.number ~ toString(p2.number),
                               TRUE ~ "other"))
  })
  #cash_histogram
  output$finalcash <- renderPlotly({
    histogram.finalcash <- ggplot(data.select.cash(), aes(x=finalcash, fill = color))+
      geom_histogram(binwidth = 60)+
      scale_x_continuous(breaks = seq(0, 12300, by = 2000))+
      scale_fill_manual(values=c("coral1", "turquoise1", "azure3"))+
      geom_vline(xintercept=10000,linetype="dashed",size = 1)+
      theme_minimal()
    
    ggplotly(histogram.finalcash)
  })
  #cash_summary
  output$finalcashsum <- renderTable({
    finalcashsum <- data.frame(min=min(data.table$finalcash),
                               max=max(data.table$finalcash),
                               mean=mean(data.table$finalcash),
                               median=median(data.table$finalcash),
                               sd=sd(data.table$finalcash))
  })
  #4.output-final stock ####
  #reactive data
  data.select.stock <- reactive({
    selected.group = as.integer(input$group.stock)
    p1.number = selected.group*2-1
    p2.number = selected.group*2
    
    data.table <- data.table %>%
      mutate(color = case_when(player==p1.number ~ toString(p1.number),
                               player==p2.number ~ toString(p2.number),
                               TRUE ~ "other"))
  })
  #stock_historam
  output$finalstock <- renderPlotly({
    histogram.finalstock <- ggplot(data.select.stock(), aes(x=finalstock, fill = color))+
      geom_histogram(binwidth = 1)+
      scale_x_continuous(breaks = seq(0, 60, by = 5))+
      scale_fill_manual(values=c("coral1", "turquoise1", "azure3"))+
      geom_vline(xintercept=10,linetype="dashed",size = 1)+
      theme_minimal()
    
    ggplotly(histogram.finalstock)
  })
  #stock_summary
  output$finalstocksum <- renderTable({
    finalstocksum <- data.frame(min=min(data.table$finalstock),
                                max=max(data.table$finalstock),
                                mean=mean(data.table$finalstock),
                                median=median(data.table$finalstock),
                                sd=sd(data.table$finalstock))
  })
  #stock_phasemap
  data.select.stockphasemap <- reactive({
    selected.group = as.integer(input$group.stock)
    p1.number = selected.group*2-1
    p2.number = selected.group*2
    
    trials = fileset[[p1.number]]$Trials
    p1 = fileset[[p1.number]]$p1Stock
    p2 = fileset[[p2.number]]$p2Stock
    data <- data.frame(trials, p1, p2)
    return(data)
  })
  
  output$finastockphasemap <- renderPlotly({
    phasemap.stock <- ggplot(data.select.stockphasemap(), aes(x=p1, y=p2))+
      ggtitle(paste("Group",input$group.stock,"phase map"))+
      geom_path(color="grey30")+
      geom_point(aes(color=trials))+
      scale_color_gradient(low = "blue", high = "red")+
      geom_vline(xintercept=10,linetype="dashed")+
      geom_hline(yintercept=10,linetype="dashed")+
      theme_minimal()
    
    ggplotly(phasemap.stock)
  })
  #5.output-change-point ####
  #mark notrade ####
  no_trade <- function(data){
    nt_start <- list()
    nt_end <- list()
    nt_trials <- list()
    for (i in 1:length(data$trials)){
      for (j in (i+1):length(data$trials)) {
        delta_stock <- max(data$Stock[i:j])-min(data$Stock[i:j])
        notrade_avg <- mean(data$notrade[i:j])
        if(j-i >=10 & notrade_avg>= 0.8 & delta_stock <= 2){
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
          section_type <- add_row(section_type, type = "unchange", start = start, end = end)
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
          section_type <- add_row(section_type, type = "flat", start = start, end = end)
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
  #mark short-position####
  short_position <- function(data){
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
      delta_stock <- min(data$Stock[max_point:i])-max(data$Stock[max_point:i])
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
  #reactive data
  cp.data1 <- reactive({
    selected.group.price = as.integer(input$group.price)
    p.number.price = selected.group.price*2-1
    data.cp <- data.frame(trials = fileset[[p.number.price]]$Trials,
                          Stock = fileset[[p.number.price]]$p1Stock, 
                          Decision = fileset[[p.number.price]]$p1Decision)
    data.cp <- data.cp %>% 
      mutate(., buy = ifelse(Decision=="buy",1,0)
             , sell = ifelse(Decision=="sell",1,0)
             , notrade = ifelse(Decision=="no trade",1,0))%>%
      mutate(., lag_Decision = lag(Decision)) %>% 
      select(., trials, Stock, Decision, lag_Decision,buy,sell,notrade) %>% 
      mutate(., player.no = p.number.price)
  })
  cp.data2 <- reactive({
    selected.group.price = as.integer(input$group.price)
    p.number.price = selected.group.price*2
    data.cp <- data.frame(trials = fileset[[p.number.price]]$Trials,
                          Stock = fileset[[p.number.price]]$p2Stock, 
                          Decision = fileset[[p.number.price]]$p2Decision)
    data.cp <- data.cp %>% 
      mutate(., buy = ifelse(Decision=="buy",1,0)
             , sell = ifelse(Decision=="sell",1,0)
             , notrade = ifelse(Decision=="no trade",1,0))%>%
      mutate(., lag_Decision = lag(Decision)) %>% 
      select(., trials, Stock, Decision, lag_Decision,buy,sell,notrade) %>% 
      mutate(., player.no = p.number.price)
  })
  output$p1changepoint4 <- renderPlotly({
    data.cp <- cp.data1()
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
    g <- ggplot()
    for(i in 1:length(result)){
      if(length(result[i][[1]]!=0)){
        g <- g+
          geom_col(data = result[i][[1]], 
                   aes(x = player.no, y = trials, group = stage, fill = type, label = start, label1 = end, alpha = 0.5),
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
  })
  output$p1changepoint3 <- renderPlotly({
    data.cp <- cp.data1()
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
    result <- list(result.st, result.lp, result.sp)
    # result <- rbind(result.nt, result.st, result.lp, result.sp)
    g <- ggplot()
    for(i in 1:length(result)){
      if(length(result[i][[1]]!=0)){
        g <- g+
          geom_col(data = result[i][[1]], 
                   aes(x = player.no, y = trials, group = stage, fill = type, label = start, label1 = end, alpha = 0.5),
                   width = 0.5,
                   position = position_stack(reverse = TRUE))
      }
    }
    g <- g +
      scale_fill_manual(breaks = c("NA", "flat", "short-position", "long-position"),
                        values = c("NA", "#fed766", "#fe4a49", "#009fb7")
      )+
      theme_classic()+
      coord_flip()
    ggplotly(g, height = 250)
  })
  output$p2changepoint4 <- renderPlotly({
    data.cp <- cp.data2()
    if(length(no_trade(data.cp)$type) != 0){
      result.nt <- no_trade(data.cp)$type %>% 
        mutate(., trials = end - start, stage = c(1:length(.)), player.no = paste0("no.", data.cp$player.no[1]))
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
    g <- ggplot()
    for(i in 1:length(result)){
      if(length(result[i][[1]]!=0)){
        g <- g+
          geom_col(data = result[i][[1]], 
                   aes(x = player.no, y = trials, group = stage, fill = type, label = start, label1 = end, alpha = 0.5),
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
  })
  output$p2changepoint3 <- renderPlotly({
    data.cp <- cp.data2()
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
    result <- list(result.st, result.lp, result.sp)
    # result <- rbind(result.nt, result.st, result.lp, result.sp)
    g <- ggplot()
    for(i in 1:length(result)){
      if(length(result[i][[1]]!=0)){
        g <- g+
          geom_col(data = result[i][[1]], 
                   aes(x = player.no, y = trials, group = stage, fill = type, label = start, label1 = end, alpha = 0.5),
                   width = 0.5,
                   position = position_stack(reverse = TRUE))
      }
    }
    g <- g +
      scale_fill_manual(breaks = c("NA", "flat", "short-position", "long-position"),
                        values = c("NA", "#fed766", "#fe4a49", "#009fb7")
      )+
      theme_classic()+
      coord_flip()
    ggplotly(g, height = 250)
  })
  #point plot
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
  output$p1point <- renderPlotly({
    cp.data1 <- cp.data1()
    PlotFunc(as.integer(cp.data1$player.no[1]))
  })
  output$p2point <- renderPlotly({
    cp.data2 <- cp.data2()
    PlotFunc(as.integer(cp.data2$player.no[1]))
  })
  
  #reactive data
  data.select.price <- reactive({
    selected.group.price = as.integer(input$group.price)
    p.number.price = selected.group.price*2
    data.price <- data.frame(
      group = selected.group.price,
      trials = fileset[[p.number.price]]$Trials,
      stockprice = fileset[[p.number.price]]$StockPrice
    )
    data.price <- data.price[-101,] %>% 
      filter(trials %in% (seq(input$trials.price[1],input$trials.price[2])))
  })
  #bcp change point plot
  output$ChangePoint <- renderPlot({
    data.select.price <- data.select.price()
    bcp.plot <- bcp(data.select.price$stockprice)
    plot(bcp.plot, separated = FALSE, main = "Change point of StockPrice",
         xlab = "trials")
  })
  output$ChangePointsum <- renderTable({
    data.select.price <- data.select.price()
    bcp <- bcp(data.select.price$stockprice)
    point.list <- list()
    threshold <- as.numeric(input$threshold.price)
    for (i in 1:(length(bcp[["posterior.prob"]])-1)){
      if(bcp$posterior.prob[i] > threshold){
        trial = bcp[["data"]][,1][i]
        prob = bcp[["posterior.prob"]][i]
        point.list[[i]] <- data.frame(trial, prob)
      }
    }
    point.df <- do.call(rbind.data.frame, point.list)
    ChangePointsum <- point.df
  })
  
  #6.output-behavior-tendencies####
  tblt1.select.data <- reactive({
    selected.group.bt = as.integer(input$group.bt)
    p1.number.bt = selected.group.bt*2-1
    bt.data1 <- data.frame(trials = fileset[[p1.number.bt]]$Trials,
                           action = fileset[[p1.number.bt]]$p1Decision,
                           stock = fileset[[p1.number.bt]]$p1Stock) %>%
      filter(., trials %in% seq(1:100)) %>%
      mutate(., phase = ifelse(trials<=input$trials.bt, paste0(1,"~",input$trials.bt), paste0(input$trials.bt+1,"~",100)))
    
    return(bt.data1)
  })
  tblt2.select.data <- reactive({
    selected.group.bt = as.integer(input$group.bt)
    p2.number.bt = selected.group.bt*2
    bt.data2 <- data.frame(trials = fileset[[p2.number.bt]]$Trials,
                           action = fileset[[p2.number.bt]]$p2Decision,
                           stock = fileset[[p2.number.bt]]$p2Stock) %>%
      filter(., trials %in% seq(1:100)) %>%
      mutate(., phase = ifelse(trials<=input$trials.bt, paste0(1,"~",input$trials.bt), paste0(input$trials.bt+1,"~",100)))
    
    return(bt.data2)
  })
  #table
  output$bt.tbl1 <- renderTable({
    bt.tbl1 <- tblt1.select.data() %>%
      group_by(., phase, action) %>%
      dplyr::summarise(., n=n())
    
    bt.tbl1.list <- list()
    for (i in unique(bt.tbl1$phase)){
      tmp <- bt.tbl1 %>% 
        filter(.,phase==i) %>%
        mutate(.,cond_prob=round(n/sum(n), digit=2))
      bt.tbl1.list[[i]] <- tmp
    }
    bt.tbl1 <- do.call(rbind, bt.tbl1.list) %>%
      select(.,phase, action, cond_prob) %>%
      spread(., action, cond_prob)
    
    bt.tbl1$avg_stock <- with(tblt1.select.data(), c(mean(stock[phase==unique(phase)[1]]),
                                                     mean(stock[phase==unique(phase)[2]])))
    return(bt.tbl1)
  })
  
  output$bt.tbl2 <- renderTable({
    bt.tbl2 <- tblt2.select.data() %>%
      group_by(., phase, action) %>%
      dplyr::summarise(., n=n())
    
    bt.tbl2.list <- list()
    for (i in unique(bt.tbl2$phase)){
      tmp <- bt.tbl2 %>% 
        filter(.,phase==i) %>%
        mutate(.,cond_prob=round(n/sum(n), digit=2))
      bt.tbl2.list[[i]] <- tmp
    }
    bt.tbl2 <- do.call(rbind, bt.tbl2.list)
    
    bt.tbl2 <- do.call(rbind, bt.tbl2.list) %>%
      select(.,phase, action, cond_prob) %>%
      spread(., action, cond_prob)
    
    bt.tbl2$avg_stock <- with(tblt2.select.data(), c(mean(stock[phase==unique(phase)[1]]),
                                                     mean(stock[phase==unique(phase)[2]])))
    return(bt.tbl2)
  })
  
  #plot
  output$bt.plot1 <- renderPlotly({
    bt.tbl1 <- tblt1.select.data() %>%
      group_by(., phase, action) %>%
      dplyr::summarise(., n=n())
    
    bt.tbl1.list <- list()
    for (i in unique(bt.tbl1$phase)){
      tmp <- bt.tbl1 %>% 
        filter(.,phase==i) %>%
        mutate(.,cond_prob=round(n/sum(n), digit=2))
      bt.tbl1.list[[i]] <- tmp
    }
    bt.tbl1 <- do.call(rbind, bt.tbl1.list)
    
    bt.plot1 <- ggplot(bt.tbl1)+
      ggtitle(label = paste0("Subject No.",as.integer(input$group.bt)*2-1))+
      geom_col(aes(x=phase, y=cond_prob, fill=action), position = position_dodge2(preserve = "single"))+
      coord_cartesian(ylim = c(0, 1))+
      theme_minimal()
    ggplotly(bt.plot1)
  })
  
  output$bt.plot2 <- renderPlotly({
    bt.tbl2 <- tblt2.select.data() %>%
      group_by(., phase, action) %>%
      dplyr::summarise(., n=n())
    
    bt.tbl2.list <- list()
    for (i in unique(bt.tbl2$phase)){
      tmp <- bt.tbl2 %>% 
        filter(.,phase==i) %>%
        mutate(.,cond_prob=round(n/sum(n), digit=2))
      bt.tbl2.list[[i]] <- tmp
    }
    bt.tbl2 <- do.call(rbind, bt.tbl2.list)
    
    bt.plot2 <- ggplot(bt.tbl2)+
      ggtitle(label = paste0("Subject No.",as.integer(input$group.bt)*2))+
      geom_col(aes(x=phase, y=cond_prob, fill=action), position = position_dodge2(preserve = "single"))+
      coord_cartesian(ylim = c(0, 1))+
      theme_minimal()
    ggplotly(bt.plot2)
  })
  #7.output-behavior-analysis####
  #7.1 long-time type####
  #prepare data
  ba1.selected.data <- reactive({
    selected.group.ba <- as.integer(input$group.ba)
    p1.number.ba = selected.group.ba*2-1
    ba1.data <- fileset[[p1.number.ba]] %>% 
      mutate(., movavg = rollmean(fileset[[p1.number.ba]]$StockPrice, as.integer(input$movavg.ba),
                                  fill = NA, align = "right")) %>%
      mutate(., buy = ifelse(fileset[[p1.number.ba]]$p1Decision == "buy",1,0)
             , sell = ifelse(fileset[[p1.number.ba]]$p1Decision == "sell",1,0)
             , notrade = ifelse(fileset[[p1.number.ba]]$p1Decision == "no trade",1,0))%>% 
      select(., Trials, buy, sell, notrade, StockPrice, movavg)
    return(ba1.data)
  })
  ba2.selected.data <- reactive({
    selected.group.ba <- as.integer(input$group.ba)
    p2.number.ba = selected.group.ba*2
    ba2.data <- fileset[[p2.number.ba]] %>% 
      mutate(., movavg = rollmean(fileset[[p2.number.ba]]$StockPrice, as.integer(input$movavg.ba),
                                  fill = NA, align = "right")) %>%
      mutate(., buy = ifelse(fileset[[p2.number.ba]]$p2Decision=="buy",1,0)
             , sell = ifelse(fileset[[p2.number.ba]]$p2Decision=="sell",1,0)
             , notrade = ifelse(fileset[[p2.number.ba]]$p2Decision=="no trade",1,0))%>% 
      select(., Trials, buy, sell, notrade, StockPrice, movavg)
    return(ba2.data)
  })
  #plot
  output$movavg <- renderPlotly({
    ba1.selected.data <- ba1.selected.data()
    ba1.selected.data.long <- gather(ba1.selected.data, price_type, price, StockPrice, movavg)
    ba.plot <- ggplot(ba1.selected.data.long, aes(x = Trials, y = price, color = price_type))+
      geom_line(alpha =0.5)+
      geom_point()+
      theme_classic()
    ggplotly(ba.plot)
  })
  #table
  output$action_type <- renderTable({
    #p1
    ba1.selected.data <- ba1.selected.data()
    n <- as.integer(input$movavg.ba)
    for (i in n:101) {
      if(ba1.selected.data$movavg[i] == max(ba1.selected.data$movavg[n:101])){max.trial = i}
    }
    ba1.selected.data.roll <- ba1.selected.data %>% 
      mutate(., roll_buy = rollsum(ba1.selected.data$buy, 10, fill = NA, align = "right")
             , roll_sell = rollsum(ba1.selected.data$sell, 10, fill = NA, align = "right")
             ,roll_nt = rollsum(ba1.selected.data$notrade, 10, fill = NA, align = "right")
      ) 
    actionlist1 <- list()
    for(j in 1:as.numeric(max.trial)){
      if(ba1.selected.data.roll$roll_buy[j+9] >= 9){actionlist1 <- append(actionlist1,1)}
      else{actionlist1 <- append(actionlist1,0)}
    }
    for(k in as.numeric(max.trial):92){
      if(ba1.selected.data.roll$roll_sell[k+9] >= 9){actionlist1 <- append(actionlist1,1)}
      else{actionlist1 <- append(actionlist1,0)}
    }
    actiondf1 <- do.call(rbind, actionlist1)
    typelist1 <- list()
    if(sum(actiondf1[1:max.trial])==0){
      typelist1 <- append(typelist1, "False")
    }else{
      typelist1 <- append(typelist1, "True")}
    if(sum(actiondf1[max.trial:92])==0){
      typelist1 <- append(typelist1, "False")
    }else{
      typelist1 <- append(typelist1, "True")}
    p1 <- do.call(rbind, typelist1)
    #p2
    ba2.selected.data <- ba2.selected.data()
    for (i in n:101) {
      if(ba2.selected.data$movavg[i] == max(ba2.selected.data$movavg[n:101])){max.trial = i}
    }
    actionlist2 <- list()
    for(j in 1:as.numeric(max.trial)){
      if(sum(ba2.selected.data$buy[j:j+9]) >= 9){actionlist2 <- append(actionlist2,1)}
      else{actionlist2 <- append(actionlist2,0)}
    }
    for(k in as.numeric(max.trial):92){
      if(sum(ba2.selected.data$sell[k:k+9]) >= 9){actionlist2 <- append(actionlist2,1)}
      else{actionlist2 <- append(actionlist2,0)}
    }
    actiondf2 <- do.call(rbind, actionlist2)
    typelist2 <- list()
    if(sum(actiondf2[1:max.trial])==0){
      typelist2 <- append(typelist2, "False")
    }else{
      typelist2 <- append(typelist2, "True")}
    if(sum(actiondf2[max.trial:92])==0){
      typelist2 <- append(typelist2, "False")
    }else{
      typelist2 <- append(typelist2, "True")}
    p2 <- do.call(rbind, typelist2)
    type <- data.frame(p1 = p1, p2 = p2, row.names = c("long-position", "short-position"))
    return(type)

  }, include.rownames = TRUE)
  #7.2 mov_stock####
  #prepare data
  ms.selected.data <- reactive({
    selected.group.ba <- as.integer(input$group.ba)
    p1.number.ms = selected.group.ba*2-1
    p2.number.ms = selected.group.ba*2
    ms.data <- data.frame(
      Trials = fileset[[p1.number.ms]]$Trials,
      p1Stock = fileset[[p1.number.ms]]$p1Stock,
      p1mov_Stock = rollmean(fileset[[p1.number.ms]]$p1Stock, as.integer(input$movavg.ba),
                             fill = NA, align = "right"),
      p2Stock = fileset[[p2.number.ms]]$p2Stock,
      p2mov_Stock = rollmean(fileset[[p2.number.ms]]$p2Stock, as.integer(input$movavg.ba),
                             fill = NA, align = "right")
      )
    return(ms.data)
  })
  #plot
  output$stock <- renderPlotly({
    ms.selected.data = ms.selected.data()
    ms.selected.data.long <- gather(ms.selected.data, group, stock, p1Stock, p2Stock)
    ms.plot <- ggplot(ms.selected.data.long, aes(x = Trials, y = stock, color = group))+
      geom_line()+
      geom_point()+
      theme_classic()
    ggplotly(ms.plot)
  })
  output$mov_stock <- renderPlotly({
    ms.selected.data = ms.selected.data()
    ms.selected.data.long <- gather(ms.selected.data, group, mov_stock, p1mov_Stock, p2mov_Stock)
    ms.plot <- ggplot(ms.selected.data.long, aes(x = Trials, y = mov_stock, color = group))+
      geom_line()+
      geom_point()+
      theme_classic()
    ggplotly(ms.plot)
  })
  #7.3 action type####
  at1.selected.data <- reactive({
    selected.group.ba <- as.integer(input$group.ba)
    p1.number.ba = selected.group.ba*2-1
    at1.data <- fileset[[p1.number.ba]] %>%
      mutate(., buy = ifelse(fileset[[p1.number.ba]]$p1Decision=="buy",1,0)
             , sell = ifelse(fileset[[p1.number.ba]]$p1Decision=="sell",1,0)
             , notrade = ifelse(fileset[[p1.number.ba]]$p1Decision=="no trade",1,0))%>%
      mutate(.,mov_buy = rollsum(buy, 5, fill = NA, align = "right"),
             mov_sell = rollsum(sell, 5, fill = NA, align = "right"),
             mov_notrade = rollsum(notrade, 5, fill = NA, align = "right")) %>% 
      mutate(.,Decision = p1Decision, lag_Decision = lag(p1Decision)) %>% 
      select(., Trials, Decision, lag_Decision, mov_buy, mov_sell, mov_notrade)
  })
  at2.selected.data <- reactive({
    selected.group.ba <- as.integer(input$group.ba)
    p2.number.ba = selected.group.ba*2
    at2.data <- fileset[[p2.number.ba]] %>%
      mutate(., buy = ifelse(fileset[[p2.number.ba]]$p2Decision=="buy",1,0)
             , sell = ifelse(fileset[[p2.number.ba]]$p2Decision=="sell",1,0)
             , notrade = ifelse(fileset[[p2.number.ba]]$p2Decision=="no trade",1,0))%>%
      mutate(.,mov_buy = rollsum(buy, 5, fill = NA, align = "right"),
             mov_sell = rollsum(sell, 5, fill = NA, align = "right"),
             mov_notrade = rollsum(notrade, 5, fill = NA, align = "right")) %>% 
      mutate(.,Decision = p2Decision,lag_Decision = lag(p2Decision)) %>% 
      select(., Trials, Decision, lag_Decision, mov_buy, mov_sell, mov_notrade)
  })
  #type table
  output$p1action_type <- renderTable({
    selected.data <- at1.selected.data()
    type_list <- list()
    trials_list <- list()
    for (i in 5:101) {
      if(selected.data$mov_buy[i] >= 4){
        trials <- paste(i-4,"~",i)
        trials_list <- append(trials_list,trials)
        type_list <- append(type_list,"80% buy")
      }else if(selected.data$mov_sell[i] >= 4){
        trials <- paste(i-4,"~",i)
        trials_list <- append(trials_list,trials)
        type_list <- append(type_list,"80% sell")
      }else if(selected.data$mov_notrade[i] >= 4){
        trials <- paste(i-4,"~",i)
        trials_list <- append(trials_list,trials)
        type_list <- append(type_list,"80% no trade")
      }else{
        j = i-4
        table <- table(selected.data$Decision[j], selected.data$lag_Decision[j])
        if(table[4,2]==1){
          j = j+1
          table1 = table(selected.data$Decision[j],selected.data$lag_Decision[j])
          if(table1[2,4]==1){
            j = j+1
            table2 = table(selected.data$Decision[j],selected.data$lag_Decision[j])
            if(table2[4,2]==1){
              j = j+1
              table3 = table(selected.data$Decision[j],selected.data$lag_Decision[j])
              if(table3[2,4]==1){
                j = j+1
                table4 = table(selected.data$Decision[j],selected.data$lag_Decision[j])
                if(table4[4,2]==1){
                  trials <- paste(selected.data$Trials[j-4],"~",selected.data$Trials[j])
                  trials_list <- append(trials_list,trials)
                  type_list <- append(type_list,"cross")
                }
              }
            }
          }
        }
        if(table[2,4]==1){
          j = j+1
          table1 = table(selected.data$Decision[j],selected.data$lag_Decision[j])
          if(table1[4,2]==1){
            j = j+1
            table2 = table(selected.data$Decision[j],selected.data$lag_Decision[j])
            if(table2[2,4]==1){
              j = j+1
              table3 = table(selected.data$Decision[j],selected.data$lag_Decision[j])
              if(table3[4,2]==1){
                j = j+1
                table4 = table(selected.data$Decision[j],selected.data$lag_Decision[j])
                if(table4[2,4]==1){
                  trials <- paste(selected.data$Trials[j-4],"~",selected.data$Trials[j])
                  trials_list <- append(trials_list,trials)
                  type_list <- append(type_list,"cross")
                }
              }
            }
          }
        }
      }
    }
    trials_df <- do.call(rbind, trials_list)
    type_df <- do.call(rbind, type_list)
    action1_type <- data.frame(trials = trials_df, type = type_df)
    return(action1_type)
  })
  
  output$p2action_type <- renderTable({
    selected.data <- at2.selected.data()
    type_list <- list()
    trials_list <- list()
    for (i in 5:101) {
      if(selected.data$mov_buy[i] >= 4){
        trials <- paste(i-4,"~",i)
        trials_list <- append(trials_list,trials)
        type_list <- append(type_list,"80% buy")
      }else if(selected.data$mov_sell[i] >= 4){
        trials <- paste(i-4,"~",i)
        trials_list <- append(trials_list,trials)
        type_list <- append(type_list,"80% sell")
      }else if(selected.data$mov_notrade[i] >= 4){
        trials <- paste(i-4,"~",i)
        trials_list <- append(trials_list,trials)
        type_list <- append(type_list,"80% no trade")
      }else{
        j = i-4
        table <- table(selected.data$Decision[j], selected.data$lag_Decision[j])
        if(table[4,2]==1){
          j = j+1
          table1 = table(selected.data$Decision[j],selected.data$lag_Decision[j])
          if(table1[2,4]==1){
            j = j+1
            table2 = table(selected.data$Decision[j],selected.data$lag_Decision[j])
            if(table2[4,2]==1){
              j = j+1
              table3 = table(selected.data$Decision[j],selected.data$lag_Decision[j])
              if(table3[2,4]==1){
                j = j+1
                table4 = table(selected.data$Decision[j],selected.data$lag_Decision[j])
                if(table4[4,2]==1){
                  trials <- paste(selected.data$Trials[j-4],"~",selected.data$Trials[j])
                  trials_list <- append(trials_list,trials)
                  type_list <- append(type_list,"cross")
                }
              }
            }
          }
        }
        if(table[2,4]==1){
          j = j+1
          table1 = table(selected.data$Decision[j],selected.data$lag_Decision[j])
          if(table1[4,2]==1){
            j = j+1
            table2 = table(selected.data$Decision[j],selected.data$lag_Decision[j])
            if(table2[2,4]==1){
              j = j+1
              table3 = table(selected.data$Decision[j],selected.data$lag_Decision[j])
              if(table3[4,2]==1){
                j = j+1
                table4 = table(selected.data$Decision[j],selected.data$lag_Decision[j])
                if(table4[2,4]==1){
                  trials <- paste(selected.data$Trials[j-4],"~",selected.data$Trials[j])
                  trials_list <- append(trials_list,trials)
                  type_list <- append(type_list,"cross")
                }
              }
            }
          }
        }
      }
    }
    trials_df <- do.call(rbind, trials_list)
    type_df <- do.call(rbind, type_list)
    action2_type <- data.frame(trials = trials_df, type = type_df)
    return(action2_type)
  })
  #change point
  output$p1change_point <- renderTable({
    #p1
    selected.data1 <- at1.selected.data()
    type_list1 <- list()
    trials_s_list1 <- list()
    trials_e_list1 <- list()
    for (i in 5:101) {
      if(selected.data1$mov_buy[i] >= 4){
        trials_s_list1 <- append(trials_s_list1, i-4)
        trials_e_list1 <- append(trials_e_list1, i)
        type_list1 <- append(type_list1,"80% buy")
      }else if(selected.data1$mov_sell[i] >= 4){
        trials_s_list1 <- append(trials_s_list1, i-4)
        trials_e_list1 <- append(trials_e_list1, i)
        type_list1 <- append(type_list1,"80% sell")
      }else if(selected.data1$mov_notrade[i] >= 4){
        trials_s_list1 <- append(trials_s_list1, i-4)
        trials_e_list1 <- append(trials_e_list1, i)
        type_list1 <- append(type_list1,"80% no trade")
      }else{
        j = i-4
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
                  trials_s_list1 <- append(trials_s_list1, selected.data1$Trials[j-4])
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
                  trials_s_list1 <- append(trials_s_list1, selected.data1$Trials[j-4])
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
  
  })
  output$p2change_point <- renderTable({
    #p2
    selected.data2 <- at2.selected.data()
    type_list2 <- list()
    trials_s_list2 <- list()
    trials_e_list2 <- list()
    for (i in 5:101) {
      if(selected.data2$mov_buy[i] >= 4){
        trials_s_list2 <- append(trials_s_list2, i-4)
        trials_e_list2 <- append(trials_e_list2, i)
        type_list2 <- append(type_list2,"80% buy")
      }else if(selected.data2$mov_sell[i] >= 4){
        trials_s_list2 <- append(trials_s_list2, i-4)
        trials_e_list2 <- append(trials_e_list2, i)
        type_list2 <- append(type_list2,"80% sell")
      }else if(selected.data2$mov_notrade[i] >= 4){
        trials_s_list2 <- append(trials_s_list2, i-4)
        trials_e_list2 <- append(trials_e_list2, i)
        type_list2 <- append(type_list2,"80% no trade")
      }else{
        j = i-4
        table <- table(selected.data2$Decision[j], selected.data2$lag_Decision[j])
        if(table[4,2]==1){
          j = j+1
          table1 = table(selected.data2$Decision[j],selected.data2$lag_Decision[j])
          if(table1[2,4]==1){
            j = j+1
            table2 = table(selected.data2$Decision[j],selected.data2$lag_Decision[j])
            if(table2[4,2]==1){
              j = j+1
              table3 = table(selected.data2$Decision[j],selected.data2$lag_Decision[j])
              if(table3[2,4]==1){
                j = j+1
                table4 = table(selected.data2$Decision[j],selected.data2$lag_Decision[j])
                if(table4[4,2]==1){
                  trials_s_list2 <- append(trials_s_list2, selected.data2$Trials[j-4])
                  trials_e_list2 <- append(trials_e_list2, selected.data2$Trials[j])
                  type_list2 <- append(type_list2,"cross")
                }
              }
            }
          }
        }
        if(table[2,4]==1){
          j = j+1
          table1 = table(selected.data2$Decision[j],selected.data2$lag_Decision[j])
          if(table1[4,2]==1){
            j = j+1
            table2 = table(selected.data2$Decision[j],selected.data2$lag_Decision[j])
            if(table2[2,4]==1){
              j = j+1
              table3 = table(selected.data2$Decision[j],selected.data2$lag_Decision[j])
              if(table3[4,2]==1){
                j = j+1
                table4 = table(selected.data2$Decision[j],selected.data2$lag_Decision[j])
                if(table4[2,4]==1){
                  trials_s_list2 <- append(trials_s_list2, selected.data2$Trials[j-4])
                  trials_e_list2 <- append(trials_e_list2, selected.data2$Trials[j])
                  type_list2 <- append(type_list2,"cross")
                }
              }
            }
          }
        }
      }
    }
    trials_s_df2 <- do.call(rbind, trials_s_list2)
    trials_e_df2 <- do.call(rbind, trials_e_list2)
    type_df2 <- do.call(rbind, type_list2)
    change_point.data2 <- data.frame(start = trials_s_df2, end = trials_e_df2, type = type_df2)
    change_point_list2 <- list()
    for(n in 1:length(change_point.data2$type)){
      for (m in 1:length(change_point.data2$type)){
        if(change_point.data2$end[n]+2 == change_point.data2$start[m]){
          if(change_point.data2$type[n] != change_point.data2$type[m]){
            change_point_list2 <- append(change_point_list2, change_point.data2$end[n]+1)
          }
        }
      }
    }
    change_point2 <- do.call(rbind, change_point_list2)
    p2change_point <- data.frame(p2 = change_point2)
    return(p2change_point)
  })
}