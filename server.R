#import package data
library(shiny)
library(tidyverse)
library(psych)
library(plyr)
library(plotly)
options(scipen = 999)
#setwd("D:/ebg408/corshiny/data")
filepath = "data"
filenames = list.files(path = filepath, pattern="*.csv")
fileset <- lapply(paste0(filepath,"/",filenames), read.csv)

server <- function(input, output) {
  
  #1.output-correlation ####
  #1.1 correlation ----
  output$tables <- renderDataTable({
    #create cortable
    full.cor.list <- list()
    groups <- strtoi(input$groups)
    for (i in c(groups*2-1,groups*2)) {
      no.subject <- substr(filenames[i],1,3)
      #data cleaning
      if (i%%2==1){
        data.self <- fileset[[i]]
        data.counter <- fileset[[i+1]]
        
        data.self <- data.self%>%
          mutate(#dummy coding
            buy = ifelse(data.self$p1Decision == "buy", 1, 0),
            notrade = ifelse(data.self$p1Decision == "no trade", 1, 0),
            sell = ifelse(data.self$p1Decision == "sell", 1, 0),
            #variables
            dprice = StockPrice-lag(StockPrice),
            dstock = p1Stock-lag(p1Stock),
            dstock.j = data.counter$p2Stock-lag(data.counter$p2Stock),
            totalasset.ij = p1TotalAsset-data.counter$p2TotalAsset
          )
      }
      else{
        data.self <- fileset[[i]]
        data.counter <- fileset[[i-1]]
        data.self <- data.self%>%
          mutate(#dummy coding
            buy = ifelse(data.self$p2Decision == "buy", 1, 0),
            notrade = ifelse(data.self$p2Decision == "no trade", 1, 0),
            sell = ifelse(data.self$p2Decision == "sell", 1, 0),
            #variables
            dprice = StockPrice-lag(StockPrice),
            dstock = p2Stock-lag(p2Stock),
            dstock.j = data.counter$p1Stock-lag(data.counter$p1Stock),
            totalasset.ij = p2TotalAsset-data.counter$p1TotalAsset
          )
      }
      
      #calculate cor
      data.self <- data.self %>% filter(Trials %in% (seq(input$trials[1],input$trials[2])))
      str.var.list <- c("subject",
                        "StockPrice",
                        "dprice",
                        "dstock",
                        "dstock.j",
                        "totalasset.ij")
      cor.list <- list()
      cor.list <- lapply(str.var.list[2:6], function(var) {
        p.value = corr.test(data.self[var],data.self[input$action])$p
        cor.value = corr.test(data.self[var],data.self[input$action])$r
        if (p.value <= input$threshold){
          cor.list$var <- cor.value
        } else{
          cor.list$var <- NA
        }
      })
      
      full.cor.list[[i]] = append(no.subject,cor.list)
      
    }
    
    
    cortable <- do.call(rbind.data.frame, full.cor.list)
    colnames(cortable) <- str.var.list
    "cortable <- data.frame(cortable , row.names = 1)"
    cortable <- data.frame(cortable[,input$show_vars, drop = FALSE])
    return(cortable)
    
  },
  options = list(lengthChange = FALSE))
  
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
  #change point plot
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
  
}