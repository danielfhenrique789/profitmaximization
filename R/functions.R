packages <- c(
  "quantmod",
  "data.table",
  "ggplot2",
  "plotly",
  "shiny",
  "forecast",
  "tseries",
  "shiny",
  "ggplot2",
  "plotly"
)
instPack <- setdiff(packages, rownames(installed.packages()))
if (length(instPack) > 0) {
  install.packages(instPack)
}
# install.packages("quantmod")
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("plotly")
# install.packages("shiny")
# install.packages("forecast")
# install.packages("tseries")
# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("plotly")
# 
# library(quantmod)
# library(data.table)
# library(ggplot2)
# library(plotly)
# library(shiny)
# library('forecast')
# library(tseries)
# library(shiny)
# library(ggplot2)
# library(plotly)

toFactor <- function(column,lev){
  return(factor(tolower(sub(" ","",column)),ordered = TRUE, levels = lev));
}
getTickets <- function(){
  return(as.character(read.csv("data/tickets.csv")[[1]]));
}
addTicket <- function(pTickets){
  tryCatch({
    tickets <- getTickets()
    x <- c(tickets[1:NROW(tickets)], toupper(pTickets))
    write.csv(factor(x), file="tickets.csv", row.names = FALSE)
  }, 
  error=function(e){
    print(e)
  })
}
formateTicketDf <- function(df){
  names(df) <- c("Date","Open","Close","High","Low","Volume","Adjusted")
  return(df)
}
write.csv(getTickets(), file="tempTickets.csv", row.names = FALSE)
updateDatasets <- function(){
  tryCatch({
    tickets <- as.character(read.csv("data/tempTickets.csv")[[1]])
    print(NROW(tickets))
    for(tticket in tickets){
      
      tickets <- tickets[ tickets != as.character(tticket) ]
      ticket <- paste(tticket,".SA",sep = "")
      if(file.exists(paste("./Datasets/",ticket,".csv",sep = ""))){
        #obj[[ticket]] <- read.csv(paste("./Datasets/",ticket,".csv",sep = ""))
      }
      
      dataset <- as.data.frame(getSymbols(Symbols = ticket, auto.assign = F,symbol.lookup = TRUE))
      setDT(dataset, keep.rownames = TRUE)[]
      dataset <- formateTicketDf(dataset)
      dataset$Amplitude <- dataset$High - dataset$Low
      write.csv(dataset, file = paste("Datasets/",ticket,".csv",sep = ""), row.names = FALSE)
    }
  }, 
  error=function(e){
    print("Error:")
    print(e)
    write.csv(tickets, file="tempTickets.csv", row.names = FALSE)
    updateDatasets()
  })
}
getEnv <- function(){
  tickets <- paste(getTickets(),".SA",sep = "")
  obj <- new.env()
  objTickets <- c();
  for(ticket in tickets){
    if(file.exists(paste("./Datasets/",ticket,".csv",sep = ""))){
      obj[[ticket]] <- read.csv(paste("./Datasets/",ticket,".csv",sep = ""))
      objTickets <- c(objTickets,ticket)
    }
  }
  write.csv(objTickets, file="objTickets.csv", row.names = FALSE)
  return(obj);
}
getLastTicket <- function(df){
  return(df[nrow(df),])
}
getTicketsObsFromDateRate <- function(df,from,to){
  return(df[as.Date(df$Date) >= as.Date(from) & as.Date(df$Date) <= as.Date(to),])
}
getTicketsFromDaysAgo <- function(df,from){
  lastYear <- as.Date(getLastTicket(df)$Date) - from
  return(getTicketsObsFromDateRate(df,lastYear,Sys.Date()))
}
getLastYear <- function(df){
  return(getTicketsFromDaysAgo(df,365))
}
getLastYear <- function(df){
  return(getTicketsFromDaysAgo(df,365))
}
getLastSemester <- function(df){
  return(getTicketsFromDaysAgo(df,182))
}
getLastTrimester <- function(df){
  return(getTicketsFromDaysAgo(df,90))
}
getLastMonth <- function(df){
  return(getTicketsFromDaysAgo(df,30))
}
getLastWeek <- function(df){
  return(getTicketsFromDaysAgo(df,7))
}
getLast3Days <- function(df){
  return(getTicketsFromDaysAgo(df,3))
}
getLastLastWeek <- function(df){
  firstDay <- as.Date(getLastTicket(df)$Date) - 16
  lastDay <- as.Date(getLastTicket(df)$Date) - 8
  return(getTicketsObsFromDateRate(df,firstDay,lastDay))
}
getSpeed <- function(df){
  lf <- 7
  ldf <- tail(df,lf)
  lmdf <- lm(ldf[1:(lf - 1),]$Adjusted ~ index(ldf[1:(lf - 1),]$Date))
  lmdff <- lm(ldf[2:lf,]$Adjusted ~ index(ldf[2:lf,]$Date))
  return(as.numeric(lmdff$coefficients[2]) - as.numeric(lmdf$coefficients[2]))
}
getFundamentusData <- function(){
  fund <- read.csv("data/BusinessJson.csv")
  fund$papel <- paste(fund$papel,".SA",sep = "")
  return(fund)
}
updateAnalises <- function(obj){
  tickets <- paste(getTickets(),".SA",sep = "")
  fund <- getFundamentusData()
  ticketValue <- c()
  name <- c()
  setor <- c()
  subsetor <- c()
  percW <- c()
  percM <- c()
  percT <- c()
  maxM <- c()
  maxS <- c()
  maxY <- c()
  mediaAmpl <- c()
  concavity <- c()
  corWeek <- c()
  corMonth <- c()
  corTri <- c()
  volD <- c()
  caD <- c()
  vD <- c()
  volW <- c()
  caW <- c()
  caW2 <- c()
  bW <- c()
  caM <- c()
  caT <- c()
  sigW <- c()
  sigM <- c()
  sigT <- c()
  for(ticket in tickets){
    tryCatch({
      name <- c(name,ifelse((fund[fund$papel == ticket,]$empresa != ""),as.character(fund[fund$papel == ticket,]$empresa),NA))
      setor <- c(setor,ifelse((fund[fund$papel == ticket,]$setor != ""),as.character(fund[fund$papel == ticket,]$setor),NA))
      subsetor <- c(subsetor,ifelse((fund[fund$papel == ticket,]$subsetor != ""),as.character(fund[fund$papel == ticket,]$subsetor),NA))
      if(!is.null(obj[[ticket]])){
        lastTicket <- getLastTicket(obj[[ticket]])
        ticketValue <- c(ticketValue,lastTicket[1,]$Adjusted)
        mediaAmpl <- c(mediaAmpl,ifelse(is.numeric(lastTicket[1,]$Amplitude) ,abs(lastTicket[1,]$Amplitude),NULL))
        D3 <- getLast3Days(obj[[ticket]])
        D3 <- D3[!is.na(D3$Adjusted),]
        week <- getLastWeek(obj[[ticket]])
        week <- week[!is.na(week$Open) | !is.na(week$Close) | !is.na(week$High) | !is.na(week$Low) | !is.na(week$Volume) | !is.na(week$Adjusted) | !is.na(week$Amplitude),]
        week2 <- getLastLastWeek(obj[[ticket]])
        week2 <- week2[!is.na(week2$Open) | !is.na(week2$Close) | !is.na(week2$High) | !is.na(week2$Low) | !is.na(week2$Volume) | !is.na(week2$Adjusted) | !is.na(week2$Amplitude),]
        month <- getLastMonth(obj[[ticket]])
        month <- week[!is.na(week$Open) | !is.na(week$Close) | !is.na(week$High) | !is.na(week$Low) | !is.na(week$Volume) | !is.na(week$Adjusted) | !is.na(week$Amplitude),]
        trimester <- getLastTrimester(obj[[ticket]])
        trimester <- trimester[!is.na(trimester$Open) | !is.na(trimester$Close) | !is.na(trimester$High) | !is.na(trimester$Low) | !is.na(trimester$Volume) | !is.na(trimester$Adjusted) | !is.na(trimester$Amplitude),]
        semester <- getLastSemester(obj[[ticket]])
        semester <- semester[!is.na(semester$Open) | !is.na(semester$Close) | !is.na(semester$High) | !is.na(semester$Low) | !is.na(semester$Volume) | !is.na(semester$Adjusted) | !is.na(semester$Amplitude),]
        year <- getLastYear(obj[[ticket]])
        year <- year[!is.na(year$Open) | !is.na(year$Close) | !is.na(year$High) | !is.na(year$Low) | !is.na(year$Volume) | !is.na(year$Adjusted) | !is.na(year$Amplitude),]
        percW <- c(percW,(week[nrow(week),]$Close*100) / (week[1,]$Open) - 100)
        percM <- c(percM,(month[nrow(month),]$Close*100) / (month[1,]$Open) - 100)
        percT <- c(percT,(trimester[nrow(trimester),]$Close*100) / (trimester[1,]$Open) - 100)
        maxM <- c(maxM,max(month[!is.na(month$Adjusted),]$Adjusted))
        maxS <- c(maxS,max(semester[!is.na(semester$Adjusted),]$Adjusted))
        maxY <- c(maxY,max(year[!is.na(year$Adjusted),]$Adjusted))
        lmD3 <- lm(D3$Adjusted ~ index(D3$Date))
        lmWeek <- lm(week$Adjusted ~ index(week$Date))
        lmWeek2 <- lm(week2$Adjusted ~ index(week2$Date))
        lmMonth <- lm(month$Adjusted ~ index(month$Date))
        lmTri <- lm(trimester$Adjusted ~ index(trimester$Date))
        corWeek <- c(corWeek,cor(week$Adjusted,index(week)))
        corMonth <- c(corMonth,cor(month$Adjusted,index(month)))
        corTri <- c(corTri,cor(trimester$Adjusted,index(trimester)))
        volW <- c(volW,mean(week$Volume))
        caD <- c(caD,summary(lmD3)$coefficients[2])
        vD <- c(vD,getSpeed(obj[[ticket]]))
        caW <- c(caW,summary(lmWeek)$coefficients[2])
        caW2 <- c(caW2,summary(lmWeek2)$coefficients[2])
        bW <- c(bW,summary(lmWeek)$coefficients[1])
        caM <- c(caM,summary(lmMonth)$coefficients[2])
        caT <- c(caT,summary(lmTri)$coefficients[2])
        sigW <- c(sigW,summary(lmWeek)$sigma)
        sigM <- c(sigM,summary(lmMonth)$sigma)
        sigT <- c(sigT,summary(lmTri)$sigma)
        concavity <- ifelse(!is.na(caW) & !is.na(caW2),caW - caW2,NA)
      }
      else{
        tickets <- tickets[!tickets %in% ticket]
      }
    }, 
    error=function(e){
      print("Error:")
      print(e)
    })
  }
  print(NROW(tickets[1:379]))
  print(NROW(name[1:379]))
  print(NROW(ticketValue[1:379]))
  print(NROW(setor[1:379]))
  print(NROW(subsetor[1:379]))
  print(NROW(maxM[1:379]))
  print(NROW(maxS[1:379]))
  print(NROW(maxY[1:379]))
  print(NROW(percW[1:379]))
  print(NROW(percM[1:379]))
  print(NROW(percT[1:379]))
  print(NROW(mediaAmpl[1:379]))
  print(NROW(concavity[1:379]))
  print(NROW(volW[1:379]))
  print(NROW(caD[1:379]))
  print(NROW(vD[1:379]))
  print(NROW(corWeek[1:379]))
  print(NROW(caW[1:379]))
  print(NROW(bW[1:379]))
  print(NROW(sigW[1:379]))
  print(NROW(corMonth[1:379]))
  print(NROW(caM[1:379]))
  print(NROW(sigM[1:379]))
  print(NROW(corTri[1:379]))
  print(NROW(caT[1:379]))
  print(NROW(sigT[1:379]))
  df <- data.frame(tickets[1:379],name[1:379],ticketValue[1:379],setor[1:379],subsetor[1:379],maxM[1:379],maxS[1:379],maxY[1:379],percW[1:379],percM[1:379],percT[1:379],mediaAmpl[1:379],concavity[1:379],volW[1:379],caD[1:379],vD[1:379],corWeek[1:379],caW[1:379],bW[1:379],sigW[1:379],corMonth[1:379],caM[1:379],sigM[1:379],corTri[1:379],caT[1:379],sigT[1:379])
  names(df) <- c("Ticket","Name","Value","Sector","Subsector","MaxM","MaxS","MaxY","PercW","PercM","PercT","MediaAmpl","Concavity","VolW","CaD","VD","CorrW","CaW","BW","SigW","CorrM","CaM","BM","SigM","CorrT","CaT","BT","SigT")
  return(df)
}
getVarCaFiles <- function(dir){
  return(list.files(path = paste("Analises/VarCA/",dir,sep = "")))
}
createDirectory <- function(pathName,dirName){
  if(!dir.exists(file.path(pathName, dirName))){
    dir.create(file.path(pathName, dirName), showWarnings = FALSE)
  }
}

updateVarCATicket <- function(pDf,dfName){
  objDf <- pDf[!is.na(pDf$Adjusted),]
  fieldsName <- c("Date","Ticket","Predict","CA","b")
  if(file.exists(paste("./Analises/VarCA/",dfName,".csv",sep = ""))){
    print(nrow(df)+1)
    if(!is.null(objDf[nrow(df)+1,])){
      to <- nrow(df)+1
      from <- ifelse(to - 7 <= 0,1,to - 7)
      objDfLm <- objDf[from:to,]
      #print(objDfLm$Adjusted)
      lmdf <- lm(objDfLm$Adjusted ~ index(objDfLm$Date))
      ca <- ifelse(is.na(summary(lmdf)$coefficients[2]),0,summary(lmdf)$coefficients[2])
      b <- summary(lmdf)$coefficients[1]
      pred <- ca*(to + 1) + b
      dfnew <- data.frame(objDf[to,]$Date,objDf[to,]$Adjusted,pred,ca,b)
      names(dfnew) <- fieldsName
      df <- rbind(df,dfnew)
      write.csv(df, file=paste("./Analises/VarCA/",dfName,".csv",sep = ""), row.names = FALSE)
      updateVarCATicket(pDf,dfName)
    }
  }else{
    df <- data.frame(objDf[1,]$Date,objDf[1,]$Adjusted,0,0,0)
    names(df) <- fieldsName
    write.csv(df, file=paste("Analises/VarCA/",dfName,".csv",sep = ""), row.names = FALSE)
    updateVarCATicket(pDf,dfName)
  }
}
updateVarCA <- function(obj){
  tickets <- paste(getTickets(),".SA",sep = "")
  for(ticket in tickets){
    updateVarCATicket(obj[[ticket]],ticket)
  }
}

analiseTendencia <- function(obj,vFast,vSlow){
  tickets <- as.character(read.csv("data/objTickets.csv")[[1]])
  newObj <- new.env()
  for(ticket in tickets){
    tryCatch({
      dataRange <- obj[[ticket]][!is.na(obj[[ticket]]$Adjusted),]
      if(nrow(dataRange) > vSlow){
        ma <- SMA(dataRange$Adjusted, n=vSlow)
        mar <- SMA(dataRange$Adjusted, n=vFast)
        newObj[[ticket]] <- data.frame(dataRange[vSlow:nrow(dataRange),]$Date,dataRange[vSlow:nrow(dataRange),]$Adjusted,ma[vSlow:nrow(dataRange)],mar[vSlow:nrow(dataRange)])
        names(newObj[[ticket]]) <- c("Date","Ticket","MA.Slow", "MA.Fast")
        newObj[[ticket]]$Tendency <- ifelse((newObj[[ticket]]$MA.Slow < newObj[[ticket]]$MA.Fast),"up","down")
      }
    }, 
    error=function(e){
      print("Error:")
      print(e)
    })
  }
  return(newObj)
}



updateCATickets <- function(obj){
  tickets <- as.character(read.csv("data/objTickets.csv")[[1]])
  newObj <- new.env()
  count <- NROW(tickets)
  for(ticket in tickets){
    tryCatch({
      print(ticket)
      dataRange <- obj[[ticket]][!is.na(obj[[ticket]]$Ticket),]
      newObj[[ticket]]$Date <- dataRange$Date
      newObj[[ticket]]$CA <- sapply(1:nrow(dataRange),function(x){
        ve <- as.data.frame(dataRange[x:(x+7),])
        return(summary(lm(ve$Ticket ~ index(ve$Ticket)))$coefficients[2])
      })
      #[1:(nrow(dataRange) - 25)]) #<- getWeekCATicket(dataRange$Adjusted) newObj[[ticket]]$CA
      print(count)
      count <- count - 1
      write.csv(newObj[[ticket]], file = paste("Analises/Tendencias/CAs/",ticket,".csv",sep = ""), row.names = FALSE)
    }, 
    error=function(e){
      print(paste("Error:",e))
    })
  }
  return(newObj)
}

plotTendencyWithMA <- function(ds){
  ggplot() +
    geom_line(data = ds, aes(x = index(Date), y = Ticket, colour = "Tickets")) +
    geom_line(data = ds, aes(x = index(Date), y = MA.Slow,   colour = "Média lenta"))  +
    geom_line(data = ds, aes(x = index(Date), y = MA.Fast, colour = "Média rápida"))  +
    ylab('Cotação')
}

plotTendency <- function(ds){
  ggplot() +
    geom_point(data = ds, aes(x = index(Date), y = Ticket, color = Tendency)) +
    ylab('Cotação')
}

getPotencialGain <- function(obj, from, to){
  tickets <- as.character(read.csv("data/objTickets.csv")[[1]])
  newObj <- new.env()
  for(ticket in tickets){
    print(ticket)
    ds <- obj[[ticket]]
    lfrom <- from
    year <- c()
    gain <- c()
    if(class(ds) == "data.frame"){
      while(as.numeric(lfrom) <= as.numeric(to)){
        gain <- c(gain,getPotencialYearGain(ds,lfrom))
        year <- c(year,lfrom)
        lfrom <- as.character(as.numeric(lfrom) + 1)
      }
      newObj[[ticket]] <- data.frame(year,gain)
      #newObj[[ticket]]$Year <- year
      #newObj[[ticket]]$Gain <- gain
    }
  }
  return(newObj)
}

getPotencialYearGain <- function(ds,year){
  from <- paste(year,"-01-01",sep = "")
  to <- paste(year,"-12-31",sep = "")
  lucro <- 0
  #print(from)
  #print(to)
  ds <- ds[as.character(ds$Date) >= from & as.character(ds$Date) <= to,]
  if(nrow(ds) > 0){
    #print(nrow(ds))
    first <- head(ds,1)
    #print(first)
    tendency <- first$Tendency
    buy <- ifelse(tendency == "up",first$Ticket,0)
    sell <- 0
    #print(paste("Compra: ",buy))
    for(idx in 1:nrow(ds)){
      day <- ds[idx,]
      if(day$Tendency != tendency){
        tendency <- day$Tendency
        if(day$Tendency == "up"){
          buy <- day$Ticket
          #print(paste("Compra: ",buy))
        }
        else{
          if(buy != 0){
            sell <- day$Ticket
            #print(paste("Venda: ",sell))
            lucro <- lucro + ((sell/buy) - 1) * 100
            buy <- 0
          }
        }
      }
    }
    if(buy != 0){
      sell <- day$Ticket
      #print(paste("Venda: ",sell))
      lucro <- lucro + ((sell/buy) - 1) * 100
    }
  }
  return(lucro)
}
getWeekCATicket <- function(tickets){
  return(c(-1,-1,-1,-1,-1,-1,getCATickets(tickets)))
}
getCATickets <- function(tickets){
  #tickets <- tickets[!is.na(tickets)]
  tryCatch({
    wTickets <- as.data.frame(tickets[1:7])
    wlm <- c(summary(lm(wTickets[,1] ~ index(wTickets[,1])))$coefficients[2])
    if((NROW(tickets) - 6) > 1 ){
      return(c(wlm,getCATickets(tickets[-1])))
    }
    else{
      return(wlm)
    }
  }, 
  error=function(e){
    print(paste("Error:",e))
    return(c(-1))
  })
}

getTendencyModel <- function(en,CAs){
  tickets <- as.character(read.csv("data/objTickets.csv")[[1]])
  newObj <- new.env()
  for(ticket in tickets){
    lengthDs <- nrow(en[[ticket]])
    # print(ticket)
    # print(lengthDs)
    # print(nrow(en[[ticket]][7:lengthDs,]))
    # print(NROW(CAs[[ticket]]$CA[1:(lengthDs - 6)]))
    newObj[[ticket]] <- en[[ticket]]
    newObj[[ticket]]$CA <- -1
    
    if(!is.null(lengthDs))
      if((lengthDs - 12) > 0)
        newObj[[ticket]][7:(lengthDs - 6),]$CA <- CAs[[ticket]]$CA[1:(lengthDs - 12)]
    
    newObj[[ticket]]$Tendency <- ifelse((newObj[[ticket]]$MA.Fast > newObj[[ticket]]$MA.Slow) & (newObj[[ticket]]$CA > 0),"up","down")
  }
  return(newObj)
}

getSectors <- function(ds){
  return(unique(ds$setor))
}

getTicketsBySetor <- function(sector,ds){
  df <- unique(ds[ds$setor == sector & !is.na(ds$setor),]$tickets)
  return(df)
}

getPotencialGainBySector <- function(obj,regr){
  sectors <- getSectors(regr)
  newObj <- new.env()
  df <- data.frame()
  for(sector in sectors){
    tickets <- getTicketsBySetor(sector,regr)
    tickets <- tickets[!is.na(tickets)]
    #print(is.na(tickets[1]))
    if(!is.na(tickets[1])){
      year <- obj[[as.character(tickets[1])]]$year
      gain <- (1:nrow(obj[[as.character(tickets[1])]]))*0
      for(ticket in tickets){
        #print(ticket)
        ds <- obj[[ticket]]
        if(!is.null(ds)){
          for(idx in 1:nrow(ds)) {
            gain <- gain + ds$gain
          }
        }
      }
    }
    df <- rbind(df,data.frame(year,(gain/NROW(tickets)),sector))
    newObj[[sector]] <- df
  }
  names(df) <- c("year","gain","sector")
  return(df)
}

plotPotencialGain <- function(ds,regr,shiny){
  
  if(!shiny){
    ggplot() +
      geom_line(data = ds[!is.na(ds$sector),], aes(x = year, y = gain, group = sector, color = sector)) +
      scale_y_log10() +
      ylab('Cotação')
  }else{
    ui <- fluidPage(
      plotlyOutput("distPlot")
    )
    
    server <- function(input, output) {
      output$distPlot <- renderPlotly({
        ggplot() +
          geom_line(data = ds[!is.na(ds$sector),], aes(x = year, y = gain, group = sector, color = sector)) +
          scale_y_log10() +
          ylab('Cotação')
      })
    }
    
    shinyApp(ui = ui, server = server)
  }
}
